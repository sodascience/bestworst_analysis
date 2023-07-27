// rank-ordered logit regression in stan
functions {
  // likelihood; see http://www.glicko.net/research/multicompetitor.pdf
  real rank_ordered_logit(vector ordered_skills, int Mk) {
    real ll = 0;
    for (m in 1:(Mk - 1)) {
      ll += ordered_skills[m] - log_sum_exp(ordered_skills[m:Mk]);
    }
    return ll;
  }
}
data {
  // metadata
  int<lower=0> num_obs;             // total number of observations
  int<lower=0> num_trials;          // total number of (pseudo)trials
  int<lower=0> num_items;           // total number of words
  int<lower=0> num_item_predictors; // number of word-level predictors
  
  // data
  // ranked_item_ids is an integer vector which contains the item ids in 
  // the order of the ranking for each (pseudo)trial 
  array[num_obs] int<lower=1, upper=num_items> ranked_item_ids; 
  
  // items_per_trial indicates for each (pseudo)trial how many items were compared
  // this is used to slice the ranked_item_ids into separate trial results
  array[num_trials] int<lower=1> items_per_trial;
  
  // loglik_weight is used to weigh the log-likelihood in case there was a tie in a trial:
  // then all rankings consistent with the trial are considered as "pseudotrials"
  // which then sum to a weight of 1.
  vector<lower=0>[num_trials] loglik_weight; 
  
  // X_item is the design matrix of the linear regression on the latent log-worth scale
  // it has num_items rows and num_item_predictors columns.
  matrix[num_items, num_item_predictors] X_item;
}

parameters {
  // use non-centered parameterization for random intercept
  vector[num_items] random_intercept_item;
  real<lower=0> sigsq_item;
  vector[num_item_predictors] beta_item; 
}

transformed parameters {
  vector[num_items] theta_item;
  theta_item = sigsq_item * random_intercept_item + X_item * beta_item;
}

model {
  // priors
  random_intercept_item ~ std_normal();
  sigsq_item ~ student_t(3, 0, 2.5);
  beta_item ~ student_t(3, 0, 2.5);
  
  // ROL likelihood
  int pos = 1; // current position in outcome vectors
  for (k in 1:num_trials) {
    int m_k = items_per_trial[k];
    array[m_k] int item_idx = segment(ranked_item_ids, pos, m_k);
    target += rank_ordered_logit(theta_item[item_idx], m_k) * loglik_weight[k];
    pos = pos + m_k;
  }
}

generated quantities {
  // log-likelihood
  vector[num_trials] log_lik;
  int pos = 1; // current position in outcome vectors
  for (k in 1:num_trials) {
    int m_k = items_per_trial[k];
    array[m_k] int item_idx = segment(ranked_item_ids, pos, m_k);
    log_lik[k] = rank_ordered_logit(theta_item[item_idx], m_k) * loglik_weight[k];
    pos = pos + m_k;
  }
}
