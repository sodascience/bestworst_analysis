# function to filter full rank data by association & wordtype and then return 
# sub-dataframes as well as a stan data list
stan_dat_filter <- function(rank_dat, item_dat, assoc = "kwaadaardigheid", wrdtp = "voornamen") {
  rank_sub <- 
    rank_dat |> 
    filter(association == assoc, wordtype == wrdtp) |>
    mutate(word = fct_drop(word))
  
  rank_expanded <- create_pseudo_trials(rank_sub)
  
  if (missing(item_dat)) return(list(
    rank_dat = rank_sub, 
    rank_expanded = rank_expanded, 
    stan_dat = create_stan_dat(rank_expanded)
  ))
  
  item_sub <- 
    item_dat |> 
    filter(wordtype == wrdtp) |> 
    mutate(word = fct_drop(word)) |> 
    select(-wordtype)
  
  return(list(
    rank_dat = rank_sub,
    rank_expanded = rank_expanded,  
    item_dat = item_sub, 
    stan_dat = create_stan_dat(rank_expanded, item_sub)
  ))
}

create_pseudo_trials <- function(rank_dat) {
  # there are two options for each incomplete ranking, 
  # average the log-likelihood for both options
  # see https://docs.displayr.com/wiki/Rank-Ordered_Logit_Model_With_Ties
  rank_dat |> 
    group_by(subj_id, trial) |> 
    arrange(ranking, .by_group = TRUE) |> 
    mutate(rank_1 = 1:n()) |> 
    mutate(rank_2 = ifelse(rank_1 == 2, 3, ifelse(rank_1 == 3, 2, rank_1))) |> 
    select(-ranking) |> 
    pivot_longer(
      cols = starts_with("rank"), 
      names_to = "pseudotrial", 
      names_prefix = "rank_", 
      values_to = "rank", 
      names_transform = parse_integer
    ) |> 
    group_by(subj_id, trial) |> 
    mutate(loglik_weight = 1/max(pseudotrial)) |> 
    group_by(subj_id, trial, pseudotrial) |> 
    arrange(rank, .by_group = TRUE)
}

# function to translate rank data into stan data
create_stan_dat <- function(rank_expanded, item_dat) {
  stan_dat <- list(
    num_obs = nrow(rank_expanded),
    num_trials = n_groups(rank_expanded),
    num_items = nlevels(rank_expanded$word),
    ranked_item_ids = as.integer(rank_expanded$word),
    items_per_trial = rank_expanded |> summarise(nwords = n(), .groups = "drop") |> pull(nwords),
    loglik_weight = rank_expanded |> summarise(wt = first(loglik_weight), .groups = "drop") |> pull(wt)
  )
  
  if (missing(item_dat)) {
    stan_dat$num_item_predictors <- 0
    stan_dat$X_item <- matrix(nrow = stan_dat$num_items, ncol = 0)
    return(stan_dat)
  } else {
    # make sure the item predictors are in the same order as the item ids
    join_idx <- match(levels(rank_expanded$word), item_dat |> pull(word))
    item_dat <- item_dat[join_idx, ]
    stan_dat$X_item <- model.matrix(word ~ ., item_dat)[, -1, drop = FALSE] # remove intercept!
    stan_dat$num_item_predictors <- ncol(stan_dat$X_item)
    return(stan_dat)
  }
}

