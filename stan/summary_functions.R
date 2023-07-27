# functions to interpret output of model fits

get_regression_coefficients <- function(fit, dat) {
  if (dat$stan_dat$num_item_predictors == 0) return()
  smry <- fit$summary("beta_item")
  smry$variable <- colnames(dat$stan_dat$X_item)
  smry
}

get_item_values <- function(fit, dat, rescale = FALSE) {
  if (isTRUE(rescale)) {
    drws <- exp(fit$draws("theta_item"))
    drws[,,] <- aperm(apply(drws, c(1, 2), \(x) x / sum(x)), c(2, 3, 1))
    smry <- summary(drws)
  } else {
    smry <- fit$summary("theta_item")
  }
  smry$variable <- levels(dat$rank_dat$word)[parse_number(smry$variable)]
  smry
}

plot_coefs <- function(item_values) {
  item_values |> 
    arrange(mean) |> 
    mutate(variable = as_factor(variable)) |> 
    ggplot(aes(x = mean, xmin = q5, xmax = q95, y = variable)) +
    geom_pointrange()
}

loglik_per_trial <- function(fit, dat) {
  dat$rank_expanded |> 
    summarize(loglik_weight = first(loglik_weight), .groups = "drop") |> 
    mutate(loglik = fit$summary("log_lik")$mean) |> 
    mutate(ll_w = loglik_weight * loglik) |> 
    summarize(ll = sum(ll_w), .by = c(subj_id, trial)) |> 
    arrange(subj_id, trial)
}

mean_loglik_per_subj <- function(fit, dat) {
  loglik_per_trial(fit, dat) |> summarize(ll = mean(ll), .by = subj_id)
}
