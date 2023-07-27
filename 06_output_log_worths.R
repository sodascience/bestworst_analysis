# analysis script to estimate item log-worths for best-worst scaling dataset
# last edited 20230419 by @vankesteren

# Preparation ----
# packages & functions
library(tidyverse)
library(cmdstanr)
library(patchwork)
source("stan/create_stan_data.R") # functions to create stan data from rank and word datasets
source("stan/summary_functions.R") # functions to easily summarize fitted model

# read data
rnk_df <- read_rds("data_processed/rnk_df_full.rds")

# compile stan model
rol_model <- cmdstan_model("stan/rank_ordered_logit_regression.stan")

# estimate word scores with variational inference for speed
word_scores <- tibble()
for (wordtype in levels(rnk_df$wordtype)) {
  for (assoc in levels(rnk_df$association)) {
    dat <- stan_dat_filter(rnk_df, assoc = assoc, wrdtp = wordtype)
    # opt <- rol_model$variational(data = dat$stan_dat) # for speedup
    opt <- rol_model$sample(dat$stan_dat, chains = 8, parallel_chains = 8, iter_sampling = 1250)
    res <- opt$summary("theta_item")
    out <- 
      res |> 
      mutate(
        word = levels(dat$rank_dat$word)[parse_number(res$variable)],
        association = assoc, 
        wordtype = wordtype
      )
    word_scores <- bind_rows(word_scores, out)
  }
}

write_rds(word_scores, "data_processed/word_scores.rds")

word_scores |>
  group_by(association, wordtype) |> 
  arrange(mean) |> 
  mutate(word = as_factor(word)) |> 
  ggplot(aes(x = mean, xmin = q5, xmax = q95, y = word, color = association)) +
  geom_pointrange() +
  facet_grid(rows = vars(wordtype), cols = vars(association), scales = "free_y")
  
  
for (wtp in levels(rnk_df$wordtype)) {
  for (assoc in levels(rnk_df$association)) {
    nm <- paste0("figures/", wtp, "_", assoc, ".png")
    cat("saving plot", nm, "\r")
    
    plt <- 
      word_scores |>
      filter(association == assoc, wordtype == wtp) |> 
      arrange(mean) |> 
      mutate(word = as_factor(word)) |> 
      ggplot(aes(x = mean, xmin = q5, xmax = q95, y = word, color = association)) +
      geom_pointrange() +
      theme_minimal()
    
    ggsave(nm, plt, width = 10, height = 18, bg = "white")
  }
}
