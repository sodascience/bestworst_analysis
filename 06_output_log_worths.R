# analysis script to estimate item log-worths for best-worst scaling dataset
# last edited 20231221 by @vankesteren

# Preparation ----
# packages & functions
library(tidyverse)
library(cmdstanr)
library(patchwork)
library(arrow)
source("stan/create_stan_data.R") # functions to create stan data from rank and word datasets
source("stan/summary_functions.R") # functions to easily summarize fitted model

# read data
rnk_df <- read_rds("data_processed/rnk_df_subset.rds")

# compile stan model
rol_model <- cmdstan_model("stan/rank_ordered_logit_regression.stan")

# Get posterior of word scores ----
# get the posterior word scores + their summary
word_draws_folder <- "data_processed/word_draws"
word_scores <- tibble()
for (wordtype in levels(rnk_df$wordtype)) {
  for (assoc in levels(rnk_df$association)) {
    dat <- stan_dat_filter(rnk_df, assoc = assoc, wrdtp = wordtype)
    fit <- rol_model$sample(dat$stan_dat, chains = 8, parallel_chains = 8, iter_sampling = 1250)
    
    # get draws
    drw <- 
      fit$draws("theta_item", format = "draws_df") |> 
      tibble() |> 
      pivot_longer(-c(.chain, .iteration, .draw), names_to = "variable") |> 
      select(
        variable, draw = .draw, value
      ) |> 
      mutate(
        word = levels(dat$rank_dat$word)[parse_number(variable)],
        association = assoc, 
        wordtype = wordtype
      ) |> 
      relocate(
        word, association, wordtype, draw, value
      ) |> 
      select(-variable)
    
    # sink draws to file system using parquet
    pq_path <- file.path(word_draws_folder, paste0(wordtype, "_", assoc, ".parquet"))
    write_parquet(drw, pq_path)
    
    # get summaries
    smy <- 
      fit$summary("theta_item") |> 
      mutate(
        word = levels(dat$rank_dat$word)[parse_number(variable)],
        association = assoc, 
        wordtype = wordtype
      )
    
    word_scores <- bind_rows(word_scores, smy)
  }
}

write_rds(word_scores, "data_processed/word_scores.rds")

# Do some additional visualization ----
word_scores |>
  group_by(association, wordtype) |> 
  arrange(mean) |> 
  mutate(word = as_factor(word)) |> 
  ggplot(aes(x = mean, xmin = q5, xmax = q95, y = word, color = association)) +
  geom_pointrange() +
  facet_grid(rows = vars(wordtype), cols = vars(association), scales = "free_y")
  
# Output plots ----
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
