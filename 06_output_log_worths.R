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
  geom_pointrange(size = 0.1) +
  facet_grid(rows = vars(wordtype), cols = vars(association), scales = "free_y") + 
  labs(
    title = "Word ratings (log-worths) across semantic dimensions and categories.",
    subtitle = "Sorted by trustworthiness (betrouwbaar)",
    x = "Posterior log-worth (bars indicate 90% CI)",
    y = "Word"
  ) +
  theme_linedraw() +
  theme(axis.text.y = element_text(size = 2))
  
ggsave("figures/wordratings.png", dpi = 600, width = 15, height = 12)

name_draws <- arrow::read_parquet("data_processed/word_draws/namen_betrouwbaar.parquet") |> 
  filter(word %in% c("Julie", "Tinus", "Adolf"))

name_draws |> 
  ggplot(aes(x = value, fill = word)) + 
  geom_density() + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_minimal() + 
  geom_rug(linewidth = 0.1, alpha = 0.5) + 
  labs(
    title = "Rated trustworthiness of the names Adolf, Julie, and Tinus", 
    subtitle = "Distributions based on 10000 posterior samples",
    x = "Log-worth", 
    y = "Density"
  ) +
  facet_grid(rows = vars(word)) +
  scale_fill_discrete(guide = "none")

ggsave("figures/trustworthiness_example.png", dpi = 600, width = 8, height = 5)

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
