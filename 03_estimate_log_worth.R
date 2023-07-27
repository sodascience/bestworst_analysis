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


# Example analysis ----
# create data to pass to stan
dat <- stan_dat_filter(rnk_df, assoc = "vrouwelijk", wrdtp = "namen")

# get 10k posterior samples
fit <- rol_model$sample(dat$stan_dat, chains = 8, parallel_chains = 8, iter_sampling = 1250)

# get the log-worths for each word
itm_vals <- get_item_values(fit, dat)

# we can plot these
plot_coefs(itm_vals)

# we can even per-trial have a look at how likely that trial is (outlier detection?)
loglik_per_trial(fit, dat) |> arrange(ll)

# we can do the same for mean likelihood per participant
mean_loglik_per_subj(fit, dat) |> arrange(ll)


# Plot ----
# make evilness plot for the three word-types
dat_firstnames <- stan_dat_filter(rnk_df, assoc = "kwaadaardigheid", wrdtp = "voornamen") 
dat_companies  <- stan_dat_filter(rnk_df, assoc = "kwaadaardigheid", wrdtp = "bedrijfsnamen") 
dat_nonwords   <- stan_dat_filter(rnk_df, assoc = "kwaadaardigheid", wrdtp = "nepwoorden") 

fit_firstnames <- rol_model$sample(dat_firstnames$stan_dat, chains = 8, parallel_chains = 8, iter_sampling = 1250)
fit_companies  <- rol_model$sample(dat_companies$stan_dat, chains = 8, parallel_chains = 8, iter_sampling = 1250)
fit_nonwords   <- rol_model$sample(dat_nonwords$stan_dat, chains = 8, parallel_chains = 8, iter_sampling = 1250)

itm_firstnames <- get_item_values(fit_firstnames, dat_firstnames)
itm_companies  <- get_item_values(fit_companies, dat_companies)
itm_nonwords   <- get_item_values(fit_nonwords, dat_nonwords)

plt_firstnames <- plot_coefs(itm_firstnames) + labs(x = "Worth (log-odds scale)", y = "", title = "Evilness association\nfirstnames")
plt_companies  <- plot_coefs(itm_companies)  + labs(x = "Worth (log-odds scale)", y = "", title = "Evilness association\ncompanies")
plt_nonwords   <- plot_coefs(itm_nonwords)   + labs(x = "Worth (log-odds scale)", y = "", title = "Evilness association\nnonwords")


plts <- (plt_firstnames + plt_companies + plt_nonwords) * theme_minimal()

ggsave(filename = "evil_summary.png", plot = plts, width = 12, height = 7)

