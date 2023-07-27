# analysis script to predict item log-worths for best-worst scaling dataset
# using word-level predictors
# last edited 20230419 by @vankesteren

# Preparation ----
# packages & functions
library(tidyverse)
library(cmdstanr)
library(patchwork)
source("stan/create_stan_data.R") # functions to create stan data from rank and word datasets
source("stan/summary_functions.R") # functions to easily summarize fitted model

# read data
rnk_df <- read_rds("data_processed/rnk_df.rds")
wrd_df <- read_rds("data_processed/wrd_df.rds")

# compile stan model
rol_model <- cmdstan_model("stan/rank_ordered_logit_regression.stan")

# Regression ----
# create data to pass to stan
dat <- stan_dat_filter(rank_dat = rnk_df, item_dat = wrd_df, assoc = "kwaadaardigheid", wrdtp = "voornamen")

# get 10k posterior samples
fit <- rol_model$sample(dat$stan_dat, chains = 8, parallel_chains = 8, iter_sampling = 1250)

# get regression coefficients
get_regression_coefficients(fit, dat) # should be no different from 0 because it's random for now

# we can still inspect the item values in this model
get_item_values(fit, dat) |> arrange(mean)
