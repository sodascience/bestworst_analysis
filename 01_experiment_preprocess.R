# pre-processing script for experiment data
# last edited 20231221 by @vankesteren
library(tidyverse)
source("0_datafunctions.R")

# Load experiment files into a single data frame
filenames <- list.files("data_raw/full/experiment_data", pattern= ".csv", full.names = TRUE)
meta_df   <- tibble()
trials_df <- tibble()
for (filename in filenames) {
  cat(filename, "\n")
  data_list <- import_experiment_file(filename)
  meta_df   <- bind_rows(meta_df,   data_list[["metadata"]])
  trials_df <- bind_rows(trials_df, data_list[["trials"]])
}

# Do response time analysis and add to meta df
rt_df <- 
  trials_df |> 
  summarize(
    mean_rt   = mean(trial_response_time), 
    median_rt = median(trial_response_time), 
    min_rt    = min(trial_response_time),
    max_rt    = max(trial_response_time),
    .by = subj_id
  )

meta_df <- 
  meta_df |> 
  mutate(attention_ok = attention_best & attention_worst) |> 
  left_join(rt_df, by = join_by(subj_id)) |> 
  arrange(subj_id)

# write to processed data folder
write_rds(meta_df, "data_processed/meta_df.rds")

# Clean up dataset, compute trial, option, ranking columns, arrange in required format
rnk_df <-
  trials_df |>
  filter(!is.na(wordtype_instructions)) |>
  group_by(subj_id) |>
  mutate(trial = 1:n()) |>
  ungroup() |>
  pivot_longer(
    cols            = starts_with("option"),
    names_to        = "option",
    names_prefix    = "option",
    values_to       = "word",
    names_transform = parse_integer
  ) |>
  group_by(subj_id, trial) |>
  mutate(ranking = if_else(word == worst, 4, if_else(word == best, 1, (n() + 1) / 2))) |>
  ungroup() |>
  select(-best, -worst) |>
  mutate(
    wordtype    = as_factor(wordtype_instructions),
    association = as_factor(association),
    word        = as_factor(word)
  ) |>
  relocate(subj_id, trial, association, wordtype, option, word, ranking) |>
  select(-wordtype_instructions)

# write to processed data folder
write_rds(rnk_df, "data_processed/rnk_df_full.rds")
