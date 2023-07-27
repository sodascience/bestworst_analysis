# pre-processing script for experiment data
# last edited 20230419 by @vankesteren
library(tidyverse)

# Load experiment files into a single data frame
filenames <- list.files("data_raw/full/experiment_data", pattern= ".csv", full.names = TRUE)
tot_df <- tibble()
for (filename in filenames) {
  current_df <- read_csv(
    file = filename,
    col_select = c(wordtype_instructions, association, starts_with("option"), worst, best),
    show_col_types = FALSE,
    name_repair = "minimal"
  ) |>
    filter(!is.na(wordtype_instructions), !is.na(association)) |> 
    mutate(subj_id = as.integer(parse_number(filename)))

  tot_df <- bind_rows(tot_df, current_df)
}

# Clean up dataset, compute trial, option, ranking columns, arrange in required format
rnk_df <-
  tot_df |>
  filter(!is.na(wordtype_instructions)) |>
  group_by(subj_id) |>
  mutate(trial = 1:n()) |>
  ungroup() |>
  pivot_longer(starts_with("option"), names_to = "option", names_prefix = "option", values_to = "word", names_transform = parse_integer) |>
  group_by(subj_id, trial) |>
  mutate(ranking = if_else(word == worst, 4, if_else(word == best, 1, (n() + 1) / 2))) |>
  ungroup() |>
  select(-best, -worst) |>
  mutate(
    wordtype = as_factor(wordtype_instructions),
    association = as_factor(association),
    word = as_factor(word)
  ) |>
  relocate(subj_id, trial, association, wordtype, option, word, ranking) |>
  select(-wordtype_instructions)

# write to processed data folder
write_rds(rnk_df, "data_processed/rnk_df_full.rds")
