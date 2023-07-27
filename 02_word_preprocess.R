# pre-processing script for word-level predictor data
# last edited 20230419 by @vankesteren
library(tidyverse)

# read data with appropriate column types
col_spec <- cols(
  word = col_factor(),
  wordtype = col_factor()
)
wrd_df <- read_csv("data_raw/word_data/word_data.csv", col_types = col_spec)

# add random predictor for testing
wrd_df <- wrd_df |> mutate(languagemodel_prediction_evilness = rnorm(n()))

# write to processed data folder
write_rds(wrd_df, "data_processed/wrd_df.rds")
