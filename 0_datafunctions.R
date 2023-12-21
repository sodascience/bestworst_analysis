# Functions for loading / parsing psychopy experiment data
# last edited 20231221 by @vankesteren
library(tidyverse)

#' Function to load experiment file into a list
#' 
#' List will contain metadata and the trials data.
#' 
#' @param path the path to the psychopy experiment file
#' 
#' @return list with two tibbles: metadata and trials.
import_experiment_file <- function(path) {
  return(list(
    metadata = get_metadata(path),
    trials   = get_trials(path)
  ))
}

#' Function to extract metadata from experiment file
#' 
#' The first few rows in an experiment file contain metadata
#' such as example trials, consent, and the attention check
#' This function applies the check and returns a metadata df
#' with subject id and whether the checks were passed.
#' 
#' @param path the path to the psychopy experiment file
#' 
#' @return single-row tibble with subj_id and check columns
get_metadata <- function(path) {
  metadata_df <- read_csv(
    file = path,
    col_select = c(
      participant,
      consent,
      attention_worst_correct,
      attention_worst_resp,
      attention_beste_correct,
      attention_best_resp,
      attention_response_time
    ),
    show_col_types = FALSE,
    name_repair = "minimal",
    n_max = 12
  )
  consent_given <- metadata_df |> pull(consent) |> na.omit() |> c()
  attention_check_best <- 
    metadata_df |> 
    filter(!is.na(attention_best_resp)) |> 
    mutate(check = attention_beste_correct == attention_best_resp) |> 
    pull(check)
  attention_check_worst  <- 
    metadata_df |> 
    filter(!is.na(attention_best_resp)) |> 
    mutate(check = attention_worst_correct == attention_worst_resp) |> 
    pull(check)
  
  return(tibble(
    subj_id = as.integer(metadata_df$participant[1]),
    consent = consent_given,
    attention_best = attention_check_best,
    attention_worst = attention_check_worst
  ))
}

#' Function to extract trials from experiment file
#' 
#' After the first few rows of the experiment file the trials
#' data starts. This function extracts this data and converts
#' it to a well-behaved tidy tibble.
#' 
#' @param path the path to the psychopy experiment file
#' 
#' @return tibble with subj_id experiment trials data
get_trials <- function(path) {
  read_csv(
    file = path,
    col_select = c(
      participant,
      wordtype_instructions,
      association,
      starts_with("option"),
      worst,
      best,
      trial_response_time
    ),
    show_col_types = FALSE,
    name_repair = "minimal"
  ) |>
    filter(!is.na(wordtype_instructions), !is.na(association)) |> 
    mutate(subj_id = as.integer(participant), .keep = "unused") |> 
    relocate(subj_id)
}