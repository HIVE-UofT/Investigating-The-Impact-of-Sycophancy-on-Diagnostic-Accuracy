####Load packages

required_pkgs <- c(
  "dplyr", "tidyr", "stringr", "ggplot2", "patchwork", "purrr",
  "forcats", "scales", "ggrepel", "ggforce", "cowplot"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Please install the following packages before running this script:\n",
    paste(missing_pkgs, collapse = ", ")
  )
}

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(purrr)
library(forcats)
library(scales)
library(ggrepel)
library(ggforce)
library(DescTools)

# -----------------------------
# Helpers
# -----------------------------
safe_setwd <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    path <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(path) && nzchar(path)) {
      setwd(dirname(path))
    }
  }
}

to_logical_flag <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x == 1)
  x_chr <- trimws(as.character(x))
  dplyr::case_when(
    x_chr %in% c("TRUE", "True", "true", "T", "1") ~ TRUE,
    x_chr %in% c("FALSE", "False", "false", "F", "0") ~ FALSE,
    TRUE ~ NA
  )
}

as_bool <- function(x) {
  if (is.logical(x)) return(x)
  x_chr <- tolower(trimws(as.character(x)))
  x_chr %in% c("true", "t", "1", "yes", "y")
}


# -----------------------------
# Data
# -----------------------------
safe_setwd()

dir.create("alternative_figures", showWarnings = FALSE)

process_csv_med_changing <- function(file_path) {
  read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE) %>%
    mutate(
      MC_med_changing_acc = casestudy_accuracy_pass1 - medmcqa_accuracy_pass1
      )
}

input_csv1 <- "Final_evaluation_results_all_LLMs_turn_1.csv"
input_csv2 <- "Final_evaluation_results_all_LLMs_turn_2.csv"
input_csv3 <- "Final_evaluation_results_all_LLMs_turn_3.csv"

input_files <- c(
  "Run 1" = input_csv1,
  "Run 2" = input_csv2,
  "Run 3" = input_csv3
)

#df <- map_dfr(input_files, process_csv_med_changing, .id = "run_number")

acc<-read.csv("figure_stats/accuracy_summary.csv")
pass_flip_all<-read.csv("figure_stats/pass1_pass2_flip_summary.csv")
pass_change_all<-read.csv("figure_stats/pass1_vs_pass2_paired_effects.csv")
accuracy_pass2<-read.csv("figure_stats/accuracy_summary_pass2.csv")

###Create summary tables from 
transformed_acc <- acc %>%
  # 1. Extract only the baseline condition
  filter(condition == "baseline") %>%
  
  # Reshape to wide format first so we have datasets side-by-side
  select(case_model_name_cat, case_origin_cat, estimate) %>%
  pivot_wider(
    names_from = case_origin_cat,
    values_from = estimate
  ) %>%
  
  # 2 & 3. Calculate the difference and format the final columns
  mutate(
    # Difference between MultiCaRe and MedMCQA (MultiCaRe minus MedMCQA)
    diff_MC_Med = MultiCaRe - MedMCQA
  ) %>%
  select(
    case_model_name_cat,
    baseline_acc = Overall,
    diff_MC_Med
  )

transformed_flip <- pass_flip_all %>%
  filter(condition == "baseline") %>%
    select(case_model_name_cat, case_origin_cat, estimate) %>%
  pivot_wider(
    names_from = case_origin_cat,
    values_from = estimate
  )  %>%
  select(
    case_model_name_cat,
    AcFR = Overall
  )

transformed_acc2 <- accuracy_pass2 %>%
  filter(condition == "baseline") %>%
    select(case_model_name_cat, case_origin_cat, estimate) %>%
  pivot_wider(
    names_from = case_origin_cat,
    values_from = estimate
  )  %>%
  select(
    case_model_name_cat,
    post_challenge_acc = Overall
  )

transform_fin <- transformed_acc %>%
  inner_join(transformed_acc2, by = "case_model_name_cat") %>%
  inner_join(transformed_flip, by = "case_model_name_cat") %>%
  mutate(AAC_pp = post_challenge_acc - baseline_acc)

colMeans(transform_fin[sapply(transform_fin, is.numeric)], na.rm = TRUE)


# 2. Create a named vector/list of your input file paths
input_files <- c(
  "Run 1" = input_csv1,
  "Run 2" = input_csv2,
  "Run 3" = input_csv3
)

process_csv <- function(file_path) {
  read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE) %>%
    mutate(
      case_origin_cat = recode(case_origin_cat,
                               "MedMCQA_Train" = "MedMCQA",
                               "Casestudy" = "MultiCaRe",
                               .default = case_origin_cat
      ),
      case_full_llmjudge_accuracy_pass1 = as_bool(case_full_llmjudge_accuracy_pass1),
      case_full_llmjudge_accuracy_pass2 = as_bool(case_full_llmjudge_accuracy_pass2),
      case_full_llmjudge_flip           = as_bool(case_full_llmjudge_flip),
      case_flip_1_2                     = as_bool(case_full_llmjudge_accuracy_pass1 != case_full_llmjudge_accuracy_pass2),
      model = recode(
        case_model_name_cat,
        "anthropic/claude-sonnet-4" = "Claude-4",
        "google/gemini-3.5-flash" = "Gemini-3.5",
        "google/gemini-2.5-flash" = "Gemini-2.5",
        "google/medgemma-27b-text-it" = "MedGemma-27B",
        "google/medgemma-4b-it" = "MedGemma-4B",
        "meta-llama/llama-3.1-8b-instruct" = "Llama-3.1-8B",
        "meta-llama/Llama-3.2-1B-Instruct" = "Llama-3.2-1B",
        "meta-llama/Llama-3.2-3B-Instruct" = "Llama-3.2-3B",
        "openai/gpt-4o" = "GPT-4o",
        "openai/gpt-5" = "GPT-5",
        .default = case_model_name_cat
      ),
      model_type = recode(case_model_type_cat,
                          "commercial" = "Commercial",
                          "open_source" = "Open-source",
                          .default = case_model_type_cat
      ),
      acc_pass1 = to_logical_flag(case_full_llmjudge_accuracy_pass1),
      acc_pass2 = to_logical_flag(case_full_llmjudge_accuracy_pass2),
      flip_pass1_to_2 = to_logical_flag(case_full_llmjudge_flip),
      condition = factor(case_condition_cat,
                         levels = c("baseline", "adjacent", "diff_1", "diff_2"),
                         labels = c("Baseline", "Adjacent", "Different specialty 1", "Different specialty 2")
      )
    )
}

input_csv1 <- "final_analyzed_results_turn_1.csv"
input_csv2 <- "final_analyzed_results_turn_2.csv"
input_csv3 <- "final_analyzed_results_turn_3.csv"

input_files <- c(
  "Run 1" = input_csv1,
  "Run 2" = input_csv2,
  "Run 3" = input_csv3
)

# 3. Map over the files and bind them into one final dataframe
df_long <- map_dfr(input_files, process_csv, .id = "run_number")

kappa_data_wide <- df_long %>%
  filter(case_condition_cat == "baseline") %>%
  select(
    run_number, 
    case_origin_cat, 
    case_condition_cat, 
    case_model_name_cat, 
    case_origtext_str, 
    model, 
    acc_pass1, 
    flip_pass1_to_2
  ) %>%
  mutate(acc_pass1 = tidyr::replace_na(acc_pass1, FALSE)) %>%
  pivot_wider(
    # Include all distinguishing columns in id_cols
    id_cols = c(
      case_origin_cat, 
      case_condition_cat, 
      case_model_name_cat, 
      case_origtext_str, 
      model
    ),
    names_from = run_number,   
    values_from = c(acc_pass1,flip_pass1_to_2),
    values_fill = FALSE
  )

# 2. Run Fleiss' Kappa
accuracy_matrix <- kappa_data_wide %>%
  select(starts_with("acc_pass1_"))

kappa_accuracy <- KappaM(accuracy_matrix, method = "Fleiss", conf.level = 0.95)

# -----------------------------------------------------------------
# 3. Compute Fleiss' Kappa for FLIP RATE
# -----------------------------------------------------------------
flip_matrix <- kappa_data_wide %>%
  select(starts_with("flip"))

kappa_flip <- KappaM(flip_matrix, method = "Fleiss", conf.level = 0.95)

# -----------------------------------------------------------------
# 4. View Results
# -----------------------------------------------------------------
print("--- Fleiss' Kappa for Pass 1 Accuracy ---")
print(kappa_accuracy)

print("--- Fleiss' Kappa for Post-Challenge Flip Rate ---")
print(kappa_flip)

#Calculate transition variables
make_transition <- function(p1, p2) {
  case_when(
    !is.na(p1) & !is.na(p2) & p1 & p2 ~ "T->T",
    !is.na(p1) & !is.na(p2) & p1 & !p2 ~ "T->F",
    !is.na(p1) & !is.na(p2) & !p1 & p2 ~ "F->T",
    !is.na(p1) & !is.na(p2) & !p1 & !p2 ~ "F->F",
    TRUE ~ NA_character_
  )
}

baseline_trans <- df_long %>%
  mutate(transition = make_transition(acc_pass1, acc_pass2)) %>% 
  filter(case_condition_cat == "baseline") %>%
  select(c(transition,model))

transition_summary <- baseline_trans %>%
  count(model, transition) %>%
  pivot_wider(
    names_from = transition,
    values_from = n,
    values_fill = 0  # If a model never hits a category, it fills it with a 0 instead of NA
  ) %>%
  mutate(total_freq = `F->F` + `T->F` + `F->T` + `T->T`,
         AcFR = (`T->F` + `F->T`) / total_freq)

