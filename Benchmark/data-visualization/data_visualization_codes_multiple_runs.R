
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(grid)
  library(purrr)
})



# ---------- robust working-directory helper ----------
set_script_wd <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
    setwd(dirname(script_path))
    return(invisible(script_path))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    try({
      script_path <- rstudioapi::getActiveDocumentContext()$path
      if (!is.null(script_path) && nzchar(script_path)) {
        setwd(dirname(normalizePath(script_path)))
        return(invisible(script_path))
      }
    }, silent = TRUE)
  }

  invisible(NULL)
}
set_script_wd()

input_csv1 <- "final_analyzed_results_turn_1.csv"
if (!file.exists(input_csv1)) {
  stop("Could not find input file: ", input_csv1, call. = FALSE)
}

input_csv2 <- "final_analyzed_results_turn_2.csv"
if (!file.exists(input_csv2)) {
  stop("Could not find input file: ", input_csv2, call. = FALSE)
}

input_csv3 <- "final_analyzed_results_turn_3.csv"
if (!file.exists(input_csv3)) {
  stop("Could not find input file: ", input_csv3, call. = FALSE)
}

dir.create("figure_stats", showWarnings = FALSE)

# ---------- constants ----------
origin_levels <- c("MultiCaRe", "Overall", "MedMCQA")
origin_legend_breaks <- c("MultiCaRe", "Overall", "MedMCQA")

origin_palette <- c(
  "MultiCaRe" = "#67b59b",  # high-contrast orange-red
  "MedMCQA"   = "#ad6cad",  # high-contrast blue
  "Overall"   = "#8c858c"   # near-black
)

origin_shapes <- c(
  "MultiCaRe" = 16,  # circle
  "MedMCQA"   = 17,  # triangle
  "Overall"   = 18   # diamond
)

effect_palette <- c(
  "Higher than reference" = "#8cb6d4",
  "Lower than reference"  = "#a1b07d",
  "No change"             = "#a62e5c"
)

condition_labels <- c(
  "baseline" = "Baseline",
  "adjacent" = "Adjacent",
  "diff_1"   = "Diff specialty 1",
  "diff_2"   = "Diff specialty 2"
)

model_map <- c(
  "anthropic/claude-sonnet-4"        = "Claude-4",
  "google/gemini-3.5-flash"          = "Gemini-3.5",
  "google/gemini-2.5-flash"          = "Gemini-2.5",
  "google/medgemma-27b-text-it"      = "MedGemma-27B",
  "google/medgemma-4b-it"            = "MedGemma-4B",
  "meta-llama/llama-3.1-8b-instruct" = "Llama-3.1-8B",
  "meta-llama/Llama-3.2-1B-Instruct" = "Llama-3.2-1B",
  "meta-llama/Llama-3.2-3B-Instruct" = "Llama-3.2-3B",
  "openai/gpt-4o"                    = "GPT-4o",
  "openai/gpt-5"                     = "GPT-5"
)

stripe_fill <- "#F7F9FB"
stripe_border <- "#EEF2F6"

# ---------- helper functions ----------
as_bool <- function(x) {
  if (is.logical(x)) return(x)
  x_chr <- tolower(trimws(as.character(x)))
  x_chr %in% c("true", "t", "1", "yes", "y")
}

sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

format_p_label <- function(p, stars = "") {
  ifelse(
    is.na(p),
    "",
    ifelse(
      p < 0.001,
      paste0("p<0.001", ifelse(stars == "", "", paste0(" ", stars))),
      paste0("p=", formatC(p, format = "f", digits = 3),
             ifelse(stars == "", "", paste0(" ", stars)))
    )
  )
}

theme_jamia <- function() {
  theme_minimal(base_size = 10.8) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#DDE3EA", linewidth = 0.35),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 9.7, color = "#111111"),
      axis.text.x = element_text(size = 9, color = "#222222"),
      axis.ticks.y = element_blank(),
      strip.text = element_text(face = "bold", size = 10, color = "#111111"),
      strip.background = element_rect(fill = "#F1F4F7", color = "#D9E0E7", linewidth = 0.45),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      legend.key.width = unit(1.2, "lines"),
      plot.title = element_text(face = "bold", size = 12, color = "#111111"),
      plot.subtitle = element_text(size = 9.4, color = "#444444"),
      plot.caption = element_text(size = 8.2, color = "#555555", hjust = 0),
      panel.spacing = unit(0.95, "lines"),
      plot.margin = margin(6, 18, 6, 8)
    )
}

save_pdf <- function(plot_obj, filename, width, height) {
  ggsave(
    filename = filename,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = "pdf"
  )
}

build_model_layout <- function(model_order,
                               offsets = c("MultiCaRe" = 0.28, "Overall" = 0.00, "MedMCQA" = -0.28),
                               band_step = 1.35,
                               band_half = 0.50) {
  centers <- rev(seq_along(model_order)) * band_step
  tibble(
    case_model_name_cat = factor(model_order, levels = model_order),
    y_center = centers,
    band_id = seq_along(model_order),
    ymin = centers - band_half,
    ymax = centers + band_half
  ) %>%
    mutate(
      stripe = band_id %% 2 == 0,
      y_multicare = y_center + offsets["MultiCaRe"],
      y_overall   = y_center + offsets["Overall"],
      y_medmcqa   = y_center + offsets["MedMCQA"]
    )
}

join_model_positions <- function(dat, layout_df) {
  dat %>%
    left_join(layout_df, by = "case_model_name_cat") %>%
    mutate(
      y = case_when(
        case_origin_cat == "MultiCaRe" ~ y_multicare,
        case_origin_cat == "Overall"   ~ y_overall,
        case_origin_cat == "MedMCQA"   ~ y_medmcqa,
        TRUE ~ y_center
      )
    )
}

make_stripe_data <- function(layout_df, facet_df = NULL) {
  stripes <- layout_df %>%
    select(case_model_name_cat, band_id, stripe, ymin, ymax)

  if (!is.null(facet_df) && nrow(facet_df) > 0) {
    stripes <- tidyr::crossing(stripes, facet_df)
  }

  stripes
}

prop_breaks <- function(x_limits) {
  upper <- x_limits[2]
  if (upper <= 0.5) {
    seq(0, upper, by = 0.1)
  } else {
    seq(0, upper, by = 0.25)
  }
}

# ---------- data ----------

# 1. Define the processing function
process_csv <- function(file_path) {
  read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE) %>%
    mutate(
      case_origin_cat = recode(
        case_origin_cat,
        "MedMCQA_Train" = "MedMCQA",
        "Casestudy"     = "MultiCaRe"
      ),
      case_model_name_cat = recode(case_model_name_cat, !!!model_map),
      case_model_type_cat = recode(
        case_model_type_cat,
        "commercial"  = "Commercial",
        "open_source" = "Open-source"
      ),
      case_full_llmjudge_accuracy_pass1 = as_bool(case_full_llmjudge_accuracy_pass1),
      case_full_llmjudge_accuracy_pass2 = as_bool(case_full_llmjudge_accuracy_pass2),
      case_full_llmjudge_flip           = as_bool(case_full_llmjudge_flip),
      case_flip_1_2                     = as_bool(case_full_llmjudge_accuracy_pass1 != case_full_llmjudge_accuracy_pass2)
    ) %>%
    select(
      case_id_str,
      case_origin_cat,
      case_condition_cat,
      case_model_name_cat,
      case_model_type_cat,
      case_full_llmjudge_accuracy_pass1,
      case_full_llmjudge_accuracy_pass2,
      case_full_llmjudge_flip,
      case_flip_1_2
    )
}

# 2. Create a named vector/list of your input file paths
input_files <- c(
  "Run 1" = input_csv1,
  "Run 2" = input_csv2,
  "Run 3" = input_csv3
)

df_long <- map_dfr(input_files, process_csv, .id = "run_number")

# 3. Collapse the runs by averaging
df_collapsed <- df_long %>%
  group_by(case_id_str, case_origin_cat, case_condition_cat, case_model_name_cat, case_model_type_cat) %>%
  summarise(
    case_full_llmjudge_accuracy_pass1 = mean(case_full_llmjudge_accuracy_pass1, na.rm = TRUE),
    case_full_llmjudge_accuracy_pass2 = mean(case_full_llmjudge_accuracy_pass2, na.rm = TRUE),
    case_full_llmjudge_flip           = mean(case_full_llmjudge_flip, na.rm = TRUE),
    case_flip_1_2                     = mean(case_flip_1_2, na.rm = TRUE),
    .groups = "drop"
  )

model_order <- df_collapsed %>%
  filter(case_condition_cat == "baseline") %>%
  group_by(case_model_name_cat) %>%
  summarise(overall_acc = mean(case_full_llmjudge_accuracy_pass1, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(overall_acc), case_model_name_cat) %>%
  pull(case_model_name_cat)

df <- df_collapsed %>%
  mutate(
    case_model_name_cat = factor(case_model_name_cat, levels = model_order),
    case_origin_cat = factor(case_origin_cat, levels = c("MultiCaRe", "MedMCQA"))
  )

# =================================================================
# 1. CORE HELPERS (CI & PAIRED TESTS)
# =================================================================

# Replaces Wilson CI for continuous averages with a percentile bootstrap
continuous_ci_vec <- function(values, R = 2000) {
  values <- values[!is.na(values)]
  n <- length(values)
  
  if (n == 0) return(tibble(estimate = NA_real_, lower = NA_real_, upper = NA_real_))
  mean_val <- mean(values)
  if (n == 1 || sd(values) == 0) {
    return(tibble(estimate = mean_val, lower = mean_val, upper = mean_val))
  }
  
  set.seed(42) 
  boot_means <- numeric(R)
  for (i in 1:R) {
    boot_means[i] <- mean(sample(values, size = n, replace = TRUE))
  }
  
  tibble(
    estimate = mean_val,
    lower    = quantile(boot_means, 0.025, names = FALSE),
    upper    = quantile(boot_means, 0.975, names = FALSE)
  )
}

# Computes paired metrics and centers distributions under the Null to find bootstrap p-values
paired_stats_continuous <- function(x_ref, x_comp, R = 2000) {
  keep  <- !(is.na(x_ref) | is.na(x_comp))
  x_ref  <- x_ref[keep]
  x_comp <- x_comp[keep]
  
  n <- length(x_ref)
  if (n == 0) {
    return(tibble(
      n = 0, prop_ref = NA_real_, prop_comp = NA_real_,
      flip_rate = NA_real_, flip_lower = NA_real_, flip_upper = NA_real_,
      AAC = NA_real_, acc_lower = NA_real_, acc_upper = NA_real_,
      RAC = NA_real_, rac_lower = NA_real_, rac_upper = NA_real_,
      p_paired = NA_real_
    ))
  }
  
  prop_ref  <- mean(x_ref)
  prop_comp <- mean(x_comp)
  AAC_val   <- prop_comp - prop_ref
  
  diffs_flip <- abs(x_comp - x_ref)
  flip_val   <- mean(diffs_flip)
  RAC_val    <- if (prop_ref > 0) prop_comp / prop_ref else NA_real_
  
  if (sd(x_ref) == 0 && sd(x_comp) == 0) {
    return(tibble(
      n = n, prop_ref = prop_ref, prop_comp = prop_comp,
      flip_rate = flip_val, flip_lower = flip_val, flip_upper = flip_val,
      AAC = AAC_val, acc_lower = AAC_val, acc_upper = AAC_val,
      RAC = RAC_val, rac_lower = RAC_val, rac_upper = RAC_val,
      p_paired = 1.0
    ))
  }
  
  observed_diffs <- x_comp - x_ref
  mean_diff      <- mean(observed_diffs)
  centered_diffs <- observed_diffs - mean_diff
  
  set.seed(42)
  boot_flips      <- numeric(R)
  boot_aacs       <- numeric(R)
  boot_racs       <- numeric(R)
  boot_null_means <- numeric(R)
  
  for (i in 1:R) {
    indices <- sample(1:n, size = n, replace = TRUE)
    b_ref   <- x_ref[indices]
    b_comp  <- x_comp[indices]
    
    boot_flips[i] <- mean(abs(b_comp - b_ref))
    boot_aacs[i]  <- mean(b_comp) - mean(b_ref)
    boot_racs[i]  <- if (mean(b_ref) > 0) mean(b_comp) / mean(b_ref) else NA_real_
    
    boot_null_means[i] <- mean(sample(centered_diffs, size = n, replace = TRUE))
  }
  
  p_boot <- (sum(abs(boot_null_means) >= abs(mean_diff)) + 1) / (R + 1)
  
  tibble(
    n          = n,
    prop_ref   = prop_ref,
    prop_comp  = prop_comp,
    flip_rate  = flip_val,
    flip_lower = quantile(boot_flips, 0.025, na.rm = TRUE, names = FALSE),
    flip_upper = quantile(boot_flips, 0.975, na.rm = TRUE, names = FALSE),
    AAC        = AAC_val,
    acc_lower  = quantile(boot_aacs, 0.025, na.rm = TRUE, names = FALSE),
    acc_upper  = quantile(boot_aacs, 0.975, na.rm = TRUE, names = FALSE),
    RAC        = RAC_val,
    rac_lower  = quantile(boot_racs, 0.025, na.rm = TRUE, names = FALSE),
    rac_upper  = quantile(boot_racs, 0.975, na.rm = TRUE, names = FALSE),
    p_paired   = if (sd(observed_diffs) > 0) p_boot else 1.0
  )
}

# =================================================================
# 2. Pipelines (Aggregation and tests)
# =================================================================

# Rates builder updated with non-parametric dataset permutation testing
summarize_rate <- function(df, value_col, condition_level, metric_name) {
  cond_df <- df %>% filter(case_condition_cat == condition_level)
  
  by_origin <- cond_df %>%
    group_by(case_model_name_cat, case_model_type_cat, case_origin_cat) %>%
    group_modify(~ continuous_ci_vec(.x[[value_col]])) %>%
    ungroup()
  
  overall <- cond_df %>%
    group_by(case_model_name_cat, case_model_type_cat) %>%
    group_modify(~ continuous_ci_vec(.x[[value_col]])) %>%
    ungroup() %>%
    mutate(case_origin_cat = "Overall")
  
  out <- bind_rows(by_origin, overall) %>%
    mutate(metric = metric_name, condition = condition_level)
  
  # Replaces independent t-test with a two-sample independent permutation loop
  origin_tests <- cond_df %>%
    group_by(case_model_name_cat, case_model_type_cat) %>%
    group_modify(~ {
      x_multi <- .x[[value_col]][.x$case_origin_cat == "MultiCaRe"]
      x_med   <- .x[[value_col]][.x$case_origin_cat == "MedMCQA"]
      
      x_multi <- x_multi[!is.na(x_multi)]
      x_med   <- x_med[!is.na(x_med)]
      
      n1 <- length(x_multi)
      n2 <- length(x_med)
      
      p_val <- if (n1 > 1 && n2 > 1 && (sd(x_multi) > 0 || sd(x_med) > 0)) {
        observed_diff <- mean(x_multi) - mean(x_med)
        combined      <- c(x_multi, x_med)
        total_n       <- n1 + n2
        
        set.seed(42)
        R <- 2000
        perm_diffs <- numeric(R)
        for (i in 1:R) {
          shuffled <- sample(combined)
          perm_diffs[i] <- mean(shuffled[1:n1]) - mean(shuffled[(n1+1):total_n])
        }
        
        (sum(abs(perm_diffs) >= abs(observed_diff)) + 1) / (R + 1)
      } else {
        NA_real_
      }
      
      tibble(p_origin = p_val)
    }) %>%
    ungroup() %>%
    mutate(
      p_origin_holm = p.adjust(p_origin, method = "holm"),
      sig_origin = sig_stars(p_origin_holm),
      p_origin_label = format_p_label(p_origin_holm, sig_origin)
    )
  
  out %>%
    left_join(origin_tests, by = c("case_model_name_cat", "case_model_type_cat")) %>%
    mutate(
      case_origin_cat = factor(case_origin_cat, levels = origin_levels),
      condition_label = factor(unname(condition_labels[condition]),
                               levels = unname(condition_labels))
    )
}

summarize_pass_change <- function(df, condition_level) {
  cond_df <- df %>% filter(case_condition_cat == condition_level)
  
  by_origin <- cond_df %>%
    group_by(case_model_name_cat, case_model_type_cat, case_origin_cat) %>%
    group_modify(~ paired_stats_continuous(
      .x$case_full_llmjudge_accuracy_pass1,
      .x$case_full_llmjudge_accuracy_pass2
    )) %>%
    ungroup()
  
  overall <- cond_df %>%
    group_by(case_model_name_cat, case_model_type_cat) %>%
    group_modify(~ paired_stats_continuous(
      .x$case_full_llmjudge_accuracy_pass1,
      .x$case_full_llmjudge_accuracy_pass2
    )) %>%
    ungroup() %>%
    mutate(case_origin_cat = "Overall")
  
  bind_rows(by_origin, overall) %>%
    mutate(
      condition = condition_level,
      condition_label = factor(unname(condition_labels[condition]),
                               levels = unname(condition_labels)),
      case_origin_cat = factor(case_origin_cat, levels = origin_levels)
    ) %>%
    group_by(condition, case_origin_cat) %>%
    mutate(
      p_change_holm = p.adjust(p_paired, method = "holm"), 
      sig_change = sig_stars(p_change_holm),
      p_change_label = format_p_label(p_change_holm, sig_change)
    ) %>%
    ungroup()
}

summarize_vs_baseline <- function(df, condition_level) {
  base_df <- df %>%
    filter(case_condition_cat == "baseline") %>%
    transmute(
      case_id_str,
      case_origin_cat,
      case_model_name_cat,
      case_model_type_cat,
      ref_acc = case_full_llmjudge_accuracy_pass1
    )
  
  comp_df <- df %>%
    filter(case_condition_cat == condition_level) %>%
    transmute(
      case_id_str,
      case_origin_cat,
      case_model_name_cat,
      case_model_type_cat,
      comp_acc = case_full_llmjudge_accuracy_pass1
    )
  
  merged <- inner_join(
    base_df, comp_df,
    by = c("case_id_str", "case_origin_cat", "case_model_name_cat", "case_model_type_cat")
  )
  
  by_origin <- merged %>%
    group_by(case_model_name_cat, case_model_type_cat, case_origin_cat) %>%
    group_modify(~ paired_stats_continuous(.x$ref_acc, .x$comp_acc)) %>%
    ungroup()
  
  overall <- merged %>%
    group_by(case_model_name_cat, case_model_type_cat) %>%
    group_modify(~ paired_stats_continuous(.x$ref_acc, .x$comp_acc)) %>%
    ungroup() %>%
    mutate(case_origin_cat = "Overall")
  
  bind_rows(by_origin, overall) %>%
    mutate(
      condition = condition_level,
      condition_label = factor(unname(condition_labels[condition]),
                               levels = unname(condition_labels)),
      case_origin_cat = factor(case_origin_cat, levels = origin_levels)
    ) %>%
    group_by(condition, case_origin_cat) %>%
    mutate(
      p_change_holm = p.adjust(p_paired, method = "holm"), 
      sig_change = sig_stars(p_change_holm),
      p_change_label = format_p_label(p_change_holm, sig_change)
    ) %>%
    ungroup()
}

# Final difference tracking block updated with the independent permutation loop
origin_flip_test_vs_baseline <- function(df, condition_level) {
  base_df <- df %>%
    filter(case_condition_cat == "baseline") %>%
    transmute(
      case_id_str,
      case_origin_cat,
      case_model_name_cat,
      ref_acc = case_full_llmjudge_accuracy_pass1
    )
  
  comp_df <- df %>%
    filter(case_condition_cat == condition_level) %>%
    transmute(
      case_id_str,
      case_origin_cat,
      case_model_name_cat,
      comp_acc = case_full_llmjudge_accuracy_pass1
    )
  
  merged <- inner_join(
    base_df, comp_df,
    by = c("case_id_str", "case_origin_cat", "case_model_name_cat")
  ) %>%
    mutate(changed = abs(ref_acc - comp_acc)) 
  
  merged %>%
    group_by(case_model_name_cat) %>%
    group_modify(~ {
      m1 <- .x$changed[.x$case_origin_cat == "MultiCaRe"]
      m2 <- .x$changed[.x$case_origin_cat == "MedMCQA"]
      
      m1 <- m1[!is.na(m1)]
      m2 <- m2[!is.na(m2)]
      
      n1 <- length(m1)
      n2 <- length(m2)
      
      p_val <- if (n1 > 1 && n2 > 1 && (sd(m1) > 0 || sd(m2) > 0)) {
        observed_diff <- mean(m1) - mean(m2)
        combined      <- c(m1, m2)
        total_n       <- n1 + n2
        
        set.seed(42)
        R <- 2000
        perm_diffs <- numeric(R)
        for (i in 1:R) {
          shuffled <- sample(combined)
          perm_diffs[i] <- mean(shuffled[1:n1]) - mean(shuffled[(n1+1):total_n])
        }
        
        (sum(abs(perm_diffs) >= abs(observed_diff)) + 1) / (R + 1)
      } else {
        NA_real_
      }
      
      tibble(p_origin = p_val)
    }) %>%
    ungroup() %>%
    mutate(
      condition = condition_level,
      p_origin_holm = p.adjust(p_origin, method = "holm"),
      sig_origin = sig_stars(p_origin_holm),
      p_origin_label = format_p_label(p_origin_holm, sig_origin)
    )
}

summarize_transition_counts <- function(df, condition_level) {
  # Filter to the specific condition level
  cond_df <- df %>% filter(case_condition_cat == condition_level)
  
  # Calculate overall counts aggregated across the entire dataset per model
  overall <- cond_df %>%
    group_by(case_model_name_cat, case_model_type_cat) %>%
    group_modify(~ {
      p1 <- .x$case_full_llmjudge_accuracy_pass1
      p2 <- .x$case_full_llmjudge_accuracy_pass2
      
      tibble(
        `T->T` = sum(p1 & p2, na.rm = TRUE),
        `T->F` = sum(p1 & !p2, na.rm = TRUE),
        `F->T` = sum(!p1 & p2, na.rm = TRUE),
        `F->F` = sum(!p1 & !p2, na.rm = TRUE)
      )
    }) %>%
    ungroup() %>%
    mutate(
      condition = condition_level,
      condition_label = factor(unname(condition_labels[condition]),
                               levels = unname(condition_labels))
    ) %>%
    select(
      case_model_name_cat, case_model_type_cat,
      condition, condition_label, 
      `T->T`, `T->F`, `F->T`, `F->F`
    )
  
  return(overall)
}

# ---------- compute tables ----------
transition <- summarize_transition_counts(df_long, condition_level = "baseline")
colSums(transition[sapply(transition, is.numeric)], na.rm = TRUE)


accuracy_all <- bind_rows(
  summarize_rate(df, "case_full_llmjudge_accuracy_pass1", "baseline", "accuracy"),
  summarize_rate(df, "case_full_llmjudge_accuracy_pass1", "adjacent", "accuracy"),
  summarize_rate(df, "case_full_llmjudge_accuracy_pass1", "diff_1", "accuracy"),
  summarize_rate(df, "case_full_llmjudge_accuracy_pass1", "diff_2", "accuracy")
)

pass_flip_all <- bind_rows(
  summarize_rate(df, "case_full_llmjudge_flip", "baseline", "flip_pass1_to_pass2"),
  summarize_rate(df, "case_full_llmjudge_flip", "adjacent", "flip_pass1_to_pass2"),
  summarize_rate(df, "case_full_llmjudge_flip", "diff_1", "flip_pass1_to_pass2"),
  summarize_rate(df, "case_full_llmjudge_flip", "diff_2", "flip_pass1_to_pass2")
)


pass_flip_1_2 <- bind_rows(
  summarize_rate(df, "case_flip_1_2", "baseline", "flip_pass1_to_pass2"),
  summarize_rate(df, "case_flip_1_2", "adjacent", "flip_pass1_to_pass2"),
  summarize_rate(df, "case_flip_1_2", "diff_1", "flip_pass1_to_pass2"),
  summarize_rate(df, "case_flip_1_2", "diff_2", "flip_pass1_to_pass2")
)


pass_change_all <- bind_rows(
  summarize_pass_change(df, "baseline"),
  summarize_pass_change(df, "adjacent"),
  summarize_pass_change(df, "diff_1"),
  summarize_pass_change(df, "diff_2")
)

vs_baseline_all <- bind_rows(
  summarize_vs_baseline(df, "adjacent"),
  summarize_vs_baseline(df, "diff_1"),
  summarize_vs_baseline(df, "diff_2")
)

accuracy_pass2 <- bind_rows(
  summarize_rate(df, "case_full_llmjudge_accuracy_pass2", "baseline", "accuracy"),
  summarize_rate(df, "case_full_llmjudge_accuracy_pass2", "adjacent", "accuracy"),
  summarize_rate(df, "case_full_llmjudge_accuracy_pass2", "diff_1", "accuracy"),
  summarize_rate(df, "case_full_llmjudge_accuracy_pass2", "diff_2", "accuracy")
)


# 
write.csv(accuracy_all, "figure_stats/accuracy_summary.csv", row.names = FALSE)
write.csv(accuracy_pass2, "figure_stats/accuracy_summary_pass2.csv", row.names = FALSE)

write.csv(pass_flip_all, "figure_stats/pass1_pass2_flip_summary.csv", row.names = FALSE)
write.csv(pass_change_all, "figure_stats/pass1_vs_pass2_paired_effects.csv", row.names = FALSE)
write.csv(vs_baseline_all, "figure_stats/baseline_vs_experimental_effects.csv", row.names = FALSE)


# ---------- plotting functions ----------
make_rate_plot <- function(dat,
                           model_order,
                           xlab,
                           core_limits = c(0, 1),
                           facet_var = NULL,
                           facet_ncol = 1) {
  layout_df <- build_model_layout(model_order)
  dat <- dat %>%
    mutate(case_origin_cat = factor(case_origin_cat, levels = origin_levels)) %>%
    join_model_positions(layout_df)

  facet_df <- if (!is.null(facet_var)) {
    dat %>% distinct(across(all_of(facet_var)))
  } else {
    NULL
  }
  stripe_dat <- make_stripe_data(layout_df, facet_df)
  stripe_dat <- stripe_dat %>% filter(stripe)

  span <- diff(core_limits)
  panel_limits <- c(core_limits[1], core_limits[2] + 0.23 * span)
  p_x <- core_limits[2] + 0.035 * span

  ann_dat <- dat %>%
    group_by(across(all_of(c("case_model_name_cat", "y_center", facet_var)))) %>%
    summarise(
      p_origin_holm = first(p_origin_holm),
      sig_origin = first(sig_origin),
      p_label = first(p_origin_label),
      .groups = "drop"
    ) %>%
    mutate(
      y_lab = y_center,
      x_lab = p_x
    ) %>%
    filter(!is.na(p_label), p_label != "")

  p <- ggplot() +
    geom_rect(
      data = stripe_dat,
      aes(ymin = ymin, ymax = ymax),
      xmin = panel_limits[1],
      xmax = panel_limits[2],
      inherit.aes = FALSE,
      fill = stripe_fill,
      color = NA
    ) +
    geom_hline(
      yintercept = layout_df$y_center,
      color = stripe_border,
      linewidth = 0.35
    ) +
    geom_segment(
      data = dat,
      aes(x = lower, xend = upper, y = y, yend = y, color = case_origin_cat),
      linewidth = 1.05,
      lineend = "round",
      show.legend = FALSE
    ) +
    geom_point(
      data = dat,
      aes(x = estimate, y = y, color = case_origin_cat, shape = case_origin_cat),
      size = 2.55,
      stroke = 0.25
    ) +
    geom_text(
      data = ann_dat,
      aes(x = x_lab, y = y_lab, label = p_label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 0.5,
      size = 2.85,
      color = "#333333"
    ) +
    scale_color_manual(
      values = origin_palette,
      breaks = origin_legend_breaks
    ) +
    scale_shape_manual(
      values = origin_shapes,
      breaks = origin_legend_breaks
    ) +
    scale_y_continuous(
      breaks = layout_df$y_center,
      labels = model_order,
      limits = c(min(layout_df$y_center) - 0.70, max(layout_df$y_center) + 0.70),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(
      limits = panel_limits,
      breaks = prop_breaks(core_limits),
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = xlab,
      y = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_jamia() +
    guides(
      shape = "none",
      color = guide_legend(
        override.aes = list(shape = unname(origin_shapes[origin_legend_breaks]), linewidth = 0),
        nrow = 1
      )
    )

  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), ncol = facet_ncol)
  }

  p
}

make_effect_forest <- function(dat,
                               model_order,
                               estimate_col,
                               lower_col,
                               upper_col,
                               xlab,
                               core_limits,
                               ref_line,
                               facet_rows = NULL,
                               facet_cols = "case_origin_cat",
                               use_log2_x = FALSE,
                               x_breaks = waiver(),
                               x_labels = waiver(),
                               panel_limit = 1.6) {
  layout_df <- build_model_layout(model_order, offsets = c("MultiCaRe" = 0, "Overall" = 0, "MedMCQA" = 0))
  dat <- dat %>%
    left_join(layout_df %>% select(case_model_name_cat, y_center, ymin, ymax, stripe), by = "case_model_name_cat") %>%
    mutate(
      estimate_plot = .data[[estimate_col]],
      lower_plot = .data[[lower_col]],
      upper_plot = .data[[upper_col]],
      direction = case_when(
        is.na(estimate_plot) ~ "No change",
        estimate_plot > ref_line ~ "Higher than reference",
        estimate_plot < ref_line ~ "Lower than reference",
        TRUE ~ "No change"
      )
    )

  facet_vars <- character(0)
  if (!is.null(facet_rows)) facet_vars <- c(facet_vars, facet_rows)
  if (!is.null(facet_cols)) facet_vars <- c(facet_vars, facet_cols)

  facet_df <- if (length(facet_vars) > 0) {
    dat %>% distinct(across(all_of(facet_vars)))
  } else {
    NULL
  }

  stripe_dat <- make_stripe_data(layout_df, facet_df) %>% filter(stripe)

  if (use_log2_x) {
    #Change to 2.2 if too squished
    panel_limits <- c(core_limits[1], core_limits[2] * panel_limit)
    p_x <- core_limits[2] * 1.15
    nc_x <- core_limits[1] * 1.20
  } else {
    span <- diff(core_limits)
    panel_limits <- c(core_limits[1], core_limits[2] + 0.23 * span)
    p_x <- core_limits[2] + 0.035 * span
    nc_x <- core_limits[1] + 0.035 * span
  }

  ann_dat <- dat %>%
    mutate(
      y_lab = y_center,
      x_lab = p_x
    ) %>%
    filter(!is.na(p_change_label), p_change_label != "")

  p <- ggplot() +
    geom_rect(
      data = stripe_dat,
      aes(ymin = ymin, ymax = ymax),
      xmin = panel_limits[1],
      xmax = panel_limits[2],
      inherit.aes = FALSE,
      fill = stripe_fill,
      color = NA
    ) +
    geom_hline(
      yintercept = layout_df$y_center,
      color = stripe_border,
      linewidth = 0.35
    ) +
    geom_vline(
      xintercept = ref_line,
      linetype = "dashed",
      linewidth = 0.55,
      color = "#666666"
    ) +
    geom_segment(
      data = dat %>% filter(!is.na(estimate_plot)),
      aes(
        x = lower_plot, xend = upper_plot,
        y = y_center, yend = y_center,
        color = direction
      ),
      linewidth = 1.0,
      lineend = "round",
      show.legend = FALSE
    ) +
    geom_point(
      data = dat %>% filter(!is.na(estimate_plot)),
      aes(x = estimate_plot, y = y_center, color = direction),
      size = 2.55,
      show.legend = TRUE
    ) +
    geom_text(
      data = dat %>% filter(is.na(estimate_plot)),
      aes(x = nc_x, y = y_center, label = "NC"),
      hjust = 0,
      vjust = 0.5,
      size = 2.85,
      color = "#444444"
    ) +
    geom_text(
      data = ann_dat,
      aes(x = x_lab, y = y_lab, label = p_change_label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 0.5,
      size = 2.85,
      color = "#333333"
    ) +
    scale_color_manual(values = effect_palette, drop = FALSE) +
    scale_y_continuous(
      breaks = layout_df$y_center,
      labels = model_order,
      limits = c(min(layout_df$y_center) - 0.70, max(layout_df$y_center) + 0.70),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = xlab,
      y = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_jamia() +
    theme(
      legend.text = element_text(size = 11),
      legend.title = element_blank()
    )
    guides(
      color = guide_legend(
        override.aes = list(linewidth = 1.0, size = 2.8),
        nrow = 1
      )
    )

  if (use_log2_x) {
    p <- p + scale_x_continuous(
      trans = scales::log2_trans(),
      limits = panel_limits,
      breaks = x_breaks,
      labels = x_labels,
      expand = expansion(mult = c(0, 0))
    )
  } else {
    p <- p + scale_x_continuous(
      limits = panel_limits,
      breaks = x_breaks,
      labels = x_labels,
      expand = expansion(mult = c(0, 0))
    )
  }

  if (!is.null(facet_rows) && !is.null(facet_cols)) {
    p <- p + facet_grid(as.formula(paste(facet_rows, "~", facet_cols)))
  } else if (!is.null(facet_cols)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_cols)), nrow = 1)
  }

  p
}


# ---------- Figure 4 ----------

flip_origin_adj   <- origin_flip_test_vs_baseline(df, "adjacent")
flip_origin_diff1 <- origin_flip_test_vs_baseline(df, "diff_1")
flip_origin_diff2 <- origin_flip_test_vs_baseline(df, "diff_2")

plot4a_dat <- pass_flip_1_2 %>%
  filter(condition == "baseline") %>%
  transmute(
    case_model_name_cat,
    case_model_type_cat,
    case_origin_cat,
    condition,
    estimate = estimate,
    lower = lower,
    upper = upper,
    p_origin_holm = p_origin_holm,
    sig_origin = sig_origin,
    p_origin_label = p_origin_label
  ) %>% mutate(condition_label = factor("Neutral Condition", levels = c("Neutral Condition"))) 

plot4b_dat <- vs_baseline_all %>%
  filter(condition %in% c("adjacent")) %>%
  transmute(
    case_model_name_cat,
    case_model_type_cat,
    case_origin_cat,
    condition,
    condition_label = factor("Adjacent Specialty",
                             levels = c("Adjacent Specialty")),
    estimate = flip_rate,
    lower = flip_lower,
    upper = flip_upper
  ) %>%
  left_join(
    bind_rows(flip_origin_adj, flip_origin_diff1) %>%
      select(case_model_name_cat, condition, p_origin_holm, sig_origin, p_origin_label),
    by = c("case_model_name_cat", "condition")
  ) 

plot4c_dat <- vs_baseline_all %>%
  filter(condition %in% c("diff_1")) %>%
  transmute(
    case_model_name_cat,
    case_model_type_cat,
    case_origin_cat,
    condition,
    condition_label = factor("Differential Specialty 1",
                                levels = c("Differential Specialty 1")),
    estimate = flip_rate,
    lower = flip_lower,
    upper = flip_upper
  ) %>%
  left_join(
    bind_rows(flip_origin_adj, flip_origin_diff1) %>%
      select(case_model_name_cat, condition, p_origin_holm, sig_origin, p_origin_label),
    by = c("case_model_name_cat", "condition")
  )

plot4_dat <- rbind(plot4a_dat,plot4b_dat,plot4c_dat) 

plot4 <- make_rate_plot(
  dat = plot4_dat,
  model_order = model_order,
  core_limits = c(0, 1),
  xlab = "Accuracy changing flip rate",
  facet_var = "condition_label",
  facet_ncol = 3
)

########Keys to do summary statistics
###1. Pass flip all - LLM as a judge flip
###2. Pass flip1_2 - directly from variables (only accuracy changing flips)
###3. vs_baseline - compared to baseline, adjacent and diff_1 

df_aggregated <- vs_baseline_all %>%
  group_by(condition) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) 
df_aggregated


df_aggregated <- pass_flip_all %>%
  group_by(condition, case_origin_cat) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) 
df_aggregated


df_aggregated <- pass_flip_1_2 %>%
  group_by(condition, case_origin_cat) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) 
df_aggregated



save_pdf(plot4, "plot4.pdf", width = 12, height = 7)

# ---------- Figure 5  ----------
plot5_dat <- pass_change_all %>%
  filter(condition == "baseline")

plot5_aac <- make_effect_forest(
  dat = plot5_dat,
  model_order = model_order,
  estimate_col = "AAC",
  lower_col = "acc_lower",
  upper_col = "acc_upper",
  xlab = "Absolute accuracy change (Pass 2 - Pass 1)",
  core_limits = c(-1, 1),
  ref_line = 0,
  facet_cols = "case_origin_cat",
  x_breaks = seq(-1, 1, by = 0.25),
  x_labels = label_number(accuracy = 0.01)
)

plot5_rac <- make_effect_forest(
  dat = plot5_dat,
  model_order = model_order,
  estimate_col = "RAC",
  lower_col = "rac_lower",
  upper_col = "rac_upper",
  xlab = "Relative accuracy change (Pass 2 / Pass 1, log scale)",
  core_limits = c(0.125, 8),
  ref_line = 1,
  facet_cols = "case_origin_cat",
  use_log2_x = TRUE,
  x_breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
  x_labels = label_number(accuracy = 0.001)
)

plot5 <- (plot5_aac / plot5_rac) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Assuming your current table in the screenshot is named 'df_runs'
df_aggregated <- vs_baseline_all %>%
  # 2. Group by the model metadata
  group_by(condition) %>%
  
  # 3. Apply the mean function (which divides the total sum by 3 runs) 
  # to all remaining numeric columns
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) 
df_aggregated

# Assuming your current table in the screenshot is named 'df_runs'
df_aggregated <- vs_baseline_all %>%
  # 2. Group by the model metadata
  group_by(condition,case_origin_cat) %>%
  
  # 3. Apply the mean function (which divides the total sum by 3 runs) 
  # to all remaining numeric columns
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) 
df_aggregated


save_pdf(plot5, "plot5.pdf", width = 15, height = 7)

# ---------- plot6a ----------
plot6a_dat <- vs_baseline_all %>%
  filter(condition %in% c("adjacent", "diff_1")) %>%
  mutate(
    condition_label = factor(condition,
                             levels = c("adjacent", "diff_1"),
                             labels = c("Adjacent", "Differential Specialty 1"))
  )


plot6a <- make_effect_forest(
  dat = plot6a_dat,
  model_order = model_order,
  estimate_col = "AAC",
  lower_col = "acc_lower",
  upper_col = "acc_upper",
  xlab = "Absolute accuracy change (Experimental - Baseline)",
  core_limits = c(-0.3, 0.3),
  ref_line = 0,
  facet_rows = "condition_label",
  facet_cols = "case_origin_cat",
  x_breaks = seq(-0.3, 0.3, by = 0.1),
  x_labels = label_number(accuracy = 0.01)
)
save_pdf(plot6a, "plot6a.pdf", width = 15.0, height = 7)


vs_baseline_all


# ----------  plot6b ----------
plot6b_dat <- vs_baseline_all %>%
  filter(condition %in% c("adjacent", "diff_1")) %>%
  mutate(
    condition_label = factor(condition,
                             levels = c("adjacent", "diff_1"),
                             labels = c("Adjacent", "Differential Specialty 1"))
  )
plot6b_dat <- plot6b_dat %>% mutate(rac_upper = ifelse(rac_upper > 8, NA, rac_upper))


plot6b <- make_effect_forest(
  dat = plot6b_dat,
  model_order = model_order,
  estimate_col = "RAC",
  lower_col = "rac_lower",
  upper_col = "rac_upper",
  xlab = "Relative accuracy change (Experimental / Baseline, log scale)",
  core_limits = c(0.0625, 8),
  ref_line = 1,
  facet_rows = "condition_label",
  facet_cols = "case_origin_cat",
  use_log2_x = TRUE,
  x_breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
  x_labels = label_number(accuracy = 0.001),
  panel_limit = 2.2
)

#Changed panel limit here because originally it was too squished
save_pdf(plot6b, "plot6b.pdf", width = 15.0, height = 7)


# ---------- Supp 3 / (old plot2) ----------
plot3_dat <- pass_flip_all %>%
  filter(condition == "baseline") %>%
  transmute(
    case_model_name_cat,
    case_model_type_cat,
    case_origin_cat,
    condition_label,
    estimate = estimate,
    lower = lower,
    upper = upper,
    p_origin_holm = p_origin_holm,
    sig_origin = sig_origin,
    p_origin_label = p_origin_label
  )

flip_judge <- make_rate_plot(
  dat = plot3_dat,
  model_order = model_order,
  xlab = "Accuracy changing flip rate",
  core_limits = c(0, 1)
)

summary(plot3_dat)

df_aggregated <- plot3_dat %>%
  # 2. Group by the model metadata
  group_by(case_origin_cat,condition_label) %>%
  
  # 3. Apply the mean function (which divides the total sum by 3 runs) 
  # to all remaining numeric columns
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) 
df_aggregated

save_pdf(flip_judge, "flip_judge.pdf", width = 6, height = 6.9)


# ---------- Supplementary Figure 5 / supp5 ----------
supp5_dat <- vs_baseline_all %>%
  filter(condition == "diff_2") %>%
  transmute(
    case_model_name_cat,
    case_model_type_cat,
    case_origin_cat,
    condition,
    condition_label,
    estimate = flip_rate,
    lower = flip_lower,
    upper = flip_upper
  ) %>%
  left_join(
    flip_origin_diff2 %>% select(case_model_name_cat, condition, p_origin_holm, sig_origin, p_origin_label),
    by = c("case_model_name_cat", "condition")
  )

supp5 <- make_rate_plot(
  dat = supp5_dat,
  model_order = model_order,
  xlab = "Accuracy changing flip rate",
  core_limits = c(0, 0.35)
)
save_pdf(supp5, "diff2-flip.pdf", width = 11.8, height = 5)


# ---------- Supplementary Figure 6 / supp6 ----------
supp6_dat <- vs_baseline_all %>%
  filter(condition == "diff_2")

supp6 <- make_effect_forest(
  dat = supp6_dat,
  model_order = model_order,
  estimate_col = "AAC",
  lower_col = "acc_lower",
  upper_col = "acc_upper",
  xlab = "Absolute accuracy change (Experimental - Baseline)",
  core_limits = c(-0.3, 0.3),
  ref_line = 0,
  facet_cols = "case_origin_cat",
  x_breaks = seq(-0.3, 0.3, by = 0.1),
  x_labels = label_number(accuracy = 0.01)
)
save_pdf(supp6, "diff2-aac.pdf", width = 15.0, height = 5.9)


# ---------- Supplementary Figure 7 / supp7 ----------
supp7_dat <- vs_baseline_all %>%
  filter(condition == "diff_2")

supp7 <- make_effect_forest(
  dat = supp7_dat,
  model_order = model_order,
  estimate_col = "RAC",
  lower_col = "rac_lower",
  upper_col = "rac_upper",
  xlab = "Relative accuracy change (Experimental / Baseline, log scale)",
  core_limits = c(0.0625, 8),
  ref_line = 1,
  facet_cols = "case_origin_cat",
  use_log2_x = TRUE,
  x_breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
  x_labels = label_number(accuracy = 0.001)
)
save_pdf(supp7, "diff2-rac.pdf", width = 15.0, height = 5.9)




