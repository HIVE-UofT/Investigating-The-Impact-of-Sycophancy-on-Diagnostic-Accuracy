
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(grid)
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

input_csv <- "llm_judge_all_revised.csv"
if (!file.exists(input_csv)) {
  stop("Could not find input file: ", input_csv, call. = FALSE)
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
  "google/gemini-2.0-flash"          = "Gemini-2.0",
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

wilson_ci_vec <- function(x, n, conf.level = 0.95) {
  out <- tibble(
    estimate = rep(NA_real_, length(x)),
    lower    = rep(NA_real_, length(x)),
    upper    = rep(NA_real_, length(x))
  )

  keep <- !is.na(x) & !is.na(n) & n > 0
  if (!any(keep)) return(out)

  z <- qnorm(1 - (1 - conf.level) / 2)
  p <- x[keep] / n[keep]
  denom  <- 1 + z^2 / n[keep]
  center <- p + z^2 / (2 * n[keep])
  spread <- z * sqrt((p * (1 - p) / n[keep]) + (z^2 / (4 * n[keep]^2)))

  out$estimate[keep] <- p
  out$lower[keep] <- pmax(0, (center - spread) / denom)
  out$upper[keep] <- pmin(1, (center + spread) / denom)
  out
}

paired_stats <- function(x_ref, x_comp) {
  keep <- !(is.na(x_ref) | is.na(x_comp))
  x_ref <- as_bool(x_ref[keep])
  x_comp <- as_bool(x_comp[keep])

  n <- length(x_ref)
  if (n == 0) {
    return(tibble(
      n = 0,
      prop_ref = NA_real_,
      prop_comp = NA_real_,
      flip_rate = NA_real_,
      flip_lower = NA_real_,
      flip_upper = NA_real_,
      AAC = NA_real_,
      acc_lower = NA_real_,
      acc_upper = NA_real_,
      RAC = NA_real_,
      rac_lower = NA_real_,
      rac_upper = NA_real_,
      count_TT = 0,
      count_TF = 0,
      count_FT = 0,
      count_FF = 0,
      p_exact = NA_real_
    ))
  }

  count_TT <- sum(x_ref & x_comp)
  count_TF <- sum(x_ref & !x_comp)
  count_FT <- sum(!x_ref & x_comp)
  count_FF <- sum(!x_ref & !x_comp)

  prop_ref  <- mean(x_ref)
  prop_comp <- mean(x_comp)

  flip_ci <- wilson_ci_vec(count_TF + count_FT, n)

  AAC <- prop_comp - prop_ref
  se_term <- (count_TF + count_FT) - ((count_TF - count_FT)^2 / n)
  se_term <- max(0, se_term)
  acc_se <- sqrt(se_term) / n
  acc_lower <- AAC - 1.96 * acc_se
  acc_upper <- AAC + 1.96 * acc_se

  if (prop_ref == 0) {
    RAC <- NA_real_
    rac_lower <- NA_real_
    rac_upper <- NA_real_
  } else {
    RAC <- prop_comp / prop_ref
    sum_ref  <- sum(x_ref)
    sum_comp <- sum(x_comp)
    se_log_rac <- sqrt((count_TF + count_FT) / ((sum_ref + 0.5) * (sum_comp + 0.5)))
    cc <- 0.5 / n
    rac_lower <- exp(log(RAC + cc) - 1.96 * se_log_rac)
    rac_upper <- exp(log(RAC + cc) + 1.96 * se_log_rac)
  }

  # exact McNemar test implemented as exact binomial test on discordant pairs
  p_exact <- if ((count_TF + count_FT) == 0) {
    1
  } else {
    binom.test(
      x = min(count_TF, count_FT),
      n = count_TF + count_FT,
      p = 0.5,
      alternative = "two.sided"
    )$p.value
  }

  tibble(
    n = n,
    prop_ref = prop_ref,
    prop_comp = prop_comp,
    flip_rate = flip_ci$estimate,
    flip_lower = flip_ci$lower,
    flip_upper = flip_ci$upper,
    AAC = AAC,
    acc_lower = acc_lower,
    acc_upper = acc_upper,
    RAC = RAC,
    rac_lower = rac_lower,
    rac_upper = rac_upper,
    count_TT = count_TT,
    count_TF = count_TF,
    count_FT = count_FT,
    count_FF = count_FF,
    p_exact = p_exact
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
df <- read.csv(input_csv, check.names = FALSE, stringsAsFactors = FALSE) %>%
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
    case_flip_1_2            = as_bool(case_full_llmjudge_accuracy_pass1 != case_full_llmjudge_accuracy_pass2)
    
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

model_order <- df %>%
  filter(case_condition_cat == "baseline") %>%
  group_by(case_model_name_cat) %>%
  summarise(overall_acc = mean(case_full_llmjudge_accuracy_pass1, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(overall_acc), case_model_name_cat) %>%
  pull(case_model_name_cat)

df <- df %>%
  mutate(
    case_model_name_cat = factor(case_model_name_cat, levels = model_order),
    case_origin_cat = factor(case_origin_cat, levels = c("MultiCaRe", "MedMCQA"))
  )

# ---------- summary builders ----------
summarize_rate <- function(df, value_col, condition_level, metric_name) {
  cond_df <- df %>% filter(case_condition_cat == condition_level)

  by_origin <- cond_df %>%
    group_by(case_model_name_cat, case_model_type_cat, case_origin_cat) %>%
    summarise(
      events = sum(.data[[value_col]], na.rm = TRUE),
      n = sum(!is.na(.data[[value_col]])),
      .groups = "drop"
    )

  overall <- cond_df %>%
    group_by(case_model_name_cat, case_model_type_cat) %>%
    summarise(
      events = sum(.data[[value_col]], na.rm = TRUE),
      n = sum(!is.na(.data[[value_col]])),
      .groups = "drop"
    ) %>%
    mutate(case_origin_cat = "Overall")

  out <- bind_rows(by_origin, overall) %>%
    mutate(metric = metric_name, condition = condition_level)

  out <- bind_cols(out, wilson_ci_vec(out$events, out$n))

  origin_tests <- cond_df %>%
    group_by(case_model_name_cat, case_model_type_cat) %>%
    group_modify(~ {
      x_multi <- .x[[value_col]][.x$case_origin_cat == "MultiCaRe"]
      x_med   <- .x[[value_col]][.x$case_origin_cat == "MedMCQA"]

      events_multi <- sum(x_multi, na.rm = TRUE)
      events_med   <- sum(x_med,   na.rm = TRUE)
      n_multi <- sum(!is.na(x_multi))
      n_med   <- sum(!is.na(x_med))

      tibble(
        p_origin = fisher.test(
          matrix(c(events_multi, n_multi - events_multi,
                   events_med,   n_med   - events_med),
                 nrow = 2, byrow = TRUE)
        )$p.value
      )
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
    group_modify(~ paired_stats(
      .x$case_full_llmjudge_accuracy_pass1,
      .x$case_full_llmjudge_accuracy_pass2
    )) %>%
    ungroup()

  overall <- cond_df %>%
    group_by(case_model_name_cat, case_model_type_cat) %>%
    group_modify(~ paired_stats(
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
      p_change_holm = p.adjust(p_exact, method = "holm"),
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
    group_modify(~ paired_stats(.x$ref_acc, .x$comp_acc)) %>%
    ungroup()

  overall <- merged %>%
    group_by(case_model_name_cat, case_model_type_cat) %>%
    group_modify(~ paired_stats(.x$ref_acc, .x$comp_acc)) %>%
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
      p_change_holm = p.adjust(p_exact, method = "holm"),
      sig_change = sig_stars(p_change_holm),
      p_change_label = format_p_label(p_change_holm, sig_change)
    ) %>%
    ungroup()
}

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
    mutate(changed = ref_acc != comp_acc)

  merged %>%
    group_by(case_model_name_cat) %>%
    group_modify(~ {
      m1 <- .x %>% filter(case_origin_cat == "MultiCaRe")
      m2 <- .x %>% filter(case_origin_cat == "MedMCQA")

      x1 <- sum(m1$changed, na.rm = TRUE)
      x2 <- sum(m2$changed, na.rm = TRUE)
      n1 <- nrow(m1)
      n2 <- nrow(m2)

      tibble(
        p_origin = fisher.test(
          matrix(c(x1, n1 - x1, x2, n2 - x2), nrow = 2, byrow = TRUE)
        )$p.value
      )
    }) %>%
    ungroup() %>%
    mutate(
      condition = condition_level,
      p_origin_holm = p.adjust(p_origin, method = "holm"),
      sig_origin = sig_stars(p_origin_holm),
      p_origin_label = format_p_label(p_origin_holm, sig_origin)
    )
}

# ---------- compute tables ----------
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
# 
write.csv(accuracy_all, "figure_stats/accuracy_summary.csv", row.names = FALSE)
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

# ----------  plot6b ----------
plot6b_dat <- vs_baseline_all %>%
  filter(condition %in% c("adjacent", "diff_1")) %>%
  mutate(
    condition_label = factor(condition,
                             levels = c("adjacent", "diff_1"),
                             labels = c("Adjacent", "Differential Specialty 1"))
  )


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

supp3 <- make_rate_plot(
  dat = plot3_dat,
  model_order = model_order,
  xlab = "Flip rate (Pass 1 vs Pass 2)",
  core_limits = c(0, 1)
)
save_pdf(supp3, "supp3.pdf", width = 6, height = 6.9)


# ---------- Supplementary Figure 4 / supp4 ----------
supp4_dat <- pass_flip_all %>%
  filter(condition %in% c("adjacent", "diff_1", "diff_2")) %>%
  transmute(
    case_model_name_cat,
    case_model_type_cat,
    case_origin_cat,
    condition_label = factor(condition,
                             levels = c("adjacent", "diff_1", "diff_2"),
                              labels = c("Adjacent", "Differential Specialty 1","Differential Specialty 2")),
    estimate = estimate,
    lower = lower,
    upper = upper,
    p_origin_holm = p_origin_holm,
    sig_origin = sig_origin,
    p_origin_label = p_origin_label
  )

supp4 <- make_rate_plot(
  dat = supp4_dat,
  model_order = model_order,
  xlab = "Flip rate (Pass 1 vs Pass 2)",
  core_limits = c(0, 1),
  facet_var = "condition_label",
  facet_ncol = 1
)
save_pdf(supp4, "supp4.pdf", width = 11.8, height = 10.8)


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
  xlab = "Flip rate vs baseline",
  core_limits = c(0, 0.5)
)
save_pdf(supp5, "supp5.pdf", width = 11.8, height = 5)


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
save_pdf(supp6, "supp6.pdf", width = 15.0, height = 5.9)


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
save_pdf(supp7, "supp7.pdf", width = 15.0, height = 5.9)

#message("Done. Wrote plot1-6.pdf, supp1-5.pdf, and figure_stats/*.csv")




