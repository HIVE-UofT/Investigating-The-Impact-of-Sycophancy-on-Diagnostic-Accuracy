

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

p_label <- function(p) {
  ifelse(
    is.na(p), "p=NA",
    ifelse(
      p < 0.001, "p<0.001",
      sprintf("p=%.3f", p)
    )
  )
}

theme_jamia <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey88", linewidth = 0.35),
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey30"),
      legend.position = "bottom"
    )
}

wilson_ci <- function(x, n, z = 1.96) {
  if (is.na(n) || n == 0) {
    return(c(NA_real_, NA_real_))
  }
  p <- x / n
  denom  <- 1 + z^2 / n
  center <- p + z^2 / (2 * n)
  spread <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2)))
  lower  <- (center - spread) / denom
  upper  <- (center + spread) / denom
  c(lower, upper)
}

fisher_origin_p <- function(data, outcome_col) {
  x <- data %>%
    filter(case_origin_cat %in% c("MultiCaRe", "MedMCQA")) %>%
    mutate(outcome = .data[[outcome_col]]) %>%
    group_by(case_origin_cat) %>%
    summarise(
      success = sum(outcome, na.rm = TRUE),
      fail = sum(!outcome, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(x) != 2) return(NA_real_)
  mat <- matrix(c(x$success[1], x$fail[1], x$success[2], x$fail[2]), nrow = 2, byrow = TRUE)
  suppressWarnings(fisher.test(mat)$p.value)
}

exact_mcnemar_p <- function(p1, p2) {
  b <- sum(p1 & !p2, na.rm = TRUE)  # T->F
  c <- sum(!p1 & p2, na.rm = TRUE)  # F->T
  if ((b + c) == 0) return(1)
  stats::binom.test(b, b + c, p = 0.5)$p.value
}

jaccard_similarity <- function(a, b) {
  inter <- sum(a & b, na.rm = TRUE)
  union <- sum(a | b, na.rm = TRUE)
  if (union == 0) return(NA_real_)
  inter / union
}

pairwise_jaccard_long <- function(wide_bool) {
  models <- colnames(wide_bool)
  grid <- expand.grid(model_1 = models, model_2 = models, stringsAsFactors = FALSE)
  
  grid$jaccard <- purrr::map2_dbl(grid$model_1, grid$model_2, function(m1, m2) {
    jaccard_similarity(wide_bool[[m1]], wide_bool[[m2]])
  })
  
  sim_mat <- grid %>%
    tidyr::pivot_wider(names_from = model_2, values_from = jaccard) %>%
    tibble::column_to_rownames("model_1") %>%
    as.matrix()
  
  sim_mat_cluster <- sim_mat
  sim_mat_cluster[is.na(sim_mat_cluster)] <- 0
  hc <- hclust(as.dist(1 - sim_mat_cluster), method = "average")
  ord <- hc$labels[hc$order]
  
  grid %>%
    mutate(
      model_1 = factor(model_1, levels = rev(ord)),
      model_2 = factor(model_2, levels = ord)
    )
}

save_pdf <- function(filename, plot_obj, width = 12, height = 8) {
  grDevices::pdf(filename, width = width, height = height, useDingbats = FALSE)
  print(plot_obj)
  grDevices::dev.off()
}

# -----------------------------
# Data
# -----------------------------
safe_setwd()

df <- read.csv("llm_judge_final_20251026_161301.csv", stringsAsFactors = FALSE)

dir.create("alternative_figures", showWarnings = FALSE)

df <- df %>%
  mutate(
    case_origin_cat = recode(case_origin_cat,
                             "MedMCQA_Train" = "MedMCQA",
                             "Casestudy" = "MultiCaRe",
                             .default = case_origin_cat
    ),
    model = recode(
      case_model_name_cat,
      "anthropic/claude-sonnet-4" = "Claude-4",
      "google/gemini-2.0-flash" = "Gemini-2.0",
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

model_order <- df %>%
  filter(case_condition_cat == "baseline") %>%
  group_by(model) %>%
  summarise(overall_acc = mean(acc_pass1, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(overall_acc)) %>%
  pull(model)

transition_levels <- c("T->T", "T->F", "F->T", "F->F")
transition_palette <- c(
  "T->T" = "#485696",
  "T->F" = "#da627d",
  "F->T" = "#ddbea9",
  "F->F" = "#bdbdbd"
)

rate_palette <- c(
  low = "#f7fbff",
  mid = "#e8dbc5",
  high = "#2f0147"
)

acc <- read.csv("figure_stats/accuracy_summary.csv")

acc_by_origin <- acc %>% filter(case_origin_cat %in% c("MultiCaRe","MedMCQA"))
acc_overall <- acc %>% filter(case_origin_cat %in% c("Overall"))
acc_p <- acc_overall %>% mutate(case_origin_cat == "Holm p") 
  

# -----------------------------
# 1) Accuracy heatmap with p-value column
# manuscript-ready alternative to repeated accuracy panels
# -----------------------------
# 1. This is where we use mutate to add the text_color column
acc_heat_df <- bind_rows(acc_by_origin, acc_overall) %>%
  mutate(
    metric_col = case_origin_cat,
    value = estimate,
    label = percent(estimate, accuracy = 1)
  ) %>%
  select(condition, case_model_name_cat, metric_col, value, label, p_origin_label) %>%
  bind_rows(
    acc_p %>%
      transmute(
        condition,
        case_model_name_cat,
        metric_col = "Holm p",
        value = NA_real_,
        label = as.character(p_origin_label)
      )
  ) %>%
  mutate(
    model = factor(case_model_name_cat, levels = rev(model_order)),
    metric_col = factor(metric_col, levels = c("MultiCaRe", "Overall", "MedMCQA", "Holm p")),
    # THIS is the new line inside mutate:
    text_color = ifelse(!is.na(value) & value > 0.65, "white", "black"),
    condition = factor(condition,
                   levels = c("baseline", "adjacent", "diff_1", "diff_2"),
                   labels = c("Baseline", "Adjacent", "Differential specialty 1", "Differential specialty 2"))
)


# 2. This is the plot, which now uses that text_color column
p_acc_heat <- ggplot(acc_heat_df, aes(x = metric_col, y = model, fill = value)) +
  geom_tile(color = "white", linewidth = 0.75, width = 0.95, height = 0.95) +
  # Use the text_color column here:
  geom_text(aes(label = label, color = text_color), size = 3.2, fontface = "bold", show.legend = FALSE) +
  scale_color_identity() + # This forces ggplot to use the literal colors "white" and "black"
  facet_wrap(~ condition, nrow = 1) +
  scale_fill_gradientn(
    colours = c(rate_palette["low"], rate_palette["mid"], rate_palette["high"]),
    limits = c(0, 1),
    na.value = "white",
    name = "Accuracy"
  ) +
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = NULL
  ) +
  theme_jamia(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

save_pdf("plot2.pdf", p_acc_heat, width = 13, height = 7)

# -----------------------------
# 2) Transition-composition plot for Pass 1 -> Pass 2 under baseline
# manuscript-ready alternative to repeated flip-rate panels
# -----------------------------
baseline_df <- df %>% filter(case_condition_cat == "baseline")

make_transition <- function(p1, p2) {
  case_when(
    !is.na(p1) & !is.na(p2) & p1 & p2 ~ "T->T",
    !is.na(p1) & !is.na(p2) & p1 & !p2 ~ "T->F",
    !is.na(p1) & !is.na(p2) & !p1 & p2 ~ "F->T",
    !is.na(p1) & !is.na(p2) & !p1 & !p2 ~ "F->F",
    TRUE ~ NA_character_
  )
}

baseline_trans <- baseline_df %>%
  mutate(transition = make_transition(acc_pass1, acc_pass2))

trans_by_origin <- baseline_trans %>%
  filter(!is.na(transition)) %>%
  count(case_origin_cat, model, transition, name = "n") %>%
  group_by(case_origin_cat, model) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

trans_overall <- baseline_trans %>%
  filter(!is.na(transition)) %>%
  count(model, transition, name = "n") %>%
  group_by(model) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(case_origin_cat = "Overall")

trans_plot_df <- bind_rows(trans_by_origin, trans_overall) %>%
  mutate(
    case_origin_cat = factor(case_origin_cat, levels = c("MultiCaRe", "Overall", "MedMCQA")),
    model = factor(model, levels = rev(model_order)),
    transition = factor(transition, levels = transition_levels)
  )

mcnemar_p_df <- bind_rows(
  baseline_trans %>%
    group_by(case_origin_cat, model) %>%
    group_modify(~ tibble(p = exact_mcnemar_p(.x$acc_pass1, .x$acc_pass2))) %>%
    ungroup(),
  baseline_trans %>%
    group_by(model) %>%
    group_modify(~ tibble(p = exact_mcnemar_p(.x$acc_pass1, .x$acc_pass2))) %>%
    ungroup() %>%
    mutate(case_origin_cat = "Overall")
) %>%
  mutate(case_origin_cat = factor(case_origin_cat, levels = c("MultiCaRe", "Overall", "MedMCQA"))) %>%
  group_by(case_origin_cat) %>%
  mutate(p_adj = p.adjust(p, method = "holm")) %>%
  ungroup() %>%
  mutate(
    model = factor(model, levels = rev(model_order)),
    label = p_label(p_adj)
  )

p_transition <- ggplot(trans_plot_df, aes(x = prop, y = model, fill = transition)) +
  geom_col(width = 0.82, color = "white", linewidth = 0.4) +
  geom_text(
    data = mcnemar_p_df,
    aes(x = 1.04, y = model, label = label),
    inherit.aes = FALSE,
    size = 3.0,
    hjust = 0,
    fontface = "bold"
  ) +
  facet_wrap(~ case_origin_cat, nrow = 1) +
  coord_cartesian(xlim = c(0, 1.18), clip = "off") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = transition_palette, name = "Transition") +
  labs(
    title = "",
    subtitle = "",
    y = NULL
  ) +
  theme_jamia(base_size = 11) +
  theme(
    plot.margin = margin(5.5, 45, 5.5, 5.5),
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.35),
    legend.position = "bottom"
  )

save_pdf("alternative_figures/alt_figB_transition_composition_baseline.pdf", p_transition, width = 15, height = 7)



# -----------------------------
# 3) UpSet plots
# supplement-friendly / novel perspective
# Revised in v8:
# - uses a compact custom UpSet layout that mirrors the original 4-panel structure
# - fixes x-axis gaps by renumbering displayed intersections after filtering
# - keeps top intersection bars neutral and colors only active dots by model family
# - colors horizontal set-size bars by model family
# - sorts model rows by set size within each plot and labels sizes at bar ends
# - aligns model labels, rows, top bars, and dot columns with shared axes/alignment
# -----------------------------
dir.create("alternative_figures/upset_tables", showWarnings = FALSE)

# 3a) Baseline pass-1 correctness intersections
upset_correct_df <- df %>%
  filter(case_condition_cat == "baseline") %>%
  select(case_origin_cat, case_id_str, model, acc_pass1) %>%
  mutate(acc_pass1 = tidyr::replace_na(acc_pass1, FALSE)) %>%
  pivot_wider(
    id_cols = c(case_origin_cat, case_id_str),
    names_from = model,
    values_from = acc_pass1,
    values_fill = FALSE
  )

all_models <- model_order


model_family_lookup <- c(
  "GPT-5" = "Proprietary general purpose",
  "Claude-4" = "Proprietary general purpose",
  "Gemini-2.5" = "Proprietary general purpose",
  "Gemini-2.0" = "Proprietary general purpose",
  "GPT-4o" = "Proprietary general purpose",
  "Llama-3.1-8B" = "Open-weight general purpose",
  "Llama-3.2-1B" = "Open-weight general purpose",
  "Llama-3.2-3B" = "Open-weight general purpose",
  "MedGemma-27B" = "Medical-specialized",
  "MedGemma-4B" = "Medical-specialized"
)

model_family_levels <- c(
  "Proprietary general purpose", 
  "Open-weight general purpose", 
  "Medical-specialized"
)

model_family_colors <- c(
  "Proprietary general purpose" = "#957d95",
  "Open-weight general purpose" = "#83c5be",
  "Medical-specialized" = "#e9c46a"
)


stopifnot(all(all_models %in% names(model_family_lookup)))

make_intersection_members <- function(data, sets) {
  mat <- as.data.frame(dplyr::select(data, dplyr::all_of(sets)))
  apply(mat, 1, function(z) sets[as.logical(z)])
}

intersection_table <- function(data, sets) {
  members_list <- make_intersection_members(data, sets)
  
  tibble::tibble(
    case_id_str = data$case_id_str,
    members = purrr::map(members_list, identity),
    degree = purrr::map_int(members_list, length)
  ) %>%
    mutate(
      intersection = purrr::map_chr(
        members,
        ~ if (length(.x) == 0) "(none)" else paste(.x, collapse = " & ")
      )
    ) %>%
    group_by(intersection, degree) %>%
    summarise(
      intersection_n = dplyr::n(),
      members = list(members[[1]]),
      case_ids = list(sort(case_id_str)),
      .groups = "drop"
    ) %>%
    arrange(desc(intersection_n), desc(degree), intersection)
}

prepare_intersection_tables <- function(data, sets, n_intersections = 15) {
  full_tbl <- intersection_table(data, sets)
  
  display_tbl <- full_tbl %>%
    filter(degree > 0) %>%
    slice_head(n = n_intersections) %>%
    mutate(
      intersection_id = dplyr::row_number(),
      x = intersection_id,
      models_in_exact_intersection = purrr::map_chr(
        members,
        ~ paste(.x, collapse = "; ")
      ),
      case_ids_in_exact_intersection = purrr::map_chr(
        case_ids,
        ~ paste(.x, collapse = "; ")
      )
    )
  
  list(full = full_tbl, display = display_tbl)
}

write_intersection_table <- function(full_tbl, display_tbl, filename) {
  out <- full_tbl %>%
    mutate(
      models_in_exact_intersection = purrr::map_chr(
        members,
        ~ if (length(.x) == 0) "(none)" else paste(.x, collapse = "; ")
      ),
      case_ids_in_exact_intersection = purrr::map_chr(
        case_ids,
        ~ paste(.x, collapse = "; ")
      )
    ) %>%
    left_join(
      display_tbl %>% select(intersection, shown_in_plot = x),
      by = "intersection"
    ) %>%
    mutate(shown_in_plot = ifelse(is.na(shown_in_plot), NA_integer_, shown_in_plot)) %>%
    transmute(
      shown_in_plot,
      intersection_n,
      degree,
      models_in_exact_intersection,
      case_ids_in_exact_intersection
    )
  
  utils::write.csv(out, filename, row.names = FALSE)
}

make_family_legend <- function(color_map, legend_order) {
  legend_df <- tibble::tibble(
    family = factor(legend_order, levels = legend_order),
    x = 1,
    y = rev(seq_along(legend_order)) # Keep these as normal integers (1, 2, 3)
  )
  
  ggplot(legend_df, aes(x = x, y = y)) +
    geom_point(aes(color = family), size = 4.2, show.legend = FALSE) +
    geom_text(
      aes(x = x + 0.22, label = family),
      hjust = 0,
      size = 3.8,
      fontface = "plain"
    ) +
    annotate(
      "text",
      x = 1,
      y = max(legend_df$y) + 1.2, # Title sits nicely above the top item
      label = "Legend",
      hjust = 0,
      fontface = "bold",
      size = 4.5
    ) +
    scale_color_manual(values = color_map, drop = FALSE) +
    coord_cartesian(
      xlim = c(0.90, 3.50),
      # THE FIX IS HERE: 
      # Decrease the first number (e.g., -4.0, -6.0) to add more empty space 
      # at the bottom, which squishes the legend items closer together at the top.
      ylim = c(-2.0, max(legend_df$y) + 1.5), 
      clip = "off"
    ) +
    theme_void() +
    theme(
      plot.margin = margin(6, 6, 2, 10)
    )
}

make_compact_upset_plot <- function(data, title_text, n_intersections = 15) {
  tables <- prepare_intersection_tables(data, all_models, n_intersections = n_intersections)
  full_tbl <- tables$full
  display_tbl <- tables$display
  
  set_sizes <- tibble::tibble(
    model = all_models,
    set_n = purrr::map_int(all_models, ~ sum(as.logical(data[[.x]]), na.rm = TRUE)),
    model_family = unname(model_family_lookup[all_models])
  ) %>%
    arrange(desc(set_n), model) %>%
    mutate(
      row_id = dplyr::row_number(),
      y = rev(row_id),
      model_family = factor(model_family, levels = model_family_levels)
    )
  
  member_lookup <- setNames(display_tbl$members, display_tbl$intersection_id)
  
  matrix_df <- expand.grid(
    intersection_id = display_tbl$intersection_id,
    model = set_sizes$model,
    stringsAsFactors = FALSE
  ) %>%
    tibble::as_tibble() %>%
    left_join(
      set_sizes %>% select(model, y, model_family),
      by = "model"
    ) %>%
    mutate(
      in_set = purrr::map2_lgl(
        intersection_id,
        model,
        ~ .y %in% member_lookup[[as.character(.x)]]
      )
    )
  
  segment_df <- matrix_df %>%
    filter(in_set) %>%
    group_by(intersection_id) %>%
    summarise(
      ymin = min(y),
      ymax = max(y),
      .groups = "drop"
    ) %>%
    filter(ymin < ymax)
  
  stripe_df <- set_sizes %>%
    mutate(stripe = row_id %% 2 == 0) %>%
    filter(stripe)
  
  x_limits <- c(0.5, nrow(display_tbl) + 0.5)
  y_limits <- c(0.5, nrow(set_sizes) + 0.5)
  
  legend_plot <- make_family_legend(
    color_map = model_family_colors,
    legend_order = model_family_levels
  )
  
  top_plot <- ggplot(display_tbl, aes(x = x, y = intersection_n)) +
    geom_col(
      width = 0.78,
      fill = "#7a7a7a",
      color = "grey25",
      linewidth = 0.18
    ) +
    geom_text(
      aes(label = intersection_n),
      vjust = -0.22,
      size = 3.8,
      fontface = "plain"
    ) +
    scale_x_continuous(
      limits = x_limits,
      breaks = display_tbl$x,
      labels = NULL,
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
    labs(x = NULL, y = "Cases in exact intersection") +
    theme_jamia(base_size = 10.5) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(4, 8, 2, 2)
    )
  
  left_plot <- ggplot(set_sizes, aes(y = y)) +
    geom_rect(
      aes(
        xmin = 0,
        xmax = set_n,
        ymin = y - 0.36,
        ymax = y + 0.36,
        fill = model_family
      ),
      color = NA,
      inherit.aes = FALSE
    ) +
    geom_text(
      aes(x = set_n, label = set_n),
      hjust = -0.12,
      size = 3.9,
      fontface = "plain"
    ) +
    scale_fill_manual(values = model_family_colors, drop = FALSE, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
    scale_y_continuous(
      limits = y_limits,
      breaks = set_sizes$y,
      labels = set_sizes$model,
      position = "right",
      expand = c(0, 0)
    ) +
    labs(x = "Cases in set", y = NULL) +
    theme_jamia(base_size = 10.5) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 11.2, face = "plain"),
      legend.position = "none",
      plot.margin = margin(2, 0, 4, 10)
    )
  
  matrix_plot <- ggplot(matrix_df, aes(x = intersection_id, y = y)) +
    geom_rect(
      data = stripe_df,
      aes(
        xmin = x_limits[1],
        xmax = x_limits[2],
        ymin = y - 0.5,
        ymax = y + 0.5
      ),
      inherit.aes = FALSE,
      fill = "grey98",
      color = NA
    ) +
    geom_point(color = "grey80", size = 1.7) +
    geom_segment(
      data = segment_df,
      aes(x = intersection_id, xend = intersection_id, y = ymin, yend = ymax),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.70,
      lineend = "round"
    ) +
    geom_point(
      data = matrix_df %>% filter(in_set),
      aes(color = model_family),
      size = 2.9,
      show.legend = FALSE
    ) +
    scale_color_manual(values = model_family_colors, drop = FALSE, guide = "none") +
    scale_x_continuous(
      limits = x_limits,
      breaks = display_tbl$x,
      labels = NULL,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = set_sizes$y,
      labels = NULL,
      expand = c(0, 0)
    ) +
    labs(x = NULL, y = NULL) +
    theme_jamia(base_size = 10.5) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.margin = margin(2, 8, 4, 0)
    )
  
  right_aligned <- cowplot::align_plots(
    top_plot,
    matrix_plot,
    align = "v",
    axis = "lr"
  )
  top_plot_aligned <- right_aligned[[1]]
  matrix_plot_aligned <- right_aligned[[2]]
  
  bottom_aligned <- cowplot::align_plots(
    left_plot,
    matrix_plot_aligned,
    align = "h",
    axis = "tb"
  )
  left_plot_aligned <- bottom_aligned[[1]]
  matrix_plot_final <- bottom_aligned[[2]]
  
  left_width <- 0.44
  right_width <- 0.56
  
  top_row <- cowplot::plot_grid(
    legend_plot,
    top_plot_aligned,
    ncol = 2,
    rel_widths = c(left_width, right_width),
    align = "v"
  )
  
  bottom_row <- cowplot::plot_grid(
    left_plot_aligned,
    matrix_plot_final,
    ncol = 2,
    rel_widths = c(left_width, right_width),
    align = "h",
    axis = "tb"
  )
  
  title_plot <- ggplot() +
    annotate(
      "text",
      x = 0,
      y = 1,
      label = title_text,
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      size = 3.9
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void() +
    theme(plot.margin = margin(0, 0, 2, 0))
  
  combined_plot <- cowplot::plot_grid(
    title_plot,
    cowplot::plot_grid(
      top_row,
      bottom_row,
      ncol = 1,
      rel_heights = c(0.4, 0.6)
    ),
    ncol = 1,
    rel_heights = c(0.06, 0.94)
  )
  
  list(
    plot = combined_plot,
    full_table = full_tbl,
    display_table = display_tbl
  )
}

p_correct_multicare_obj <- make_compact_upset_plot(
  upset_correct_df %>% filter(case_origin_cat == "MultiCaRe"),
  "",
  n_intersections = 15
)

p_correct_medmcqa_obj <- make_compact_upset_plot(
  upset_correct_df %>% filter(case_origin_cat == "MedMCQA"),
  "",
  n_intersections = 15
)

save_pdf(
  "alternative_figures/alt_supp_upset_baseline_correct_MultiCaRe.pdf",
  p_correct_multicare_obj$plot,
  width = 11,
  height = 4.5
)

save_pdf(
  "alternative_figures/alt_supp_upset_baseline_correct_MedMCQA.pdf",
  p_correct_medmcqa_obj$plot,
  width = 11,
  height = 4.5
)

write_intersection_table(
  p_correct_multicare_obj$full_table,
  p_correct_multicare_obj$display_table,
  "alternative_figures/upset_tables/alt_supp_upset_baseline_correct_MultiCaRe_intersections.csv"
)

write_intersection_table(
  p_correct_medmcqa_obj$full_table,
  p_correct_medmcqa_obj$display_table,
  "alternative_figures/upset_tables/alt_supp_upset_baseline_correct_MedMCQA_intersections.csv"
)

# 3b) Baseline pass1->pass2 flip intersections
upset_flip_df <- df %>%
  filter(case_condition_cat == "baseline") %>%
  select(case_origin_cat, case_id_str, model, flip_pass1_to_2) %>%
  mutate(flip_pass1_to_2 = tidyr::replace_na(flip_pass1_to_2, FALSE)) %>%
  pivot_wider(
    id_cols = c(case_origin_cat, case_id_str),
    names_from = model,
    values_from = flip_pass1_to_2,
    values_fill = FALSE
  )

p_flip_multicare_obj <- make_compact_upset_plot(
  upset_flip_df %>% filter(case_origin_cat == "MultiCaRe"),
  "Pass1->Pass2 flip intersections under baseline: MultiCaRe",
  n_intersections = 15
)

p_flip_medmcqa_obj <- make_compact_upset_plot(
  upset_flip_df %>% filter(case_origin_cat == "MedMCQA"),
  "Pass1->Pass2 flip intersections under baseline: MedMCQA",
  n_intersections = 15
)

save_pdf(
  "alternative_figures/alt_supp_upset_baseline_flips_MultiCaRe.pdf",
  p_flip_multicare_obj$plot,
  width = 14,
  height = 6.4
)

save_pdf(
  "alternative_figures/alt_supp_upset_baseline_flips_MedMCQA.pdf",
  p_flip_medmcqa_obj$plot,
  width = 14,
  height = 6.4
)

write_intersection_table(
  p_flip_multicare_obj$full_table,
  p_flip_multicare_obj$display_table,
  "alternative_figures/upset_tables/alt_supp_upset_baseline_flips_MultiCaRe_intersections.csv"
)

write_intersection_table(
  p_flip_medmcqa_obj$full_table,
  p_flip_medmcqa_obj$display_table,
  "alternative_figures/upset_tables/alt_supp_upset_baseline_flips_MedMCQA_intersections.csv"
)

# -----------------------------
# 4) Pairwise model-similarity heatmaps
# supplement-friendly and scientifically useful
# -----------------------------
wide_acc_baseline <- df %>%
  filter(case_condition_cat == "baseline") %>%
  select(case_origin_cat, case_id_str, model, acc_pass1) %>%
  mutate(task_id = paste(case_origin_cat, case_id_str, sep = " | ")) %>%
  select(task_id, model, acc_pass1) %>%
  pivot_wider(names_from = model, values_from = acc_pass1, values_fill = FALSE) %>%
  select(all_of(all_models))

wide_flip_baseline <- df %>%
  filter(case_condition_cat == "baseline") %>%
  select(case_origin_cat, case_id_str, model, flip_pass1_to_2) %>%
  mutate(task_id = paste(case_origin_cat, case_id_str, sep = " | ")) %>%
  select(task_id, model, flip_pass1_to_2) %>%
  pivot_wider(names_from = model, values_from = flip_pass1_to_2, values_fill = FALSE) %>%
  select(all_of(all_models))

sim_acc_long <- pairwise_jaccard_long(wide_acc_baseline)
sim_flip_long <- pairwise_jaccard_long(wide_flip_baseline)

p_sim_acc <- ggplot(sim_acc_long, aes(model_2, model_1, fill = jaccard)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", jaccard)), size = 3) +
  scale_fill_gradientn(
    colours = c("#fff5f0", "#fcae91", "#fb6a4a", "#cb181d"),
    limits = c(0, 1),
    name = "Jaccard"
  ) +
  labs(
    title = "Model similarity based on baseline pass-1 correctness",
    x = NULL, y = NULL
  ) +
  theme_jamia(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

p_sim_flip <- ggplot(sim_flip_long, aes(model_2, model_1, fill = jaccard)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(is.na(jaccard), "NA", sprintf("%.2f", jaccard))), size = 3) +
  scale_fill_gradientn(
    colours = c("#f7fcf5", "#a1d99b", "#31a354", "#005a32"),
    limits = c(0, 1),
    na.value = "grey95",
    name = "Jaccard"
  ) +
  labs(
    title = "Model similarity based on baseline pass1->pass2 flips",
    x = NULL, y = NULL
  ) +
  theme_jamia(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

p_similarity <- p_sim_acc / p_sim_flip +
  plot_annotation(
    title = "Alternative Figure C. Pairwise model-similarity heatmaps",
    subtitle = "Top panel uses shared correctness patterns; bottom panel uses shared flip patterns."
  )

# save_pdf("alternative_figures/alt_supp_similarity_heatmaps.pdf", p_similarity, width = 12, height = 14)

# -----------------------------
# 5) Exploratory MDS + convex hull
# exploratory only; not recommended as the primary manuscript figure
# -----------------------------
behavior_signature <- df %>%
  transmute(
    model,
    model_type,
    task_id = paste(case_origin_cat, case_condition_cat, case_id_str, sep = " | "),
    signature = ifelse(is.na(acc_pass1), FALSE, acc_pass1)
  ) %>%
  pivot_wider(
    id_cols = c(model, model_type),
    names_from = task_id,
    values_from = signature,
    values_fill = FALSE
  )

sig_mat <- behavior_signature %>%
  select(-model, -model_type) %>%
  as.matrix()

rownames(sig_mat) <- behavior_signature$model

model_names <- rownames(sig_mat)
dist_mat <- matrix(0, nrow = length(model_names), ncol = length(model_names), dimnames = list(model_names, model_names))

for (i in seq_along(model_names)) {
  for (j in seq_along(model_names)) {
    sim_ij <- jaccard_similarity(sig_mat[i, ], sig_mat[j, ])
    dist_mat[i, j] <- ifelse(is.na(sim_ij), 1, 1 - sim_ij)
  }
}

mds_coords <- cmdscale(as.dist(dist_mat), k = 2)

mds_df <- tibble::tibble(
  model = rownames(mds_coords),
  dim1 = mds_coords[, 1],
  dim2 = mds_coords[, 2]
) %>%
  left_join(
    behavior_signature %>% distinct(model, model_type),
    by = "model"
  ) %>%
  mutate(
    model = factor(model, levels = model_order),
    model_type = factor(model_type, levels = c("Commercial", "Open-source"))
  )

p_mds <- ggplot(mds_df, aes(dim1, dim2, color = model_type, fill = model_type)) +
  ggforce::geom_mark_hull(
    aes(label = model_type),
    alpha = 0.08,
    expand = grid::unit(2, "mm"),
    show.legend = FALSE
  ) +
  geom_point(size = 3.5) +
  ggrepel::geom_text_repel(aes(label = model), size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("Commercial" = "#1f78b4", "Open-source" = "#e31a1c")) +
  scale_fill_manual(values = c("Commercial" = "#1f78b4", "Open-source" = "#e31a1c")) +
  labs(
    title = "Exploratory Figure D. Behavioral map of model similarity",
    subtitle = "MDS of case-level correctness signatures across all origins and prompting conditions. Convex hulls show model-type groupings only.",
    x = "MDS dimension 1",
    y = "MDS dimension 2",
    color = "Model type"
  ) +
  theme_jamia(base_size = 11)

save_pdf("alternative_figures/exploratory_alt_convex_hull_behavior_map.pdf", p_mds, width = 11, height = 8)

# -----------------------------
# 6) Optional compact heatmap for AAC direction across conditions
# supplementary descriptive figure
# -----------------------------
compare_to_baseline <- function(data, cond_name) {
  cond_data <- data %>%
    filter(case_condition_cat %in% c("baseline", cond_name)) %>%
    select(case_origin_cat, case_id_str, model, case_condition_cat, acc_pass1) %>%
    pivot_wider(names_from = case_condition_cat, values_from = acc_pass1)
  
  out_by_origin <- cond_data %>%
    group_by(case_origin_cat, model) %>%
    summarise(
      ft = sum(!baseline & .data[[cond_name]], na.rm = TRUE),
      tf = sum(baseline & !.data[[cond_name]], na.rm = TRUE),
      n = sum(!is.na(baseline) & !is.na(.data[[cond_name]])),
      AAC = ifelse(n == 0, NA_real_, (ft - tf) / n),
      p = exact_mcnemar_p(baseline, .data[[cond_name]]),
      .groups = "drop"
    )
  
  out_overall <- cond_data %>%
    group_by(model) %>%
    summarise(
      ft = sum(!baseline & .data[[cond_name]], na.rm = TRUE),
      tf = sum(baseline & !.data[[cond_name]], na.rm = TRUE),
      n = sum(!is.na(baseline) & !is.na(.data[[cond_name]])),
      AAC = ifelse(n == 0, NA_real_, (ft - tf) / n),
      p = exact_mcnemar_p(baseline, .data[[cond_name]]),
      .groups = "drop"
    ) %>%
    mutate(case_origin_cat = "Overall")
  
  bind_rows(out_by_origin, out_overall) %>%
    mutate(comparison = cond_name)
}

aac_heat_df <- bind_rows(
  compare_to_baseline(df, "adjacent"),
  compare_to_baseline(df, "diff_1"),
  compare_to_baseline(df, "diff_2")
) %>%
  mutate(
    comparison = factor(comparison,
                        levels = c("adjacent", "diff_1", "diff_2"),
                        labels = c("Adjacent vs baseline", "Different specialty 1 vs baseline", "Different specialty 2 vs baseline")
    ),
    case_origin_cat = factor(case_origin_cat, levels = c("MultiCaRe", "Overall", "MedMCQA")),
    model = factor(model, levels = rev(model_order))
  ) %>%
  group_by(comparison, case_origin_cat) %>%
  mutate(p_adj = p.adjust(p, method = "holm")) %>%
  ungroup() %>%
  mutate(label = paste0(sprintf("%.2f", AAC), "\n", p_label(p_adj)))

p_aac_heat <- ggplot(aac_heat_df, aes(x = case_origin_cat, y = model, fill = AAC)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = label), size = 3) +
  facet_wrap(~ comparison, nrow = 1) +
  scale_fill_gradient2(
    low = "#b2182b",
    mid = "white",
    high = "#2166ac",
    midpoint = 0,
    limits = c(-0.5, 0.5),
    name = "AAC"
  ) +
  labs(
    title = "Supplementary alternative. AAC heatmap versus baseline",
    subtitle = "Each tile shows AAC and the Holm-adjusted exact McNemar p-value within each origin panel.",
    x = NULL,
    y = NULL
  ) +
  theme_jamia(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

save_pdf("alternative_figures/alt_supp_AAC_heatmap.pdf", p_aac_heat, width = 15, height = 7)

