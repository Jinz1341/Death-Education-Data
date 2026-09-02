required_packages <- c("readr", "dplyr", "tidyr", "purrr", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Install required packages before running: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("Run this file with Rscript code/study1_grade_comparison.R.")
script_path <- gsub("~\\+~", " ", sub("^--file=", "", script_argument))
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
data_path <- file.path(repo_root, "data", "study1_public.csv")
results_dir <- file.path(repo_root, "results", "study1_grade_comparison")
figure_dir <- file.path(repo_root, "figures", "main")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

outcomes <- tibble::tribble(
  ~outcome, ~label, ~unit, ~pre, ~post,
  "ln_k", "ln(k)", "log units", "ln_k_pre", "ln_k_post",
  "allocation_immediate", "Immediate allocation", "percentage points", "allocation_immediate_pre", "allocation_immediate_post",
  "allocation_short", "Short-term allocation", "percentage points", "allocation_short_pre", "allocation_short_post",
  "allocation_long", "Long-term allocation", "percentage points", "allocation_long_pre", "allocation_long_post",
  "subjective_time", "Subjective feeling of time passage", "scale points", "subjective_time_pre", "subjective_time_post"
)

study1 <- readr::read_csv(data_path, show_col_types = FALSE)
required_columns <- unique(c(
  "participant_id", "story_condition", "gender", "age_years", "school_level",
  "ses_hollingshead", outcomes$pre, outcomes$post
))
if (!all(required_columns %in% names(study1))) {
  stop("Study 1 data are missing: ", paste(setdiff(required_columns, names(study1)), collapse = ", "))
}
if (nrow(study1) != 230L || n_distinct(study1$participant_id) != 230L) {
  stop("Study 1 data must contain 230 unique participants.")
}
if (!identical(as.integer(table(factor(study1$school_level, levels = c("grade_4", "grade_8")))), c(104L, 126L))) {
  stop("Study 1 grade sizes must be 104 and 126.")
}
if (any(is.na(study1[c("story_condition", "gender", "school_level", "ses_hollingshead", outcomes$pre, outcomes$post)]))) {
  stop("Unexpected missing design, demographic, or outcome values.")
}

grade4_age <- study1$age_years[study1$school_level == "grade_4"]
grade8_age <- study1$age_years[study1$school_level == "grade_8"]
if (any(is.na(grade4_age)) || !setequal(grade4_age, c(9, 10))) {
  stop("Grade 4 ages must be observed and limited to 9 and 10 years.")
}
if (sum(is.na(grade8_age)) != 9L || !setequal(grade8_age[!is.na(grade8_age)], 13:16)) {
  stop("Unexpected Grade 8 age distribution.")
}

allocation_pre <- with(study1, allocation_immediate_pre + allocation_short_pre + allocation_long_pre)
allocation_post <- with(study1, allocation_immediate_post + allocation_short_post + allocation_long_post)
max_pre_allocation_error <- max(abs(allocation_pre - 100))
max_post_allocation_error <- max(abs(allocation_post - 100))
if (max_pre_allocation_error > 1e-8 || max_post_allocation_error > 1e-8) {
  stop("Temporal-allocation percentages must sum to 100 at each wave.")
}

study1 <- study1 %>%
  mutate(
    school_level = factor(school_level, levels = c("grade_4", "grade_8")),
    change_immediate = allocation_immediate_post - allocation_immediate_pre,
    change_short = allocation_short_post - allocation_short_pre
  )

write_result <- function(data, filename) {
  readr::write_csv(data, file.path(results_dir, filename), na = "")
}

hedges_g <- function(grade4, grade8) {
  n4 <- length(grade4)
  n8 <- length(grade8)
  pooled_sd <- sqrt(((n4 - 1L) * var(grade4) + (n8 - 1L) * var(grade8)) / (n4 + n8 - 2L))
  correction <- 1 - 3 / (4 * (n4 + n8) - 9)
  correction * (mean(grade8) - mean(grade4)) / pooled_sd
}

format_p <- function(p_value) {
  ifelse(p_value < .001, "< .001", sub("^0", "", sprintf("%.3f", p_value)))
}

data_checks <- tibble(
  check = c(
    "analytic_n", "unique_participant_ids", "grade_4_n", "grade_8_n",
    "missing_age_years", "max_pre_allocation_total_error", "max_post_allocation_total_error"
  ),
  value = c(
    nrow(study1), n_distinct(study1$participant_id),
    sum(study1$school_level == "grade_4"), sum(study1$school_level == "grade_8"),
    sum(is.na(study1$age_years)), max_pre_allocation_error, max_post_allocation_error
  ),
  expected = c("230", "230", "104", "126", "9", "<= 1e-8", "<= 1e-8")
)
write_result(data_checks, "data_checks.csv")

long_changes <- purrr::pmap_dfr(
  outcomes,
  function(outcome, label, unit, pre, post) {
    study1 %>%
      transmute(
        participant_id,
        school_level,
        outcome = outcome,
        label = label,
        unit = unit,
        pre = .data[[pre]],
        post = .data[[post]],
        change = .data[[post]] - .data[[pre]]
      )
  }
)

descriptives <- long_changes %>%
  group_by(outcome, label, unit, school_level) %>%
  summarise(
    n = n(),
    pre_mean = mean(pre),
    pre_sd = sd(pre),
    post_mean = mean(post),
    post_sd = sd(post),
    mean_change = mean(change),
    change_sd = sd(change),
    change_se = change_sd / sqrt(n),
    change_ci_low = mean_change - qt(.975, n - 1L) * change_se,
    change_ci_high = mean_change + qt(.975, n - 1L) * change_se,
    .groups = "drop"
  )
write_result(descriptives, "grade_prepost_descriptives.csv")

simple_effects <- long_changes %>%
  group_by(outcome, label, unit, school_level) %>%
  group_modify(~{
    test <- t.test(.x$post, .x$pre, paired = TRUE)
    change <- .x$post - .x$pre
    tibble(
      n = length(change),
      mean_change = mean(change),
      ci_low = unname(test$conf.int[1]),
      ci_high = unname(test$conf.int[2]),
      t_statistic = unname(test$statistic),
      df = unname(test$parameter),
      p_value = test$p.value,
      dz = mean(change) / sd(change),
      significant = test$p.value < .05
    )
  }) %>%
  ungroup()
write_result(simple_effects, "grade_simple_effects.csv")

grade_contrasts <- long_changes %>%
  group_by(outcome, label, unit) %>%
  group_modify(~{
    grade4 <- .x$change[.x$school_level == "grade_4"]
    grade8 <- .x$change[.x$school_level == "grade_8"]
    test <- t.test(grade8, grade4)
    tibble(
      contrast = "grade_8_minus_grade_4",
      n_grade4 = length(grade4),
      n_grade8 = length(grade8),
      grade4_mean_change = mean(grade4),
      grade8_mean_change = mean(grade8),
      mean_change_difference = mean(grade8) - mean(grade4),
      ci_low = unname(test$conf.int[1]),
      ci_high = unname(test$conf.int[2]),
      t_statistic = unname(test$statistic),
      df = unname(test$parameter),
      p_value = test$p.value,
      hedges_g = hedges_g(grade4, grade8),
      significant = test$p.value < .05
    )
  }) %>%
  ungroup()
write_result(grade_contrasts, "grade_change_contrasts.csv")

allocation_manova <- manova(cbind(change_immediate, change_short) ~ school_level, data = study1)
allocation_stats <- summary(allocation_manova, test = "Pillai")$stats["school_level", ]
grade_allocation_manova <- tibble(
  analysis = "grade_difference_in_allocation_change",
  term = "school_level",
  response_components = "change_immediate + change_short",
  redundant_component = "change_long",
  pillai_trace = unname(allocation_stats["Pillai"]),
  approx_f = unname(allocation_stats["approx F"]),
  df1 = unname(allocation_stats["num Df"]),
  df2 = unname(allocation_stats["den Df"]),
  p_value = unname(allocation_stats["Pr(>F)"])
)
write_result(grade_allocation_manova, "grade_allocation_manova.csv")

plot_data <- simple_effects %>%
  left_join(grade_contrasts %>% select(outcome, between_p = p_value), by = "outcome") %>%
  mutate(
    grade = factor(as.character(school_level), levels = c("grade_8", "grade_4")),
    label = factor(label, levels = outcomes$label),
    label_expression = ifelse(
      outcome == "ln_k",
      "paste('ln(', italic(k), ')')",
      paste0("'", label, "'")
    ),
    panel_label = paste0(
      "atop(", label_expression,
      ", paste('Between-cohort change difference: ', italic(p), ' ",
      ifelse(between_p < .001, "< .001", paste0("= ", format_p(between_p))), "'))"
    ),
    row_label = paste0(
      "paste('Change = ", sprintf("%+.2f", mean_change), "; paired ', italic(p), ' ",
      ifelse(p_value < .001, "< .001", paste0("= ", format_p(p_value))), "')"
    )
  )
panel_levels <- plot_data %>%
  distinct(label, panel_label) %>%
  arrange(label) %>%
  pull(panel_label)
plot_data <- plot_data %>% mutate(panel_label = factor(panel_label, levels = panel_levels))

grade_colours <- c(
  grade_4 = "#0072B2",
  grade_8 = "#D55E00"
)
grade_shapes <- c(
  grade_4 = 21,
  grade_8 = 22
)
grade_axis_labels <- c(
  grade_4 = expression(atop("Pre-adolescent cohort", paste("(recruited in Grade 4, ", italic(n), " = 104)"))),
  grade_8 = expression(atop("Middle-adolescent cohort", paste("(recruited in Grade 8, ", italic(n), " = 126)")))
)

grade_plot <- ggplot(
  plot_data,
  aes(x = mean_change, y = grade, colour = grade, shape = grade, fill = grade)
) +
  geom_vline(xintercept = 0, colour = "#777777", linewidth = .5, linetype = "dashed") +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = .16, linewidth = .8) +
  geom_point(size = 3.4, stroke = .7) +
  geom_text(
    aes(x = ci_high, label = row_label),
    hjust = -.06, colour = "#252525", size = 3.25, show.legend = FALSE,
    parse = TRUE
  ) +
  facet_wrap(~panel_label, ncol = 1, scales = "free_x", labeller = label_parsed) +
  scale_colour_manual(values = grade_colours) +
  scale_fill_manual(values = grade_colours) +
  scale_shape_manual(values = grade_shapes) +
  scale_y_discrete(labels = grade_axis_labels) +
  scale_x_continuous(expand = expansion(mult = c(.12, .40))) +
  labs(
    title = "Pre-post change within the two developmental cohorts",
    subtitle = "Points show mean posttest-minus-pretest change; whiskers show 95% confidence intervals",
    x = "Mean change (posttest - pretest)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, lineheight = 1.1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(colour = "#4A4A4A"),
    axis.text.y = element_text(face = "bold", colour = "#252525"),
    plot.margin = margin(8, 62, 8, 8)
  )

ggsave(file.path(figure_dir, "Figure_1_grade_change_comparison.png"), grade_plot, width = 9.4, height = 9.2, dpi = 600, bg = "white")
ggsave(file.path(figure_dir, "Figure_1_grade_change_comparison.pdf"), grade_plot, width = 9.4, height = 9.2, device = "pdf", bg = "white")
grDevices::postscript(
  file.path(figure_dir, "Figure_1_grade_change_comparison.eps"),
  horizontal = FALSE, onefile = FALSE, paper = "special",
  width = 9.4, height = 9.2, family = "Helvetica"
)
print(grade_plot)
invisible(grDevices::dev.off())

assert_close <- function(actual, expected, label, tolerance = 1e-5) {
  if (is.na(actual) || abs(actual - expected) > tolerance) {
    stop(label, " did not match the bundled data. Expected ", expected, "; found ", actual, ".")
  }
}
assert_close(grade_allocation_manova$approx_f, 6.3509244, "Study 1 allocation MANOVA F")
assert_close(
  grade_contrasts$mean_change_difference[grade_contrasts$outcome == "allocation_immediate"],
  5.7833028, "Study 1 immediate-allocation grade contrast"
)
assert_close(
  grade_contrasts$mean_change_difference[grade_contrasts$outcome == "allocation_short"],
  -7.5746490, "Study 1 short-allocation grade contrast"
)

writeLines(capture.output(sessionInfo()), file.path(results_dir, "session_info.txt"))
cat("Study 1 grade-comparison analysis completed successfully. Results: ", results_dir, "\n", sep = "")
