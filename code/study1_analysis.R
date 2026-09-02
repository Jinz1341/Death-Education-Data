required_packages <- c("readr", "dplyr", "tidyr", "purrr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Install required packages before running: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("Run this file with Rscript code/study1_analysis.R.")
script_path <- gsub("~\\+~", " ", sub("^--file=", "", script_argument))
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
data_path <- file.path(repo_root, "data", "study1_public.csv")
results_dir <- file.path(repo_root, "results", "study1")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

outcomes <- tibble::tribble(
  ~outcome, ~label, ~pre, ~post,
  "ln_k", "ln(k)", "ln_k_pre", "ln_k_post",
  "allocation_immediate", "Immediate allocation", "allocation_immediate_pre", "allocation_immediate_post",
  "allocation_short", "Short-term allocation", "allocation_short_pre", "allocation_short_post",
  "allocation_long", "Long-term allocation", "allocation_long_pre", "allocation_long_post",
  "subjective_time", "Subjective time", "subjective_time_pre", "subjective_time_post"
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
if (!setequal(study1$story_condition, c("ant_and_grasshopper", "last_leaf_maple_valley"))) {
  stop("Unexpected Study 1 story-condition coding.")
}
if (!setequal(study1$gender, c("female", "male"))) stop("Unexpected gender coding.")
if (!setequal(study1$school_level, c("grade_4", "grade_8"))) stop("Unexpected grade coding.")

grade4_age <- study1$age_years[study1$school_level == "grade_4"]
grade8_age <- study1$age_years[study1$school_level == "grade_8"]
if (any(is.na(grade4_age)) || !setequal(grade4_age, c(9, 10))) {
  stop("Grade 4 ages must be observed and limited to 9 and 10 years.")
}
if (sum(is.na(grade8_age)) != 9L || !setequal(grade8_age[!is.na(grade8_age)], 13:16)) {
  stop("Unexpected Grade 8 age distribution.")
}
if (any(is.na(study1[c("story_condition", "gender", "school_level", "ses_hollingshead", outcomes$pre, outcomes$post)]))) {
  stop("Unexpected missing design, demographic, or outcome values.")
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
    story_condition = factor(story_condition, levels = c("ant_and_grasshopper", "last_leaf_maple_valley")),
    gender = factor(gender, levels = c("female", "male"))
  )

write_result <- function(data, filename) {
  readr::write_csv(data, file.path(results_dir, filename), na = "")
}

paired_test <- function(data, outcome, pre, post) {
  change <- data[[post]] - data[[pre]]
  test <- t.test(data[[post]], data[[pre]], paired = TRUE)
  tibble(
    outcome = outcome,
    n = length(change),
    pre_mean = mean(data[[pre]]),
    pre_sd = sd(data[[pre]]),
    post_mean = mean(data[[post]]),
    post_sd = sd(data[[post]]),
    mean_change = mean(change),
    ci_low = unname(test$conf.int[1]),
    ci_high = unname(test$conf.int[2]),
    t_statistic = unname(test$statistic),
    df = unname(test$parameter),
    p_value = test$p.value,
    dz = mean(change) / sd(change),
    significant = test$p.value < .05
  )
}

story_change_test <- function(data, outcome, pre, post) {
  model_data <- data %>% transmute(story_condition, change = .data[[post]] - .data[[pre]])
  model <- lm(change ~ story_condition, data = model_data)
  model_anova <- anova(model)
  tibble(
    outcome = outcome,
    df_story = model_anova$Df[1],
    df_residual = model_anova$Df[2],
    f_statistic = model_anova$F[1],
    p_value = model_anova$`Pr(>F)`[1],
    partial_eta_squared = model_anova$`Sum Sq`[1] / sum(model_anova$`Sum Sq`[1:2]),
    significant = model_anova$`Pr(>F)`[1] < .05
  )
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

sample_characteristics <- study1 %>%
  group_by(school_level) %>%
  summarise(
    n = n(),
    age_n = sum(!is.na(age_years)),
    age_missing_n = sum(is.na(age_years)),
    age_mean = mean(age_years, na.rm = TRUE),
    age_variance = var(age_years, na.rm = TRUE),
    age_sd = sd(age_years, na.rm = TRUE),
    age_min = min(age_years, na.rm = TRUE),
    age_max = max(age_years, na.rm = TRUE),
    female_n = sum(gender == "female"),
    female_percent = 100 * female_n / n,
    .groups = "drop"
  )
write_result(sample_characteristics, "sample_characteristics.csv")

age_model <- lm(age_years ~ school_level, data = study1)
age_anova <- anova(age_model)
age_coefficient <- summary(age_model)$coefficients["school_levelgrade_8", ]
age_interval <- confint(age_model, "school_levelgrade_8")
age_grade_test <- tibble(
  analysis = "grade_difference_in_observed_age",
  n_with_age = nobs(age_model),
  grade8_minus_grade4 = unname(age_coefficient["Estimate"]),
  ci_low = unname(age_interval[1]),
  ci_high = unname(age_interval[2]),
  f_statistic = age_anova$F[1],
  df1 = age_anova$Df[1],
  df2 = age_anova$Df[2],
  p_value = age_anova$`Pr(>F)`[1],
  eta_squared = age_anova$`Sum Sq`[1] / sum(age_anova$`Sum Sq`)
)
write_result(age_grade_test, "age_grade_test.csv")

gender_table <- table(study1$school_level, study1$gender)
gender_test <- chisq.test(gender_table, correct = FALSE)
gender_grade_test <- tibble(
  analysis = "grade_difference_in_gender_distribution",
  chi_square = unname(gender_test$statistic),
  df = unname(gender_test$parameter),
  p_value = gender_test$p.value
)
write_result(gender_grade_test, "gender_grade_test.csv")

story_version_tests <- purrr::pmap_dfr(
  outcomes,
  function(outcome, label, pre, post) story_change_test(study1, outcome, pre, post)
)
write_result(story_version_tests, "story_version_tests.csv")

pooled_pre_post_tests <- purrr::pmap_dfr(
  outcomes,
  function(outcome, label, pre, post) paired_test(study1, outcome, pre, post)
)
write_result(pooled_pre_post_tests, "pooled_pre_post_tests.csv")

grade4_ln_k <- study1$ln_k_pre[study1$school_level == "grade_4"]
grade8_ln_k <- study1$ln_k_pre[study1$school_level == "grade_8"]
baseline_test <- t.test(grade8_ln_k, grade4_ln_k)
pooled_sd <- sqrt(
  ((length(grade4_ln_k) - 1L) * var(grade4_ln_k) + (length(grade8_ln_k) - 1L) * var(grade8_ln_k)) /
    (length(grade4_ln_k) + length(grade8_ln_k) - 2L)
)
hedges_correction <- 1 - 3 / (4 * (length(grade4_ln_k) + length(grade8_ln_k)) - 9)
baseline_grade_cohort_tests <- tibble(
  analysis = "grade_8_minus_grade_4_baseline_ln_k",
  n_grade4 = length(grade4_ln_k),
  n_grade8 = length(grade8_ln_k),
  grade4_mean = mean(grade4_ln_k),
  grade8_mean = mean(grade8_ln_k),
  mean_difference = mean(grade8_ln_k) - mean(grade4_ln_k),
  ci_low = unname(baseline_test$conf.int[1]),
  ci_high = unname(baseline_test$conf.int[2]),
  t_statistic = unname(baseline_test$statistic),
  df = unname(baseline_test$parameter),
  p_value = baseline_test$p.value,
  hedges_g = hedges_correction * (mean(grade8_ln_k) - mean(grade4_ln_k)) / pooled_sd
)
write_result(baseline_grade_cohort_tests, "baseline_grade_cohort_tests.csv")

assert_close <- function(actual, expected, label, tolerance = 1e-5) {
  if (is.na(actual) || abs(actual - expected) > tolerance) {
    stop(label, " did not match the bundled data. Expected ", expected, "; found ", actual, ".")
  }
}
assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "ln_k"], 4.7671793, "Study 1 ln(k) t")
assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "allocation_immediate"], 5.8077188, "Study 1 immediate-allocation t")
assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "allocation_long"], -4.4197364, "Study 1 long-allocation t")
assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "subjective_time"], -3.7494599, "Study 1 subjective-time t")
assert_close(baseline_grade_cohort_tests$t_statistic, 4.0464803, "Study 1 baseline-grade Welch t")
assert_close(age_grade_test$f_statistic, 5697.756, "Study 1 age-grade F", .01)

writeLines(capture.output(sessionInfo()), file.path(results_dir, "session_info.txt"))
cat("Study 1 analysis completed successfully. Results: ", results_dir, "\n", sep = "")
