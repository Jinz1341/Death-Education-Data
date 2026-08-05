required_packages <- c("readr", "dplyr", "tidyr", "purrr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Install required packages before running: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(tidyr)

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("Run this file with Rscript code/study1_analysis.R.")
script_path <- gsub("~\\+~", " ", sub("^--file=", "", script_argument))
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
data_path <- file.path(repo_root, "data", "study1_public.csv")
results_dir <- file.path(repo_root, "results", "study1")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

required_columns <- c(
  "participant_id", "story_condition", "gender", "age_years", "school_level", "ses_hollingshead",
  "ln_k_pre", "ln_k_post", "allocation_immediate_pre", "allocation_immediate_post",
  "allocation_short_pre", "allocation_short_post", "allocation_long_pre", "allocation_long_post",
  "subjective_time_pre", "subjective_time_post"
)
study1 <- readr::read_csv(data_path, show_col_types = FALSE)
if (!all(required_columns %in% names(study1))) {
  stop("Study 1 data are missing required columns: ", paste(setdiff(required_columns, names(study1)), collapse = ", "))
}
if (nrow(study1) != 250L) stop("Expected 250 Study 1 records; found ", nrow(study1), ".")
if (anyDuplicated(study1$participant_id)) stop("participant_id values must be unique.")
if (!all(study1$story_condition %in% c("last_leaf_maple_valley", "ant_and_grasshopper"))) {
  stop("Unexpected Study 1 story condition.")
}
if (!all(study1$gender %in% c("female", "male"))) stop("Unexpected gender coding.")
if (!all(study1$school_level %in% c("grade_4", "grade_8"))) stop("Unexpected school-level coding.")
if (!all(is.na(study1$age_years) | (study1$age_years >= 9 & study1$age_years <= 16))) {
  stop("Study 1 ages must be between 9 and 16 years when observed.")
}

allocation_columns <- grep("^allocation_", names(study1), value = TRUE)
time_columns <- grep("^subjective_time_", names(study1), value = TRUE)
if (!all(unlist(study1[c(allocation_columns, time_columns)]) >= 0 &
         unlist(study1[c(allocation_columns, time_columns)]) <= 100)) {
  stop("Allocation and subjective-time values must be within 0-100.")
}

write_result <- function(data, name) {
  readr::write_csv(data, file.path(results_dir, name), na = "")
}

mean_ci <- function(values) {
  values <- values[!is.na(values)]
  n <- length(values)
  error <- qt(0.975, df = n - 1L) * sd(values) / sqrt(n)
  tibble(n = n, mean = mean(values), sd = sd(values), ci_low = mean(values) - error, ci_high = mean(values) + error)
}

paired_test <- function(data, outcome, pre_column, post_column) {
  complete <- data %>% select(all_of(c(pre_column, post_column))) %>% tidyr::drop_na()
  pre <- complete[[pre_column]]
  post <- complete[[post_column]]
  change <- post - pre
  test <- t.test(post, pre, paired = TRUE)
  tibble(
    outcome = outcome,
    n = length(change),
    pre_mean = mean(pre),
    pre_sd = sd(pre),
    post_mean = mean(post),
    post_sd = sd(post),
    mean_change = mean(change),
    ci_low = unname(test$conf.int[1]),
    ci_high = unname(test$conf.int[2]),
    t_statistic = unname(test$statistic),
    df = unname(test$parameter),
    p_value = test$p.value,
    dz = mean(change) / sd(change)
  )
}

story_change_test <- function(data, outcome, pre_column, post_column) {
  model_data <- data %>%
    transmute(story_condition, change = .data[[post_column]] - .data[[pre_column]]) %>%
    drop_na()
  model <- lm(change ~ story_condition, data = model_data)
  model_anova <- anova(model)
  tibble(
    outcome = outcome,
    df_story = model_anova$Df[1],
    df_residual = model_anova$Df[2],
    f_statistic = model_anova$F[1],
    p_value = model_anova$`Pr(>F)`[1],
    partial_eta_squared = model_anova$`Sum Sq`[1] / sum(model_anova$`Sum Sq`[1:2])
  )
}

outcomes <- tibble(
  outcome = c("ln_k", "allocation_immediate", "allocation_short", "allocation_long", "subjective_time"),
  label = c("K value", "Immediate allocation", "Short-term allocation", "Long-term allocation", "Subjective time"),
  pre_column = c("ln_k_pre", "allocation_immediate_pre", "allocation_short_pre", "allocation_long_pre", "subjective_time_pre"),
  post_column = c("ln_k_post", "allocation_immediate_post", "allocation_short_post", "allocation_long_post", "subjective_time_post")
)

data_checks <- bind_rows(
  tibble(check = "analytic_n", value = nrow(study1), expected = "250"),
  tibble(check = "unique_participant_ids", value = n_distinct(study1$participant_id), expected = "250"),
  tibble(check = "missing_age_years", value = sum(is.na(study1$age_years)), expected = "11"),
  tibble(check = "story_conditions", value = n_distinct(study1$story_condition), expected = "2")
)
write_result(data_checks, "data_checks.csv")

story_version_tests <- purrr::pmap_dfr(
  outcomes,
  function(outcome, label, pre_column, post_column) story_change_test(study1, outcome, pre_column, post_column)
)
write_result(story_version_tests, "story_version_tests.csv")

pooled_pre_post_tests <- purrr::pmap_dfr(
  outcomes,
  function(outcome, label, pre_column, post_column) paired_test(study1, outcome, pre_column, post_column)
)
write_result(pooled_pre_post_tests, "pooled_pre_post_tests.csv")

baseline_data <- study1 %>% select(ln_k_pre, school_level, gender, ses_hollingshead) %>% drop_na()
grade4 <- baseline_data %>% filter(school_level == "grade_4") %>% pull(ln_k_pre)
grade8 <- baseline_data %>% filter(school_level == "grade_8") %>% pull(ln_k_pre)
welch <- t.test(ln_k_pre ~ school_level, data = baseline_data)
wilcoxon <- wilcox.test(ln_k_pre ~ school_level, data = baseline_data, exact = FALSE)
pooled_sd <- sqrt(((length(grade4) - 1L) * var(grade4) + (length(grade8) - 1L) * var(grade8)) /
                    (length(grade4) + length(grade8) - 2L))
cohen_d <- (mean(grade8) - mean(grade4)) / pooled_sd
hedges_correction <- 1 - 3 / (4 * (length(grade4) + length(grade8)) - 9)
hedges_g <- hedges_correction * cohen_d

baseline_grade_cohort_tests <- tibble(
  test = c("Welch independent-samples t test", "Hedges g", "Wilcoxon rank-sum test"),
  statistic = c(unname(welch$statistic), hedges_g, unname(wilcoxon$statistic)),
  df = c(unname(welch$parameter), NA_real_, NA_real_),
  p_value = c(welch$p.value, NA_real_, wilcoxon$p.value),
  grade4_mean = mean(grade4),
  grade8_mean = mean(grade8),
  grade8_minus_grade4 = mean(grade8) - mean(grade4),
  ci_low = c(unname(welch$conf.int[1]), NA_real_, NA_real_),
  ci_high = c(unname(welch$conf.int[2]), NA_real_, NA_real_)
)
write_result(baseline_grade_cohort_tests, "baseline_grade_cohort_tests.csv")

adjusted_model <- lm(ln_k_pre ~ school_level + gender + ses_hollingshead, data = baseline_data)
adjusted_ci <- confint(adjusted_model)
baseline_adjusted_model <- tibble(
  term = names(coef(adjusted_model)),
  estimate = unname(coef(adjusted_model)),
  std_error = summary(adjusted_model)$coefficients[, "Std. Error"],
  t_statistic = summary(adjusted_model)$coefficients[, "t value"],
  p_value = summary(adjusted_model)$coefficients[, "Pr(>|t|)"],
  ci_low = adjusted_ci[, 1],
  ci_high = adjusted_ci[, 2]
)
write_result(baseline_adjusted_model, "baseline_adjusted_model.csv")

assert_close <- function(actual, expected, label, tolerance = 1e-5) {
  if (is.na(actual) || abs(actual - expected) > tolerance) {
    stop(label, " did not reproduce the reference result. Expected ", expected, "; found ", actual, ".")
  }
}

assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "ln_k"], 5.004781, "Study 1 ln(K) t")
assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "allocation_immediate"], 5.857022, "Study 1 immediate-allocation t")
assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "allocation_long"], -4.220955, "Study 1 long-allocation t")
assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "subjective_time"], -3.807399, "Study 1 subjective-time t")
assert_close(baseline_grade_cohort_tests$statistic[baseline_grade_cohort_tests$test == "Welch independent-samples t test"], -4.664173, "Study 1 Welch t")
assert_close(baseline_adjusted_model$estimate[baseline_adjusted_model$term == "school_levelgrade_8"], 1.201258, "Study 1 adjusted grade coefficient")

writeLines(capture.output(sessionInfo()), file.path(results_dir, "session_info.txt"))
cat("Study 1 analysis completed successfully. Results: ", results_dir, "\n", sep = "")
