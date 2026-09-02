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
if (length(script_argument) != 1L) stop("Run this file with Rscript code/study2_analysis.R.")
script_path <- gsub("~\\+~", " ", sub("^--file=", "", script_argument))
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
wide_path <- file.path(repo_root, "data", "study2_public.csv")
long_path <- file.path(repo_root, "data", "study2_item_choice_purpose_long.csv")
results_dir <- file.path(repo_root, "results", "study2")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

purpose_levels <- c(
  "saving_for_future", "immediate_consumption", "family_support",
  "learning_or_education", "friends_or_gifts"
)
purpose_labels <- c(
  saving_for_future = "Saving for future",
  immediate_consumption = "Immediate consumption",
  family_support = "Family support",
  learning_or_education = "Learning or education",
  friends_or_gifts = "Friends or gifts"
)

outcomes <- tibble::tribble(
  ~outcome, ~label, ~pre, ~post,
  "ln_k", "ln(k)", "ln_k_pre", "ln_k_post",
  "allocation_immediate", "Immediate allocation", "allocation_immediate_pre", "allocation_immediate_post",
  "allocation_short", "Short-term allocation", "allocation_short_pre", "allocation_short_post",
  "allocation_long", "Long-term allocation", "allocation_long_pre", "allocation_long_post",
  "subjective_time", "Subjective time", "subjective_time_pre", "subjective_time_post"
)

wide <- readr::read_csv(wide_path, show_col_types = FALSE)
item_long <- readr::read_csv(long_path, show_col_types = FALSE)
required_wide <- unique(c(
  "participant_id", "story_condition", "gender", "age_years", outcomes$pre, outcomes$post,
  paste0("purpose_pre_", sprintf("%02d", 1:7)),
  paste0("purpose_post_", sprintf("%02d", 1:7)),
  paste0("intention_pre_", sprintf("%02d", 1:7)),
  paste0("intention_post_", sprintf("%02d", 1:7))
))
required_long <- c(
  "participant_id", "wave", "item", "choice", "choice_ss", "purpose",
  "implementation_intention"
)
if (!all(required_wide %in% names(wide))) {
  stop("Study 2 wide data are missing: ", paste(setdiff(required_wide, names(wide)), collapse = ", "))
}
if (!all(required_long %in% names(item_long))) {
  stop("Study 2 item data are missing: ", paste(setdiff(required_long, names(item_long)), collapse = ", "))
}
if (nrow(wide) != 184L || n_distinct(wide$participant_id) != 184L) {
  stop("Study 2 wide data must contain 184 unique participants.")
}
if (min(wide$age_years) != 10 || max(wide$age_years) != 16) {
  stop("Study 2 ages must range from 10 to 16.")
}
if (nrow(item_long) != 2576L || n_distinct(item_long$participant_id) != 184L) {
  stop("Study 2 item data must contain 2,576 rows for 184 participants.")
}
if (anyDuplicated(item_long[c("participant_id", "wave", "item")])) {
  stop("participant_id, wave, and item must uniquely identify item rows.")
}
if (!setequal(item_long$participant_id, wide$participant_id)) stop("Wide and item-level participant IDs differ.")
if (!setequal(wide$story_condition, c("badgers_parting_gifts", "fall_of_freddie_leaf", "goodbye_grandma_erma"))) {
  stop("Unexpected story-condition coding.")
}
if (!setequal(wide$gender, c("female", "male"))) stop("Unexpected gender coding.")
if (!setequal(item_long$wave, c("pretest", "posttest"))) stop("Unexpected wave coding.")
if (!setequal(item_long$choice, c("SS", "LL"))) stop("Unexpected choice coding.")
if (!all(item_long$choice_ss == as.integer(item_long$choice == "SS"))) stop("choice and choice_ss disagree.")
if (!setequal(item_long$purpose, purpose_levels)) stop("Unexpected purpose coding.")
if (!all(item_long$implementation_intention %in% 1:3)) stop("Intention ratings must be 1, 2, or 3.")
if (any(is.na(wide)) || any(is.na(item_long))) stop("Unexpected missing values in Study 2 public data.")

allocation_pre <- with(wide, allocation_immediate_pre + allocation_short_pre + allocation_long_pre)
allocation_post <- with(wide, allocation_immediate_post + allocation_short_post + allocation_long_post)
max_pre_allocation_error <- max(abs(allocation_pre - 100))
max_post_allocation_error <- max(abs(allocation_post - 100))
if (max_pre_allocation_error > 1e-8 || max_post_allocation_error > 1e-8) {
  stop("Temporal-allocation percentages must sum to 100 at each wave.")
}

wide_purpose <- item_long %>%
  mutate(
    item_text = sprintf("%02d", item),
    name = paste0("purpose_", ifelse(wave == "pretest", "pre", "post"), "_", item_text)
  ) %>%
  select(participant_id, name, value = purpose) %>%
  pivot_wider(names_from = name, values_from = value)
wide_intention <- item_long %>%
  mutate(
    item_text = sprintf("%02d", item),
    name = paste0("intention_", ifelse(wave == "pretest", "pre", "post"), "_", item_text)
  ) %>%
  select(participant_id, name, value = implementation_intention) %>%
  pivot_wider(names_from = name, values_from = value)
purpose_columns <- c(paste0("purpose_pre_", sprintf("%02d", 1:7)), paste0("purpose_post_", sprintf("%02d", 1:7)))
intention_columns <- c(paste0("intention_pre_", sprintf("%02d", 1:7)), paste0("intention_post_", sprintf("%02d", 1:7)))
purpose_check <- wide %>%
  select(participant_id, all_of(purpose_columns)) %>%
  left_join(wide_purpose, by = "participant_id", suffix = c("_wide", "_item"))
intention_check <- wide %>%
  select(participant_id, all_of(intention_columns)) %>%
  left_join(wide_intention, by = "participant_id", suffix = c("_wide", "_item"))
purpose_mismatch_n <- sum(vapply(purpose_columns, function(column) {
  sum(as.character(purpose_check[[paste0(column, "_wide")]]) != as.character(purpose_check[[paste0(column, "_item")]]))
}, integer(1)))
intention_mismatch_n <- sum(vapply(intention_columns, function(column) {
  sum(as.integer(intention_check[[paste0(column, "_wide")]]) != as.integer(intention_check[[paste0(column, "_item")]]))
}, integer(1)))
if (purpose_mismatch_n != 0L || intention_mismatch_n != 0L) {
  stop("Wide and item-level questionnaire values disagree.")
}

wide <- wide %>%
  mutate(
    story_condition = factor(
      story_condition,
      levels = c("fall_of_freddie_leaf", "badgers_parting_gifts", "goodbye_grandma_erma")
    ),
    gender = factor(gender, levels = c("female", "male"))
  )
item_long <- item_long %>%
  mutate(
    wave = factor(wave, levels = c("pretest", "posttest")),
    choice = factor(choice, levels = c("LL", "SS")),
    purpose = factor(purpose, levels = purpose_levels),
    item = factor(item, levels = 1:7)
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

cluster_vcov <- function(fit, cluster) {
  design <- model.matrix(fit)
  observations <- nrow(design)
  coefficients <- ncol(design)
  clusters <- factor(cluster)
  cluster_count <- nlevels(clusters)
  if (inherits(fit, "glm")) {
    score_scalar <- fit$y - fitted(fit)
    bread <- solve(crossprod(design, design * fit$weights))
  } else {
    score_scalar <- residuals(fit)
    bread <- solve(crossprod(design))
  }
  cluster_scores <- rowsum(design * score_scalar, clusters, reorder = FALSE)
  correction <- (cluster_count / (cluster_count - 1)) * ((observations - 1) / (observations - coefficients))
  correction * bread %*% crossprod(cluster_scores) %*% bread
}

cluster_terms <- function(fit, cluster, model_name, exponentiate = FALSE) {
  covariance <- cluster_vcov(fit, cluster)
  estimates <- coef(fit)
  standard_errors <- sqrt(diag(covariance))
  degrees_freedom <- n_distinct(cluster) - 1L
  statistics <- estimates / standard_errors
  p_values <- 2 * pt(abs(statistics), df = degrees_freedom, lower.tail = FALSE)
  critical <- qt(.975, degrees_freedom)
  output <- tibble(
    model = model_name,
    term = names(estimates),
    estimate = unname(estimates),
    cluster_standard_error = unname(standard_errors),
    statistic = unname(statistics),
    cluster_df = degrees_freedom,
    p_value = unname(p_values),
    ci_low = unname(estimates - critical * standard_errors),
    ci_high = unname(estimates + critical * standard_errors)
  )
  if (exponentiate) {
    output <- output %>%
      mutate(
        odds_ratio = exp(estimate),
        odds_ratio_ci_low = exp(ci_low),
        odds_ratio_ci_high = exp(ci_high)
      )
  }
  output
}

cluster_omnibus <- function(fit, cluster, model_name) {
  covariance <- cluster_vcov(fit, cluster)
  design <- model.matrix(fit)
  assignments <- attr(design, "assign")
  term_labels <- attr(terms(fit), "term.labels")
  purrr::map_dfr(seq_along(term_labels), function(term_index) {
    selected <- which(assignments == term_index)
    estimates <- coef(fit)[selected]
    selected_covariance <- covariance[selected, selected, drop = FALSE]
    statistic <- as.numeric(t(estimates) %*% solve(selected_covariance, estimates))
    tibble(
      model = model_name,
      term = term_labels[term_index],
      coefficients_tested = length(selected),
      chi_square = statistic,
      df = length(selected),
      p_value = pchisq(statistic, length(selected), lower.tail = FALSE)
    )
  })
}

data_integrity <- tibble(
  check = c(
    "wide_rows", "unique_participant_ids", "minimum_age", "maximum_age",
    "item_rows", "unique_participant_wave_item_rows", "wide_item_purpose_mismatches",
    "wide_item_intention_mismatches", "max_pre_allocation_total_error",
    "max_post_allocation_total_error", "missing_wide_cells", "missing_item_cells"
  ),
  value = c(
    nrow(wide), n_distinct(wide$participant_id), min(wide$age_years), max(wide$age_years),
    nrow(item_long), nrow(distinct(item_long, participant_id, wave, item)), purpose_mismatch_n,
    intention_mismatch_n, max_pre_allocation_error, max_post_allocation_error,
    sum(is.na(wide)), sum(is.na(item_long))
  ),
  expected = c("184", "184", "10", "16", "2576", "2576", "0", "0", "<= 1e-8", "<= 1e-8", "0", "0")
)
write_result(data_integrity, "data_integrity_diagnostics.csv")

story_cell_characteristics <- wide %>%
  group_by(story_condition) %>%
  summarise(
    n = n(),
    female_n = sum(gender == "female"),
    female_percent = 100 * female_n / n,
    age_mean = mean(age_years),
    age_sd = sd(age_years),
    age_min = min(age_years),
    age_max = max(age_years),
    .groups = "drop"
  )
write_result(story_cell_characteristics, "story_cell_characteristics.csv")

story_version_tests <- purrr::pmap_dfr(
  outcomes,
  function(outcome, label, pre, post) story_change_test(wide, outcome, pre, post)
)

pooled_pre_post_tests <- purrr::pmap_dfr(
  outcomes,
  function(outcome, label, pre, post) paired_test(wide, outcome, pre, post)
)
write_result(pooled_pre_post_tests, "pooled_pre_post_tests.csv")

allocation_changes <- wide %>%
  transmute(
    immediate = allocation_immediate_post - allocation_immediate_pre,
    short = allocation_short_post - allocation_short_pre
  )
allocation_matrix <- as.matrix(allocation_changes)
allocation_n <- nrow(allocation_matrix)
allocation_dimensions <- ncol(allocation_matrix)
allocation_t2 <- as.numeric(
  allocation_n * t(colMeans(allocation_matrix)) %*% solve(cov(allocation_matrix)) %*% colMeans(allocation_matrix)
)
allocation_f <- ((allocation_n - allocation_dimensions) / (allocation_dimensions * (allocation_n - 1L))) * allocation_t2
allocation_global_test <- tibble(
  analysis = "joint_pre_post_change_in_two_nonredundant_allocation_components",
  components = "immediate + short",
  redundant_component = "long",
  n = allocation_n,
  hotelling_t_squared = allocation_t2,
  f_statistic = allocation_f,
  df1 = allocation_dimensions,
  df2 = allocation_n - allocation_dimensions,
  p_value = pf(allocation_f, allocation_dimensions, allocation_n - allocation_dimensions, lower.tail = FALSE)
)
write_result(allocation_global_test, "allocation_global_test.csv")

intention_pre <- rowMeans(as.matrix(wide[paste0("intention_pre_", sprintf("%02d", 1:7))]))
intention_post <- rowMeans(as.matrix(wide[paste0("intention_post_", sprintf("%02d", 1:7))]))
implementation_intention_test <- paired_test(
  tibble(pre = intention_pre, post = intention_post),
  "implementation_intention", "pre", "post"
)
write_result(implementation_intention_test, "implementation_intention_test.csv")

intention_story_data <- wide %>% transmute(story_condition, change = intention_post - intention_pre)
intention_story_model <- lm(change ~ story_condition, data = intention_story_data)
intention_story_anova <- anova(intention_story_model)
story_version_tests <- bind_rows(
  story_version_tests,
  tibble(
    outcome = "implementation_intention",
    df_story = intention_story_anova$Df[1],
    df_residual = intention_story_anova$Df[2],
    f_statistic = intention_story_anova$F[1],
    p_value = intention_story_anova$`Pr(>F)`[1],
    partial_eta_squared = intention_story_anova$`Sum Sq`[1] / sum(intention_story_anova$`Sum Sq`[1:2]),
    significant = intention_story_anova$`Pr(>F)`[1] < .05
  )
)
write_result(story_version_tests, "story_version_tests.csv")

overall_purpose_distribution <- item_long %>%
  count(wave, purpose, .drop = FALSE, name = "count") %>%
  group_by(wave) %>%
  mutate(percent = 100 * count / sum(count)) %>%
  ungroup() %>%
  mutate(purpose_label = unname(purpose_labels[as.character(purpose)]))
write_result(overall_purpose_distribution, "overall_purpose_distribution.csv")

choice_purpose_distribution <- item_long %>%
  count(wave, choice, purpose, .drop = FALSE, name = "count") %>%
  group_by(wave, choice) %>%
  mutate(percent = 100 * count / sum(count)) %>%
  ungroup() %>%
  mutate(purpose_label = unname(purpose_labels[as.character(purpose)]))
write_result(choice_purpose_distribution, "choice_purpose_distribution.csv")

choice_model <- glm(choice_ss ~ wave * purpose + item, family = binomial(), data = item_long, y = TRUE)
choice_model_terms <- cluster_terms(choice_model, item_long$participant_id, "clustered_choice_logit", exponentiate = TRUE)
choice_model_omnibus <- cluster_omnibus(choice_model, item_long$participant_id, "clustered_choice_logit")
write_result(choice_model_terms, "choice_model_terms.csv")
write_result(choice_model_omnibus, "choice_model_omnibus.csv")

intention_model <- lm(implementation_intention ~ wave * choice + purpose + item, data = item_long)
intention_model_terms <- cluster_terms(intention_model, item_long$participant_id, "clustered_intention_linear")
intention_model_omnibus <- cluster_omnibus(intention_model, item_long$participant_id, "clustered_intention_linear")
write_result(intention_model_terms, "intention_model_terms.csv")
write_result(intention_model_omnibus, "intention_model_omnibus.csv")

intention_choice_purpose_descriptives <- item_long %>%
  group_by(participant_id, wave, choice, purpose) %>%
  summarise(participant_cell_mean = mean(implementation_intention), .groups = "drop") %>%
  group_by(wave, choice, purpose) %>%
  summarise(
    participants = n(),
    mean_intention = mean(participant_cell_mean),
    sd_intention = sd(participant_cell_mean),
    standard_error = sd_intention / sqrt(participants),
    ci_low = mean_intention - qt(.975, participants - 1L) * standard_error,
    ci_high = mean_intention + qt(.975, participants - 1L) * standard_error,
    .groups = "drop"
  ) %>%
  mutate(purpose_label = unname(purpose_labels[as.character(purpose)]))
write_result(intention_choice_purpose_descriptives, "intention_choice_purpose_descriptives.csv")

assert_close <- function(actual, expected, label, tolerance = 1e-5) {
  if (is.na(actual) || abs(actual - expected) > tolerance) {
    stop(label, " did not match the bundled data. Expected ", expected, "; found ", actual, ".")
  }
}
assert_close(pooled_pre_post_tests$mean_change[pooled_pre_post_tests$outcome == "ln_k"], .410, "Study 2 ln(k) change", .001)
assert_close(pooled_pre_post_tests$mean_change[pooled_pre_post_tests$outcome == "allocation_immediate"], 7.38, "Study 2 immediate change", .01)
assert_close(allocation_global_test$f_statistic, 11.61, "Study 2 allocation global F", .02)
assert_close(implementation_intention_test$mean_change, .078, "Study 2 intention change", .002)

writeLines(capture.output(sessionInfo()), file.path(results_dir, "session_info.txt"))
cat("Study 2 analysis completed successfully. Results: ", results_dir, "\n", sep = "")
