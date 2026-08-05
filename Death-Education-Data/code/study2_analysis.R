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
if (length(script_argument) != 1L) stop("Run this file with Rscript code/study2_analysis.R.")
script_path <- gsub("~\\+~", " ", sub("^--file=", "", script_argument))
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
data_path <- file.path(repo_root, "data", "study2_public.csv")
results_dir <- file.path(repo_root, "results", "study2")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
purpose_levels <- c(
  "immediate_consumption",
  "friends_or_gifts",
  "family_support",
  "learning_or_education",
  "saving_for_future"
)
purpose_labels <- c(
  immediate_consumption = "Immediate consumption",
  friends_or_gifts = "Friends/gifts",
  family_support = "Family support",
  learning_or_education = "Learning",
  saving_for_future = "Saving for future use"
)

required_columns <- c(
  "participant_id", "story_condition", "gender", "age_years", "ln_k_pre", "ln_k_post",
  "allocation_immediate_pre", "allocation_immediate_post", "allocation_short_pre", "allocation_short_post",
  "allocation_long_pre", "allocation_long_post", "subjective_time_pre", "subjective_time_post",
  paste0("purpose_pre_", sprintf("%02d", 1:7)),
  paste0("purpose_post_", sprintf("%02d", 1:7)),
  paste0("intention_pre_", sprintf("%02d", 1:7)),
  paste0("intention_post_", sprintf("%02d", 1:7))
)
study2 <- readr::read_csv(data_path, show_col_types = FALSE)
if (!all(required_columns %in% names(study2))) {
  stop("Study 2 data are missing required columns: ", paste(setdiff(required_columns, names(study2)), collapse = ", "))
}
if (nrow(study2) != 190L) stop("Expected 190 Study 2 records; found ", nrow(study2), ".")
if (anyDuplicated(study2$participant_id)) stop("participant_id values must be unique.")
if (!all(study2$story_condition %in% c("fall_of_freddie_leaf", "badgers_parting_gifts", "goodbye_grandma_erma"))) {
  stop("Unexpected Study 2 story condition.")
}
if (!all(study2$gender %in% c("female", "male"))) stop("Unexpected gender coding.")
if (!all(study2$age_years >= 8 & study2$age_years <= 16)) stop("Study 2 ages must be between 8 and 16 years.")

allocation_columns <- grep("^allocation_", names(study2), value = TRUE)
time_columns <- grep("^subjective_time_", names(study2), value = TRUE)
if (!all(unlist(study2[c(allocation_columns, time_columns)]) >= 0 &
         unlist(study2[c(allocation_columns, time_columns)]) <= 100)) {
  stop("Allocation and subjective-time values must be within 0-100.")
}
purpose_columns <- grep("^purpose_(pre|post)_", names(study2), value = TRUE)
intention_columns <- grep("^intention_(pre|post)_", names(study2), value = TRUE)
if (!all(unlist(study2[purpose_columns]) %in% purpose_levels)) stop("Unexpected purpose category.")
if (!all(unlist(study2[intention_columns]) %in% 1:3)) stop("Implementation-intention ratings must be 1, 2, or 3.")

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
  complete <- data %>% select(all_of(c(pre_column, post_column))) %>% drop_na()
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
  tibble(check = "analytic_n", value = nrow(study2), expected = "190"),
  tibble(check = "unique_participant_ids", value = n_distinct(study2$participant_id), expected = "190"),
  tibble(check = "story_conditions", value = n_distinct(study2$story_condition), expected = "3"),
  tibble(check = "purpose_responses_per_wave", value = nrow(study2) * 7L, expected = "1330")
)
write_result(data_checks, "data_checks.csv")

story_version_tests <- purrr::pmap_dfr(
  outcomes,
  function(outcome, label, pre_column, post_column) story_change_test(study2, outcome, pre_column, post_column)
)
write_result(story_version_tests, "story_version_tests.csv")

pooled_pre_post_tests <- purrr::pmap_dfr(
  outcomes,
  function(outcome, label, pre_column, post_column) paired_test(study2, outcome, pre_column, post_column)
)
write_result(pooled_pre_post_tests, "pooled_pre_post_tests.csv")

pre_purposes <- study2 %>%
  select(participant_id, all_of(paste0("purpose_pre_", sprintf("%02d", 1:7)))) %>%
  pivot_longer(-participant_id, names_to = "item", values_to = "purpose") %>%
  transmute(participant_id, wave = "Pretest", item = sub("purpose_pre_", "", item), purpose)
post_purposes <- study2 %>%
  select(participant_id, all_of(paste0("purpose_post_", sprintf("%02d", 1:7)))) %>%
  pivot_longer(-participant_id, names_to = "item", values_to = "purpose") %>%
  transmute(participant_id, wave = "Posttest", item = sub("purpose_post_", "", item), purpose)
purposes_long <- bind_rows(pre_purposes, post_purposes) %>%
  mutate(
    purpose = factor(purpose, levels = purpose_levels),
    wave = factor(wave, levels = c("Pretest", "Posttest"))
  )

purpose_distributions <- purposes_long %>%
  count(wave, purpose, .drop = FALSE, name = "count") %>%
  group_by(wave) %>%
  mutate(percent = 100 * count / sum(count)) %>%
  ungroup() %>%
  mutate(purpose_label = unname(purpose_labels[as.character(purpose)]))
write_result(purpose_distributions, "purpose_distributions.csv")

subgroup_ids <- study2 %>% filter(ln_k_post > ln_k_pre) %>% pull(participant_id)
if (length(subgroup_ids) != 71L) stop("Expected 71 participants with increased ln(K); found ", length(subgroup_ids), ".")
subgroup_pairs <- pre_purposes %>%
  filter(participant_id %in% subgroup_ids) %>%
  rename(pretest_purpose = purpose) %>%
  inner_join(
    post_purposes %>% filter(participant_id %in% subgroup_ids) %>% rename(posttest_purpose = purpose),
    by = c("participant_id", "item")
  )
if (nrow(subgroup_pairs) != 497L) stop("Expected 497 paired purpose observations; found ", nrow(subgroup_pairs), ".")

transition_matrix <- table(
  factor(subgroup_pairs$pretest_purpose, levels = purpose_levels),
  factor(subgroup_pairs$posttest_purpose, levels = purpose_levels)
)
transition_table <- as.data.frame.matrix(transition_matrix) %>%
  tibble::rownames_to_column("pretest_purpose") %>%
  rename_with(~ paste0("posttest_", .x), -pretest_purpose)
write_result(transition_table, "purpose_transition_table.csv")

bowker <- mcnemar.test(transition_matrix, correct = FALSE)

participant_counts <- subgroup_pairs %>%
  pivot_longer(c(pretest_purpose, posttest_purpose), names_to = "wave", values_to = "purpose") %>%
  mutate(wave = recode(wave, pretest_purpose = "pre", posttest_purpose = "post")) %>%
  count(participant_id, wave, purpose, name = "count") %>%
  complete(participant_id, wave, purpose = purpose_levels, fill = list(count = 0L)) %>%
  pivot_wider(names_from = wave, values_from = count, names_prefix = "count_") %>%
  mutate(change = count_post - count_pre)
write_result(participant_counts, "purpose_participant_counts.csv")

change_wide <- participant_counts %>%
  select(participant_id, purpose, change) %>%
  pivot_wider(names_from = purpose, values_from = change) %>%
  arrange(participant_id)
change_matrix <- as.matrix(change_wide[purpose_levels])
independent_changes <- change_matrix[, 1:4, drop = FALSE]
sample_size <- nrow(independent_changes)
dimensions <- ncol(independent_changes)
mean_vector <- colMeans(independent_changes)
inverse_covariance <- solve(cov(independent_changes))
hotelling_t2 <- as.numeric(sample_size * t(mean_vector) %*% inverse_covariance %*% mean_vector)
hotelling_f <- ((sample_size - dimensions) / (dimensions * (sample_size - 1L))) * hotelling_t2
hotelling_p <- pf(hotelling_f, dimensions, sample_size - dimensions, lower.tail = FALSE)

set.seed(20260727)
permutation_repetitions <- 100000L
permutation_statistics <- numeric(permutation_repetitions)
for (iteration in seq_len(permutation_repetitions)) {
  signs <- sample(c(-1, 1), sample_size, replace = TRUE)
  permuted_mean <- colMeans(independent_changes * signs)
  permutation_statistics[iteration] <- as.numeric(
    sample_size * t(permuted_mean) %*% inverse_covariance %*% permuted_mean
  )
}
global_permutation_p <- (1 + sum(permutation_statistics >= hotelling_t2)) / (permutation_repetitions + 1)

purpose_cluster_aware_tests <- tibble(
  analysis = c(
    "Conventional item-level Bowker benchmark",
    "Participant-level Hotelling T2",
    "Participant-level sign-flip permutation"
  ),
  statistic = c(unname(bowker$statistic), hotelling_t2, hotelling_t2),
  transformed_f = c(NA_real_, hotelling_f, NA_real_),
  df1 = c(unname(bowker$parameter), dimensions, NA_real_),
  df2 = c(NA_real_, sample_size - dimensions, NA_real_),
  repetitions = c(NA_integer_, NA_integer_, permutation_repetitions),
  p_value = c(bowker$p.value, hotelling_p, global_permutation_p),
  clustering_handled = c(FALSE, TRUE, TRUE)
)
write_result(purpose_cluster_aware_tests, "purpose_cluster_aware_tests.csv")

set.seed(20260727)
pairwise_bootstrap_repetitions <- 10000L
pairwise_permutation_repetitions <- 100000L
pairwise_transitions <- list()
pairwise_index <- 1L
for (first_index in seq_len(length(purpose_levels) - 1L)) {
  for (second_index in (first_index + 1L):length(purpose_levels)) {
    first <- purpose_levels[first_index]
    second <- purpose_levels[second_index]
    participant_net <- vapply(
      subgroup_ids,
      function(id) {
        participant_rows <- subgroup_pairs[subgroup_pairs$participant_id == id, ]
        sum(participant_rows$pretest_purpose == first & participant_rows$posttest_purpose == second) -
          sum(participant_rows$pretest_purpose == second & participant_rows$posttest_purpose == first)
      },
      numeric(1)
    )
    bootstrap_means <- replicate(
      pairwise_bootstrap_repetitions,
      mean(sample(participant_net, replace = TRUE))
    )
    sign_matrix <- matrix(
      sample(
        c(-1, 1),
        length(participant_net) * pairwise_permutation_repetitions,
        replace = TRUE
      ),
      nrow = length(participant_net)
    )
    permuted_totals <- colSums(participant_net * sign_matrix)
    observed_total <- abs(sum(participant_net))
    pair_permutation_p <- (
      1 + sum(abs(permuted_totals) >= observed_total)
    ) / (pairwise_permutation_repetitions + 1)
    pairwise_transitions[[pairwise_index]] <- tibble(
      from_purpose = first,
      to_purpose = second,
      forward_count = transition_matrix[first, second],
      reverse_count = transition_matrix[second, first],
      net_flow = sum(participant_net),
      participants_net_forward = sum(participant_net > 0),
      participants_net_reverse = sum(participant_net < 0),
      participants_net_zero = sum(participant_net == 0),
      mean_net_per_participant = mean(participant_net),
      bootstrap_ci_low = unname(quantile(bootstrap_means, 0.025)),
      bootstrap_ci_high = unname(quantile(bootstrap_means, 0.975)),
      permutation_p = pair_permutation_p
    )
    pairwise_index <- pairwise_index + 1L
  }
}
pairwise_transitions <- bind_rows(pairwise_transitions) %>%
  mutate(holm_adjusted_p = p.adjust(permutation_p, method = "holm")) %>%
  arrange(permutation_p)
write_result(pairwise_transitions, "purpose_pairwise_transitions.csv")

intention_pre <- study2 %>% select(all_of(paste0("intention_pre_", sprintf("%02d", 1:7)))) %>% as.matrix() %>% rowMeans()
intention_post <- study2 %>% select(all_of(paste0("intention_post_", sprintf("%02d", 1:7)))) %>% as.matrix() %>% rowMeans()
implementation_intention_test <- paired_test(
  tibble(pre = intention_pre, post = intention_post),
  "implementation_intention",
  "pre",
  "post"
)
write_result(implementation_intention_test, "implementation_intention_test.csv")

assert_close <- function(actual, expected, label, tolerance = 1e-5) {
  if (is.na(actual) || abs(actual - expected) > tolerance) {
    stop(label, " did not reproduce the reference result. Expected ", expected, "; found ", actual, ".")
  }
}

assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "ln_k"], 2.286692, "Study 2 ln(K) t")
assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "allocation_immediate"], 4.337577, "Study 2 immediate-allocation t")
assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "allocation_long"], -2.173582, "Study 2 long-allocation t")
assert_close(pooled_pre_post_tests$t_statistic[pooled_pre_post_tests$outcome == "subjective_time"], -2.772781, "Study 2 subjective-time t")
assert_close(implementation_intention_test$t_statistic, 3.563266, "Study 2 implementation-intention t")
assert_close(unname(bowker$statistic), 21.97525, "Study 2 Bowker chi-square")
assert_close(hotelling_t2, 5.614454, "Study 2 Hotelling T-squared")
if (round(global_permutation_p, 3) != 0.228) stop("Study 2 sign-flip permutation p did not round to .228.")
if (sum(purpose_distributions$count[purpose_distributions$wave == "Pretest"]) != 1330L ||
    sum(purpose_distributions$count[purpose_distributions$wave == "Posttest"]) != 1330L) {
  stop("Study 2 purpose totals must equal 1,330 responses at each wave.")
}

writeLines(capture.output(sessionInfo()), file.path(results_dir, "session_info.txt"))
cat("Study 2 analysis completed successfully. Results: ", results_dir, "\n", sep = "")
