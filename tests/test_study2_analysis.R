repo_root <- normalizePath(getwd(), mustWork = TRUE)
script <- file.path(repo_root, "code", "study2_analysis.R")

analysis_output <- system2("Rscript", shQuote(script), stdout = TRUE, stderr = TRUE)
status <- attr(analysis_output, "status")
if (is.null(status)) status <- 0L
stopifnot(status == 0L)
stopifnot(!any(grepl("warning", analysis_output, ignore.case = TRUE)))

wide <- read.csv(file.path(repo_root, "data", "study2_public.csv"))
item_long <- read.csv(file.path(repo_root, "data", "study2_item_choice_purpose_long.csv"))
stopifnot(nrow(wide) == 184L, length(unique(wide$participant_id)) == 184L)
stopifnot(min(wide$age_years) == 10, max(wide$age_years) == 16)
stopifnot(nrow(item_long) == 2576L, length(unique(item_long$participant_id)) == 184L)
stopifnot(!anyDuplicated(item_long[c("participant_id", "wave", "item")]))

results_dir <- file.path(repo_root, "results", "study2")
pooled <- read.csv(file.path(results_dir, "pooled_pre_post_tests.csv"))
stopifnot(abs(pooled$mean_change[pooled$outcome == "ln_k"] - .4096698) < 1e-6)
stopifnot(abs(pooled$mean_change[pooled$outcome == "allocation_immediate"] - 7.3842391) < 1e-6)
stopifnot(abs(pooled$mean_change[pooled$outcome == "allocation_short"] + 3.0849638) < 1e-6)
stopifnot(abs(pooled$mean_change[pooled$outcome == "allocation_long"] + 4.2992754) < 1e-6)
stopifnot(abs(pooled$mean_change[pooled$outcome == "subjective_time"] + 3.1594203) < 1e-6)

allocation <- read.csv(file.path(results_dir, "allocation_global_test.csv"))
stopifnot(abs(allocation$f_statistic - 11.6099549) < 1e-6, allocation$p_value < .001)

intention <- read.csv(file.path(results_dir, "implementation_intention_test.csv"))
stopifnot(abs(intention$mean_change - .0776398) < 1e-6, intention$p_value < .001)

choice_distribution <- read.csv(file.path(results_dir, "choice_purpose_distribution.csv"))
stopifnot(sum(choice_distribution$count) == 2576L)
stopifnot(all(abs(tapply(choice_distribution$percent, list(choice_distribution$wave, choice_distribution$choice), sum) - 100) < 1e-8))

choice_omnibus <- read.csv(file.path(results_dir, "choice_model_omnibus.csv"))
intention_omnibus <- read.csv(file.path(results_dir, "intention_model_omnibus.csv"))
stopifnot(nrow(choice_omnibus) == 4L, nrow(intention_omnibus) == 5L)

expected_csv <- c(
  "allocation_global_test.csv", "choice_model_omnibus.csv", "choice_model_terms.csv",
  "choice_purpose_distribution.csv", "data_integrity_diagnostics.csv",
  "implementation_intention_test.csv", "intention_choice_purpose_descriptives.csv",
  "intention_model_omnibus.csv", "intention_model_terms.csv",
  "overall_purpose_distribution.csv", "pooled_pre_post_tests.csv",
  "story_cell_characteristics.csv", "story_version_tests.csv"
)
stopifnot(identical(sort(list.files(results_dir, pattern = "[.]csv$")), sort(expected_csv)))
stopifnot(file.exists(file.path(results_dir, "session_info.txt")))

cat("Study 2 analysis contract passed.\n")
