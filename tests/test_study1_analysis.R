repo_root <- normalizePath(getwd(), mustWork = TRUE)
script <- file.path(repo_root, "code", "study1_analysis.R")

analysis_output <- system2("Rscript", shQuote(script), stdout = TRUE, stderr = TRUE)
status <- attr(analysis_output, "status")
if (is.null(status)) status <- 0L
stopifnot(status == 0L)
stopifnot(!any(grepl("warning", analysis_output, ignore.case = TRUE)))

data <- read.csv(file.path(repo_root, "data", "study1_public.csv"))
stopifnot(nrow(data) == 230L, length(unique(data$participant_id)) == 230L)
stopifnot(sum(data$school_level == "grade_4") == 104L)
stopifnot(sum(data$school_level == "grade_8") == 126L)
stopifnot(!anyNA(data$age_years[data$school_level == "grade_4"]))
stopifnot(setequal(data$age_years[data$school_level == "grade_4"], c(9, 10)))

results_dir <- file.path(repo_root, "results", "study1")
sample <- read.csv(file.path(results_dir, "sample_characteristics.csv"))
stopifnot(sample$n[sample$school_level == "grade_4"] == 104L)
stopifnot(sample$n[sample$school_level == "grade_8"] == 126L)
stopifnot(abs(sample$age_mean[sample$school_level == "grade_4"] - 9.9326923) < 1e-6)
stopifnot(abs(sample$age_mean[sample$school_level == "grade_8"] - 14.0256410) < 1e-6)

age <- read.csv(file.path(results_dir, "age_grade_test.csv"))
stopifnot(age$df1 == 1L, age$df2 == 219L)
stopifnot(abs(age$f_statistic - 5697.7560) < .01, age$p_value < .001)

story <- read.csv(file.path(results_dir, "story_version_tests.csv"))
stopifnot(nrow(story) == 5L, all(story$p_value > .05))

pooled <- read.csv(file.path(results_dir, "pooled_pre_post_tests.csv"))
stopifnot(abs(pooled$mean_change[pooled$outcome == "ln_k"] - .5774176) < 1e-6)
stopifnot(abs(pooled$t_statistic[pooled$outcome == "allocation_immediate"] - 5.8077188) < 1e-6)
stopifnot(abs(pooled$t_statistic[pooled$outcome == "allocation_long"] + 4.4197364) < 1e-6)

baseline <- read.csv(file.path(results_dir, "baseline_grade_cohort_tests.csv"))
stopifnot(abs(baseline$mean_difference - 1.0912425) < 1e-6)
stopifnot(abs(baseline$t_statistic - 4.0464803) < 1e-6, baseline$p_value < .001)

expected_csv <- c(
  "age_grade_test.csv", "baseline_grade_cohort_tests.csv", "data_checks.csv",
  "gender_grade_test.csv", "pooled_pre_post_tests.csv", "sample_characteristics.csv",
  "story_version_tests.csv"
)
stopifnot(identical(sort(list.files(results_dir, pattern = "[.]csv$")), sort(expected_csv)))
stopifnot(file.exists(file.path(results_dir, "session_info.txt")))

cat("Study 1 analysis contract passed.\n")
