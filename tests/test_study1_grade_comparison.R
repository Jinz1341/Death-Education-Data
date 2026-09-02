repo_root <- normalizePath(getwd(), mustWork = TRUE)
script <- file.path(repo_root, "code", "study1_grade_comparison.R")

analysis_output <- system2("Rscript", shQuote(script), stdout = TRUE, stderr = TRUE)
status <- attr(analysis_output, "status")
if (is.null(status)) status <- 0L
stopifnot(status == 0L)
stopifnot(!any(grepl("warning", analysis_output, ignore.case = TRUE)))

results_dir <- file.path(repo_root, "results", "study1_grade_comparison")
manova_result <- read.csv(file.path(results_dir, "grade_allocation_manova.csv"))
stopifnot(abs(manova_result$pillai_trace - .0529902) < 1e-6)
stopifnot(abs(manova_result$approx_f - 6.3509244) < 1e-6)
stopifnot(manova_result$df1 == 2L, manova_result$df2 == 227L)
stopifnot(abs(manova_result$p_value - .00207125) < 1e-7)

contrasts <- read.csv(file.path(results_dir, "grade_change_contrasts.csv"))
immediate <- contrasts[contrasts$outcome == "allocation_immediate", ]
short <- contrasts[contrasts$outcome == "allocation_short", ]
stopifnot(abs(immediate$mean_change_difference - 5.7833028) < 1e-6, immediate$p_value < .01)
stopifnot(abs(short$mean_change_difference + 7.5746490) < 1e-6, short$p_value < .01)
stopifnot(all(contrasts$p_value[contrasts$outcome %in% c("ln_k", "allocation_long", "subjective_time")] > .05))

within <- read.csv(file.path(results_dir, "grade_simple_effects.csv"))
stopifnot(nrow(within) == 10L)
stopifnot(within$p_value[within$outcome == "allocation_short" & within$school_level == "grade_4"] > .05)
stopifnot(within$p_value[within$outcome == "allocation_short" & within$school_level == "grade_8"] < .01)

expected_csv <- c(
  "data_checks.csv", "grade_allocation_manova.csv", "grade_change_contrasts.csv",
  "grade_prepost_descriptives.csv", "grade_simple_effects.csv"
)
stopifnot(identical(sort(list.files(results_dir, pattern = "[.]csv$")), sort(expected_csv)))

figure_dir <- file.path(repo_root, "figures", "main")
figure_files <- paste0("Figure_1_grade_change_comparison.", c("png", "pdf", "eps"))
stopifnot(all(file.exists(file.path(figure_dir, figure_files))))
stopifnot(all(file.info(file.path(figure_dir, figure_files))$size > 1000))
stopifnot(file.exists(file.path(results_dir, "session_info.txt")))

cat("Study 1 grade-comparison contract passed.\n")
