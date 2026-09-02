repo_root <- normalizePath(getwd(), mustWork = TRUE)
script <- file.path(repo_root, "code", "figure_generation.R")

analysis_output <- system2("Rscript", shQuote(script), stdout = TRUE, stderr = TRUE)
status <- attr(analysis_output, "status")
if (is.null(status)) status <- 0L
stopifnot(status == 0L)
stopifnot(!any(grepl("warning", analysis_output, ignore.case = TRUE)))

figure_dir <- file.path(repo_root, "figures", "main")
expected <- c(
  paste0("Figure_1_grade_change_comparison.", c("eps", "pdf", "png")),
  paste0("Figure_2_ln_k_pre_post.", c("pdf", "png")),
  paste0("Figure_3_temporal_allocation_bars.", c("pdf", "png")),
  paste0("Figure_4_subjective_time_pre_post.", c("pdf", "png"))
)
stopifnot(identical(sort(list.files(figure_dir)), sort(expected)))
stopifnot(all(file.info(file.path(figure_dir, expected))$size > 1000))

statistics <- read.csv(file.path(repo_root, "results", "figures", "figure_main_statistics.csv"))
stopifnot(nrow(statistics) == 10L)
stopifnot(setequal(statistics$study, c("Study 1", "Study 2")))
stopifnot(statistics$n[statistics$study == "Study 1"][1] == 230L)
stopifnot(statistics$n[statistics$study == "Study 2"][1] == 184L)

cat("Figure-generation contract passed.\n")

