repo_root <- normalizePath(file.path(getwd()), mustWork = TRUE)
script <- file.path(repo_root, "code", "figure_generation.R")

status <- system(paste("Rscript", shQuote(script)))
stopifnot(status == 0L)

required_figures <- c(
  "Figure_1_K_value_pre_post.png",
  "Figure_1_K_value_pre_post.pdf",
  "Figure_2_temporal_allocation_bars.png",
  "Figure_2_temporal_allocation_bars.pdf",
  "Figure_3_subjective_time_pre_post.png",
  "Figure_3_subjective_time_pre_post.pdf"
)
for (figure in required_figures) {
  path <- file.path(repo_root, "figures", "main", figure)
  stopifnot(file.exists(path), file.info(path)$size > 0L)
}

statistics_path <- file.path(repo_root, "results", "figures", "figure_main_statistics.csv")
stopifnot(file.exists(statistics_path))
statistics <- read.csv(statistics_path)
stopifnot(nrow(statistics) == 10L)

study1_k <- statistics[statistics$study == "Study 1" & statistics$outcome == "K value", ]
study2_time <- statistics[statistics$study == "Study 2" & statistics$outcome == "Subjective time estimate", ]
stopifnot(nrow(study1_k) == 1L, abs(study1_k$t - 5.004781) < 1e-5)
stopifnot(nrow(study2_time) == 1L, abs(study2_time$t + 2.772781) < 1e-5)

cat("Figure-generation contract passed.\n")
