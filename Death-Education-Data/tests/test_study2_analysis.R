repo_root <- normalizePath(file.path(getwd()), mustWork = TRUE)
script <- file.path(repo_root, "code", "study2_analysis.R")

status <- system(paste("Rscript", shQuote(script)))
stopifnot(status == 0L)

results <- read.csv(file.path(repo_root, "results", "study2", "pooled_pre_post_tests.csv"))
subjective_time <- results[results$outcome == "subjective_time", ]
stopifnot(nrow(subjective_time) == 1L)
stopifnot(abs(subjective_time$t_statistic + 2.772781) < 1e-5)

pairwise <- read.csv(file.path(repo_root, "results", "study2", "purpose_pairwise_transitions.csv"))
stopifnot(round(min(pairwise$holm_adjusted_p), 3) == 0.151)
stopifnot(min(pairwise$holm_adjusted_p) > 0.05)

stopifnot(file.exists(file.path(repo_root, "results", "study2", "session_info.txt")))

cat("Study 2 public analysis contract passed.\n")
