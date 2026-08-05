repo_root <- normalizePath(file.path(getwd()), mustWork = TRUE)
script <- file.path(repo_root, "code", "study1_analysis.R")

status <- system(paste("Rscript", shQuote(script)))
stopifnot(status == 0L)

results <- read.csv(file.path(repo_root, "results", "study1", "pooled_pre_post_tests.csv"))
ln_k <- results[results$outcome == "ln_k", ]
stopifnot(nrow(ln_k) == 1L)
stopifnot(abs(ln_k$t_statistic - 5.004781) < 1e-5)

stopifnot(file.exists(file.path(repo_root, "results", "study1", "session_info.txt")))

cat("Study 1 public analysis contract passed.\n")
