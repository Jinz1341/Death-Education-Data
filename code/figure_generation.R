script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("Run this file with Rscript code/figure_generation.R.")
script_path <- gsub("~\\+~", " ", sub("^--file=", "", script_argument))
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

study1_path <- file.path(repo_root, "data", "study1_public.csv")
study2_path <- file.path(repo_root, "data", "study2_public.csv")
figures_dir <- file.path(repo_root, "figures", "main")
results_dir <- file.path(repo_root, "results", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

s1 <- read.csv(study1_path, check.names = FALSE)
s2 <- read.csv(study2_path, check.names = FALSE)
if (nrow(s1) != 230L) stop("Study 1 figure data must contain 230 rows.")
if (nrow(s2) != 184L) stop("Study 2 figure data must contain 184 rows.")

ink <- "#1F1F1F"
grid <- "#D9D9D9"
blue <- "#2F6B9A"
red <- "#B94B4B"

open_png <- function(filename, width, height) {
  png(file.path(figures_dir, filename), width = width, height = height, units = "in", res = 400, bg = "white")
}

open_pdf <- function(filename, width, height) {
  pdf(file.path(figures_dir, filename), width = width, height = height, useDingbats = FALSE)
}

mean_ci <- function(values) {
  values <- values[!is.na(values)]
  n <- length(values)
  mean_value <- mean(values)
  error <- qt(.975, n - 1L) * sd(values) / sqrt(n)
  c(mean = mean_value, low = mean_value - error, high = mean_value + error, n = n)
}

format_p <- function(p_value) {
  if (p_value < .001) {
    "italic(p) < .001"
  } else {
    paste0("italic(p) == ", sub("^0", "", sprintf("%.3f", p_value)))
  }
}

paired_summary <- function(data, pre_column, post_column, study_label, outcome) {
  pre <- data[[pre_column]]
  post <- data[[post_column]]
  change <- post - pre
  test <- t.test(post, pre, paired = TRUE)
  data.frame(
    study = study_label,
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
    stringsAsFactors = FALSE
  )
}

prepost_panel <- function(data, pre_column, post_column, panel_title, y_label, y_limits) {
  pre <- mean_ci(data[[pre_column]])
  post <- mean_ci(data[[post_column]])
  paired <- t.test(data[[post_column]], data[[pre_column]], paired = TRUE)
  x <- c(1, 2)
  means <- c(pre["mean"], post["mean"])
  lows <- c(pre["low"], post["low"])
  highs <- c(pre["high"], post["high"])
  plot(
    x, means, type = "n", xlim = c(.65, 2.35), ylim = y_limits,
    xaxt = "n", xlab = "", ylab = y_label, main = panel_title,
    bty = "l", col.axis = ink, col.lab = ink, col.main = ink
  )
  abline(h = pretty(y_limits), col = grid, lwd = .7)
  segments(x[1], means[1], x[2], means[2], col = "#6B6B6B", lwd = 1.5)
  arrows(x, lows, x, highs, angle = 90, code = 3, length = .06, col = c(blue, red), lwd = 1.2)
  points(x, means, pch = 21, bg = c(blue, red), col = ink, cex = 1.55)
  axis(1, at = x, labels = c("Pretest", "Posttest"), col.axis = ink)
  text(
    1.5, y_limits[2] - .05 * diff(y_limits),
    labels = parse(text = format_p(paired$p.value)), cex = .9, col = ink
  )
  mtext(bquote(italic(n) == .(nrow(data))), side = 3, line = .15, cex = .75, col = "#4D4D4D")
}

allocation_panel <- function(data, panel_title) {
  pre_columns <- c("allocation_immediate_pre", "allocation_short_pre", "allocation_long_pre")
  post_columns <- c("allocation_immediate_post", "allocation_short_post", "allocation_long_post")
  labels <- c("Immediate", "Short-term", "Long-term")
  pre_summary <- t(vapply(pre_columns, function(column) mean_ci(data[[column]])[c("mean", "low", "high")], numeric(3)))
  post_summary <- t(vapply(post_columns, function(column) mean_ci(data[[column]])[c("mean", "low", "high")], numeric(3)))
  means <- rbind(pre_summary[, "mean"], post_summary[, "mean"])
  lows <- rbind(pre_summary[, "low"], post_summary[, "low"])
  highs <- rbind(pre_summary[, "high"], post_summary[, "high"])
  y_max <- max(highs) + 12
  mids <- barplot(
    means, beside = TRUE, col = c(blue, red), border = NA,
    names.arg = labels, ylim = c(0, y_max),
    ylab = "Mean allocation (%)", main = panel_title,
    col.axis = ink, col.lab = ink, col.main = ink
  )
  abline(h = seq(0, y_max, 10), col = grid, lwd = .7)
  for (row in 1:2) {
    arrows(mids[row, ], lows[row, ], mids[row, ], highs[row, ], angle = 90, code = 3, length = .04, lwd = 1)
  }
  p_values <- vapply(seq_along(pre_columns), function(index) {
    t.test(data[[post_columns[index]]], data[[pre_columns[index]]], paired = TRUE)$p.value
  }, numeric(1))
  text(
    colMeans(mids), apply(highs, 2, max) + 3.5,
    labels = parse(text = vapply(p_values, format_p, character(1))), cex = .72
  )
  legend("topright", legend = c("Pretest", "Posttest"), fill = c(blue, red), border = NA, bty = "n", cex = .8)
  mtext(bquote(italic(n) == .(nrow(data))), side = 3, line = .15, cex = .75, col = "#4D4D4D")
}

core_specs <- list(
  ln_k = c("ln_k_pre", "ln_k_post"),
  allocation_immediate = c("allocation_immediate_pre", "allocation_immediate_post"),
  allocation_short = c("allocation_short_pre", "allocation_short_post"),
  allocation_long = c("allocation_long_pre", "allocation_long_post"),
  subjective_time = c("subjective_time_pre", "subjective_time_post")
)
summary_rows <- list()
for (outcome in names(core_specs)) {
  summary_rows[[length(summary_rows) + 1L]] <- paired_summary(
    s1, core_specs[[outcome]][1], core_specs[[outcome]][2], "Study 1", outcome
  )
  summary_rows[[length(summary_rows) + 1L]] <- paired_summary(
    s2, core_specs[[outcome]][1], core_specs[[outcome]][2], "Study 2", outcome
  )
}
figure_stats <- do.call(rbind, summary_rows)
write.csv(figure_stats, file.path(results_dir, "figure_main_statistics.csv"), row.names = FALSE)

draw_figure2 <- function() {
  par(mfrow = c(1, 2), mar = c(4.4, 4.4, 3.8, 1.3), oma = c(0, 0, 2.1, 0), family = "sans", las = 1)
  prepost_panel(
    s1, "ln_k_pre", "ln_k_post", "Study 1",
    expression(paste("Mean ln(", italic(k), ") (95% CI)")), c(-6.9, -4.55)
  )
  prepost_panel(
    s2, "ln_k_pre", "ln_k_post", "Study 2",
    expression(paste("Mean ln(", italic(k), ") (95% CI)")), c(-6.9, -4.55)
  )
  mtext(
    expression(paste("ln(", italic(k), ") Before and After the Session")),
    outer = TRUE, cex = 1.15, font = 2, col = ink
  )
}

open_png("Figure_2_ln_k_pre_post.png", 10.5, 5.2)
draw_figure2()
dev.off()
open_pdf("Figure_2_ln_k_pre_post.pdf", 10.5, 5.2)
draw_figure2()
dev.off()

draw_figure3 <- function() {
  par(mfrow = c(1, 2), mar = c(5.2, 4.5, 3.8, 1.2), oma = c(0, 0, 3.2, 0), family = "sans", las = 1)
  allocation_panel(s1, "Study 1")
  allocation_panel(s2, "Study 2")
  mtext("Resource Allocation Across Temporal Accounts", outer = TRUE, line = 1.7, cex = 1.15, font = 2, col = ink)
  mtext("Error bars show 95% confidence intervals", outer = TRUE, line = .35, cex = .8, col = "#4D4D4D")
}

open_png("Figure_3_temporal_allocation_bars.png", 12, 5.6)
draw_figure3()
dev.off()
open_pdf("Figure_3_temporal_allocation_bars.pdf", 12, 5.6)
draw_figure3()
dev.off()

draw_figure4 <- function() {
  par(mfrow = c(1, 2), mar = c(4.4, 4.4, 3.8, 1.3), oma = c(0, 0, 2.1, 0), family = "sans", las = 1)
  prepost_panel(
    s1, "subjective_time_pre", "subjective_time_post", "Study 1",
    "Mean feeling of time passage (0-100)", c(36, 59)
  )
  prepost_panel(
    s2, "subjective_time_pre", "subjective_time_post", "Study 2",
    "Mean feeling of time passage (0-100)", c(36, 59)
  )
  mtext("Subjective Feeling of Time Passage Before and After the Session", outer = TRUE, cex = 1.15, font = 2, col = ink)
}

open_png("Figure_4_subjective_time_pre_post.png", 10.5, 5.2)
draw_figure4()
dev.off()
open_pdf("Figure_4_subjective_time_pre_post.pdf", 10.5, 5.2)
draw_figure4()
dev.off()

cat("Figure generation completed successfully. Figures: ", figures_dir, "\n", sep = "")
