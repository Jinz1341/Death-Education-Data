options(stringsAsFactors = FALSE)

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("Run this file with Rscript code/figure_generation.R.")
script_path <- gsub("~\\+~", " ", sub("^--file=", "", script_argument))
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
figures_dir <- file.path(repo_root, "figures", "main")
results_dir <- file.path(repo_root, "results", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

s1 <- read.csv(file.path(repo_root, "data", "study1_public.csv"))
s2 <- read.csv(file.path(repo_root, "data", "study2_public.csv"))

required_columns <- c(
  "ln_k_pre", "ln_k_post",
  "allocation_immediate_pre", "allocation_immediate_post",
  "allocation_short_pre", "allocation_short_post",
  "allocation_long_pre", "allocation_long_post",
  "subjective_time_pre", "subjective_time_post"
)
if (!all(required_columns %in% names(s1))) {
  stop("Study 1 data are missing figure columns: ", paste(setdiff(required_columns, names(s1)), collapse = ", "))
}
if (!all(required_columns %in% names(s2))) {
  stop("Study 2 data are missing figure columns: ", paste(setdiff(required_columns, names(s2)), collapse = ", "))
}
if (nrow(s1) != 250L || nrow(s2) != 190L) stop("Unexpected analytic sample size.")

blue <- "#0072B2"
red <- "#D73027"
ink <- "#252525"
grid <- "#D9D9D9"

mean_ci <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  crit <- qt(.975, n - 1)
  c(n = n, mean = m, low = m - crit * se, high = m + crit * se)
}

paired_summary <- function(data, pre, post, study, outcome) {
  ok <- complete.cases(data[, c(pre, post)])
  x <- data[ok, pre]
  y <- data[ok, post]
  tt <- t.test(y, x, paired = TRUE)
  pre_ci <- mean_ci(x)
  post_ci <- mean_ci(y)
  delta_ci <- mean_ci(y - x)
  data.frame(
    study = study,
    outcome = outcome,
    n = sum(ok),
    pre_mean = unname(pre_ci["mean"]),
    pre_ci_low = unname(pre_ci["low"]),
    pre_ci_high = unname(pre_ci["high"]),
    post_mean = unname(post_ci["mean"]),
    post_ci_low = unname(post_ci["low"]),
    post_ci_high = unname(post_ci["high"]),
    mean_change = unname(delta_ci["mean"]),
    change_ci_low = unname(delta_ci["low"]),
    change_ci_high = unname(delta_ci["high"]),
    t = unname(tt$statistic),
    df = unname(tt$parameter),
    p = tt$p.value,
    dz = mean(y - x) / sd(y - x)
  )
}

p_text <- function(p) {
  if (p < .001) "p < .001" else sprintf("p = %.3f", p)
}

open_png <- function(filename, width, height) {
  png(
    file.path(figures_dir, filename),
    width = width,
    height = height,
    units = "in",
    res = 600
  )
}

open_pdf <- function(filename, width, height) {
  pdf(file.path(figures_dir, filename), width = width, height = height)
}

prepost_panel <- function(data, pre, post, title, ylab, ylim, digits = 2) {
  ok <- complete.cases(data[, c(pre, post)])
  x <- data[ok, pre]
  y <- data[ok, post]
  cis <- rbind(mean_ci(x), mean_ci(y))
  tt <- t.test(y, x, paired = TRUE)

  plot(
    c(1, 2),
    cis[, "mean"],
    type = "n",
    xlim = c(.55, 2.45),
    ylim = ylim,
    xaxt = "n",
    xlab = "",
    ylab = ylab,
    main = title,
    col.axis = ink,
    col.lab = ink,
    col.main = ink,
    bty = "l"
  )
  abline(h = pretty(ylim), col = grid, lwd = .7)
  axis(1, at = c(1, 2), labels = c("Pretest", "Posttest"), col.axis = ink)
  segments(1, cis[1, "mean"], 2, cis[2, "mean"], col = "#6F6F6F", lwd = 1.5)
  arrows(
    c(1, 2),
    cis[, "low"],
    c(1, 2),
    cis[, "high"],
    angle = 90,
    code = 3,
    length = .06,
    lwd = 1.3,
    col = c(blue, red)
  )
  points(
    c(1, 2),
    cis[, "mean"],
    pch = c(21, 22),
    bg = c(blue, red),
    col = ink,
    cex = 1.55,
    lwd = 1
  )
  labels <- formatC(cis[, "mean"], digits = digits, format = "f")
  text(c(1, 2), cis[, "high"], labels, pos = 3, cex = .88, col = ink)
  mtext(
    sprintf("N = %d; paired %s", sum(ok), p_text(tt$p.value)),
    side = 3,
    line = .15,
    cex = .82,
    col = "#4D4D4D"
  )
}

allocation_panel <- function(data, title) {
  specs <- list(
    Immediate = c("allocation_immediate_pre", "allocation_immediate_post"),
    "Short-term" = c("allocation_short_pre", "allocation_short_post"),
    "Long-term" = c("allocation_long_pre", "allocation_long_post")
  )
  means <- matrix(NA_real_, nrow = 2, ncol = 3)
  lows <- means
  highs <- means
  ps <- numeric(3)
  for (j in seq_along(specs)) {
    pre <- data[[specs[[j]][1]]]
    post <- data[[specs[[j]][2]]]
    means[, j] <- c(mean(pre), mean(post))
    pre_ci <- mean_ci(pre)
    post_ci <- mean_ci(post)
    lows[, j] <- c(pre_ci["low"], post_ci["low"])
    highs[, j] <- c(pre_ci["high"], post_ci["high"])
    ps[j] <- t.test(post, pre, paired = TRUE)$p.value
  }

  mids <- barplot(
    means,
    beside = TRUE,
    col = c(blue, red),
    border = ink,
    names.arg = names(specs),
    ylim = c(0, 62),
    yaxt = "n",
    ylab = "Mean allocation (%)",
    main = title,
    cex.names = .9,
    col.axis = ink,
    col.lab = ink,
    col.main = ink
  )
  axis(2, las = 1, col.axis = ink)
  arrows(
    as.vector(mids),
    as.vector(lows),
    as.vector(mids),
    as.vector(highs),
    angle = 90,
    code = 3,
    length = .04,
    lwd = 1,
    col = ink
  )
  text(
    as.vector(mids),
    as.vector(highs),
    labels = formatC(as.vector(means), format = "f", digits = 1),
    pos = 3,
    cex = .72,
    col = ink
  )
  group_x <- colMeans(mids)
  text(group_x, rep(59, 3), labels = vapply(ps, p_text, character(1)), cex = .76, col = ink)
}

core_specs <- list(
  "K value" = c("ln_k_pre", "ln_k_post"),
  "Immediate allocation" = c("allocation_immediate_pre", "allocation_immediate_post"),
  "Short-term allocation" = c("allocation_short_pre", "allocation_short_post"),
  "Long-term allocation" = c("allocation_long_pre", "allocation_long_post"),
  "Subjective time estimate" = c("subjective_time_pre", "subjective_time_post")
)

summary_rows <- list()
for (nm in names(core_specs)) {
  summary_rows[[length(summary_rows) + 1L]] <- paired_summary(
    s1, core_specs[[nm]][1], core_specs[[nm]][2], "Study 1", nm
  )
  summary_rows[[length(summary_rows) + 1L]] <- paired_summary(
    s2, core_specs[[nm]][1], core_specs[[nm]][2], "Study 2", nm
  )
}
figure_stats <- do.call(rbind, summary_rows)
write.csv(figure_stats, file.path(results_dir, "figure_main_statistics.csv"), row.names = FALSE)

draw_figure1 <- function() {
  par(
    mfrow = c(1, 2),
    mar = c(4.4, 4.4, 3.8, 1.3),
    oma = c(0, 0, 2.1, 0),
    family = "sans",
    las = 1
  )
  prepost_panel(s1, "ln_k_pre", "ln_k_post", "Study 1", "Mean K value (95% CI)", c(-6.9, -4.8))
  prepost_panel(s2, "ln_k_pre", "ln_k_post", "Study 2", "Mean K value (95% CI)", c(-6.9, -4.8))
  mtext("K Values Before and After the Session", outer = TRUE, cex = 1.15, font = 2, col = ink)
}

open_png("Figure_1_K_value_pre_post.png", 10.5, 5.2)
draw_figure1()
dev.off()
open_pdf("Figure_1_K_value_pre_post.pdf", 10.5, 5.2)
draw_figure1()
dev.off()

draw_figure2 <- function() {
  par(
    mfrow = c(1, 2),
    mar = c(5.2, 4.5, 3.8, 1.2),
    oma = c(0, 0, 3.2, 0),
    family = "sans",
    las = 1
  )
  allocation_panel(s1, "Study 1")
  allocation_panel(s2, "Study 2")
  mtext(
    "Temporal Allocation Before and After the Session",
    outer = TRUE,
    line = 1.7,
    cex = 1.15,
    font = 2,
    col = ink
  )
  mtext(
    "Blue = Pretest; Red = Posttest; error bars are 95% CIs",
    outer = TRUE,
    line = .35,
    cex = .8,
    col = "#4D4D4D"
  )
}

open_png("Figure_2_temporal_allocation_bars.png", 12, 5.6)
draw_figure2()
dev.off()
open_pdf("Figure_2_temporal_allocation_bars.pdf", 12, 5.6)
draw_figure2()
dev.off()

draw_figure3 <- function() {
  par(
    mfrow = c(1, 2),
    mar = c(4.4, 4.4, 3.8, 1.3),
    oma = c(0, 0, 2.1, 0),
    family = "sans",
    las = 1
  )
  prepost_panel(
    s1,
    "subjective_time_pre",
    "subjective_time_post",
    "Study 1",
    "Mean subjective time estimate (0-100)",
    c(36, 59)
  )
  prepost_panel(
    s2,
    "subjective_time_pre",
    "subjective_time_post",
    "Study 2",
    "Mean subjective time estimate (0-100)",
    c(36, 59)
  )
  mtext(
    "Subjective Time Estimates Before and After the Session",
    outer = TRUE,
    cex = 1.15,
    font = 2,
    col = ink
  )
}

open_png("Figure_3_subjective_time_pre_post.png", 10.5, 5.2)
draw_figure3()
dev.off()
open_pdf("Figure_3_subjective_time_pre_post.pdf", 10.5, 5.2)
draw_figure3()
dev.off()

cat("Figure generation completed successfully. Figures: ", figures_dir, "\n", sep = "")
