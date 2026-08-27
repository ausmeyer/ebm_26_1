#!/usr/bin/env Rscript

# Reproducible explanatory figures for EBM 26_1.
# All plotted values are synthetic teaching data unless they reproduce
# arithmetic stated in the source PREP question.

primary <- "#43418A"
secondary <- "#8CB3D9"
accent <- "#96C3CE"
success <- "#4E9F62"
warning <- "#D95F59"
ink <- "#272822"
muted <- "#6B7280"
paper <- "#FFFFFF"

out_dir <- "figs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

open_png <- function(name, width = 1600, height = 900) {
  png(file.path(out_dir, name), width = width, height = height, res = 160,
      bg = paper, type = "cairo")
  par(family = "sans", fg = ink, col.axis = ink, col.lab = ink,
      mar = c(5, 6, 4, 2) + 0.1)
}

panel_title <- function(main, sub = NULL) {
  title(main = main, col.main = primary, font.main = 2, cex.main = 1.45,
        line = 1.2)
  if (!is.null(sub)) {
    mtext(sub, side = 3, line = 0.05, cex = 0.88, col = muted)
  }
}

# Figure 1: Stable incidence but increasing prevalence when duration increases.
open_png("q10_incidence_prevalence.png")
years <- 1:12
incidence <- rep(18, length(years))
prevalence_old <- 18 * (1 - exp(-years / 2.2)) * 2.2
prevalence_new <- c(prevalence_old[1:4], prevalence_old[4] + cumsum(incidence[5:12] * 0.72))
plot(years, prevalence_new, type = "n", ylim = c(0, max(prevalence_new) * 1.08),
     xlab = "Time", ylab = "People / new cases per period", axes = FALSE)
axis(1, at = years, labels = paste0("T", years), col.axis = muted)
axis(2, las = 1, col.axis = muted)
abline(h = seq(0, max(prevalence_new), length.out = 6), col = "#ECECF3", lwd = 1)
lines(years, incidence, col = secondary, lwd = 6)
lines(years, prevalence_new, col = primary, lwd = 7)
abline(v = 4.5, col = warning, lty = 2, lwd = 3)
points(years, incidence, pch = 21, bg = paper, col = secondary, lwd = 2, cex = 1.25)
points(years, prevalence_new, pch = 21, bg = paper, col = primary, lwd = 2, cex = 1.25)
text(8.2, incidence + 10, "Incidence: stable inflow", col = secondary, font = 2, cex = 1.1)
text(8.0, prevalence_new[10] + 15, "Prevalence: accumulating pool", col = primary,
     font = 2, cex = 1.1)
text(4.35, max(prevalence_new) * 0.91, "Longer survival\nbegins", col = warning,
     pos = 2, cex = 0.9)
panel_title("Stable incidence can coexist with rising prevalence",
            "Synthetic stock-flow illustration: prevalence grows when disease duration increases")
box(col = "#D8DAE6")
dev.off()

# Figure 2: Precision vs accuracy as repeated measurement patterns.
open_png("q24_precision_accuracy.png", 1700, 900)
par(mfrow = c(1, 3), mar = c(2, 2, 4, 2), oma = c(0, 0, 2, 0))
draw_target <- function(points, heading, caption, point_col) {
  plot(0, 0, type = "n", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  for (r in c(1, 0.72, 0.45, 0.18)) {
    symbols(0, 0, circles = r, inches = FALSE, add = TRUE,
            fg = if (r == 0.18) warning else "#C9CBD8", lwd = if (r == 0.18) 3 else 2)
  }
  abline(h = 0, v = 0, col = "#E6E7EE", lwd = 1)
  points(points[, 1], points[, 2], pch = 21, bg = point_col,
         col = paper, lwd = 1.5, cex = 2.1)
  title(main = heading, col.main = primary, font.main = 2, cex.main = 1.25)
  mtext(caption, side = 1, line = 0.3, cex = 0.9, col = ink)
}
precise_not_accurate <- matrix(c(0.58, 0.51, 0.64, 0.55, 0.61, 0.46,
                                 0.56, 0.49, 0.60, 0.43, 0.67, 0.48), ncol = 2)
accurate_not_precise <- matrix(c(-0.60, -0.36, 0.48, 0.51, -0.42, 0.39,
                                 0.58, -0.44, -0.18, 0.63, 0.24, -0.58), ncol = 2)
accurate_precise <- matrix(c(-0.08, 0.03, 0.09, -0.04, -0.03, 0.06,
                             0.04, -0.07, 0.02, 0.08, -0.06, -0.02), ncol = 2)
draw_target(precise_not_accurate, "Precise, not accurate", "Repeatable - but biased", primary)
draw_target(accurate_not_precise, "Accurate on average", "Near truth - but variable", secondary)
draw_target(accurate_precise, "Accurate and precise", "Near truth - and repeatable", success)
mtext("Repeated samples assess precision; a reference standard is needed for accuracy",
      outer = TRUE, side = 3, line = 0.2, col = primary, font = 2, cex = 1.35)
dev.off()

# Figure 3: Cross-sectional design is a snapshot.
open_png("q20_cross_sectional_snapshot.png", 1700, 850)
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 6), axes = FALSE,
     xlab = "", ylab = "")
abline(h = 1.15, col = "#CDD0DC", lwd = 4)
arrows(0.7, 1.15, 9.35, 1.15, length = 0.12, col = muted, lwd = 3)
text(0.7, 0.65, "Past", col = muted, cex = 1.1)
text(9.3, 0.65, "Future", col = muted, cex = 1.1)
rect(4.35, 0.65, 5.65, 5.1, col = "#E8F1F7", border = secondary, lwd = 4)
text(5, 4.55, "ONE TIME WINDOW", col = primary, font = 2, cex = 1.15)
text(5, 3.35, "Exposure measured", col = ink, cex = 1.2)
text(5, 2.65, "+", col = warning, cex = 1.4, font = 2)
text(5, 1.95, "Outcome measured", col = ink, cex = 1.2)
text(1.9, 4.0, "No direction\nof inquiry", col = muted, cex = 1.15)
arrows(3.75, 3.7, 2.7, 3.7, length = 0.1, col = muted, lwd = 2)
text(8.1, 4.0, "Temporality\nnot established", col = warning, cex = 1.15)
arrows(6.25, 3.7, 7.3, 3.7, length = 0.1, col = warning, lwd = 2)
panel_title("A cross-sectional study measures exposure and outcome together",
            "It can estimate prevalence and association, but cannot establish which came first")
dev.off()

# Figure 4: Biological gradient as one causal consideration.
open_png("q18_biological_gradient.png")
exposure <- 1:5
rate <- c(1.00, 1.08, 1.20, 1.42, 1.70)
plot(exposure, rate, type = "n", xlim = c(0.7, 5.3), ylim = c(0.8, 1.9),
     xlab = "Increasing exposure intensity", ylab = "Relative outcome frequency",
     axes = FALSE)
axis(1, at = exposure, labels = c("Lowest", "Low", "Middle", "High", "Highest"),
     col.axis = muted)
axis(2, at = seq(0.8, 1.8, 0.2), las = 1, col.axis = muted)
abline(h = seq(0.8, 1.8, 0.2), col = "#ECECF3")
polygon(c(exposure, rev(exposure)),
        c(rate - c(.06, .07, .08, .11, .14), rev(rate + c(.06, .07, .08, .11, .14))),
        col = "#DCECF2", border = NA)
lines(exposure, rate, col = primary, lwd = 7)
points(exposure, rate, pch = 21, bg = paper, col = primary, lwd = 3, cex = 1.7)
text(3.8, 0.92, "Supports a causal argument", col = success, font = 2, cex = 1.05)
text(3.8, 0.83, "Does not prove causation", col = warning, font = 2, cex = 1.05)
panel_title("A biological gradient is a dose-response pattern",
            "Synthetic illustration - temporality, bias, confounding, replication, and plausibility still matter")
box(col = "#D8DAE6")
dev.off()

# Figure 5: Exact age distributions from PREP Question 15.
open_png("q15_age_distributions.png", 1700, 900)
bubbles <- c(rep(1.5, 3), rep(2, 9), rep(2.5, 3), 5, 6, 7, 8, 9)
puppy <- c(rep(3, 5), rep(4, 8), rep(5, 5))
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2), oma = c(0, 0, 2, 0))
draw_dotstack <- function(values, heading, col) {
  tab <- table(values)
  xs <- as.numeric(names(tab))
  max_n <- max(tab)
  plot(0, 0, type = "n", xlim = c(1, 9.5), ylim = c(0, max_n + 2.2),
       xlab = "Age (years)", ylab = "Children", axes = FALSE)
  axis(1, at = 1:9, col.axis = muted)
  axis(2, at = 0:max_n, las = 1, col.axis = muted)
  abline(h = 0:max_n, col = "#F0F0F5")
  for (i in seq_along(xs)) {
    points(rep(xs[i], tab[i]), seq_len(tab[i]), pch = 21, bg = col,
           col = paper, lwd = 1, cex = 1.8)
  }
  abline(v = mean(values), col = warning, lwd = 4, lty = 2)
  abline(v = median(values), col = success, lwd = 4)
  title(main = heading, col.main = primary, font.main = 2, cex.main = 1.3)
  legend("topright", legend = c(sprintf("Mean %.2f", mean(values)),
                                sprintf("Median %.2f", median(values))),
         col = c(warning, success), lty = c(2, 1), lwd = 4,
         bty = "n", cex = 0.95)
}
draw_dotstack(bubbles, "Bubble booth (n = 20): long right tail", primary)
draw_dotstack(puppy, "Puppy booth (n = 18): symmetric", secondary)
mtext("Same-looking means can hide clinically different distributions",
      outer = TRUE, side = 3, line = 0.2, col = primary, font = 2, cex = 1.35)
dev.off()

# Figure 6: r and r-squared with exact synthetic correlation of 0.80.
open_png("q16_r_squared.png", 1700, 900)
n <- 28
x_std <- as.numeric(scale(seq_len(n)))
residual <- sin(seq_len(n) * 1.77) + cos(seq_len(n) * 0.83)
residual <- residual - mean(residual)
residual <- residual - x_std * sum(residual * x_std) / sum(x_std^2)
residual <- as.numeric(scale(residual))
y_std <- 0.8 * x_std + sqrt(1 - 0.8^2) * residual
r_observed <- cor(x_std, y_std)
layout(matrix(c(1, 2), nrow = 1), widths = c(1.4, 0.8))
par(mar = c(5, 5, 4, 2), oma = c(0, 0, 2, 0))
plot(x_std, y_std, pch = 21, bg = secondary, col = paper, cex = 1.55,
     xlab = "SOFA score (standardized)", ylab = "BNP level (standardized)",
     main = "Linear association")
grid(col = "#ECECF3")
points(x_std, y_std, pch = 21, bg = secondary, col = paper, cex = 1.55)
abline(lm(y_std ~ x_std), col = primary, lwd = 5)
text(-1.65, 1.85, sprintf("r = %.2f", r_observed), col = primary,
     font = 2, cex = 1.25, pos = 4)
barplot(c(64, 36), names.arg = c("Explained by\nlinear model", "Unexplained\nvariation"),
        col = c(primary, "#D9DAE4"), border = NA, ylim = c(0, 80),
        ylab = "Percent of variance", main = expression(r^2 == 0.64))
abline(h = seq(0, 80, 20), col = "#ECECF3")
text(c(0.7, 1.9), c(64, 36) + 5, labels = c("64%", "36%"),
     col = c(primary, muted), font = 2, cex = 1.25)
mtext("r = 0.80  ->  r² = 0.64; explained variance is not causal attribution",
      outer = TRUE, side = 3, line = 0.2, col = primary, font = 2, cex = 1.3)
dev.off()

# Figure 7: Synthetic run chart with a post-intervention shift.
open_png("q44_run_chart.png", 1700, 900)
week <- 1:24
wait <- c(79, 82, 76, 85, 78, 81, 77, 84, 80, 75, 83, 79,
          65, 61, 63, 58, 60, 56, 59, 54, 57, 52, 55, 51)
baseline_median <- median(wait[1:12])
plot(week, wait, type = "n", ylim = c(45, 90), xlim = c(1, 24),
     xlab = "Week", ylab = "Median wait time (minutes)", axes = FALSE)
axis(1, at = seq(1, 24, 2), col.axis = muted)
axis(2, at = seq(45, 90, 5), las = 1, col.axis = muted)
abline(h = seq(45, 90, 5), col = "#ECECF3")
rect(12.5, 45, 24.5, 90, col = "#EEF7F0", border = NA)
abline(h = baseline_median, col = primary, lwd = 4, lty = 2)
abline(v = 12.5, col = warning, lwd = 4, lty = 2)
lines(week, wait, col = ink, lwd = 4)
points(week[1:12], wait[1:12], pch = 21, bg = secondary, col = paper, lwd = 1.5, cex = 1.4)
points(week[13:24], wait[13:24], pch = 21, bg = success, col = paper, lwd = 1.5, cex = 1.4)
text(12.7, 88, "New process", col = warning, font = 2, pos = 4, cex = 1.05)
text(3, baseline_median + 2.3, sprintf("Baseline median = %.1f", baseline_median),
     col = primary, font = 2, pos = 4, cex = 1.0)
text(18.5, 70, "12 consecutive points\nbelow the baseline median", col = success,
     font = 2, cex = 1.05)
panel_title("A run chart reveals whether improvement is sustained",
            "Synthetic clinic data - the post-change shift is unlikely to be random variation")
box(col = "#D8DAE6")
dev.off()

cat("Generated figures:\n")
cat(paste0("- ", list.files(out_dir, pattern = "\\.png$")), sep = "\n")
