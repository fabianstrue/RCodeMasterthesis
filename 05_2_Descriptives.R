library(tidyverse)
library(ggplot2)
library(ggrastr)
library(poweRlaw)
library(dplyr)

setwd("C:/Users/fabia/Documents/#Köln/Uni/Masterarbeit/RCodeMasterthesis")
plot_dir <- file.path(getwd(), "plots")

df_des = regular

# Analyse the distribution of expenditures

## Weighted hare of 0 spenders
weighted.mean(df_des$EXPTOT == 0, df_des$PERWEIGHT)
# Among the insured and uninsured
bind_rows(
  df_des %>%
  group_by(INSSHR = as.character(INSSHR)) %>%
  summarise(zero_share = weighted.mean(EXPTOT == 0, PERWEIGHT), n = n()),
  
  df_des %>%
    summarise(INSSHR = "Total",
              zero_share = weighted.mean(EXPTOT == 0, PERWEIGHT), n = n())
  )

## Table of most common Spending amounts
head(sort(table(df_des$EXPTOT), decreasing = TRUE), 25)

## Create a log-rank-log plot to check if the distribution could be Pareto 
df_pareto = tibble(expenses = df_des$EXPTOT, insurance = df_des$INSSHR) %>%
  mutate(expenses = if_else(expenses == 0, 1, expenses)) %>%
  group_by(insurance) %>%
  arrange(desc(expenses), .by_group = TRUE) %>%
  mutate(
    rank = row_number(),
    log_expenses = log(expenses),
    log_rank = log(rank)
  ) %>%
  ungroup() %>%
  mutate(
    insurance = factor(
      insurance,
      levels = c(0, 1),
      labels = c("Uninsured", "Insured")
    )
  )

ggplot(df_pareto, aes(x=log_expenses, y=log_rank, colour=insurance)) +
  geom_point(alpha = 0.3, size = 0.5) +
  labs(x="log Spending", y="log Rank", colour="Insurance Status",
       title="Log-Rank-Log Plot of Medical Spending") +
  theme_minimal()
ggsave(file.path(plot_dir, "Descriptives/pareto_full.pdf"), width = 6, height = 4)

# Neither curve shows a linear segment in the log-rank-log-size plot, suggesting the distribution is not clearly Pareto — more consistent with log-normal behavior
# Both curves remain concave throughout, meaning OLS-based inference on the conditional mean is likely insufficient to capture the full distributional differences between groups
# Zero-expenditure observations recoded to $1 prior to log transformation to preserve non-utilisers in the plot
# Uninsured individuals show a pronounced mass at x=0, indicating a higher rate of non-utilization (extensive margin)
# Insured individuals have substantially higher maximum spending, with the tail extending further right
# The conditional spending distribution of insured individuals is shifted rightward, suggesting higher spending

ggplot(df_pareto, aes(x=log_expenses, y=log_rank, colour =insurance)) +
  geom_point(alpha = 0.3, size = 0.5) +
  labs(x="log Spending", y="log Rank", colour="Insurance Status",
       title="Log-Rank-Log Plot of Medical Spending") +
  theme_minimal() + 
  xlim(8, NA)
# Save as high-res PNG
ggsave(file.path(plot_dir, "Descriptives/pareto_crop.pdf"), width = 6, height = 4, dpi = 300)


# Although the tail begins to look almost straight in the end
df_pareto_tail = df_des %>%
  mutate(EXPTOT=ifelse(EXPTOT==0, 1, EXPTOT)) %>%
  select(EXPTOT)

m <- conpl$new(df_pareto_tail$EXPTOT)
est <- estimate_xmin(m, xmax=max(df_pareto_tail$EXPTOT))
m$setXmin(est)
estimate_pars(m)

## Create spending plots

# Cumulative spending share held by the top `cutoffs` of the pop
concentration_shares <- function(x, w = NULL,
                                 cutoffs = c(0.01, 0.05, 0.10, 0.25, 0.50)) {
  if (is.null(w)) w <- rep(1, length(x))
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  o <- order(x, decreasing = TRUE)
  x <- x[o]; w <- w[o]
  cum_pop   <- cumsum(w)     / sum(w)
  cum_spend <- cumsum(w * x) / sum(w * x)
  idx    <- vapply(cutoffs, function(c) which(cum_pop >= c)[1], integer(1))
  shares <- cum_spend[idx]
  means  <- vapply(idx, function(j) sum(w[1:j] * x[1:j]) / sum(w[1:j]), numeric(1))
  list(shares = shares, means = means)
}

plot_concentration  <- function(x, w = NULL,
                                 cutoffs = c(0.01, 0.05, 0.10, 0.25, 0.50),
                                 main   = "",
                                 labels = c("Population", "Health\nexpenditures"),
                                 cols   = c("#08306b","#2171b5","#4292c6",
                                            "#6baed6","#9ecae1","#deebf7"),
                                 legend = FALSE) {
  cs   <- concentration_shares(x, w, cutoffs)
  sh   <- cs$shares; mn <- cs$means
  popb <- c(0, cutoffs, 1) * 100
  spnb <- c(0, sh,      1) * 100
  nseg <- length(popb) - 1
  xl <- 1.0; xr <- 2.2; bw <- 0.3
  
  plot(NA, xlim = c(0.4, 3.2), ylim = c(0, 100),
       xlab = "", ylab = "Percentage", axes = FALSE, main = "")
  axis(2, seq(0, 100, 10), paste0(seq(0, 100, 10), "%"), las = 1)
  axis(1, c(xl, xr), labels, tick = FALSE)
  
  for (i in seq_along(popb))
    segments(xl + bw, popb[i], xr - bw, spnb[i], col = "gray50", lwd = 0.7, lty = 2)
  for (i in seq_len(nseg)) {
    rect(xl - bw, popb[i], xl + bw, popb[i + 1], col = cols[i], border = "gray50")
    rect(xr - bw, spnb[i], xr + bw, spnb[i + 1], col = cols[i], border = "gray50")
  }
  mtext(main, side = 3, line = 1, at = (xl + xr) / 2, cex = 0.9, font = 2)
  text(xl - bw + 0.01, cutoffs * 100, sprintf("%g%%", cutoffs * 100),
       pos = 2, cex = .7)
  text(xr, sh * 100-2, sprintf("%.1f%%", sh * 100), cex = .7, col="white")     # shares, in right bar
  text(xr + bw - 0.01, sh * 100-2,      # dollars, top-right of segment
  #mids <- (spnb[-length(spnb)] + spnb[-1]) / 2          # segment centers
  #text(xr + bw - 0.01, mids[seq_along(mn)],             # dollars, mid-segment
       sprintf("$%s", formatC(round(mn), format = "d", big.mark = ",")),
       pos = 4, cex = .7)
  if (legend)
    legend("bottom", bty = "n", cex = .75, fill = cols,
           legend = c("Top 1%","Top 5%","Top 10%","Top 25%","Top 50%","Bottom 50%"))
  invisible(setNames(sh, paste0("top", cutoffs * 100)))
}

d <- regular %>%
  select(EXPTOT, PERWEIGHT, INSSHR)

pdf(file.path(plot_dir, "Descriptives/concentration_ABC.pdf"), width = 12, height = 5.)   # = \textwidth
par(mfrow = c(1, 3), mar = c(3, 4, 4, 0.25))
plot_concentration(d$EXPTOT,                d$PERWEIGHT,                main = "All")
plot_concentration(d$EXPTOT[d$INSSHR == 1], d$PERWEIGHT[d$INSSHR == 1], main = "Insured")
plot_concentration(d$EXPTOT[d$INSSHR == 0], d$PERWEIGHT[d$INSSHR == 0], main = "Uninsured")
dev.off()

pdf(file.path(plot_dir, "Descriptives/concentration_A.pdf"), width = 4, height = 5.)   # = \textwidth
par(mfrow = c(1, 1), mar = c(3, 4, 4, 1))
plot_concentration(d$EXPTOT,                d$PERWEIGHT,                main = "All")
dev.off()

pdf(file.path(plot_dir, "Descriptives/concentration_B.pdf"), width = 4, height = 5.)   # = \textwidth
par(mfrow = c(1, 1), mar = c(3, 4, 4, 1))
plot_concentration(d$EXPTOT[d$INSSHR == 1], d$PERWEIGHT[d$INSSHR == 1], main = "Insured")
dev.off()

pdf(file.path(plot_dir, "Descriptives/concentration_C.pdf"), width = 4, height = 5.)   # = \textwidth
par(mfrow = c(1, 1), mar = c(3, 4, 4, 1))
plot_concentration(d$EXPTOT[d$INSSHR == 0], d$PERWEIGHT[d$INSSHR == 0], main = "Uninsured")
dev.off()

pdf(file.path(plot_dir, "Descriptives/concentration_BC.pdf"), width = 8, height = 5.)   # = \textwidth
par(mfrow = c(1, 2), mar = c(3, 4, 4, 0.25))
plot_concentration(d$EXPTOT[d$INSSHR == 1], d$PERWEIGHT[d$INSSHR == 1], main = "Insured")
plot_concentration(d$EXPTOT[d$INSSHR == 0], d$PERWEIGHT[d$INSSHR == 0], main = "Uninsured")
dev.off()

rm(d)
