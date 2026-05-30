library(tidyverse)
library(ggplot2)
library(ggrastr)
library(poweRlaw)

setwd("C:/Users/fabia/Documents/#Köln/Uni/Masterarbeit/RCodeMasterthesis")
plot_dir <- file.path(getwd(), "plots/Descriptives")

df_des = regular

# Analyse the distribution of expenditures
## Share of 0 spenders
zero_spenders = sum(df_des$EXPTOT == 0)
zero_spenders/nrow(df_des)
## Table of most common Spending amounts
head(sort(table(df_des$EXPTOT), decreasing = TRUE), 25)
## Create a log-rank-log plot to check if the distribution could be Pareto 
df_pareto = tibble(expenses = df_des$EXPTOT, insurance = df_des$INSSHR) %>%
  filter(expenses > 0) %>%
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

p1 <- ggplot(df_pareto, aes(x=log_expenses, y=log_rank, colour=insurance)) +
  geom_point(alpha = 0.3, size = 0.5) +
  labs(x="log Spending", y="log Rank", colour="Insurance Status",
       title="Log-Rank-Log Plot of Medical Spending") +
  theme_minimal()
# Save as high-res PNG
ggsave(file.path(plot_dir, "pareto_full.png"), plot = p1, width = 6, height = 4, dpi = 300)
rm(p1)

# Neither curve shows a linear segment in the log-rank-log-size plot, suggesting the distribution is not clearly Pareto — more consistent with log-normal behavior
# Both curves remain concave throughout, meaning OLS-based inference on the conditional mean is likely insufficient to capture the full distributional differences between groups
# Zero-expenditure observations recoded to $1 prior to log transformation to preserve non-utilisers in the plot
# Uninsured individuals show a pronounced mass at x=0, indicating a higher rate of non-utilization (extensive margin)
# Insured individuals have substantially higher maximum spending, with the tail extending further right
# The conditional spending distribution of insured individuals is shifted rightward, suggesting higher spending across the whole distribution
# The uninsured tail drops more steeply beyond log spending ≈8, indicating a thinner right tail — consistent with financial barriers preventing catastrophically expensive care
# COVID-19 years (2020–2022) may inflate the zero-spending mass due to deferred care — pooled results should be interpreted with this in mind
# The uninsured sample conditions on positive spenders disproportionately, as zeros are more prevalent among the uninsured — tail comparisons should be interpreted carefully

p2 <- ggplot(df_pareto, aes(x=log_expenses, y=log_rank, colour =insurance)) +
  geom_point(alpha = 0.3, size = 0.5) +
  labs(x="log Spending", y="log Rank", colour="Insurance Status",
       title="Log-Rank-Log Plot of Medical Spending") +
  theme_minimal() + 
  xlim(8, NA)
# Save as high-res PNG
ggsave(file.path(plot_dir, "pareto_crop.png"), plot = p2, width = 6, height = 4, dpi = 300)
rm(p2)

# Although the tail begins to look almost straight in the end
df_pareto_tail = df_des %>%
  mutate(EXPTOT=ifelse(EXPTOT==0, 1, EXPTOT)) %>%
  select(EXPTOT)

m <- conpl$new(df_pareto_tail$EXPTOT)
est <- estimate_xmin(m, xmax=max(df_pareto_tail$EXPTOT))
m$setXmin(est)
estimate_pars(m)

              