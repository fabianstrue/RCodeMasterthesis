library(AER)
library(dplyr)
library(haven)
setwd("C:/Users/fabia/Documents/#Köln/Uni/Masterarbeit/RCodeMasterthesis")
plot_dir <- file.path(getwd(), "plots")
# Evaluation profiles from "plot quantile grid.R"

# Helper Function
compute_ks_subgroup <- function(mask, endo_val, profile_result, thresholds) {
  # Empirical CDF for this subgroup and insurance status
  sub_y   <- model_df$y[mask & model_df$endo == endo_val]
  emp_cdf <- ecdf(sub_y)(thresholds)
  
  # Estimated CDFs from the profile
  if (endo_val == 1) {
    est_noIV <- profile_result$iso_ins_noIV$yf
    est_IV   <- profile_result$iso_ins_IV$yf
  } else {
    est_noIV <- profile_result$iso_un_noIV$yf
    est_IV   <- profile_result$iso_un_IV$yf
  }
  
  data.frame(
    threshold    = thresholds,
    dev_noIV     = abs(est_noIV - emp_cdf),
    dev_IV       = abs(est_IV   - emp_cdf),
    improvement  = abs(est_noIV - emp_cdf) - abs(est_IV - emp_cdf)
  )
}

plot_ks_improvement <- function(plot_foulder, file, subgroup_list, labels, title) {
  colors <- rainbow(length(subgroup_list))
  ylim   <- range(sapply(subgroup_list, function(d) d$improvement))
  
  pdf(file = file.path(plot_foulder, sub("\\.pdf$", paste0("_", first_stage_mode,".pdf"), file)),
      width = 9, height = 6)
  plot(thresholds, subgroup_list[[1]]$improvement,
       type = "l", lwd = 2, col = colors[1],
       xlab = "sqrt(Total Expenditure)",
       ylab = "KS Improvement (Non-IV minus IV)",
       main = title, ylim = ylim,
       cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
  abline(h = 0, lty = 3, col = "grey50")
  for (i in seq_along(subgroup_list)[-1]) {
    lines(thresholds, subgroup_list[[i]]$improvement, lwd = 2, col = colors[i])
  }
  legend("topright", labels, lwd = 2, col = colors)
  dev.off()
}

plot_cdf_empirical <- function(plot_foulder, file, x, y_noIV, y_IV, y_emp, title,
                               ylab = "Estimated CDF", ylim = c(0, 1)) {
  pdf(file = file.path(plot_foulder, sub("\\.pdf$", paste0("_", first_stage_mode,".pdf"), file)),
      width = 9, height = 6)
  plot(x, y_emp, lwd = 2, type = "l", col = "grey50", lty = 3,
       xlab = "sqrt(Total Expenditure)", ylab = ylab, ylim = ylim, main = title,
       cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
  lines(x, y_IV,    lwd = 2, col = "darkred")
  lines(x, y_noIV,  lwd = 2, col = "black")
  legend("bottomright",
         c("Empirical CDF", "Non-IV", "IV (ASF)"),
         lwd = 2, lty = c(3, 1, 1),
         col = c("grey50", "black", "darkred"))
  dev.off()
}

# ── Define subgroup masks ─────────────────────────────────────────────────────
mask_all      <- rep(TRUE, nrow(model_df))
mask_low_inc  <- model_df$INCTOT <= quantile(model_df$INCTOT, 0.25)
mask_mid_inc  <- model_df$INCTOT >  quantile(model_df$INCTOT, 0.375) &
  model_df$INCTOT <= quantile(model_df$INCTOT, 0.625)
mask_high_inc <- model_df$INCTOT >= quantile(model_df$INCTOT, 0.75)
mask_female   <- model_df$SEX == 0
mask_male     <- model_df$SEX == 1
mask_young    <- model_df$AGE <= quantile(model_df$AGE, 0.25)
mask_midage  <- model_df$AGE >  quantile(model_df$AGE, 0.375) &
  model_df$AGE <= quantile(model_df$AGE, 0.625)
mask_old      <- model_df$AGE >= quantile(model_df$AGE, 0.75)

mask_young_male   <- model_df$AGE <= quantile(model_df$AGE, 0.25) &
            model_df$SEX == 1
mask_midage_male  <- model_df$AGE >  quantile(model_df$AGE, 0.375) &
            model_df$AGE <= quantile(model_df$AGE, 0.625) & model_df$SEX == 1
mask_old_male     <- model_df$AGE >= quantile(model_df$AGE, 0.75) &
            model_df$SEX == 1
mask_young_female <- model_df$AGE <= quantile(model_df$AGE, 0.25) &
            model_df$SEX == 0
mask_midage_female<- model_df$AGE >  quantile(model_df$AGE, 0.375) &
            model_df$AGE <= quantile(model_df$AGE, 0.625) & model_df$SEX == 0
mask_old_female   <- model_df$AGE >= quantile(model_df$AGE, 0.75) &
            model_df$SEX == 0

# ── Compute KS improvement for each subgroup ───────────────────────────────────
ks_sub <- list(
  # Income
  low_inc_ins   = compute_ks_subgroup(mask_low_inc,      1, res_inc[["Low income (p10)"]],    thresholds),
  mid_inc_ins   = compute_ks_subgroup(mask_mid_inc,      1, res_inc[["Middle income (p50)"]], thresholds),
  high_inc_ins  = compute_ks_subgroup(mask_high_inc,     1, res_inc[["High income (p90)"]],   thresholds),
  low_inc_un    = compute_ks_subgroup(mask_low_inc,      0, res_inc[["Low income (p10)"]],    thresholds),
  mid_inc_un    = compute_ks_subgroup(mask_mid_inc,      0, res_inc[["Middle income (p50)"]], thresholds),
  high_inc_un   = compute_ks_subgroup(mask_high_inc,     0, res_inc[["High income (p90)"]],   thresholds),
  # Sex
  female_ins    = compute_ks_subgroup(mask_female,       1, res_sex[["Female"]],              thresholds),
  male_ins      = compute_ks_subgroup(mask_male,         1, res_sex[["Male"]],                thresholds),
  female_un     = compute_ks_subgroup(mask_female,       0, res_sex[["Female"]],              thresholds),
  male_un       = compute_ks_subgroup(mask_male,         0, res_sex[["Male"]],                thresholds),
  # Age
  young_ins     = compute_ks_subgroup(mask_young,        1, res_age[["Young (AGE p10)"]],      thresholds),
  midage_ins    = compute_ks_subgroup(mask_midage,       1, res_age[["Middle-aged (AGE p50)"]], thresholds),
  old_ins       = compute_ks_subgroup(mask_old,          1, res_age[["Older (AGE p90)"]],       thresholds),
  young_un      = compute_ks_subgroup(mask_young,        0, res_age[["Young (AGE p10)"]],       thresholds),
  midage_un     = compute_ks_subgroup(mask_midage,       0, res_age[["Middle-aged (AGE p50)"]], thresholds),
  old_un        = compute_ks_subgroup(mask_old,          0, res_age[["Older (AGE p90)"]],       thresholds),
  # Age x Sex combined
  young_male_ins    = compute_ks_subgroup(mask_young_male,   1, res_combined[["Young, Male"]],    thresholds),
  midage_male_ins   = compute_ks_subgroup(mask_midage_male,  1, res_combined[["Middle, Male"]],   thresholds),
  old_male_ins      = compute_ks_subgroup(mask_old_male,     1, res_combined[["Older, Male"]],    thresholds),
  young_female_ins  = compute_ks_subgroup(mask_young_female, 1, res_combined[["Young, Female"]],  thresholds),
  midage_female_ins = compute_ks_subgroup(mask_midage_female,1, res_combined[["Middle, Female"]], thresholds),
  old_female_ins    = compute_ks_subgroup(mask_old_female,   1, res_combined[["Older, Female"]],  thresholds),
  young_male_un     = compute_ks_subgroup(mask_young_male,   0, res_combined[["Young, Male"]],    thresholds),
  midage_male_un    = compute_ks_subgroup(mask_midage_male,  0, res_combined[["Middle, Male"]],   thresholds),
  old_male_un       = compute_ks_subgroup(mask_old_male,     0, res_combined[["Older, Male"]],    thresholds),
  young_female_un   = compute_ks_subgroup(mask_young_female, 0, res_combined[["Young, Female"]],  thresholds),
  midage_female_un  = compute_ks_subgroup(mask_midage_female,0, res_combined[["Middle, Female"]], thresholds),
  old_female_un     = compute_ks_subgroup(mask_old_female,   0, res_combined[["Older, Female"]],  thresholds)
)

# ── Compute KS improvement for full population ────────────────────────────────
emp_all_ins <- ecdf(model_df$y[model_df$endo == 1])(thresholds)
emp_all_un  <- ecdf(model_df$y[model_df$endo == 0])(thresholds)

# Add directly to ks_sub
ks_sub$all_ins <- data.frame(
  threshold   = thresholds,
  dev_noIV    = abs(iso_ins_noIV$yf - emp_all_ins),
  dev_IV      = abs(iso_ins_IV$yf   - emp_all_ins),
  improvement = abs(iso_ins_noIV$yf - emp_all_ins) - abs(iso_ins_IV$yf - emp_all_ins)
)

ks_sub$all_un <- data.frame(
  threshold   = thresholds,
  dev_noIV    = abs(iso_un_noIV$yf - emp_all_un),
  dev_IV      = abs(iso_un_IV$yf   - emp_all_un),
  improvement = abs(iso_un_noIV$yf - emp_all_un) - abs(iso_un_IV$yf - emp_all_un)
)

# ── Aggregate KS table ────────────────────────────────────────────────────────
suppressWarnings(rm(ks_sub_table))
# Anderson-Darling weights deviations more heavily in the tails.
# Anderson-Darling is trimmed to F in (0.05, 0.95) to avoid boundary instability
# where the empirical CDF reaches 0 or 1 and weights explode.

ks_sub_table <- do.call(rbind, lapply(names(ks_sub), function(nm) {
  d <- ks_sub[[nm]]
  
  # Determine insurance status from name
  endo_val <- if (grepl("_ins$", nm) | nm == "all_ins") 1 else 0

  # Determine correct mask
  mask_nm <- sub("_ins$|_un$", "", nm)
  mask <- switch(mask_nm,
                 low_inc        = mask_low_inc,
                 mid_inc        = mask_mid_inc,
                 high_inc       = mask_high_inc,
                 female         = mask_female,
                 male           = mask_male,
                 young          = mask_young,
                 midage         = mask_midage,
                 old            = mask_old,
                 young_male     = mask_young_male,
                 midage_male    = mask_midage_male,
                 old_male       = mask_old_male,
                 young_female   = mask_young_female,
                 midage_female  = mask_midage_female,
                 old_female     = mask_old_female,
                 all            = mask_all,
                 rep(TRUE, nrow(model_df))
  )
  
  emp_vals <- ecdf(model_df$y[mask & model_df$endo == endo_val])(thresholds)
  
  # Anderson-Darling weights — trimmed to interior of distribution
  interior    <- emp_vals > 0.05 & emp_vals < 0.95
  w           <- rep(0, length(emp_vals))
  w[interior] <- 1 / (emp_vals[interior] * (1 - emp_vals[interior]))
  
  data.frame(
    Subgroup        = nm,
    KS_noIV         = round(max(d$dev_noIV),                       4),
    KS_IV           = round(max(d$dev_IV),                         4),
    KS_improvement  = round(max(d$dev_noIV) - max(d$dev_IV),       4),
    MISE_noIV        = round(mean(d$dev_noIV^2),                    6),
    MISE_IV          = round(mean(d$dev_IV^2),                      6),
    MISE_improvement = round(mean(d$dev_noIV^2) - mean(d$dev_IV^2), 6),
    AD_noIV         = round(mean(w * d$dev_noIV^2),                        6),
    AD_IV           = round(mean(w * d$dev_IV^2),                          6),
    AD_improvement  = round(mean(w * d$dev_noIV^2) - mean(w * d$dev_IV^2), 6)
    
  )
}))

# Add mean row
ks_sub_table <- rbind(
  ks_sub_table,
  data.frame(
    Subgroup        = "Mean",
    KS_noIV         = round(mean(ks_sub_table$KS_noIV),         4),
    KS_IV           = round(mean(ks_sub_table$KS_IV),           4),
    KS_improvement  = round(mean(ks_sub_table$KS_improvement),  4),
    MISE_noIV        = round(mean(ks_sub_table$MISE_noIV),        6),
    MISE_IV          = round(mean(ks_sub_table$MISE_IV),          6),
    MISE_improvement = round(mean(ks_sub_table$MISE_improvement), 6),
    AD_noIV         = round(mean(ks_sub_table$AD_noIV),         6),
    AD_IV           = round(mean(ks_sub_table$AD_IV),           6),
    AD_improvement  = round(mean(ks_sub_table$AD_improvement),  6)
  )
)
print(ks_sub_table)

# ── Pointwise improvement plots ────────────────────────────────────────────────
# Age — insured and uninsured (now with midage)
plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_age_insured.pdf",
                    list(ks_sub$young_ins, ks_sub$midage_ins, ks_sub$old_ins),
                    c("Young (p10)", "Middle-aged (p50)", "Older (p90)"),
                    "KS Improvement from IV — Insured, by Age")

plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_age_uninsured.pdf",
                    list(ks_sub$young_un, ks_sub$midage_un, ks_sub$old_un),
                    c("Young (p10)", "Middle-aged (p50)", "Older (p90)"),
                    "KS Improvement from IV — Uninsured, by Age")

# Income — insured and uninsured
plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_income_insured.pdf",
                    list(ks_sub$low_inc_ins, ks_sub$mid_inc_ins, ks_sub$high_inc_ins),
                    c("Low income", "Middle income", "High income"),
                    "KS Improvement from IV — Insured, by Income")

plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_income_uninsured.pdf",
                    list(ks_sub$low_inc_un, ks_sub$mid_inc_un, ks_sub$high_inc_un),
                    c("Low income", "Middle income", "High income"),
                    "KS Improvement from IV — Uninsured, by Income")

# Sex — insured and uninsured
plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_sex_insured.pdf",
                    list(ks_sub$female_ins, ks_sub$male_ins),
                    c("Female", "Male"),
                    "KS Improvement from IV — Insured, by Sex")

plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_sex_uninsured.pdf",
                    list(ks_sub$female_un, ks_sub$male_un),
                    c("Female", "Male"),
                    "KS Improvement from IV — Uninsured, by Sex")

# Combined Age x Sex — insured
plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_combined_insured_male.pdf",
                    list(ks_sub$young_male_ins, ks_sub$midage_male_ins, ks_sub$old_male_ins),
                    c("Young Male", "Middle-aged Male", "Older Male"),
                    "KS Improvement from IV — Insured, Male by Age")

plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_combined_insured_female.pdf",
                    list(ks_sub$young_female_ins, ks_sub$midage_female_ins, ks_sub$old_female_ins),
                    c("Young Female", "Middle-aged Female", "Older Female"),
                    "KS Improvement from IV — Insured, Female by Age")

# Combined Age x Sex — uninsured
plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_combined_uninsured_male.pdf",
                    list(ks_sub$young_male_un, ks_sub$midage_male_un, ks_sub$old_male_un),
                    c("Young Male", "Middle-aged Male", "Older Male"),
                    "KS Improvement from IV — Uninsured, Male by Age")

plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_combined_uninsured_female.pdf",
                    list(ks_sub$young_female_un, ks_sub$midage_female_un, ks_sub$old_female_un),
                    c("Young Female", "Middle-aged Female", "Older Female"),
                    "KS Improvement from IV — Uninsured, Female by Age")

# Full population — insured and uninsured
plot_ks_improvement(plot_dir, "Fit/KS/KS_improvement_full_pop.pdf",
                    list(ks_sub$all_ins, ks_sub$all_un),
                    c("Insured", "Uninsured"),
                    "KS Improvement from IV")

# ── Compare Estimated with Emperical CDF ──────────────────────────────────────

# Insured
plot_cdf_empirical(plot_dir, "CDF/Plot_CDF_comparison_insured.pdf",
                   thresholds,
                   iso_ins_noIV$yf,
                   iso_ins_IV$yf,
                   emp_all_ins,
                   "Conditional CDF Comparison — Insured")

# Uninsured
plot_cdf_empirical(plot_dir, "CDF/Plot_CDF_comparison_uninsured.pdf",
                   thresholds,
                   iso_un_noIV$yf,
                   iso_un_IV$yf,
                   emp_all_un,
                   "Conditional CDF Comparison — Uninsured")

