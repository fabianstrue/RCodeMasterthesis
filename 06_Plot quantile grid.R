library(AER)
library(dplyr)
library(haven)
setwd("C:/Users/fabia/Documents/#Köln/Uni/Masterarbeit/RCodeMasterthesis")
plot_dir <- file.path(getwd(), "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir)

# ── Build evaluation profiles ─────────────────────────────────────────────────
q_sex = c(0, 1)
inc = zap_labels(model_df$INCTOT)
q_inc = quantile(inc, probs = c(0.1, 0.5, 0.9))
age = zap_labels(model_df$AGE)
q_age = quantile(age, probs = c(0.1, 0.5, 0.9))
# Fix all other controls at their sample means
eval_means <- colMeans(model_df[, controls, drop = FALSE])


# Helper function: create an eval_means vector with some values overridden
make_profile <- function(base, overrides) {
  profile <- base
  for (nm in names(overrides)) {
    profile[nm] <- overrides[[nm]]
  }
  return(profile)
}

# Profile set B: Vary INCTOT (low / mid / high), fix everything else at means
profiles_inc <- list(
  list(label = "Low income (p10)",    overrides = list(INCTOT = q_inc[1])),
  list(label = "Middle income (p50)", overrides = list(INCTOT = q_inc[2])),
  list(label = "High income (p90)",   overrides = list(INCTOT = q_inc[3]))
)

# Profile set A: Vary AGE (low / mid / high), fix everything else at means
profiles_age <- list(
  list(label = "Young (AGE p10)",      overrides = list(AGE = q_age[1])),
  list(label = "Middle-aged (AGE p50)", overrides = list(AGE = q_age[2])),
  list(label = "Older (AGE p90)",       overrides = list(AGE = q_age[3]))
)

# Profile set C: Vary SEX, fix everything else at means
profiles_sex <- list(
  list(label = "Female", overrides = list(SEX = 0)),
  list(label = "Male",   overrides = list(SEX = 1))
)

# Profile set D: Combined AGE x SEX (3x =  profiles)
profiles_combined <- list(
  list(label = "Young, Male",  overrides = list(AGE = q_age[1], SEX = 1)),
  list(label = "Middle, Male", overrides = list(AGE = q_age[2], SEX = 1)),
  list(label = "Older, Male", overrides = list(AGE = q_age[3], SEX = 1)),
  list(label = "Young, Female",  overrides = list(AGE = q_age[1], SEX = 0)),
  list(label = "Middle, Female", overrides = list(AGE = q_age[2], SEX = 0)),
  list(label = "Older, Female", overrides = list(AGE = q_age[3], SEX = 0))
)

# ── Compute CDFs for each profile set ─────────────────────────────────────────

compute_profile_cdfs <- function(profiles, base_means, betahatsnonIV, betahatsIV,
                                 Y2hat, n_ctrl, thresholds) {
  results <- list()
  for (p in profiles) {
    eval_pt <- make_profile(base_means, p$overrides)
    
    cdf_un  <- compute_cdf(0, betahatsnonIV, betahatsIV, eval_pt, Y2hat, n_ctrl, model_df[[weight_var]])
    cdf_ins <- compute_cdf(1, betahatsnonIV, betahatsIV, eval_pt, Y2hat, n_ctrl, model_df[[weight_var]])
    
    # Isotonic regression for monotonicity
    iso_un_noIV  <- isoreg(thresholds, cdf_un$nonIV)
    iso_un_IV    <- isoreg(thresholds, cdf_un$IV)
    iso_ins_noIV <- isoreg(thresholds, cdf_ins$nonIV)
    iso_ins_IV   <- isoreg(thresholds, cdf_ins$IV)
    
    results[[p$label]] <- list(
      label       = p$label,
      eval_pt     = eval_pt,
      cdf_un      = cdf_un,
      cdf_ins     = cdf_ins,
      iso_un_noIV  = iso_un_noIV,
      iso_un_IV    = iso_un_IV,
      iso_ins_noIV = iso_ins_noIV,
      iso_ins_IV   = iso_ins_IV
    )
    cat("Computed CDFs for profile:", p$label, "\n")
  }
  return(results)
}

cat("\n=== Computing CDFs for AGE profiles ===\n")
res_age <- compute_profile_cdfs(profiles_age, eval_means, betahatsnonIV, betahatsIV,
                                Y2hat, n_ctrl, thresholds)

cat("\n=== Computing CDFs for INCOME profiles ===\n")
res_inc <- compute_profile_cdfs(profiles_inc, eval_means, betahatsnonIV, betahatsIV,
                                Y2hat, n_ctrl, thresholds)

cat("\n=== Computing CDFs for SEX profiles ===\n")
res_sex <- compute_profile_cdfs(profiles_sex, eval_means, betahatsnonIV, betahatsIV,
                                Y2hat, n_ctrl, thresholds)

cat("\n=== Computing CDFs for COMBINED profiles ===\n")
res_combined <- compute_profile_cdfs(profiles_combined, eval_means, betahatsnonIV, betahatsIV,
                                     Y2hat, n_ctrl, thresholds)

# ── 5. Plotting functions for profile comparisons ─────────────────────────────

# Plot A: One panel per profile, showing insured vs uninsured (like Wied Fig 6.1)
plot_profile_cdf <- function(file, res, thresholds, iv = TRUE, main_prefix = "") {
  n_profiles <- length(res)
  
  pdf(file = file.path(plot_dir, sub("\\.pdf$", paste0("_", first_stage_mode,".pdf"), file)),
      width = 9, height = 5 * n_profiles)
  par(mfrow = c(n_profiles, 1), mar = c(5, 5, 4, 2))
  
  for (r in res) {
    if (iv) {
      y_ins <- r$iso_ins_IV$yf
      y_un  <- r$iso_un_IV$yf
      suffix <- "IV (ASF)"
    } else {
      y_ins <- r$iso_ins_noIV$yf
      y_un  <- r$iso_un_noIV$yf
      suffix <- "No IV"
    }
    
    plot(thresholds, y_ins, lwd = 2, type = "l", col = "darkred",
         xlab = "sqrt(Total Expenditure)", ylab = "Estimated CDF",
         ylim = c(0, 1),
         main = paste0(main_prefix, r$label, " — ", suffix),
         cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.3)
    lines(thresholds, y_un, lwd = 2, col = "black")
    legend("bottomright", c("Uninsured", "Insured"),
           lwd = 2, col = c("black", "darkred"))
  }
  dev.off()
}

# Plot B: Difference plots per profile (like Wied Fig 6.3)
plot_profile_diff <- function(file, res, thresholds, iv = TRUE,
                              main_prefix = "", ylim = c(-0.5, 0.3)) {
  n_profiles <- length(res)
  
  pdf(file = file.path(plot_dir, sub("\\.pdf$", paste0("_", first_stage_mode,".pdf"), file)),
      width = 9, height = 5 * n_profiles)
  par(mfrow = c(n_profiles, 1), mar = c(5, 5, 4, 2))
  
  for (r in res) {
    if (iv) {
      diff <- r$iso_ins_IV$yf - r$iso_un_IV$yf
      suffix <- "IV (ASF)"
    } else {
      diff <- r$iso_ins_noIV$yf - r$iso_un_noIV$yf
      suffix <- "No IV"
    }
    
    plot(thresholds, diff, lwd = 2, type = "l",
         xlab = "sqrt(Total Expenditure)",
         ylab = "Difference in Estimated CDF",
         ylim = ylim,
         main = paste0(main_prefix, "Difference: ", r$label, " — ", suffix),
         cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.3)
    abline(h = 0, lty = 3, col = "grey50")
  }
  dev.off()
}

# Plot C: Overlay profiles on one plot (compare how CDF shifts across groups)
plot_profiles_overlay <- function(file, res, thresholds, iv = TRUE, insured = TRUE,
                                  main = "", colors = NULL) {
  n_profiles <- length(res)
  if (is.null(colors)) colors <- rainbow(n_profiles)
  
  pdf(file = file.path(plot_dir, sub("\\.pdf$", paste0("_", first_stage_mode,".pdf"), file)),
      width = 9, height = 6)
  
  first <- TRUE
  for (i in seq_along(res)) {
    r <- res[[i]]
    if (iv && insured)       y <- r$iso_ins_IV$yf
    else if (iv && !insured) y <- r$iso_un_IV$yf
    else if (!iv && insured) y <- r$iso_ins_noIV$yf
    else                     y <- r$iso_un_noIV$yf
    
    if (first) {
      plot(thresholds, y, lwd = 2, type = "l", col = colors[i],
           xlab = "sqrt(Total Expenditure)", ylab = "Estimated CDF",
           ylim = c(0, 1), main = main,
           cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.3)
      first <- FALSE
    } else {
      lines(thresholds, y, lwd = 2, col = colors[i])
    }
  }
  
  legend("bottomright", sapply(res, function(r) r$label),
         lwd = 2, col = colors[1:n_profiles])
  dev.off()
}

# Plot D: Overlay IV and Non-IV on one plot (compare how CDF shifts)
plot_iv_comparison <- function(file, res, thresholds, insured = TRUE,
                               main_prefix = "") {
  n_profiles <- length(res)
  ins_label <- if (insured) "Insured" else "Uninsured"
  
  pdf(file = file.path(plot_dir, sub("\\.pdf$", paste0("_", first_stage_mode,".pdf"), file)),
      width = 9, height = 5 * n_profiles)
  par(mfrow = c(n_profiles, 1), mar = c(5, 5, 4, 2))
  
  for (r in res) {
    if (insured) {
      y_iv   <- r$iso_ins_IV$yf
      y_noiv <- r$iso_ins_noIV$yf
    } else {
      y_iv   <- r$iso_un_IV$yf
      y_noiv <- r$iso_un_noIV$yf
    }
    
    plot(thresholds, y_noiv, lwd = 2, type = "l", col = "black",
         xlab = "sqrt(Total Expenditure)", ylab = "Estimated CDF",
         ylim = c(0, 1),
         main = paste0(main_prefix, r$label, " — ", ins_label),
         cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.3)
    lines(thresholds, y_iv, lwd = 2, col = "darkred")
    legend("bottomright", c("Without IV", "With IV (ASF)"),
           lwd = 2, col = c("black", "darkred"))
  }
  dev.off()
}

# Plot D2: Compare IV and Non-IV on one plot with all variables fixed at the mean
plot_iv_comparison_single <- function(file, iso_iv, iso_noiv, thresholds,
                                      main = "") {
  pdf(file = file.path(plot_dir, sub("\\.pdf$", paste0("_", first_stage_mode,".pdf"), file)),
      width = 9, height = 6)
  plot(thresholds, iso_noiv$yf, lwd = 2, type = "l", col = "black",
       xlab = "sqrt(Total Expenditure)", ylab = "Estimated CDF",
       ylim = c(0, 1), main = main,
       cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
  lines(thresholds, iso_iv$yf, lwd = 2, col = "darkred")
  legend("bottomright", c("Without IV", "With IV (ASF)"),
         lwd = 2, col = c("black", "darkred"))
  dev.off()
}

# ── Generate all plots ────────────────────────────────────────────────────────

plot_iv_comparison_single("IVcomp_Insured.pdf",
                          iso_ins_IV, iso_ins_noIV, thresholds,
                          "Insured: IV vs Non-IV")

plot_iv_comparison_single(paste0("IVcomp_Uninsured_", first_stage_mode, ".pdf"),
                          iso_un_IV, iso_un_noIV, thresholds,
                          "Uninsured: IV vs Non-IV")

# --- AGE profiles ---
plot_profile_cdf("Age/QGrid_Age_CDF_IV.pdf", res_age, thresholds, iv = TRUE)
plot_profile_cdf("Age/QGrid_Age_CDF_noIV.pdf", res_age, thresholds, iv = FALSE)
plot_profile_diff("Age/QGrid_Age_Diff_IV.pdf", res_age, thresholds, iv = TRUE)
plot_profile_diff("Age/QGrid_Age_Diff_noIV.pdf", res_age, thresholds, iv = FALSE)

plot_profiles_overlay("Age/QGrid_Age_Overlay_Insured_IV.pdf", res_age, thresholds,
                      iv = TRUE, insured = TRUE,
                      main = "Insured CDF by Age Group — IV (ASF)",
                      colors = c("steelblue", "black", "firebrick"))

plot_profiles_overlay("Age/QGrid_Age_Overlay_Uninsured_IV.pdf", res_age, thresholds,
                      iv = TRUE, insured = FALSE,
                      main = "Uninsured CDF by Age Group — IV (ASF)",
                      colors = c("steelblue", "black", "firebrick"))

plot_iv_comparison("Age/IVcomp_Age_Insured.pdf", res_age, thresholds,
                   insured = TRUE)
plot_iv_comparison("Age/IVcomp_Age_Uninsured.pdf", res_age, thresholds,
                   insured = FALSE)

# --- INCOME profiles ---
plot_profile_cdf("Income/QGrid_Inc_CDF_IV.pdf", res_inc, thresholds, iv = TRUE)
plot_profile_cdf("Income/QGrid_Inc_CDF_noIV.pdf", res_inc, thresholds, iv = FALSE)
plot_profile_diff("Income/QGrid_Inc_Diff_IV.pdf", res_inc, thresholds, iv = TRUE)
plot_profile_diff("Income/QGrid_Inc_Diff_noIV.pdf", res_inc, thresholds, iv = FALSE)

plot_profiles_overlay("Income/QGrid_Inc_Overlay_Insured_IV.pdf", res_inc, thresholds,
                      iv = TRUE, insured = TRUE,
                      main = "Insured CDF by Income Group — IV (ASF)",
                      colors = c("steelblue", "black", "firebrick"))

plot_profiles_overlay("Income/QGrid_Inc_Overlay_Uninsured_IV.pdf", res_inc, thresholds,
                      iv = TRUE, insured = FALSE,
                      main = "Uninsured CDF by Income Group — IV (ASF)",
                      colors = c("steelblue", "black", "firebrick"))

plot_iv_comparison("Income/IVcomp_Inc_Insured.pdf", res_inc, thresholds,
                   insured = TRUE)
plot_iv_comparison("Income/IVcomp_Inc_Uninsured.pdf", res_inc, thresholds,
                   insured = FALSE)

# --- SEX profiles ---
plot_profile_cdf("Sex/QGrid_Sex_CDF_IV.pdf", res_sex, thresholds, iv = TRUE)
plot_profile_cdf("Sex/QGrid_Sex_CDF_noIV.pdf", res_sex, thresholds, iv = FALSE)
plot_profile_diff("Sex/QGrid_Sex_Diff_IV.pdf", res_sex, thresholds, iv = TRUE)
plot_profile_diff("Sex/QGrid_Sex_Diff_noIV.pdf", res_sex, thresholds, iv = FALSE)

plot_profiles_overlay("Sex/QGrid_Sex_Overlay_Insured_IV.pdf", res_sex, thresholds,
                      iv = TRUE, insured = TRUE,
                      main = "Insured CDF by Sex — IV (ASF)",
                      colors = c("steelblue", "firebrick"))

plot_profiles_overlay("Sex/QGrid_Sex_Overlay_Uninsured_IV.pdf", res_sex, thresholds,
                      iv = TRUE, insured = FALSE,
                      main = "Uninsured CDF by Sex — IV (ASF)",
                      colors = c("steelblue", "firebrick"))

plot_iv_comparison("Sex/IVcomp_Sex_Insured.pdf", res_sex, thresholds,
                   insured = TRUE)
plot_iv_comparison("Sex/IVcomp_Sex_Uninsured.pdf", res_sex, thresholds,
                   insured = FALSE)

# --- COMBINED SEX and AGE profiles ---
plot_profile_cdf("Age_Sex/QGrid_Combined_CDF_IV.pdf", res_combined, thresholds, iv = TRUE)
plot_profile_cdf("Age_Sex/QGrid_Combined_CDF_noIV.pdf", res_combined, thresholds, iv = FALSE)
plot_profile_diff("Age_Sex/QGrid_Combined_Diff_IV.pdf", res_combined, thresholds, iv = TRUE)
plot_profile_diff("Age_Sex/QGrid_Combined_Diff_noIV.pdf", res_combined, thresholds, iv = FALSE)

plot_profiles_overlay("Age_Sex/QGrid_Combined_Overlay_Insured_IV.pdf", res_combined, thresholds,
                      iv = TRUE, insured = TRUE,
                      main = "Insured CDF by Combined Profile — IV (ASF)",
                      colors = c("darkblue", "blue", "lightblue", 
                                 "darkred", "red", "salmon"))

plot_profiles_overlay("Age_Sex/QGrid_Combined_Overlay_Uninsured_IV.pdf", res_combined, thresholds,
                      iv = TRUE, insured = FALSE,
                      main = "Uninsured CDF by Combined Profile — IV (ASF)",
                      colors = c("darkblue", "blue", "lightblue", 
                                 "darkred", "red", "salmon"))

plot_iv_comparison("Age_Sex/IVcomp_Combined_Insured.pdf", res_combined, thresholds,
                   insured = TRUE)
plot_iv_comparison("Age_Sex/IVcomp_Combined_Uninsured.pdf", res_combined, thresholds,
                   insured = FALSE)


# ── Save results ──────────────────────────────────────────────────────────────
save(res_age, res_inc, res_sex, res_combined,
     q_age, q_inc, q_sex,
     file = "quantile_grid_results.RData")

cat("\n=== All quantile grid plots saved to:", plot_dir, "===\n")

rm(inc, age)

