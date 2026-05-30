library(AER)
library(dplyr)
setwd("C:/Users/fabia/Documents/#Köln/Uni/Masterarbeit/RCodeMasterthesis")
plot_dir <- file.path(getwd(), "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir)

# ══════════════════════════════════════════════════════════════════════════════
# Configuration
# ══════════════════════════════════════════════════════════════════════════════
# Unsure if I should use insurable months, as it could include unknowable information like death
# But Less insurable month also leads to less time for spending

# All not selected variables: = c("HIDEG", "FTOTVAL", "FOODSTYN", "HEALTH",
# "EXPSELFPAY", "EXPMCPAY", "EXPMAPAY", "EXPPRPAY", "EXPVAPAY", "EXPTRIPAY", "EXPOFPAY",
# "EXPOLPAY", "EXPWCPAY", "EXPOPRPAY", "EXPOPUPAY", "EXPOSPAY", "EXPPTRPAY", "EXPOTHPAY",
# "CHGTOT", "ADINSA", "ADINSB", "ADRISK", "NDUIDMBRS", "CHOEMINS", "EMPHICOV", "EHICOV")
# of which are insignificant: c("FTOTVAL", "INSABLEMNS", "INSDMNS", "CHOEMINS")
# or not interesting: c("HINOTCOV", "HIPRIVATE", "EXPSRCPAY", "INSABLEMNS", "INSDMNS",)
# "HEALTH" could be added later, but many NIUs
# Maybe change the target to ER spending, as caution could have an higher effect here

controls         <- c("YEAR", "NDUIDMBRS", "AGE", "SEX", "EDUCYR", "STUDENT",
                      "INCTOT", "EMPSTAT")
endogen          <- "INSSHR"
instrument       <- "NUMEMPS"
target           <- "EXPTOT"
t_transform      <- "sqrt"
first_stage_mode <- "Probit"   # "OLS" or "Probit"
weight_var       <- "PERWEIGHT"
weight_rescaling <- TRUE
n_thresholds     <- 500
B                <- 100        # bootstrap replications
alpha            <- 0.05       # for 95% CIs
pct              <- round((1 - alpha) * 100)
set.seed(12)

# ══════════════════════════════════════════════════════════════════════════════
# Data preparation
# ══════════════════════════════════════════════════════════════════════════════

df = regular %>%
  filter(EMPSTAT != "NIU") %>%
  filter(!is.na(NUMEMPS))
df$SEX     <- ifelse(df$SEX     == "Male",     1, 0)  # 1 = Male,     0 = Female
df$STUDENT <- ifelse(df$STUDENT == "Student",  1, 0)  # 1 = Student,  0 = Not student
df$EMPSTAT <- ifelse(df$EMPSTAT == "Employed", 1, 0)  # 1 = Employed, 0 = Unemployed

# ── Expand YEAR into fixed-effect dummies (reference = earliest year) ─────────
df$YEAR      <- factor(df$YEAR)
year_dummies <- model.matrix(~ YEAR, data = df)[, -1, drop = FALSE]
df           <- cbind(df, year_dummies)

controls <- c(setdiff(controls, "YEAR"), colnames(year_dummies))
rm(year_dummies)
# ══════════════════════════════════════════════════════════════════════════════
# Helper functions
# ══════════════════════════════════════════════════════════════════════════════

# Assemble model data frame with transformed target
def_variables <- function(df, target, endogen, instrument, controls, weight_var, t_transform = "No", weight_rescale = FALSE){
  y = switch(t_transform,
              "No"   = df[[target]],
              "sqrt" = sqrt(df[[target]]),
              "log"  = log(df[[target]]),
              stop("Only log, sqrt or no transformation available"))
  model_df <- cbind(
    y    = y,
    df[, controls, drop = FALSE],
    endo = df[[endogen]],
    inst = df[[instrument]])
  if (weight_rescale) {
    model_df[[weight_var]] <- df[[weight_var]] / mean(df[[weight_var]])
  } else {
    model_df[[weight_var]] <- df[[weight_var]]
  }
  return(model_df)
}

# First stage: returns control-function residuals (OLS residuals or generalized
# residuals from a probit, depending on `mode`)
fit_first_stage <- function(formula, data, weight_var, mode = "OLS") {
  data[[".w"]] <- data[[weight_var]]
  if (mode == "OLS") {
    return(lm(formula, data = data, weights = .w)$residuals)
  }
  if (mode == "Probit") {
    fs <- glm(formula, family = quasibinomial(link = "probit"), data = data,
              weights = .w, control = glm.control(maxit = 100))
    xb <- predict(fs, type = "link")
    return(ifelse(data$endo == 1,
                  dnorm(xb) / pnorm(xb),
                  -dnorm(xb) / (1 - pnorm(xb))))
  }
  stop("first_stage_mode must be 'OLS' or 'Probit'")
}

# Run the probit loop over all thresholds, returning coefficient matrices
# for both the non-IV and IV (control function) specifications.
run_probit_loop <- function(model_df, Y2hat, thresholds,
                            probit_formula_noIV, probit_formula_IV,
                            n_coef_noIV, n_coef_IV, weight_var) {
  n_th <- length(thresholds)
  res_noIV    <- matrix(NA_real_, ncol = n_coef_noIV, nrow = n_th)
  res_IV      <- matrix(NA_real_, ncol = n_coef_IV,   nrow = n_th)
  df_work     <- cbind(model_df, dependent = NA, Y2hat = Y2hat)
  df_work$.w  <- df_work[[weight_var]]
  
  for (i in seq_len(n_th)) {
    df_work$dependent <- as.numeric(df_work$y <= thresholds[i])
    
    res_noIV[i, ] <- tryCatch(
      glm(probit_formula_noIV, data = df_work,
          family = quasibinomial(link = "probit"),
          weights = .w, control = glm.control(maxit=100))$coeff,
      error = function(e) rep(NA_real_, n_coef_noIV))
    
    res_IV[i, ] <- tryCatch(
      glm(probit_formula_IV, data = df_work,
          family = quasibinomial(link = "probit"),
          weights = .w, control = glm.control(maxit=100))$coeff,
      error = function(e) rep(NA_real_, n_coef_IV))
  }
  list(noIV = res_noIV, IV = res_IV)
}

# Compute CDFs for insured (endo=1) and uninsured (endo=0) at the evaluation
# point. For the IV version, average over observed Y2hat residuals (ASF).
compute_cdf <- function(endo_val, res_noIV, res_IV, eval_means, Y2hat, n_ctrl, wts) {
  n <- nrow(res_noIV)
  
  # Without IV: probit prediction at eval point
  yvaluesnonIV <- sapply(seq_len(n), function(i) {
    lp <- res_noIV[i, 1] +
      sum(eval_means * res_noIV[i, 2:(n_ctrl + 1)]) +
      endo_val * res_noIV[i, n_ctrl + 2]
    pnorm(lp)
  })
  
  # With IV: Average Structural Function - vectorized over residuals
  base_lp <- res_IV[, 1] +
    as.vector(res_IV[, 2:(n_ctrl + 1)] %*% eval_means) +
    endo_val * res_IV[, n_ctrl + 2]
  
  yvaluesIV <- sapply(seq_len(n), function(i) {
    weighted.mean(pnorm(base_lp[i] + res_IV[i, n_ctrl + 3] * Y2hat), w = wts)
  })
  
  list(nonIV = yvaluesnonIV, IV = yvaluesIV)
}

# Monotonic rearrangement
rearrange <- function(thresholds, yvalues, u) {
  Qhat <- sapply(u, function(x) thresholds[min(length(thresholds),
                                               min(which(yvalues >= x)))])
  sapply(thresholds, function(x) mean(Qhat <= x, na.rm = TRUE))
}

# Confidence band helpers
ci_pointwise <- function(mat, alpha = 0.05) {
  list(lower = apply(mat, 1, quantile, probs = alpha / 2,     na.rm = TRUE),
       upper = apply(mat, 1, quantile, probs = 1 - alpha / 2, na.rm = TRUE))
}

ci_uniform <- function(mat, main_estimate, alpha = 0.05) {
  sup_devs <- apply(mat, 2, function(col) max(abs(col - main_estimate),
                                              na.rm = TRUE))
  crit <- quantile(sup_devs, probs = 1 - alpha, na.rm = TRUE)
  list(lower = main_estimate - crit,
       upper = main_estimate + crit)
}

# ── Plot helpers ──────────────────────────────────────────────────────────────

plot_cdf <- function(file, x, y_un, y_ins, title,
                     ylab = "Estimated CDF", ylim = c(0, 1)) {
  pdf(file = file.path(plot_dir, file), width = 9, height = 6)
  plot(x, y_ins, lwd = 2, type = "l", col = "darkred",
       xlab = "sqrt(Total Expenditure)", ylab = ylab, ylim = ylim, main = title,
       cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
  lines(x, y_un, lwd = 2, col = "black")
  legend("bottomright", c("Uninsured", "Insured"), lwd = 2,
         col = c("black", "darkred"))
  dev.off()
}

plot_cdf_ci <- function(file, x, y_un, y_ins, ci_un, ci_ins, title) {
  pdf(file = file.path(plot_dir, file), width = 9, height = 6)
  plot(x, y_ins, lwd = 2, type = "l", col = "darkred",
       xlab = "sqrt(Total Expenditure)", ylab = "Estimated CDF",
       ylim = c(0, 1), main = title,
       cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
  lines(x, ci_ins$upper, lwd = 1, lty = 2, col = "darkred")
  lines(x, ci_ins$lower, lwd = 1, lty = 2, col = "darkred")
  lines(x, y_un,         lwd = 2, col = "black")
  lines(x, ci_un$upper,  lwd = 1, lty = 2, col = "black")
  lines(x, ci_un$lower,  lwd = 1, lty = 2, col = "black")
  legend("bottomright", c("Uninsured", "Insured"), lwd = 2,
         col = c("black", "darkred"))
  dev.off()
}

plot_diff <- function(file, x, diff_main, ci_diff, title, ylim = c(-0.3, 0.3)) {
  pdf(file = file.path(plot_dir, file), width = 9, height = 6)
  plot(x, diff_main, lwd = 2, type = "l",
       xlab = "sqrt(Total Expenditure)", ylab = "Difference in Estimated CDF",
       ylim = ylim, main = title,
       cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
  abline(h = 0, lty = 3, col = "grey50")
  lines(x, ci_diff$upper, lwd = 1, lty = 2)
  lines(x, ci_diff$lower, lwd = 1, lty = 2)
  dev.off()
}

# ══════════════════════════════════════════════════════════════════════════════
# Formulas (built once, reused throughout)
# ══════════════════════════════════════════════════════════════════════════════

model_df = def_variables(df, target, endogen, instrument, controls, weight_var,
                         t_transform = "sqrt", weight_rescale = weight_rescaling)
model_df <- model_df[complete.cases(model_df), ]
nn       <- nrow(model_df)
n_ctrl   <- length(controls)

ctrl_str            <- paste(controls, collapse = " + ")
probit_formula_noIV <- as.formula(paste("dependent ~", ctrl_str, "+ endo"))
first_stage_formula <- as.formula(paste("endo ~",      ctrl_str, "+ inst"))
probit_formula_IV   <- as.formula(paste("dependent ~", ctrl_str, "+ endo + Y2hat"))
iv_formula = as.formula(paste(
            "y ~", paste(c(controls, "endo"), collapse = " + "),
            "|", paste(c(controls, "inst"), collapse = " + ")))
rm(ctrl_str, df)

# ══════════════════════════════════════════════════════════════════════════════
# OLS and IV diagnostics
# ══════════════════════════════════════════════════════════════════════════════
lm_formula <- as.formula(paste("y ~", paste(c(controls, "endo"), collapse = " + ")))
linmod <- lm(lm_formula, data = model_df, weights = model_df[[weight_var]])
ivmod  <- ivreg(iv_formula, data = model_df, weights = model_df[[weight_var]])
print(summary(linmod))
print(summary(ivmod, diagnostics = TRUE))

n_coef_noIV <- length(linmod$coeff)    # intercept + controls + endo
n_coef_IV   <- n_ctrl + 3              # intercept + controls + endo + Y2hat

# ══════════════════════════════════════════════════════════════════════════════
# Main estimation on full sample
# ══════════════════════════════════════════════════════════════════════════════

Y2hat <- fit_first_stage(first_stage_formula, model_df,
                         mode=first_stage_mode, weight_var=weight_var)


# Threshold grid: zero plus 499 quantile-based points on the positive support
thresholds <- c(0,  # explicitly include the zero point mass
  unique(quantile(model_df$y[model_df$y > 0],
                  probs = seq(0.01, 0.99, length.out = n_thresholds - 1),
                  type  = 1)))
thresholds       <- sort(unique(thresholds))
n_thresholds     <- length(thresholds)
u                <- seq(0.01, 0.99, by = 0.01)
cat("Number of thresholds:", n_thresholds, "\n")

probit_res <- run_probit_loop(model_df, Y2hat, thresholds,
                              probit_formula_noIV, probit_formula_IV,
                              n_coef_noIV, n_coef_IV, weight_var = weight_var)

betahatsnonIV <- probit_res$noIV
betahatsIV   <- probit_res$IV

# CDFs at sample means
eval_means    <- apply(model_df[, controls, drop = FALSE], 2,
                       weighted.mean, w = model_df[[weight_var]])
cdf_uninsured <- compute_cdf(0, betahatsnonIV, betahatsIV, eval_means, Y2hat, n_ctrl, model_df[[weight_var]])
cdf_insured   <- compute_cdf(1, betahatsnonIV, betahatsIV, eval_means, Y2hat, n_ctrl, model_df[[weight_var]])

# Monotonization
iso_un_noIV  <- isoreg(thresholds, cdf_uninsured$nonIV)
iso_un_IV    <- isoreg(thresholds, cdf_uninsured$IV)
iso_ins_noIV <- isoreg(thresholds, cdf_insured$nonIV)
iso_ins_IV   <- isoreg(thresholds, cdf_insured$IV)

rear_un_noIV  <- rearrange(thresholds, cdf_uninsured$nonIV, u)
rear_un_IV    <- rearrange(thresholds, cdf_uninsured$IV,    u)
rear_ins_noIV <- rearrange(thresholds, cdf_insured$nonIV,   u)
rear_ins_IV   <- rearrange(thresholds, cdf_insured$IV,      u)

save(betahatsnonIV, betahatsIV, cdf_uninsured, cdf_insured,
     thresholds, n_thresholds,
     file = paste0("main_results_", first_stage_mode, ".RData"))

# ══════════════════════════════════════════════════════════════════════════════
# Main plots (no CI)
# ══════════════════════════════════════════════════════════════════════════════

# Raw (no monotonisation)
plot_cdf(paste0("Plot_Raw_noIV_", first_stage_mode, ".pdf"),
         thresholds, cdf_uninsured$nonIV, cdf_insured$nonIV, "Conditional CDF — Without IV")
plot_cdf(paste0("Plot_Raw_IV_", first_stage_mode, ".pdf"),
         thresholds, cdf_uninsured$IV,    cdf_insured$IV,    "Conditional CDF — With IV (ASF)")

# Isotonic regression
plot_cdf(paste0("Plot_Isotonic_noIV_", first_stage_mode, ".pdf"),
         iso_un_noIV$x, iso_un_noIV$yf, iso_ins_noIV$yf, "Conditional CDF (Isotonic) — Without IV")
plot_cdf(paste0("Plot_Isotonic_IV_", first_stage_mode, ".pdf"),
         iso_un_IV$x,   iso_un_IV$yf,   iso_ins_IV$yf,   "Conditional CDF (Isotonic) — With IV (ASF)")

# Monotonic rearrangement
plot_cdf(paste0("Plot_Rear_noIV_", first_stage_mode, ".pdf"),
         thresholds, rear_un_noIV, rear_ins_noIV, "Conditional CDF (Rearrangement) — Without IV")
plot_cdf(paste0("Plot_Rear_IV_", first_stage_mode, ".pdf"),
         thresholds, rear_un_IV,   rear_ins_IV,   "Conditional CDF (Rearrangement) — With IV (ASF)")

# ══════════════════════════════════════════════════════════════════════════════
# Bootstrap
# ══════════════════════════════════════════════════════════════════════════════

bs_un_noIV   <- matrix(NA_real_, nrow = n_thresholds, ncol = B)
bs_un_IV     <- matrix(NA_real_, nrow = n_thresholds, ncol = B)
bs_ins_noIV  <- matrix(NA_real_, nrow = n_thresholds, ncol = B)
bs_ins_IV    <- matrix(NA_real_, nrow = n_thresholds, ncol = B)
bs_diff_noIV <- matrix(NA_real_, nrow = n_thresholds, ncol = B)
bs_diff_IV   <- matrix(NA_real_, nrow = n_thresholds, ncol = B)

for (b in seq_len(B)) { 
  idx  <- sample(seq_len(nn), replace = TRUE)
  dfBS <- model_df[idx, ]
  
  Y2hat_b <- fit_first_stage(first_stage_formula, dfBS,
                             mode = first_stage_mode, weight_var = weight_var)
  
  probit_b <- run_probit_loop(dfBS, Y2hat_b, thresholds,
                              probit_formula_noIV, probit_formula_IV,
                              n_coef_noIV, n_coef_IV, weight_var = weight_var)
  
  cdf_b_un  <- compute_cdf(0, probit_b$noIV, probit_b$IV, eval_means, Y2hat_b,
                           n_ctrl, dfBS[[weight_var]])
  cdf_b_ins <- compute_cdf(1, probit_b$noIV, probit_b$IV, eval_means, Y2hat_b,
                           n_ctrl, dfBS[[weight_var]])
  
  bs_un_noIV[,   b] <- isoreg(thresholds, cdf_b_un$nonIV)$yf
  bs_un_IV[,     b] <- isoreg(thresholds, cdf_b_un$IV)$yf
  bs_ins_noIV[,  b] <- isoreg(thresholds, cdf_b_ins$nonIV)$yf
  bs_ins_IV[,    b] <- isoreg(thresholds, cdf_b_ins$IV)$yf
  bs_diff_noIV[, b] <- bs_ins_noIV[, b] - bs_un_noIV[, b]
  bs_diff_IV[,   b] <- bs_ins_IV[,   b] - bs_un_IV[,   b]
  
  cat("Completed draw:", b, "\n")
  if (b %% 10 == 0) {
    save(bs_un_noIV, bs_un_IV, bs_ins_noIV, bs_ins_IV, bs_diff_noIV, bs_diff_IV, 
         file = paste0("bootstrap_progress_", first_stage_mode, ".RData"))}
}

# ══════════════════════════════════════════════════════════════════════════════
# Confidence bands
# ══════════════════════════════════════════════════════════════════════════════

# Main CDF estimates (used as reference for uniform bands)
main_un_noIV  <- iso_un_noIV$yf
main_un_IV    <- iso_un_IV$yf
main_ins_noIV <- iso_ins_noIV$yf
main_ins_IV   <- iso_ins_IV$yf
main_diff_noIV <- main_ins_noIV - main_un_noIV
main_diff_IV   <- main_ins_IV   - main_un_IV


# 95% confidence bands pointwise
# Pointwise bands
ci_pw <- list(
  un_noIV   = ci_pointwise(bs_un_noIV,   alpha),
  un_IV     = ci_pointwise(bs_un_IV,     alpha),
  ins_noIV  = ci_pointwise(bs_ins_noIV,  alpha),
  ins_IV    = ci_pointwise(bs_ins_IV,    alpha),
  diff_noIV = ci_pointwise(bs_diff_noIV, alpha),
  diff_IV   = ci_pointwise(bs_diff_IV,   alpha)
)

# Uniform bands
ci_uni <- list(
  un_noIV   = ci_uniform(bs_un_noIV,  main_un_noIV,    alpha),
  un_IV     = ci_uniform(bs_un_IV,    main_un_IV,      alpha),
  ins_noIV  = ci_uniform(bs_ins_noIV, main_ins_noIV,   alpha),
  ins_IV    = ci_uniform(bs_ins_IV,   main_ins_IV,     alpha),
  diff_noIV = ci_uniform(bs_diff_noIV, main_diff_noIV, alpha),
  diff_IV   = ci_uniform(bs_diff_IV,   main_diff_IV,   alpha)
)

# ══════════════════════════════════════════════════════════════════════════════
# CI plots
# ══════════════════════════════════════════════════════════════════════════════

plot_cdf_ci(paste0("Plot_Isotonic_noIV_pwCI_", first_stage_mode, ".pdf"),
            thresholds, main_un_noIV, main_ins_noIV,
            ci_pw$un_noIV, ci_pw$ins_noIV,
            paste0("Conditional CDF (Isotonic) — Without IV, ", pct, "% pointwise CI"))

plot_cdf_ci(paste0("Plot_Isotonic_IV_pwCI_", first_stage_mode, ".pdf"),
            thresholds, main_un_IV, main_ins_IV,
            ci_pw$un_IV, ci_pw$ins_IV,
            paste0("Conditional CDF (Isotonic) — With IV (ASF), ", pct, "% pointwise CI"))

plot_cdf_ci(paste0("Plot_Isotonic_noIV_uniCI_", first_stage_mode, ".pdf"),
            thresholds, main_un_noIV, main_ins_noIV,
            ci_uni$un_noIV, ci_uni$ins_noIV,
            paste0("Conditional CDF (Isotonic) — Without IV, ", pct, "% uniform CI"))

plot_cdf_ci(paste0("Plot_Isotonic_IV_uniCI_", first_stage_mode, ".pdf"),
            thresholds, main_un_IV, main_ins_IV,
            ci_uni$un_IV, ci_uni$ins_IV,
            paste0("Conditional CDF (Isotonic) — With IV (ASF), ", pct, "% uniform CI"))

# Difference plots (pointwise)
plot_diff(paste0("Plot_Difference_noIV_pwCI_", first_stage_mode, ".pdf"),
          thresholds, main_diff_noIV, ci_pw$diff_noIV,
          paste0("CDF Difference: Insured − Uninsured (No IV), ", pct, "% pointwise CI"))

plot_diff(paste0("Plot_Difference_IV_pwCI_", first_stage_mode, ".pdf"),
          thresholds, main_diff_IV, ci_pw$diff_IV,
          paste0("CDF Difference: Insured − Uninsured (IV / ASF), ", pct, "% pointwise CI"))

# Difference plots (uniform)
plot_diff(paste0("Plot_Difference_noIV_uniCI_", first_stage_mode, ".pdf"),
          thresholds, main_diff_noIV, ci_uni$diff_noIV,
          paste0("CDF Difference: Insured − Uninsured (No IV), ", pct, "% uniform CI"))

plot_diff(paste0("Plot_Difference_IV_uniCI_", first_stage_mode, ".pdf"),
          thresholds, main_diff_IV, ci_uni$diff_IV,
          paste0("CDF Difference: Insured − Uninsured (IV / ASF), ", pct, "% uniform CI"))


rm(u, b, Y2hat_b, dfBS, rear_un_noIV, rear_un_IV, rear_ins_noIV, rear_ins_IV)
