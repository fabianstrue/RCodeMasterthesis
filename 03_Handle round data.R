library(dplyr)
library(tidyr)
library(labelled)
library(survey)

stat_cols = c("PANEL", "PSUANN", "STRATANN", "PSUPLD", "STRATAPLD", "PANELYR", "RELYR", "PERWEIGHT", "SAQWEIGHT", "DIABWEIGHT", "SDOHWT")

rd_vars <- names(data_cl)[grepl("RD$", names(data_cl))] %>%
  setdiff("ROUNDRD")

data_cl_wide = data_cl %>%
  pivot_wider(
    names_from = ROUNDRD,
    values_from = all_of(rd_vars),
    names_glue = "{.value}_R{ROUNDRD}"
  )

cols_EMPSTATRD = grep("^EMPSTATRD_R", names(data_cl_wide), value = TRUE)
cols_CHOEMINSRD = grep("^CHOEMINSRD_R", names(data_cl_wide), value = TRUE)
cols_EMPHICOVRD = grep("^EMPHICOVRD_R", names(data_cl_wide), value = TRUE)
cols_EHICOVRD = grep("^EHICOVRD_R", names(data_cl_wide), value = TRUE)
cols_NUMEMPSRD = grep("^NUMEMPSRD_R", names(data_cl_wide), value = TRUE)
cols_SLFEMPRD = grep("^SLFEMPRD_R", names(data_cl_wide), value = TRUE)

data_cll = data_cl_wide %>%
  mutate(EMPSTAT = apply(pick(all_of(cols_EMPSTATRD)), 1, function(vals) {
    vals = na.omit(vals)
    if (n_distinct(vals) == 1) return(vals[1])
    if (any(vals == 1)) return(1)
    return(4)
  })) %>%
  mutate(CHOEMINS = apply(pick(all_of(cols_CHOEMINSRD)), 1, function(vals) {
    vals = na.omit(vals)
    if (any(vals == 2)) return(2)
    if (any(vals == 1)) return(1)
    return(0)
  })) %>%
  mutate(EMPHICOV = apply(pick(all_of(cols_EMPHICOVRD)), 1, function(vals) {
    vals = na.omit(vals)
    if (any(vals == 2)) return(2)
    if (any(vals == 1)) return(1)
    return(0)
  })) %>%
  mutate(EHICOV = apply(pick(all_of(cols_EHICOVRD)), 1, function(vals) {
    vals = na.omit(vals)
    if (any(vals == 2)) return(2)
    if (any(vals == 1)) return(1)
    return(0)
  })) %>%
  mutate(NUMEMPS = apply(pick(all_of(cols_NUMEMPSRD)), 1, function(vals) {
    vals <- na.omit(vals)
    if (length(vals) == 0) return(NA_real_)
    maxV = max(vals)
    if (maxV < 0) return(NA_real_)
    return(maxV)
  })) %>%
  mutate(SLFEMP = apply(pick(all_of(cols_SLFEMPRD)), 1, function(vals) {
    vals = na.omit(vals)
    if (any(vals == 2)) return(2) # 2 = Self employed
    return(1)
  }))  %>%
  select(-all_of(c(cols_EMPSTATRD, cols_CHOEMINSRD, cols_EMPHICOVRD,
                   cols_EHICOVRD, cols_NUMEMPSRD, cols_SLFEMPRD))) %>%
  mutate(
    SEX = factor(SEX, levels = c(1, 2), 
                     labels = c("Male", "Female")),
    HIDEG = factor(HIDEG, levels = c(10, 20, 30, 40, 51, 52, 53, 96), 
                     labels = c("No Degree", "GED", "High school diploma", 
                                "Bachelor's degree", "Master's degree",
                                "Doctorate degree", "Other degree", "NIU")),
    EMPSTAT = factor(EMPSTAT, levels = c(0, 1, 4), 
                     labels = c("NIU", "Employed", "Unemployed")),
    CHOEMINS  = factor(CHOEMINS,  levels = c(0, 1, 2),  
                     labels = c("NIU", "No", "Yes")),
    EMPHICOV  = factor(EMPHICOV,  levels = c(0, 1, 2),  
                     labels = c("NIU", "No", "Yes")),
    EHICOV  = factor(EHICOV,  levels = c(0, 1, 2),  
                     labels = c("NIU", "No", "Yes"))) %>%
  set_variable_labels(
    EMPSTAT  = "Employment status aggregated",
    CHOEMINS = "Choice of health insurance status aggregated",
    EMPHICOV = "Current main job offers health insurance aggregated",
    EHICOV   = "Held health insurance from current main job aggregated",
    NUMEMPS  = "N employees at firm aggregated"
   ) %>%
  filter(!(EMPSTAT == "Employed" & NUMEMPS == 0),
         PERWEIGHT > 0)

# EMPHICOV and EHICOV are almost perfectly collinear, thus I create a combination
data_cll <- data_cll %>%
  mutate(EMPHICOV_STATUS = case_when(
    EMPHICOV == "NIU"  ~ "NIU",
    EMPHICOV == "No"   ~ "No offer",
    EMPHICOV == "Yes" & EHICOV == "No"  ~ "Offered, not covered",
    EMPHICOV == "Yes" & EHICOV == "Yes" ~ "Offered and covered"
  ) %>% factor())

# For all NIU in HIDEG, people under the AGE of 19, it is fair to assume they have no degree jet.
data_cll <- data_cll %>%
  mutate(HIDEG = factor(
    ifelse(HIDEG == "NIU", "No Degree", as.character(HIDEG)),
    levels = c("No Degree", "GED", "High school diploma", 
               "Bachelor's degree", "Master's degree", 
               "Doctorate degree", "Other degree")
  ))

data_cll_stat = data_cll %>%
  select(MEPSID, YEAR, all_of(stat_cols))
data_cll = data_cll %>%
  select(-all_of(setdiff(stat_cols, "PERWEIGHT")))


# These variables together amount to the same as EXPTOT (withing rounding errors)
data_cll$EXPOWNSUM <- rowSums(data_cll[, c(
  "EXPSELFPAY", "EXPMCPAY", "EXPMAPAY", "EXPPRPAY", "EXPTRIPAY",
  "EXPVAPAY", "EXPOFPAY", "EXPOLPAY", "EXPWCPAY", "EXPOSPAY"
)], na.rm = TRUE)

# Calcualte the share of self-spending on total spending
data_cll <- data_cll %>%
  mutate(EXPSELFPAYSHR = if_else(
    EXPOWNSUM > 0,
    round(EXPSELFPAY / EXPTOT, 3),
    NA_real_
  ))
# Number of uninsured, that dont have any spending => Spending share cound not be computed
sum(is.na(data_cll$EXPSELFPAYSHR) & data_cll$INSSHR == 0)
# Number of uninsured
sum(data_cll$INSSHR == 0)


# remove columns, that split spending by spurce (except self spending)
data_cll <- data_cll %>%
  select(-any_of(c("EXPSRCPAY", "EXPOPUPAY", "EXPOPRPAY", "EXPPTRPAY",
                   "EXPOTHPAY", "ALLEXPINSU", "ALLEXPSELF", "EXPMCPAY",
                   "EXPMAPAY", "XPPRPAY", "EXPTRIPAY", "EXPVAPAY",
                   "EXPOFPAY", "EXPOLPAY", "EXPWCPAY", "EXPOSPAY")))


# Save data locally
write_csv(data_cll, "data/data_cll.csv")
write_csv(data_cll_stat, "data/data_cll_statistical_variables.csv")

# Clean environment
rm(cols_EMPSTATRD, cols_CHOEMINSRD, cols_EMPHICOVRD, cols_EHICOVRD,
   cols_NUMEMPSRD, cols_SLFEMPRD, data_cl_wide, rd_vars, data_cl)
gc()
