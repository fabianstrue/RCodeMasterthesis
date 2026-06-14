library(survey)

# create survey data
data_wide = data %>%
  pivot_wider(
    names_from = ROUNDRD,
    values_from = all_of(rd_vars),
    names_glue = "{.value}_R{ROUNDRD}"
  )

cols_EMPSTATRD = grep("^EMPSTATRD_R", names(data_wide), value = TRUE)

data_wide = data_wide %>%
  mutate(EMPSTAT = apply(pick(all_of(cols_EMPSTATRD)), 1, function(vals) {
    vals = na.omit(vals)
    if (n_distinct(vals) == 1) return(vals[1])
    if (any(vals == 1)) return(1)
    return(4)
  })) %>%
  filter(between(AGE, 17, 64)) %>%
  mutate(EMPSTAT = factor(EMPSTAT, levels = c(0, 1, 4), 
                          labels = c("NIU", "Employed", "Unemployed"))) %>%
  mutate(STUDENT = factor(
    ifelse(replace(STUDENT, STUDENT == 6, 3) %in% c(1, 2), "Student", "Not student"),
    levels = c("Not student", "Student")))

regular_full = data_wide %>%
  svydesign(ids = ~1, weights = ~PERWEIGHT, data = .)

regular_cleaned = data_cll %>%
  filter(between(AGE, 17, 64))%>%
  filter(EMPSTAT != "NIU") %>%
  filter(!is.na(NUMEMPS)) %>%
  svydesign(ids = ~1, weights = ~PERWEIGHT, data = .)

# Calculate mean and SE for the surveys
svymean(~AGE + factor(SEX) + INCTOT + EXPTOT + EDUCYR + NDUIDMBRS + STUDENT + INSSHR, regular_full)
svymean(~EMPSTAT, regular_full, na.rm = TRUE)

svymean(~AGE + factor(SEX) + INCTOT + EXPTOT + EDUCYR + NDUIDMBRS + STUDENT + EMPSTAT + INSSHR, regular_cleaned)

# Calculate Standard Mean Differences (SMD) for full and cleaned data
vF <- svyvar(~AGE + INCTOT + EXPTOT + EDUCYR + NDUIDMBRS, regular_full)
vC <- svyvar(~AGE + INCTOT + EXPTOT + EDUCYR + NDUIDMBRS, regular_cleaned)

vF <- diag(as.matrix(vF))   # pull the variances onto a plain vector
vC <- diag(as.matrix(vC))

# continuous SMDs: (mean_cleaned - mean_full) / pooled SD
cont <- c("AGE","INCTOT","EXPTOT","EDUCYR","NDUIDMBRS")

mF <- c(AGE=40.225, INCTOT=50371, EXPTOT=6106.8, EDUCYR=14.042, NDUIDMBRS=3.2486)
mC <- c(AGE=40.518, INCTOT=50691, EXPTOT=6631.1, EDUCYR=13.580, NDUIDMBRS=3.2090)

smd_cont <- (mC[cont] - mF[cont]) / sqrt((vF[cont] + vC[cont]) / 2)

# --- binary SMDs: (p_cleaned - p_full) / pooled SD of a proportion ---
# read the proportions straight off your svymean output
pF <- c(male=0.49511, student=0.088823, employed=0.79691)
pC <- c(male=0.48875, student=0.095229, employed=0.77075)

smd_bin <- (pC - pF) / sqrt((pF*(1-pF) + pC*(1-pC)) / 2)

round(abs(c(smd_cont, smd_bin)), 3)

#rm(data_wide, regular_full, regular_cleaned, cols_EMPSTATRD)

# YEAR
