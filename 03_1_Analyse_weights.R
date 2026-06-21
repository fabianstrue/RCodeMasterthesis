library(ggplot2)
library(patchwork)
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
    levels = c("Not student", "Student"))) %>%
  mutate(
    SEX = factor(SEX, levels = c(1, 2), 
                 labels = c("Male", "Female"))) %>%
  mutate(EDUCYR = replace(EDUCYR, EDUCYR %in% c(96, 97, 98, 99), NA))

regular_full = data_wide %>%
  svydesign(ids = ~1, weights = ~PERWEIGHT, data = .)

regular_full$variables$EMPSTAT   <- droplevels(regular_full$variables$EMPSTAT)

regular_cleaned = data_cll %>%
  filter(between(AGE, 17, 64))%>%
  filter(EMPSTAT != "NIU") %>%
  filter(!is.na(NUMEMPS)) %>%
  svydesign(ids = ~1, weights = ~PERWEIGHT, data = .)

# Calculate mean and SE for the surveys
svymean(~AGE + factor(SEX) + INCTOT + EXPTOT + NDUIDMBRS + STUDENT + INSSHR, regular_full)
svymean(~EDUCYR, regular_full, na.rm = TRUE)
svymean(~EMPSTAT, regular_full, na.rm = TRUE) # Remove NA from the EMPSTAT calculation only. The goal is to not influence the mean of the other variables

svymean(~AGE + factor(SEX) + INCTOT + EXPTOT + EDUCYR + NDUIDMBRS + STUDENT + EMPSTAT + INSSHR, regular_cleaned)


# Calculate Standard Mean Differences (SMD) for full and cleaned data
# continuous SMDs: (mean_cleaned - mean_full) / pooled SD
cont <- c("AGE","INCTOT","EXPTOT","EDUCYR","NDUIDMBRS", "INSSHR")

smd_one <- function(v) {
  f  <- as.formula(paste0("~", v))
  mF <- coef(svymean(f, regular_full,    na.rm = TRUE))
  mC <- coef(svymean(f, regular_cleaned, na.rm = TRUE))
  vF <- coef(svyvar(f,  regular_full,    na.rm = TRUE))
  vC <- coef(svyvar(f,  regular_cleaned, na.rm = TRUE))
  unname((mC - mF) / sqrt((vF + vC) / 2))
  }
smd_cont <- sapply(cont, smd_one)

# --- binary SMDs: (p_cleaned - p_full) / pooled SD of a proportion ---
# read the proportions straight off your svymean output
pF_all <- coef(svymean(~STUDENT + SEX, regular_full))
pF <- c(male  = pF_all[["SEXMale"]],
         student  = pF_all[["STUDENTStudent"]],
         employed = coef(svymean(~EMPSTAT, regular_full, na.rm = TRUE))[["EMPSTATEmployed"]])

pC_all <- coef(svymean(~EMPSTAT + STUDENT + SEX, regular_cleaned))
pC <- c(male  = pC_all[["SEXMale"]],
         student  = pC_all[["STUDENTStudent"]],
         employed = pC_all[["EMPSTATEmployed"]])

smd_bin <- (pC - pF) / sqrt((pF*(1-pF) + pC*(1-pC)) / 2)

round(abs(c(smd_cont, smd_bin)), 3)

#rm(data_wide, regular_full, regular_cleaned, cols_EMPSTATRD)

table(regular_full$variables$EDUCYR)

# Density
plot_df <- rbind(
  data.frame(EDUCYR = regular_full$variables$EDUCYR,
             INCTOT = regular_full$variables$INCTOT,
             EXPTOT = regular_full$variables$EXPTOT,
             w      = weights(regular_full),
             source = "Full distribution"),
  data.frame(EDUCYR = regular_cleaned$variables$EDUCYR,
             INCTOT = regular_cleaned$variables$INCTOT,
             EXPTOT = regular_cleaned$variables$EXPTOT,
             w      = weights(regular_cleaned),
             source = "Analytical sample")
)

ggplot(plot_df, aes(x = INCTOT, fill = source, weight = w)) +
  geom_density(alpha = 0.2, colour = "black", linewidth = 0.3, na.rm = TRUE) +
  coord_cartesian(xlim = c(0, 250000)) +
  scale_fill_manual(values = c("Full distribution" = "green",
                               "Analytical sample" = "red")) +
  labs(x = "Years of education", y = "Density",
       fill = NULL, title = "EDUCYR: full vs. analytical sample") +
  theme_minimal()
