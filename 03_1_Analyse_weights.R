library(survey)

data_wide = data %>%
  pivot_wider(
    names_from = ROUNDRD,
    values_from = all_of(rd_vars),
    names_glue = "{.value}_R{ROUNDRD}"
  )

cols_EMPSTATRD = grep("^EMPSTATRD_R", names(data_wide), value = TRUE)

data_wide2 = data_wide %>%
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

regular_full = data_wide2 %>%
  svydesign(ids = ~1, weights = ~PERWEIGHT, data = .)
svymean(~AGE + factor(SEX) + INCTOT + EXPTOT + EDUCYR + NDUIDMBRS + STUDENT, regular_full)
svymean(~EMPSTAT, regular_full, na.rm = TRUE)


regular_cleaned = data_cll %>%
  filter(between(AGE, 17, 64))%>%
  filter(EMPSTAT != "NIU") %>%
  filter(!is.na(NUMEMPS)) %>%
  svydesign(ids = ~1, weights = ~PERWEIGHT, data = .)
svymean(~AGE + factor(SEX) + INCTOT + EXPTOT + EDUCYR + NDUIDMBRS + STUDENT + EMPSTAT, regular_cleaned)

rm(regular_full, regular_cleaned)

# YEAR
