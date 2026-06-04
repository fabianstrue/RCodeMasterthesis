library(survey)

data_wide = data %>%
  pivot_wider(
    names_from = ROUNDRD,
    values_from = all_of(rd_vars),
    names_glue = "{.value}_R{ROUNDRD}"
  )

regular_full = data_wide %>%
  filter(between(AGE, 17, 64)) %>%
  svydesign(ids = ~1, weights = ~PERWEIGHT, data = .)
svymean(~AGE + factor(SEX) + INCTOT + EXPTOT + EDUCYR + NDUIDMBRS, regular_full)

regular_cleaned = data_cll %>%
  filter(between(AGE, 17, 64))%>%
  filter(EMPSTAT != "NIU") %>%
  filter(!is.na(NUMEMPS)) %>%
  svydesign(ids = ~1, weights = ~PERWEIGHT, data = .)
svymean(~AGE + factor(SEX) + INCTOT + EXPTOT + EDUCYR + NDUIDMBRS, regular_cleaned)

rm(regular_full, regular_cleaned)


