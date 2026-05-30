library(survey)

regular_full = data %>%
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


