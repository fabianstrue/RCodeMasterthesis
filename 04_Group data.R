library(dplyr)

#kids = filter(data_cll, AGE <= 16) %>% select(-STUDENT, -HIDEG, -HEALTH, -EMPSTAT, -CHOEMINS, -NUMEMPS, -EHICOV, -EMPHICOV)

regular = data_cll %>% 
  filter(between(AGE, 17, 64)) %>%
  filter(EMPSTAT != "NIU") %>%
  filter(!is.na(NUMEMPS))

#elderly = filter(data_cll, AGE >= 65)

#head(sort(table(elderly$INSSHR), decreasing = TRUE), 25)

rm(data_cll, data_cll_stat)
gc()
