library(dplyr)

#kids = filter(data_cll, AGE <= 16) %>% select(-STUDENT, -HIDEG, -HEALTH, -EMPSTAT, -CHOEMINS, -NUMEMPS, -EHICOV, -EMPHICOV)

regular = filter(data_cll, between(AGE, 17, 64))

#elderly = filter(data_cll, AGE >= 65)

#head(sort(table(elderly$INSSHR), decreasing = TRUE), 25)

rm(data_cll, data_cll_stat)
gc()
