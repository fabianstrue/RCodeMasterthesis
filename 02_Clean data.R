library(dplyr)
library(haven)

data_cl = data %>%
  mutate(across(where(haven::is.labelled), as.numeric)) %>%
  filter(INSSHR %in% c(0, 1)) %>%
  filter(AGE != 996) %>%
  filter(STUDENT != 7) %>%
  filter(!EDUCYR %in% c(97, 98, 99)) %>%
  filter(HEALTH != 8) %>% # maybe remove HEALTH many NIU
  filter(!EMPSTATRD %in% c(9, 8, 7)) %>%
  filter(!CHOEMINSRD %in% c(4, 7, 8, 9)) %>%
  filter(!NUMEMPSRD %in% c(994, 997, 998, 999)) %>%
  filter(!EHICOVRD %in% c(7, 8, 9)) %>%
  filter(!EMPHICOVRD %in% c(7, 8, 9)) %>%
  filter(STUDENT != 9) %>%
  mutate(STUDENT = replace(STUDENT, STUDENT == 6, 3),
         STUDENT = factor(
           ifelse(STUDENT %in% c(1, 2), "Student", "Not student"),
           levels = c("Not student", "Student")        # set reference level explicitly
         )) %>% # NIU seen as no students
  mutate(EMPSTATRD = replace(EMPSTATRD, EMPSTATRD %in% c(2, 3), 1))

#rm(data)
