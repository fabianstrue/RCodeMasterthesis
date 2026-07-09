library(dplyr)
library(haven)
library(zoo)

data_cl = data %>%
  mutate(across(where(haven::is.labelled), as.numeric)) %>%
  filter(INSSHR %in% c(0, 1)) %>%
  filter(AGE != 996) %>%
  filter(!STUDENT %in% c(7, 9)) %>%
  filter(!EDUCYR %in% c(97, 98, 99)) %>%
  filter(!EMPSTATRD %in% c(9, 8, 7)) %>%
  filter(!NUMEMPSRD %in% c(997, 998, 999)) %>%
  #filter(!CHOEMINSRD %in% c(4, 7, 8, 9)) %>%
  #filter(!EHICOVRD %in% c(7, 8, 9)) %>%
  #filter(!EMPHICOVRD %in% c(7, 8, 9)) %>%
  mutate(NUMEMPSRD = replace(NUMEMPSRD, NUMEMPSRD == -10, 500), # For 2023 the value for top coding was -10, else 500
         EMPSTATRD = replace(EMPSTATRD, EMPSTATRD == 0, 4), # NIU for work -> not employed
         EMPSTATRD = replace(EMPSTATRD, EMPSTATRD %in% c(2, 3), 1),
         STUDENT = replace(STUDENT, STUDENT == 6, 3), # NIU seen as no students
         STUDENT = factor(
           ifelse(STUDENT %in% c(1, 2), "Student", "Not student"),
           levels = c("Not student", "Student")        # set reference level explicitly
         ))  %>%
  arrange(MEPSID, YEAR, ROUNDRD) %>%
  group_by(MEPSID) %>%
  mutate(NUMEMPSRD = replace(NUMEMPSRD, NUMEMPSRD == 994, NA_real_),
         NUMEMPSRD = zoo::na.locf(NUMEMPSRD, na.rm = FALSE)) %>%
  ungroup()

# data_cl = data %>%
#   mutate(across(where(haven::is.labelled), as.numeric)) %>%
#   filter(INSSHR %in% c(0, 1)) %>%
#   filter(AGE != 996) %>%
#   filter(!STUDENT %in% c(7, 9)) %>%
#   filter(!EDUCYR %in% c(97, 98, 99)) %>%
#   mutate(NUMEMPSRD = replace(NUMEMPSRD, NUMEMPSRD == -10, 500), # For 2023 the value for top coding was -10, else 500
#          EMPSTATRD = replace(EMPSTATRD, EMPSTATRD %in% c(2, 3), 1),
#          EMPSTATRD = replace(EMPSTATRD, EMPSTATRD == 0, 4), # NIU -> not employed
#          EMPSTATRD = replace(EMPSTATRD, EMPSTATRD %in% c(7, 8, 9), NA_real_),
#          NUMEMPSRD = replace(NUMEMPSRD, NUMEMPSRD %in% c(994, 997, 998, 999), NA_real_),
#          STUDENT = replace(STUDENT, STUDENT == 6, 3), # NIU seen as no students
#          STUDENT = factor(
#            ifelse(STUDENT %in% c(1, 2), "Student", "Not student"),
#            levels = c("Not student", "Student")
#          )) %>%
#   group_by(MEPSID, YEAR) %>%
#   mutate(
#     all_na_EMPSTATRD = any(is.na(EMPSTATRD)) & !any(EMPSTATRD == 1, na.rm = TRUE),
#     all_na_NUMEMPS = all(is.na(NUMEMPSRD)) & !any(NUMEMPSRD >= 1, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   filter(!all_na_EMPSTATRD, !all_na_NUMEMPS) %>%
#   select(-all_na_EMPSTATRD, -all_na_NUMEMPS)
