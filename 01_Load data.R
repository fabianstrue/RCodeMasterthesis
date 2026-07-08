library(ipumsr)
library(dplyr)
library(readr)

setwd("C:/Users/fabia/Documents/#Köln/Uni/Masterarbeit/RCodeMasterthesis")

# Should Health expenses be split by payment source self/insurance
# Health expenses <- Insurance status + education + personal characteristics
# insurance status has hidden factors like how cautious someone is or hidden health status. Instrument to approximate insurance status is needed

month_cols = c("INSJAX", "INSFEX", "INSMAX", "INSAPX", "INSMYX", "INSJUX", "INSJLX", "INSAUX", "INSSEX", "INSOCX", "INSNOX", "INSDEX")

ddi <- read_ipums_ddi("data/meps_00012_2019to2023.xml")
data <- read_ipums_micro(ddi) %>%
  filter(YEAR != 2018) %>%
  select(-INSMMX, -DUIDRD, -PIDRD, -PANELRD, -MEPSIDRD) %>%
  mutate(INSABLEMNS = rowSums(select(., all_of(month_cols)) != 0, na.rm = TRUE),
         INSDMNS = rowSums(select(., all_of(month_cols)) == 2, na.rm = TRUE)) %>%
  mutate(INSABLEMNS = {
    attr(INSABLEMNS, "label") <- "Sum of month person was in universe for insurance (born and alive)"
    INSABLEMNS}) %>%
  mutate(INSDMNS = {
    attr(INSDMNS, "label") <- "Sum of months person had insurance"
    INSDMNS}) %>%
  mutate(INSSHR = INSDMNS/INSABLEMNS) %>%
  mutate(INSSHR = {
    attr(INSSHR, "label") <- "Percentage of in universe months with insurance coverage"
    INSSHR}) %>%
  select(-all_of(month_cols)) %>%
  group_by(DUID) %>%
  mutate(NDUIDMBRS = max(PERNUM)) %>%
  ungroup() %>%
  mutate(NDUIDMBRS = {
    attr(NDUIDMBRS, "label") <- "Number of people within dwelling unit"
    NDUIDMBRS})

rm(ddi, month_cols)
