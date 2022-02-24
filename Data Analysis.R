#Load packages
library(easypackages)
libraries("car", "crosstable", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "gmodels", "here", "knitr", "pander", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")

dataset[, race_cat :=
               ifelse(MRACEHISP == 1, "Non-Hispanic White",
                      ifelse(MRACEHISP == 2, "Non-Hispanic Black",
                             ifelse(MRACEHISP == 3, "Non-Hispanic AIAN",
                                    ifelse(MRACEHISP == 4, "Non-Hispanic Asian",
                                           ifelse(MRACEHISP == 5, "Non-Hispanic Native Hawaiian/Pacific Islander",
                                                  ifelse(MRACEHISP == 7, "Hispanic", "Other"))))))]

#APCUI Cross Tab by Race
race_crosstab <- table(dataset$race_cat, dataset$Two_factor_summary_index)
ftable(race_crosstab)
summary(race_crosstab)

race_crosstab2 <- crosstable(dataset, Two_factor_summary_index, by=race_cat)
race_crosstab2

race_crosstab3 <- pander(CrossTable(dataset$race_cat, dataset$Two_factor_summary_index,
                             digits = 2, format = 'SAS'))

race_crosstab4 <- dataset %>% 
  group_by(race_cat, Two_factor_summary_index) %>% 
  tally() %>% 
  spread(race_cat, n)
race_crosstab4

race_crosstab_prop <- prop.table(race_crosstab, 1)
race_crosstab_prop

write.table(race_crosstab_prop, file = "race_crosstab_prop.txt", sep = ",")

#Note: Reporting results for 2/24 meeting, use race_crosstab and race_crosstab_prop


