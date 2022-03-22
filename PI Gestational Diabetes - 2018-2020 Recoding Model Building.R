#Load packages
library(easypackages)
libraries("car", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")

#Access Pacific Islander Sample
data_us_2018_2020_combined_sample_PI <- data_us_2018_2020_combined %>% 
  filter(between(MRACE15, 11, 14))

#Recode Gestational Diabetes Column
data_us_2018_2020_combined_sample_PI[, RF_GDIAB :=
                                    ifelse(RF_GDIAB == "Y", 1, 0)]

#Recode Race/Ethnicity Variable
data_us_2018_2020_combined_sample_PI[, RACE_RECODE :=
                                       ifelse(MRACE15 == 11, "Hawaiian", 
                                              ifelse(MRACE15 == 12, "Guamanian",
                                                     ifelse(MRACE15 == 13 | MBSTATE_REC == 1, "Samoan (US Born)",
                                                            ifelse(MRACE15 == 13 | MBSTATE_REC == 2, "Samoan (Non-US Born)",
                                                                   ifelse(MRACE15 == 14 | MBSTATE_REC == 1, "Other Pacific Islander (US Born)",
                                                                          ifelse(MRACE15 == 14 | MBSTATE_REC == 2, "Other Pacific Islander (Non-US Born)", NA))))))]
