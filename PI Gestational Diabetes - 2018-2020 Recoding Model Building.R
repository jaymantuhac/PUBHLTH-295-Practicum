#Load packages
library(easypackages)
libraries("car", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")

#Access Pacific Islander Sample
data_us_2018_2020_combined_PI <- data_us_2018_2020_combined %>% 
  filter(between(MRACE15, 11, 14))

#Recode Gestational Diabetes Column
data_us_2018_2020_combined_PI[, RF_GDIAB :=
                                    ifelse(RF_GDIAB == "Y", 1, 
                                           ifelse(RF_GDIAB == "N", 0, NA))]

#Recode Race/Ethnicity Variable
data_us_2018_2020_combined_PI[, RACE_RECODE :=
                                       ifelse(MRACE15 == 11, "Hawaiian", 
                                              ifelse(MRACE15 == 12, "Guamanian",
                                                     ifelse(MRACE15 == 13 & MBSTATE_REC == 1, "Samoan (US Born)",
                                                            ifelse(MRACE15 == 13 & MBSTATE_REC == 2, "Samoan (Non-US Born)",
                                                                   ifelse(MRACE15 == 14 & MBSTATE_REC == 1, "Other Pacific Islander (US Born)",
                                                                          ifelse(MRACE15 == 14 & MBSTATE_REC == 2, "Other Pacific Islander (Non-US Born)", NA))))))]

#Pre-pregnancy Diabetes Recoding
data_us_2018_2020_combined_PI[, RF_PDIAB :=
                                       ifelse(RF_PDIAB == "Y", 1, 
                                              ifelse(RF_PDIAB == "N", 0, NA))]

#BMI Recoding
data_us_2018_2020_combined_PI[, BMI25 :=
                                ifelse(BMI_R == 3, 1, 0)]

data_us_2018_2020_combined_PI[, BMI30 :=
                                ifelse(between(BMI_R, 4, 6), 1, 0)]

#Inadequate Access to Care (Note: Make sure to run "APCUI Metric Formation" script on dataset before running!)
data_us_2018_2020_combined_PI[, INADEQUATE_PRENATAL_CARE :=
                                ifelse(Two_factor_summary_index == 1, 1, 0)]

#Insurance Recoding
data_us_2018_2020_combined_PI[, INSURANCE :=
                                ifelse(PAY == 1, "Medicaid",
                                       ifelse(PAY == 2, "Private Insurance",
                                              ifelse(PAY == 3, "Self-Pay",
                                                     ifelse(PAY != 9, "Other", NA))))]

#Received WIC Recoding
data_us_2018_2020_combined_PI[, WIC :=
                                ifelse(WIC == "Y", 1,
                                       ifelse(WIC == "N", 0, NA))]

#Education Recoding
data_us_2018_2020_combined_PI[, EDUCATION_RECODE :=
                                ifelse(MEDUC == 1 | MEDUC == 2, "Some high school or less",
                                       ifelse(MEDUC == 3, "High school graduate",
                                              ifelse(MEDUC != 9, "Any college", NA)))]
#Note: Age variable coded as MAGER
