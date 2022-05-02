#Load packages
library(easypackages)
libraries("car", "crosstable", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "pander", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")


#Access Pacific Islander Samples data + generate 2018-2020 combined PI sample data
data_2018_restricted_PI <- data_2018_restricted %>% filter(between(mrace15, 11, 14))
write.csv(data_2018_restricted_PI, '/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/natl2018us_PIsample.csv')

data_2019_restricted_PI <- data_2019_restricted %>% filter(between(mrace15, 11, 14))
write.csv(data_2019_restricted_PI, '/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/natl2019us_PIsample.csv')

data_2020_restricted_PI <- data_2020_restricted %>% filter(between(mrace15, 11, 14))
write.csv(data_2020_restricted_PI, '/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/natl2020us_PIsample.csv')

data_2018_2019_restricted_PI <- rbind(data_2018_restricted_PI, data_2019_restricted_PI)
data_2018_2020_restricted_PI <- rbind(data_2018_2019_restricted_PI, data_2020_restricted_PI)
write.csv(data_2018_2020_restricted_PI, '/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/restricted_2018_2020_PIsample.csv')


#Recode Gestational Diabetes Column
data_us_2018_2020_combined_PI[, GESTATIONAL_DIABETES :=
                                ifelse(RF_GDIAB == "Y", 1, 
                                       ifelse(RF_GDIAB == "N", 0, NA))]

#APCUI Measurement Construction

NPCVBC <- data_us_2018_2020_combined_PI$PREVIS
MPCBBC <- data_us_2018_2020_combined_PI$PRECARE
SEXBC <- data_us_2018_2020_combined_PI$SEX
GAGEBC <- data_us_2018_2020_combined_PI$COMBGEST
BWGRAMS <- data_us_2018_2020_combined_PI$DBWT

data_us_2018_2020_combined_PI[, NPCVBC :=
                                ifelse(PREVIS < 0 | PREVIS > 90, NA, PREVIS)] #Acceptable Values 0-90

data_us_2018_2020_combined_PI[, MPCBBC :=
                                ifelse(PRECARE < 0 | PRECARE > 10, NA, PRECARE)] #Acceptable Values 0-10

data_us_2018_2020_combined_PI[, GAGEBC :=
                                ifelse(COMBGEST < 18 | COMBGEST > 50, NA, COMBGEST)] #Acceptable values 18-50

data_us_2018_2020_combined_PI[, MPCBBC :=
                                ifelse(is.na(GAGEBC) & (MPCBBC > (GAGEBC/4)), NA, MPCBBC)] #Omits inconsistent values of MPCBBC OR GAGEBC

data_us_2018_2020_combined_PI[, SEXBC :=
                                ifelse(SEX == 'M', 1, 2)] #Acceptable values 1 (MALE),2 (FEMALE) - Unknown SEX = FEMALE (2)

data_us_2018_2020_combined_PI[, BWGRAMS :=
                                ifelse(DBWT < 400 | DBWT > 6000, NA, DBWT)] #Acceptable values 400-6000

data_us_2018_2020_combined_PI[, MPCBBC :=
                                ifelse(MPCBBC == 0, 1, MPCBBC)] #Turns all 0s into 1s

data_us_2018_2020_combined_PI[, NPCVBC :=
                                ifelse((NPCVBC == 0 & MPCBBC >= 1) | (MPCBBC == 0 & NPCVBC >= 1), NA, NPCVBC)]

data_us_2018_2020_combined_PI[, MPCBBC :=
                                ifelse((NPCVBC == 0 & MPCBBC >= 1) | (MPCBBC == 0 & NPCVBC >= 1), NA, MPCBBC)]

#Note: Code takes care of invalid combinations of number of PNC visits and month PNC began, vars recoded to NA

data_us_2018_2020_combined_PI[, MPCBBC :=
                                ifelse(NPCVBC == 0 & is.na(MPCBBC), 0, MPCBBC)] #If number of visits = 0 and month PNC began is NA, PNC recoded as 0

data_us_2018_2020_combined_PI[, NPCVBC :=
                                ifelse(MPCBBC == 0 & is.na(NPCVBC), 0, NPCVBC)] #Number of visits is NA, number of visits recoded to 0

data_us_2018_2020_combined_PI[, GESTIMP :=
                                ifelse(GAGEBC >= 18 & GAGEBC <= 50, 1, 2)] # 1 = NOT IMPUTED, 2 = IMPUTED

#Imputes GAGEBC when SEXBC == 1 for GAGEBC 22-30
data_us_2018_2020_combined_PI[, GAGEBC :=
                                ifelse(SEXBC == 1 & BWGRAMS >= 530 & BWGRAMS <= 608, 22,
                                       ifelse(SEXBC == 1 & BWGRAMS >= 609 & BWGRAMS <= 698, 23,
                                              ifelse(SEXBC == 1 & BWGRAMS >= 699 & BWGRAMS <= 799, 24,
                                                     ifelse(SEXBC == 1 & BWGRAMS >= 800 & BWGRAMS <= 912, 25,
                                                            ifelse(SEXBC == 1 & BWGRAMS >= 913 & BWGRAMS <= 1040, 26,
                                                                   ifelse(SEXBC == 1 & BWGRAMS >= 1041 & BWGRAMS <= 1183, 27,
                                                                          ifelse(SEXBC == 1 & BWGRAMS >= 1184 & BWGRAMS <= 1342, 28,
                                                                                 ifelse(SEXBC == 1 & BWGRAMS >= 1343 & BWGRAMS <= 1536, 29,
                                                                                        ifelse(SEXBC == 1 & BWGRAMS <= 1537 & BWGRAMS <= 1751, 30, NA)))))))))]

#Imputes GAGEBC when SEXBC == 1 for GAGEBC 31-40
data_us_2018_2020_combined_PI[, GAGEBC :=
                                ifelse(SEXBC == 1 & BWGRAMS >= 1752 & BWGRAMS <= 1978, 31,
                                       ifelse(SEXBC == 1 & BWGRAMS >= 1979 & BWGRAMS <= 2219, 32,
                                              ifelse(SEXBC == 1 & BWGRAMS >= 2200 & BWGRAMS <= 2458, 33,
                                                     ifelse(SEXBC == 1 & BWGRAMS >= 2459 & BWGRAMS <= 2693, 34,
                                                            ifelse(SEXBC == 1 & BWGRAMS >= 2694 & BWGRAMS <= 2909, 35,
                                                                   ifelse(SEXBC == 1 & BWGRAMS >= 2910 & BWGRAMS <= 3111, 36,
                                                                          ifelse(SEXBC == 1 & BWGRAMS >= 3112 & BWGRAMS <= 3291, 37,
                                                                                 ifelse(SEXBC == 1 & BWGRAMS >= 3292 & BWGRAMS <= 3433, 38,
                                                                                        ifelse(SEXBC == 1 & BWGRAMS >= 3434 & BWGRAMS <= 3533, 39,
                                                                                               ifelse(SEXBC == 1 & BWGRAMS >= 3534 & BWGRAMS <= 6000, 40, GAGEBC))))))))))]

#Imputes GAGEBC when SEXBC == 2 for GAGEBC 22-30
data_us_2018_2020_combined_PI[, GAGEBC :=
                                ifelse(SEXBC == 2 & BWGRAMS >= 496 & BWGRAMS <= 568, 22,
                                       ifelse(SEXBC == 2 & BWGRAMS >= 569 & BWGRAMS <= 650, 23,
                                              ifelse(SEXBC == 2 & BWGRAMS >= 651 & BWGRAMS <= 744, 24,
                                                     ifelse(SEXBC == 2 & BWGRAMS >= 745 & BWGRAMS <= 849, 25,
                                                            ifelse(SEXBC == 2 & BWGRAMS >= 850 & BWGRAMS <= 968, 26,
                                                                   ifelse(SEXBC == 2 & BWGRAMS >= 969 & BWGRAMS <= 1101, 27,
                                                                          ifelse(SEXBC == 2 & BWGRAMS >= 1102 & BWGRAMS <= 1251, 28,
                                                                                 ifelse(SEXBC == 2 & BWGRAMS >= 1252 & BWGRAMS <= 1429, 29,
                                                                                        ifelse(SEXBC == 2 & BWGRAMS >= 1430 & BWGRAMS <= 1636, 30, GAGEBC)))))))))]

#Imputes GAGEBC when SEXBC == 2 for GAGEBC 31-40
data_us_2018_2020_combined_PI[, GAGEBC :=
                                ifelse(SEXBC == 2 & BWGRAMS >= 1637 & BWGRAMS <= 1860, 31,
                                       ifelse(SEXBC == 2 & BWGRAMS >= 1861 & BWGRAMS <= 2089, 32,
                                              ifelse(SEXBC == 2 & BWGRAMS >= 2090 & BWGRAMS <= 2328, 33,
                                                     ifelse(SEXBC == 2 & BWGRAMS >= 2329 & BWGRAMS <= 2561, 34,
                                                            ifelse(SEXBC == 2 & BWGRAMS >= 2562 & BWGRAMS <= 2787, 35,
                                                                   ifelse(SEXBC == 2 & BWGRAMS >= 2788 & BWGRAMS <= 2991, 36,
                                                                          ifelse(SEXBC == 2 & BWGRAMS >= 2992 & BWGRAMS <= 3160, 37,
                                                                                 ifelse(SEXBC == 2 & BWGRAMS >= 3161 & BWGRAMS <= 3293, 38,
                                                                                        ifelse(SEXBC == 2 & BWGRAMS >= 3294 & BWGRAMS <= 3388, 39,
                                                                                               ifelse(SEXBC == 2 & BWGRAMS >= 3389 & BWGRAMS <= 6000, 40, GAGEBC))))))))))]



data_us_2018_2020_combined_PI[, MOINDEX4 := 
                                ifelse(is.na(MPCBBC) | is.na(NPCVBC) | NPCVBC == 0, 0, 0)]

data_us_2018_2020_combined_PI[, MOINDEX4 :=
                                ifelse(MPCBBC >= 1 & MPCBBC <= 2, 4,
                                       ifelse(MPCBBC >= 3 & MPCBBC <= 4, 3,
                                              ifelse(MPCBBC >= 5 & MPCBBC <= 6, 2, 1)))]

#Initialize expected and unadjusted expected visits
data_us_2018_2020_combined_PI[, `:=` (
  EXPVIS = NA,
  UEXPVIS = NA
)]

data_us_2018_2020_combined_PI[, UEXPVIS:=
                                ifelse(GAGEBC >= 35, (GAGEBC-35)+9,
                                       ifelse(GAGEBC == 34, 9,
                                              ifelse(GAGEBC >= 32, 8, 
                                                     ifelse(GAGEBC >= 30, 7,
                                                            ifelse(GAGEBC >= 26, 6,
                                                                   ifelse(GAGEBC >= 22, 5, 
                                                                          ifelse(GAGEBC >= 18, 4,
                                                                                 ifelse(GAGEBC >= 14, 3,
                                                                                        ifelse(GAGEBC >= 10, 2,
                                                                                               ifelse(GAGEBC >= 6, 1, 0))))))))))]

#Final step of expected visits calculation adjusts for month of prenatal care initiation
data_us_2018_2020_combined_PI[, EXPVIS := 
                                ifelse(is.na(MPCBBC) | MPCBBC ==0, UEXPVIS,
                                       ifelse(MPCBBC == 10, UEXPVIS - 17,
                                              ifelse(MPCBBC == 9, UEXPVIS - 13,
                                                     ifelse(MPCBBC == 8, UEXPVIS - 9,
                                                            ifelse(MPCBBC == 7, UEXPVIS - 7,
                                                                   ifelse(MPCBBC == 6, UEXPVIS - 6,
                                                                          ifelse(MPCBBC == 5, UEXPVIS - 5,
                                                                                 ifelse(MPCBBC == 4, UEXPVIS - 3,
                                                                                        ifelse(MPCBBC == 3, UEXPVIS - 2,
                                                                                               ifelse(MPCBBC == 2, UEXPVIS - 1,
                                                                                                      ifelse(MPCBBC == 1, UEXPVIS, 1)))))))))))]

#Calculation of observed/expected ratio (expected visits ratio)
data_us_2018_2020_combined_PI[, EVRATIO := (NPCVBC/EXPVIS) * 100]

data_us_2018_2020_combined_PI[, EVINDEX := 
                                ifelse(is.na(EVRATIO), 0,
                                       ifelse(is.na(MPCBBC), 0,
                                              ifelse(EVRATIO > 109.99, 4,
                                                     ifelse(EVRATIO > 79.99 & EVRATIO <= 109.99, 3,
                                                            ifelse(EVRATIO > 49.99 & EVRATIO <= 79.99, 2,
                                                                   ifelse(EVRATIO <= 49.99, 1, NA))))))]

#   * Coding: 1=INADEQUATE 2=INTERMEDIATE 3=ADEQUATE 4=ADEQUATE
# PLUS *
#   * 0=MISSING INFORMATION *

data_us_2018_2020_combined_PI[, INDEXSUM := NA]
data_us_2018_2020_combined_PI[, INDEXSUM :=
                                ifelse(EVINDEX == 0 | MOINDEX4 == 0, 0,
                                       ifelse(EVINDEX == 1 | (MOINDEX4 >= 1 & MOINDEX4 <= 2), 1,
                                              ifelse(EVINDEX == 3 & (MOINDEX4 >= 3 & MOINDEX4 <= 4), 3,
                                                     ifelse(EVINDEX == 4 & (MOINDEX4 >= 3 & MOINDEX4 <= 4), 4, 2))))]


#Create variable to flag observations with no PNC
#Coding: 1 = no PNC, 2 = some PNC, NA = no PNC
data_us_2018_2020_combined_PI[, NOPNC :=
                                ifelse((NPCVBC == 0 & (MPCBBC == 0 | is.na(MPCBBC))) | (MPCBBC == 0 & (NPCVBC == 0 | is.na(NPCVBC))), 1,
                                       ifelse(is.na(NPCVBC) | is.na(MPCBBC), NA, 2))]

#Rename column labels for readability

data_us_2018_2020_combined_PI <- rename(data_us_2018_2020_combined_PI, Number_of_prenatal_care_visits = NPCVBC,
                                        Month_prenatal_care_visits_began = MPCBBC,
                                        Sex_of_infant = SEXBC,
                                        Gestational_age_in_weeks = GAGEBC,
                                        Birth_weight_in_grams = BWGRAMS,
                                        Unadjusted_expected_prenatal_care_visits = UEXPVIS,
                                        Expected_prenatal_care_visits = EXPVIS,
                                        Expected_visit_ratio = EVRATIO,
                                        Expected_visit_index = EVINDEX,
                                        Month_prenatal_care_initiation_index = MOINDEX4,
                                        Two_factor_summary_index = INDEXSUM,
                                        Gestational_age_imputation_marker = GESTIMP,
                                        No_prenatal_care_received = NOPNC)

#Recode Race/Ethnicity Variable
data_2018_2020_restricted[, RACE_CAT :=
               ifelse(MRACEHISP == 1, "Non-Hispanic White",
                      ifelse(MRACEHISP == 2, "Non-Hispanic Black",
                             ifelse(MRACEHISP == 3, "Non-Hispanic AIAN",
                                    ifelse(MRACEHISP == 4, "Non-Hispanic Asian",
                                           ifelse(MRACEHISP == 5, "Non-Hispanic Native Hawaiian/Pacific Islander",
                                                  ifelse(MRACEHISP == 7, "Hispanic", "Other"))))))]

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

data_us_2018_2020_combined_PI$INSURANCE <- as.factor(data_us_2018_2020_combined_PI$INSURANCE)
levels(data_us_2018_2020_combined_PI$INSURANCE) <- c("Self-Pay", "Medicaid",
                                                     "Private Insurance", "Other")

#Received WIC Recoding
data_us_2018_2020_combined_PI[, WIC_R :=
                                ifelse(WIC == "Y", 1,
                                       ifelse(WIC == "N", 0, NA))]

#Education Recoding
data_us_2018_2020_combined_PI[, EDUCATION_RECODE :=
                                ifelse(MEDUC == 1 | MEDUC == 2, "Some high school or less",
                                       ifelse(MEDUC == 3, "High school graduate",
                                              ifelse(MEDUC != 9, "Any college", NA)))]


#Advanced age recoding
data_us_2018_2020_combined_PI[, ADVANCED_AGE :=
                                ifelse(MAGER >= 35, 1, 0)]

#Descriptive statistics/crosstabs
gestational_diabetes_xtab <- data_us_2018_2020_combined_PI %>% 
  group_by_at(c("RACE_RECODE")) %>% 
  summarize_at(c("GESTATIONAL_DIABETES"), 
               list(mean =~ mean(.), sd =~ sd(.)))
gestational_diabetes_xtab

bmi25_xtab <- data_us_2018_2020_combined_PI %>% 
  group_by_at(c("RACE_RECODE")) %>% 
  summarize_at(c("BMI25"), 
               list(mean =~ mean(.), sd =~ sd(.)))
bmi25_xtab

bmi30_xtab <- data_us_2018_2020_combined_PI %>% 
  group_by_at(c("RACE_RECODE")) %>% 
  summarize_at(c("BMI30"), 
               list(mean =~ mean(.), sd =~ sd(.)))
bmi30_xtab

inadequate_prenatal_care_xtab <- data_us_2018_2020_combined_PI %>% 
  group_by_at(c("RACE_RECODE")) %>% 
  summarize_at(c("INADEQUATE_PRENATAL_CARE"), 
               list(mean =~ mean(.), sd =~ sd(.)))
inadequate_prenatal_care_xtab

WIC_xtab <- data_us_2018_2020_combined_PI %>% 
  group_by_at(c("RACE_RECODE")) %>% 
  summarize_at(c("WIC_R"), 
               list(mean =~ mean(.), sd =~ sd(.)))
WIC_xtab

advanced_age_xtab <- data_us_2018_2020_combined_PI %>% 
  group_by_at(c("RACE_RECODE")) %>% 
  summarize_at(c("ADVANCED_AGE"), 
               list(mean =~ mean(.), sd =~ sd(.)))
advanced_age_xtab

data_us_2018_2020_combined_PI %>% 
  ggplot(aes(x = factor(EDUCATION_RECODE))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1)

data_us_2018_2020_combined_PI %>% 
  ggplot(aes(x = factor(INADEQUATE_PRENATAL_CARE))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1)

summary(lm(GESTATIONAL_DIABETES ~ EDUCATION_RECODE, data = data_us_2018_2020_combined_PI))

#Provides means of categorical variables (and other variables that can't get means/sd through methods above)
prop.table(table(data_us_2018_2020_combined_PI$RACE_RECODE, data_us_2018_2020_combined_PI$INSURANCE), 1)
prop.table(table(data_us_2018_2020_combined_PI$RACE_RECODE, data_us_2018_2020_combined_PI$EDUCATION_RECODE), 1)
prop.table(table(data_us_2018_2020_combined_PI$RACE_RECODE, data_us_2018_2020_combined_PI$WIC_R), 1)
prop.table(table(data_us_2018_2020_combined_PI$RACE_RECODE, data_us_2018_2020_combined_PI$GESTATIONAL_DIABETES), 1)

tapply(data_us_2018_2020_combined_PI$GESTATIONAL_DIABETES, data_us_2018_2020_combined_PI$INSURANCE, mean)

#Logistic Regression Model
model1 <- glm(GESTATIONAL_DIABETES ~ RACE_RECODE + RF_PDIAB +
                BMI25 + BMI30 + INADEQUATE_PRENATAL_CARE +
                INSURANCE + WIC + EDUCATION_RECODE + MAGER, data = data_us_2018_2020_combined_PI,
              family = binomial(link = "logit"))
summary(model1)
tab_model(model1)

model2 <- glm(GESTATIONAL_DIABETES ~ RACE_RECODE + 
                BMI25 + BMI30 + INADEQUATE_PRENATAL_CARE +
                INSURANCE + WIC + EDUCATION_RECODE + ADVANCED_AGE, data = data_us_2018_2020_combined_PI,
              family = binomial(link = "logit"))
summary(model2)
tab_model(model2)