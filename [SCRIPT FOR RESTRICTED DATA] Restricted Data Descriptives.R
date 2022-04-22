#Load packages
library(easypackages)
libraries("car", "crosstable", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "pander", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")

#Combining Datasets
data_2018_2019_intermediate <- rbind(data_2018_restricted, data_2019_restricted)
data_2018_2020_restricted <- rbind(data_2018_2019_intermediate, data_2020_restricted)
write.csv(data_2018_2020_restricted, '/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/restricted_2018_2020us.csv')

colnames(data_2018_2020_restricted) <- toupper(colnames(data_2018_2020_restricted))


#Recode Gestational Diabetes Column
data_2018_2020_restricted[, GESTATIONAL_DIABETES :=
                                ifelse(RF_GDIAB == "Y", 1, 
                                       ifelse(RF_GDIAB == "N", 0, NA))]

#APCUI Measurement Construction

NPCVBC <- data_2018_2020_restricted$PREVIS
MPCBBC <- data_2018_2020_restricted$PRECARE
SEXBC <- data_2018_2020_restricted$SEX
GAGEBC <- data_2018_2020_restricted$COMBGEST
BWGRAMS <- data_2018_2020_restricted$DBWT

data_2018_2020_restricted[, NPCVBC :=
                                ifelse(PREVIS < 0 | PREVIS > 90, NA, PREVIS)] #Acceptable Values 0-90

data_2018_2020_restricted[, MPCBBC :=
                                ifelse(PRECARE < 0 | PRECARE > 10, NA, PRECARE)] #Acceptable Values 0-10

data_2018_2020_restricted[, GAGEBC :=
                                ifelse(COMBGEST < 18 | COMBGEST > 50, NA, COMBGEST)] #Acceptable values 18-50

data_2018_2020_restricted[, MPCBBC :=
                                ifelse(is.na(GAGEBC) & (MPCBBC > (GAGEBC/4)), NA, MPCBBC)] #Omits inconsistent values of MPCBBC OR GAGEBC

data_2018_2020_restricted[, SEXBC :=
                                ifelse(SEX == 'M', 1, 2)] #Acceptable values 1 (MALE),2 (FEMALE) - Unknown SEX = FEMALE (2)

data_2018_2020_restricted[, BWGRAMS :=
                                ifelse(DBWT < 400 | DBWT > 6000, NA, DBWT)] #Acceptable values 400-6000

data_2018_2020_restricted[, MPCBBC :=
                                ifelse(MPCBBC == 0, 1, MPCBBC)] #Turns all 0s into 1s

data_2018_2020_restricted[, NPCVBC :=
                                ifelse((NPCVBC == 0 & MPCBBC >= 1) | (MPCBBC == 0 & NPCVBC >= 1), NA, NPCVBC)]

data_2018_2020_restricted[, MPCBBC :=
                                ifelse((NPCVBC == 0 & MPCBBC >= 1) | (MPCBBC == 0 & NPCVBC >= 1), NA, MPCBBC)]

#Note: Code takes care of invalid combinations of number of PNC visits and month PNC began, vars recoded to NA

data_2018_2020_restricted[, MPCBBC :=
                                ifelse(NPCVBC == 0 & is.na(MPCBBC), 0, MPCBBC)] #If number of visits = 0 and month PNC began is NA, PNC recoded as 0

data_2018_2020_restricted[, NPCVBC :=
                                ifelse(MPCBBC == 0 & is.na(NPCVBC), 0, NPCVBC)] #Number of visits is NA, number of visits recoded to 0

data_2018_2020_restricted[, GESTIMP :=
                                ifelse(GAGEBC >= 18 & GAGEBC <= 50, 1, 2)] # 1 = NOT IMPUTED, 2 = IMPUTED

#Imputes GAGEBC when SEXBC == 1 for GAGEBC 22-30
data_2018_2020_restricted[, GAGEBC :=
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
data_2018_2020_restricted[, GAGEBC :=
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
data_2018_2020_restricted[, GAGEBC :=
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
data_2018_2020_restricted[, GAGEBC :=
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



data_2018_2020_restricted[, MOINDEX4 := 
                                ifelse(is.na(MPCBBC) | is.na(NPCVBC) | NPCVBC == 0, 0, 0)]

data_2018_2020_restricted[, MOINDEX4 :=
                                ifelse(MPCBBC >= 1 & MPCBBC <= 2, 4,
                                       ifelse(MPCBBC >= 3 & MPCBBC <= 4, 3,
                                              ifelse(MPCBBC >= 5 & MPCBBC <= 6, 2, 1)))]

#Initialize expected and unadjusted expected visits
data_2018_2020_restricted[, `:=` (
  EXPVIS = NA,
  UEXPVIS = NA
)]

data_2018_2020_restricted[, UEXPVIS:=
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
data_2018_2020_restricted[, EXPVIS := 
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
data_2018_2020_restricted[, EVRATIO := (NPCVBC/EXPVIS) * 100]

data_2018_2020_restricted[, EVINDEX := 
                                ifelse(is.na(EVRATIO), 0,
                                       ifelse(is.na(MPCBBC), 0,
                                              ifelse(EVRATIO > 109.99, 4,
                                                     ifelse(EVRATIO > 79.99 & EVRATIO <= 109.99, 3,
                                                            ifelse(EVRATIO > 49.99 & EVRATIO <= 79.99, 2,
                                                                   ifelse(EVRATIO <= 49.99, 1, NA))))))]

#   * Coding: 1=INADEQUATE 2=INTERMEDIATE 3=ADEQUATE 4=ADEQUATE
# PLUS *
#   * 0=MISSING INFORMATION *

data_2018_2020_restricted[, INDEXSUM := NA]
data_2018_2020_restricted[, INDEXSUM :=
                                ifelse(EVINDEX == 0 | MOINDEX4 == 0, 0,
                                       ifelse(EVINDEX == 1 | (MOINDEX4 >= 1 & MOINDEX4 <= 2), 1,
                                              ifelse(EVINDEX == 3 & (MOINDEX4 >= 3 & MOINDEX4 <= 4), 3,
                                                     ifelse(EVINDEX == 4 & (MOINDEX4 >= 3 & MOINDEX4 <= 4), 4, 2))))]


#Create variable to flag observations with no PNC
#Coding: 1 = no PNC, 2 = some PNC, NA = no PNC
data_2018_2020_restricted[, NOPNC :=
                                ifelse((NPCVBC == 0 & (MPCBBC == 0 | is.na(MPCBBC))) | (MPCBBC == 0 & (NPCVBC == 0 | is.na(NPCVBC))), 1,
                                       ifelse(is.na(NPCVBC) | is.na(MPCBBC), NA, 2))]

#Rename column labels for readability

data_2018_2020_restricted <- rename(data_2018_2020_restricted, Number_of_prenatal_care_visits = NPCVBC,
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
data_2018_2020_restricted[, RF_PDIAB :=
                                ifelse(RF_PDIAB == "Y", 1, 
                                       ifelse(RF_PDIAB == "N", 0, NA))]


#BMI Recoding
data_2018_2020_restricted[, BMI25 :=
                                ifelse(BMI_R == 3, 1, 0)]

data_2018_2020_restricted[, BMI30 :=
                                ifelse(between(BMI_R, 4, 6), 1, 0)]

#Inadequate Access to Care (Note: Make sure to run "APCUI Metric Formation" script on dataset before running!)
data_2018_2020_restricted[, INADEQUATE_PRENATAL_CARE :=
                                ifelse(PRECARE5 == 2 | PRECARE5 == 3 | PRECARE5 == 4, 1,
                                       ifelse(PRECARE == 1, 0, NA))]

data_2018_2020_restricted[, ADEQUATE_PRENATAL_CARE := 
                            ifelse(PRECARE5 == 1, 1, 
                                   ifelse(PRECARE5 == 5, NA, 0))]

#Insurance Recoding
data_2018_2020_restricted[, INSURANCE :=
                                ifelse(PAY == 1, "Medicaid",
                                       ifelse(PAY == 2, "Private Insurance",
                                              ifelse(PAY == 3, "Self-Pay",
                                                     ifelse(PAY != 9, "Other", NA))))]

data_2018_2020_restricted$INSURANCE <- as.factor(data_2018_2020_restricted$INSURANCE)
levels(data_2018_2020_restricted$INSURANCE) <- c("Self-Pay", "Medicaid",
                                                     "Private Insurance", "Other")

#Received WIC Recoding
data_2018_2020_restricted[, WIC_R :=
                                ifelse(WIC == "Y", 1,
                                       ifelse(WIC == "N", 0, NA))]

#Education Recoding
data_2018_2020_restricted[, EDUCATION_RECODE :=
                                ifelse(MEDUC == 1 | MEDUC == 2, "Some high school or less",
                                       ifelse(MEDUC == 3, "High school graduate",
                                              ifelse(MEDUC == 4 | MEDUC == 5, "Any college", 
                                                     ifelse(MEDUC != 9, "Bachelors and Beyond", NA))))]

levels(data_2018_2020_restricted$EDUCATION_RECODE) <- c("Bachelors and Beyond", "Any college", "High school graduate", "Some high school or less")


#Advanced age recoding
data_2018_2020_restricted[, ADVANCED_AGE :=
                                ifelse(MAGER >= 35, 1, 0)]

#Asian Ethnicity Recoding
data_2018_2020_restricted[, AZN_ETHNICITY_RECODE :=
                            ifelse(MRACE15 == 4, "Asian Indian",
                                   ifelse(MRACE15 == 5, "Chinese",
                                          ifelse(MRACE15 == 6, "Filipino",
                                                 ifelse(MRACE15 == 7, "Japanese",
                                                        ifelse(MRACE15 == 8, "Korean",
                                                               ifelse(MRACE15 == 9, "Vietnamese",
                                                                      ifelse(MRACE15 == 10, "Other Asian", NA)))))))]

#Asian subsample and ethnicity/education recoding
data_2018_2020_restricted_Asian <- data_2018_2020_restricted %>% subset(RACE_CAT == "Non-Hispanic Asian")

data_2018_2020_restricted_Asian$AZN_ETHNICITY_RECODE <- as.factor(data_2018_2020_restricted_Asian$AZN_ETHNICITY_RECODE)
data_2018_2020_restricted_Asian$AZN_ETHNICITY_RECODE <- relevel(data_2018_2020_restricted_Asian$AZN_ETHNICITY_RECODE, ref = "Chinese")

data_2018_2020_restricted_Asian$EDUCATION_RECODE <- as.factor(data_2018_2020_restricted_Asian$EDUCATION_RECODE)
data_2018_2020_restricted_Asian$EDUCATION_RECODE <- relevel(data_2018_2020_restricted_Asian$EDUCATION_RECODE, ref = "Bachelors and Beyond")

#Extract CA and AZ subsamples of Asian data
data_2018_2020_restricted_AZ <- data_2018_2020_restricted_Asian %>% subset(MRTERR == "AZ")
data_2018_2020_restricted_CA <- data_2018_2020_restricted_Asian %>% subset(MRTERR == "CA")


#Descriptives: Gestational Diabetes
prop.table(table(data_2018_2020_restricted_Asian$GESTATIONAL_DIABETES))
prop.table(table(data_2018_2020_restricted_CA$GESTATIONAL_DIABETES))
prop.table(table(data_2018_2020_restricted_AZ$GESTATIONAL_DIABETES))

#Descriptives: Hepatitis B
prop.table(table(data_2018_2020_restricted_Asian$IP_HEPB))
prop.table(table(data_2018_2020_restricted_CA$IP_HEPATB))
prop.table(table(data_2018_2020_restricted_AZ$IP_HEPATB))

#Descriptives: Ethnicity
prop.table(table(data_2018_2020_restricted_Asian$AZN_ETHNICITY_RECODE))
prop.table(table(data_2018_2020_restricted_CA$AZN_ETHNICITY_RECODE))
prop.table(table(data_2018_2020_restricted_AZ$AZN_ETHNICITY_RECODE))

#Descriptives: Inadequate Prenatal Care
prop.table(table(data_2018_2020_restricted_Asian$INADEQUATE_PRENATAL_CARE))
prop.table(table(data_2018_2020_restricted_CA$INADEQUATE_PRENATAL_CARE))
prop.table(table(data_2018_2020_restricted_AZ$INADEQUATE_PRENATAL_CARE))

#Descriptives: Insurance
prop.table(table(data_2018_2020_restricted_Asian$INSURANCE))
prop.table(table(data_2018_2020_restricted_CA$INSURANCE))
prop.table(table(data_2018_2020_restricted_AZ$INSURANCE))

#Descriptives: WIC
prop.table(table(data_2018_2020_restricted_Asian$WIC_R))
prop.table(table(data_2018_2020_restricted_CA$WIC_R))
prop.table(table(data_2018_2020_restricted_AZ$WIC_R))

#Descriptives: Nativity
prop.table(table(data_2018_2020_restricted_Asian$MBSTATE_REC))
prop.table(table(data_2018_2020_restricted_CA$MBSTATE_REC))
prop.table(table(data_2018_2020_restricted_AZ$MBSTATE_REC))

#Descriptives: Education
prop.table(table(data_2018_2020_restricted_Asian$EDUCATION_RECODE))
prop.table(table(data_2018_2020_restricted_CA$EDUCATION_RECODE))
prop.table(table(data_2018_2020_restricted_AZ$EDUCATION_RECODE))

#Descriptives: Adequate Prenatal Care
prop.table(table(data_2018_2020_restricted_Asian$ADEQUATE_PRENATAL_CARE))
prop.table(table(data_2018_2020_restricted_CA$ADEQUATE_PRENATAL_CARE))
prop.table(table(data_2018_2020_restricted_AZ$ADEQUATE_PRENATAL_CARE))

levels(data_2018_2020_restricted_Asian$EDUCATION_RECODE) <- c("Some high school or less", "High school graduate", "Any college", "Bachelors and Beyond")
levels(data_2018_2020_restricted_CA$EDUCATION_RECODE) <- c("Some high school or less", "High school graduate", "Any college", "Bachelors and Beyond")
levels(data_2018_2020_restricted_AZ$EDUCATION_RECODE) <- c("Some high school or less", "High school graduate", "Any college", "Bachelors and Beyond")



#Logistic Regression Models
model_full <- glm(GESTATIONAL_DIABETES ~ AZN_ETHNICITY_RECODE + 
                BMI25 + BMI30 + INADEQUATE_PRENATAL_CARE +
                INSURANCE + WIC_R + EDUCATION_RECODE + ADVANCED_AGE, data = data_2018_2020_restricted_Asian,
              family = binomial(link = "logit"))
summary(model_full)

model_CA <- glm(GESTATIONAL_DIABETES ~ AZN_ETHNICITY_RECODE + 
                  BMI25 + BMI30 + INADEQUATE_PRENATAL_CARE +
                  INSURANCE + WIC_R + EDUCATION_RECODE + ADVANCED_AGE, data = data_2018_2020_restricted_CA,
                family = binomial(link = "logit"))
summary(model_CA)

model_AZ <- glm(GESTATIONAL_DIABETES ~ AZN_ETHNICITY_RECODE + 
                  BMI25 + BMI30 + INADEQUATE_PRENATAL_CARE +
                  INSURANCE + WIC_R + EDUCATION_RECODE + ADVANCED_AGE, data = data_2018_2020_restricted_AZ,
                family = binomial(link = "logit"))
summary(model_AZ)

tab_model(model_full, model_CA, model_AZ,
          dv.labels = c("US Model", "CA Model", "AZ Model"))

#Diagnostics for INADEQUATE_PRENATAL CARE models
car::vif(model_full)
car::vif(model_CA)
car::vif((model_AZ)) #No issues with multicollinearity

plot(model_full, which = 4, id.n = 3)
plot(model_CA, which = 4, id.n = 3)
plot(model_AZ, which = 4, id.n = 3)


#Models with ADEQUATE_PRENATAL_CARE

#Logistic Regression Models
model_full2 <- glm(GESTATIONAL_DIABETES ~ AZN_ETHNICITY_RECODE + 
                    BMI25 + BMI30 + ADEQUATE_PRENATAL_CARE +
                    INSURANCE + WIC_R + EDUCATION_RECODE + ADVANCED_AGE, data = data_2018_2020_restricted_Asian,
                  family = binomial(link = "logit"))
summary(model_full2)

model_CA2 <- glm(GESTATIONAL_DIABETES ~ AZN_ETHNICITY_RECODE + 
                  BMI25 + BMI30 + ADEQUATE_PRENATAL_CARE +
                  INSURANCE + WIC_R + EDUCATION_RECODE + ADVANCED_AGE, data = data_2018_2020_restricted_CA,
                family = binomial(link = "logit"))
summary(model_CA2)

model_AZ2 <- glm(GESTATIONAL_DIABETES ~ AZN_ETHNICITY_RECODE + 
                  BMI25 + BMI30 + ADEQUATE_PRENATAL_CARE +
                  INSURANCE + WIC_R + EDUCATION_RECODE + ADVANCED_AGE, data = data_2018_2020_restricted_AZ,
                family = binomial(link = "logit"))
summary(model_AZ2)

tab_model(model_full2, model_CA2, model_AZ2,
          dv.labels = c("US Model", "CA Model", "AZ Model"))

#Diagnostics for ADEQUATE_PRENATAL CARE models
car::vif(model_full2)
car::vif(model_CA2)
car::vif((model_AZ2))

plot(model_full2, which = 4, id.n = 3)
plot(model_CA2, which = 4, id.n = 3)
plot(model_AZ2, which = 4, id.n = 3)
