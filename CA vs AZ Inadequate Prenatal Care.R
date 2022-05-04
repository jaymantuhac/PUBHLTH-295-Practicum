#Load packages
library(easypackages)
libraries("car", "crosstable", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "pander", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")

#Install packages again due to R update
#install.packages(c('crosstable', 'formattable', 'here', 'pander', 'qwraps2', 'RcmdrMisc',
#                   'tableone', 'weights'))

data_2018_2020_restricted_Asian[, INADEQUATE_SERVICES_RECEIVED := 
                                  ifelse(Expected_visit_index == 1 | Expected_visit_index == 2, 1,
                                         ifelse(Expected_visit_index == 3 | Expected_visit_index == 4, 0, NA))]

data_2018_2020_restricted_Asian[, FOREIGN_BORN :=
                                  ifelse(MBSTATE_REC == 1, 0,
                                         ifelse(MBSTATE_REC == 2, 1, NA))]

data_2018_2020_restricted_Asian[, FIRST_ORDER_PARITY :=
                                  ifelse(TBO_REC == 1, 1, 
                                         ifelse(TBO_REC != 9, 0, NA))]

#Change Reference levels
data_2018_2020_restricted_Asian$EDUCATION_RECODE <- factor(data_2018_2020_restricted_Asian$EDUCATION_RECODE, ordered = FALSE)
data_2018_2020_restricted_Asian$EDUCATION_RECODE <- relevel(data_2018_2020_restricted_Asian$EDUCATION_RECODE, ref = "Some high school or less")

data_2018_2020_restricted_Asian$INSURANCE <- factor(data_2018_2020_restricted_Asian$INSURANCE, ordered = FALSE)
data_2018_2020_restricted_Asian$INSURANCE <- relevel(data_2018_2020_restricted_Asian$INSURANCE, ref = "Private Insurance")


#Extract CA vs. AZ datasets
data_2018_2020_restricted_AZ <- data_2018_2020_restricted_Asian %>% subset(MRTERR == "AZ")
data_2018_2020_restricted_CA <- data_2018_2020_restricted_Asian %>% subset(MRTERR == "CA")


#Inadequate Services Received Models
modelUS <- glm(INADEQUATE_SERVICES_RECEIVED ~ AZN_ETHNICITY_RECODE +  FOREIGN_BORN +
                 ADVANCED_AGE + FIRST_ORDER_PARITY + EDUCATION_RECODE + 
                 WIC_R + INSURANCE,data = data_2018_2020_restricted_Asian,
              family = binomial(link = "logit"))
summary(modelUS)

modelCA <- glm(INADEQUATE_SERVICES_RECEIVED ~ AZN_ETHNICITY_RECODE +  FOREIGN_BORN +
                 ADVANCED_AGE + FIRST_ORDER_PARITY + EDUCATION_RECODE + 
                 WIC_R + INSURANCE,data = data_2018_2020_restricted_CA,
               family = binomial(link = "logit"))
summary(modelCA)

modelAZ <- glm(INADEQUATE_SERVICES_RECEIVED ~ AZN_ETHNICITY_RECODE +  FOREIGN_BORN +
                 ADVANCED_AGE + FIRST_ORDER_PARITY + EDUCATION_RECODE + 
                 WIC_R + INSURANCE,data = data_2018_2020_restricted_AZ,
               family = binomial(link = "logit"))
summary(modelAZ)

tab_model(modelUS, modelCA, modelAZ,
          dv.labels = c("US Model", "CA Model", "AZ Model"))
