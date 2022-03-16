#Load packages
library(easypackages)
libraries("car", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")

#Load datasets
data_us_2018 <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2018 Natality Data/natl2018us.csv')
data_us_2018 <- data.table(data_us_2018)
data_us_2018_sample <- data.table(head(data_us_2018, 100))

data_us_2019 <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2019 Natality Data/birth_2019_nber_us.csv')
data_us_2019 <- data.table(data_us_2019)
data_us_2019_sample <- data.table(head(data_us_2019, 100))

data_us_2020 <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2020 Natality Data/birth_2020_nber_us.csv')
data_us_2020 <- data.table(data_us_2020)
data_us_2020_sample <- data.table(head(data_us_2020, 100))

#Ensures all names are uppercase for consistency
colnames(data_us_2018_sample) <- toupper(colnames(data_us_2018_sample))
colnames(data_us_2019_sample) <- toupper(colnames(data_us_2019_sample)) 
colnames(data_us_2020_sample) <- toupper(colnames(data_us_2020_sample))

colnames(data_us_2018) <- toupper(colnames(data_us_2018))
colnames(data_us_2019) <- toupper(colnames(data_us_2019)) 
colnames(data_us_2020) <- toupper(colnames(data_us_2020))


#Find mismatching columns
setdiff(colnames(data_us_2018), colnames(data_us_2019)) #Gives list of columns present in 2nd dataset, but not the other

setdiff(colnames(data_us_2019), colnames(data_us_2020))
setdiff(colnames(data_us_2020), colnames(data_us_2019))

#Mitigating column name differences 2019 vs 2020 dataset
data_us_2020_sample[, MBRACE := NA]
data_us_2020_sample[, MHISPX := NA]
data_us_2020_sample[, M_HT_IN := NA]
data_us_2020_sample[, F_ME_PRES := NA]
data_us_2020_sample[, F_ME_ROUT := NA]
data_us_2020_sample[, F_ME_TRIAL := NA]
data_us_2020_sample[, F_MM__PLAC := NA]
data_us_2020_sample[, F_MM_RUPT := NA]
data_us_2020_sample[, F_MM_UHYST := NA]
data_us_2020_sample[, F_MM_AICU := NA]
data_us_2020_sample[, SETORDER_R := NA]
data_us_2020_sample[, CA_DOWN := NA]
data_us_2020_sample[, CA_DISOR := NA]
data_us_2020_sample[, CA_HYPO := NA]
data_us_2020_sample[, F_CA_LIMB := NA]
data_us_2020_sample[, F_CA_CLEFTLP := NA]
data_us_2020_sample[, BFED := NA]

data_us_2019_sample[, OCTERR := NA]
data_us_2019_sample[, OCNTYFIPS := NA]
data_us_2019_sample[, OCNTYPOP := NA]
data_us_2019_sample[, MBCNTRY := NA]
data_us_2019_sample[, MRCNTRY := NA]
data_us_2019_sample[, MRTERR := NA]
data_us_2019_sample[, RCNTY := NA]
data_us_2019_sample[, RCNTY_POP := NA]
data_us_2019_sample[, RCITY_POP := NA]
data_us_2019_sample[, RECTYPE := NA]
data_us_2019_sample[, MRACEIMP := NA]
data_us_2019_sample[, MAR_IMP := NA]
data_us_2019_sample[, RISK_FACTORS := NA]
data_us_2019_sample[, RF_PPTERM := NA]
data_us_2019_sample[, F_RF_PDIAB := NA]
data_us_2019_sample[, F_RF_GDIAB := NA]
data_us_2019_sample[, F_RF_PHYPER := NA]
data_us_2019_sample[, F_RF_GHYPER := NA]
data_us_2019_sample[, F_RF_ECLAMP := NA]
data_us_2019_sample[, F_RF_PPB := NA]
data_us_2019_sample[, IP_GON := NA]
data_us_2019_sample[, INFECTIONS_PRESENT := NA]
data_us_2019_sample[, IP_SYPH := NA]
data_us_2019_sample[, IP_CHLAM := NA]
data_us_2019_sample[, IP_HEPB := NA]
data_us_2019_sample[, IP_HEPC := NA]
data_us_2019_sample[, F_IP_GONOR := NA]
data_us_2019_sample[, F_IP_SYPH := NA]
data_us_2019_sample[, F_IP_CHLAM := NA]
data_us_2019_sample[, F_IP_HEPATB := NA]
data_us_2019_sample[, F_IP_HEPATC := NA]
data_us_2019_sample[, NO_INFEC := NA]
data_us_2019_sample[, DLMP_YY := NA]
data_us_2019_sample[, CA_ANEN := NA]
data_us_2019_sample[, CONGENITAL_ANOMALIES_OF_THE_NEWBORN_ := NA]
data_us_2019_sample[, CA_MNSB := NA]
data_us_2019_sample[, CA_CCHD := NA]
data_us_2019_sample[, CA_CDH := NA]
data_us_2019_sample[, CA_OMPH := NA]
data_us_2019_sample[, CA_GAST := NA]
data_us_2019_sample[, F_CA_ANEN := NA]
data_us_2019_sample[, F_CA_MENIN := NA]
data_us_2019_sample[, F_CA_HEART := NA]
data_us_2019_sample[, F_CA_HERNIA := NA]
data_us_2019_sample[, F_CA_OMPHA := NA]
data_us_2019_sample[, F_CA_GASTRO := NA]

#Combine 2019 and 2020 datasets with imputed columns
combined_2019_2020_dataset <- rbind(data_us_2019_sample, data_us_2020_sample)

#Note (3/15): Repeat column imputation/combination above for 2018 dataset and 2019/2020 combined dataset
setdiff(colnames(data_us_2018), colnames(combined_2019_2020_dataset))
setdiff(colnames(combined_2019_2020_dataset), colnames(data_us_2018))
