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
combined_2019_2020_sample <- rbind(data_us_2019_sample, data_us_2020_sample)

#Missing column imputation for 2018 dataset and 2019/2020 combined dataset
setdiff(colnames(data_us_2018), colnames(combined_2019_2020_sample))
setdiff(colnames(combined_2019_2020_sample), colnames(data_us_2018))

combined_2019_2020_sample[, FBRACE := NA]
combined_2019_2020_sample[, PWGT_R := NA]
combined_2019_2020_sample[, IP_HEPATB := NA]
combined_2019_2020_sample[, IP_HEPATC := NA]
combined_2019_2020_sample[, ME_PRES := NA]
combined_2019_2020_sample[, ME_ROUT := NA]
combined_2019_2020_sample[, ME_TRIAL := NA]
combined_2019_2020_sample[, F_MM_ := NA]
combined_2019_2020_sample[, F_AB_NICU := NA]
combined_2019_2020_sample[, CA_DOWNS := NA]
combined_2019_2020_sample[, UBFACIL := NA]
combined_2019_2020_sample[, URF_DIAB := NA]
combined_2019_2020_sample[, URF_CHYPER := NA]
combined_2019_2020_sample[, URF_PHYPER := NA]
combined_2019_2020_sample[, URF_ECLAM := NA]
combined_2019_2020_sample[, UME_FORCEP := NA]
combined_2019_2020_sample[, UME_VAC := NA]
combined_2019_2020_sample[, UOP_INDUC := NA]
combined_2019_2020_sample[, ULD_BREECH := NA]
combined_2019_2020_sample[, UCA_ANEN := NA]
combined_2019_2020_sample[, UCA_SPINA := NA]
combined_2019_2020_sample[, UCA_OMPHA := NA]
combined_2019_2020_sample[, UCA_CLEFTLP := NA]
combined_2019_2020_sample[, UCA_HERNIA := NA]
combined_2019_2020_sample[, UCA_DOWNS := NA]

data_us_2018_sample[, MHISPX := NA]
data_us_2018_sample[, FHISPX := NA]
data_us_2018_sample[, F_FEDUC := NA]
data_us_2018_sample[, F_RF_INFT := NA]
data_us_2018_sample[, OBSTETRIC_PROCEDURES := NA]
data_us_2018_sample[, CHARACTERISTICS_OF_LABOR_AND_DELIVERY := NA]
data_us_2018_sample[, MATERNAL_MORBIDITY := NA]
data_us_2018_sample[, F_MM__PLAC := NA]
data_us_2018_sample[, ABNORMAL_CONDITIONS_OF_THE_NEWBORN := NA]
data_us_2018_sample[, F_AB_NIUC := NA]
data_us_2018_sample[, CA_DOWN := NA]
data_us_2018_sample[, OCTERR := NA]
data_us_2018_sample[, OCNTYFIPS := NA]
data_us_2018_sample[, OCNTYPOP := NA]
data_us_2018_sample[, MBCNTRY := NA]
data_us_2018_sample[, MRCNTRY := NA]
data_us_2018_sample[, MRTERR := NA]
data_us_2018_sample[, RCNTY := NA]
data_us_2018_sample[, RCNTY_POP := NA]
data_us_2018_sample[, RCITY_POP := NA]
data_us_2018_sample[, RECTYPE := NA]
data_us_2018_sample[, RISK_FACTORS := NA]
data_us_2018_sample[, INFECTIONS_PRESENT := NA]
data_us_2018_sample[, IP_HEPB := NA]
data_us_2018_sample[, IP_HEPC := NA]
data_us_2018_sample[, CONGENITAL_ANOMALIES_OF_THE_NEWBORN_ := NA]

#Forms full dataset with all 3 years (2018-2020)
full_dataset_sample <- rbind(combined_2019_2020_sample, data_us_2018_sample)

