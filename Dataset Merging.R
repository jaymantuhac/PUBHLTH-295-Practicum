#Load packages
library(easypackages)
libraries("car", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")


#Ensures all names are uppercase for consistency
colnames(data_us_2018) <- toupper(colnames(data_us_2018))
colnames(data_us_2019) <- toupper(colnames(data_us_2019)) 
colnames(data_us_2020) <- toupper(colnames(data_us_2020))


#Find mismatching columns
setdiff(colnames(data_us_2018), colnames(data_us_2019)) #Gives list of columns present in 2nd dataset, but not the other

setdiff(colnames(data_us_2019), colnames(data_us_2020))
setdiff(colnames(data_us_2020), colnames(data_us_2019))

#Mitigating column name differences 2019 vs 2020 dataset
data_us_2020[, MBRACE := NA]
data_us_2020[, MHISPX := NA]
data_us_2020[, M_HT_IN := NA]
data_us_2020[, F_ME_PRES := NA]
data_us_2020[, F_ME_ROUT := NA]
data_us_2020[, F_ME_TRIAL := NA]
data_us_2020[, F_MM__PLAC := NA]
data_us_2020[, F_MM_RUPT := NA]
data_us_2020[, F_MM_UHYST := NA]
data_us_2020[, F_MM_AICU := NA]
data_us_2020[, SETORDER_R := NA]
data_us_2020[, CA_DOWN := NA]
data_us_2020[, CA_DISOR := NA]
data_us_2020[, CA_HYPO := NA]
data_us_2020[, F_CA_LIMB := NA]
data_us_2020[, F_CA_CLEFTLP := NA]
data_us_2020[, BFED := NA]

data_us_2019[, OCTERR := NA]
data_us_2019[, OCNTYFIPS := NA]
data_us_2019[, OCNTYPOP := NA]
data_us_2019[, MBCNTRY := NA]
data_us_2019[, MRCNTRY := NA]
data_us_2019[, MRTERR := NA]
data_us_2019[, RCNTY := NA]
data_us_2019[, RCNTY_POP := NA]
data_us_2019[, RCITY_POP := NA]
data_us_2019[, RECTYPE := NA]
data_us_2019[, MRACEIMP := NA]
data_us_2019[, MAR_IMP := NA]
data_us_2019[, RISK_FACTORS := NA]
data_us_2019[, RF_PPTERM := NA]
data_us_2019[, F_RF_PDIAB := NA]
data_us_2019[, F_RF_GDIAB := NA]
data_us_2019[, F_RF_PHYPER := NA]
data_us_2019[, F_RF_GHYPER := NA]
data_us_2019[, F_RF_ECLAMP := NA]
data_us_2019[, F_RF_PPB := NA]
data_us_2019[, IP_GON := NA]
data_us_2019[, INFECTIONS_PRESENT := NA]
data_us_2019[, IP_SYPH := NA]
data_us_2019[, IP_CHLAM := NA]
data_us_2019[, IP_HEPB := NA]
data_us_2019[, IP_HEPC := NA]
data_us_2019[, F_IP_GONOR := NA]
data_us_2019[, F_IP_SYPH := NA]
data_us_2019[, F_IP_CHLAM := NA]
data_us_2019[, F_IP_HEPATB := NA]
data_us_2019[, F_IP_HEPATC := NA]
data_us_2019[, NO_INFEC := NA]
data_us_2019[, DLMP_YY := NA]
data_us_2019[, CA_ANEN := NA]
data_us_2019[, CONGENITAL_ANOMALIES_OF_THE_NEWBORN_ := NA]
data_us_2019[, CA_MNSB := NA]
data_us_2019[, CA_CCHD := NA]
data_us_2019[, CA_CDH := NA]
data_us_2019[, CA_OMPH := NA]
data_us_2019[, CA_GAST := NA]
data_us_2019[, F_CA_ANEN := NA]
data_us_2019[, F_CA_MENIN := NA]
data_us_2019[, F_CA_HEART := NA]
data_us_2019[, F_CA_HERNIA := NA]
data_us_2019[, F_CA_OMPHA := NA]
data_us_2019[, F_CA_GASTRO := NA]

#Combine 2019 and 2020 datasets with imputed columns
combined_2019_2020_data <- rbind(data_us_2019, data_us_2020)

#Missing column imputation for 2018 dataset and 2019/2020 combined dataset
setdiff(colnames(data_us_2018), colnames(combined_2019_2020_data))
setdiff(colnames(combined_2019_2020_data), colnames(data_us_2018))

combined_2019_2020_data[, FBRACE := NA]
combined_2019_2020_data[, PWGT_R := NA]
combined_2019_2020_data[, IP_HEPATB := NA]
combined_2019_2020_data[, IP_HEPATC := NA]
combined_2019_2020_data[, ME_PRES := NA]
combined_2019_2020_data[, ME_ROUT := NA]
combined_2019_2020_data[, ME_TRIAL := NA]
combined_2019_2020_data[, F_MM_ := NA]
combined_2019_2020_data[, F_AB_NICU := NA]
combined_2019_2020_data[, CA_DOWNS := NA]
combined_2019_2020_data[, UBFACIL := NA]
combined_2019_2020_data[, URF_DIAB := NA]
combined_2019_2020_data[, URF_CHYPER := NA]
combined_2019_2020_data[, URF_PHYPER := NA]
combined_2019_2020_data[, URF_ECLAM := NA]
combined_2019_2020_data[, UME_FORCEP := NA]
combined_2019_2020_data[, UME_VAC := NA]
combined_2019_2020_data[, UOP_INDUC := NA]
combined_2019_2020_data[, ULD_BREECH := NA]
combined_2019_2020_data[, UCA_ANEN := NA]
combined_2019_2020_data[, UCA_SPINA := NA]
combined_2019_2020_data[, UCA_OMPHA := NA]
combined_2019_2020_data[, UCA_CLEFTLP := NA]
combined_2019_2020_data[, UCA_HERNIA := NA]
combined_2019_2020_data[, UCA_DOWNS := NA]

data_us_2018[, MHISPX := NA]
data_us_2018[, FHISPX := NA]
data_us_2018[, F_FEDUC := NA]
data_us_2018[, F_RF_INFT := NA]
data_us_2018[, OBSTETRIC_PROCEDURES := NA]
data_us_2018[, CHARACTERISTICS_OF_LABOR_AND_DELIVERY := NA]
data_us_2018[, MATERNAL_MORBIDITY := NA]
data_us_2018[, F_MM__PLAC := NA]
data_us_2018[, ABNORMAL_CONDITIONS_OF_THE_NEWBORN := NA]
data_us_2018[, F_AB_NIUC := NA]
data_us_2018[, CA_DOWN := NA]
data_us_2018[, OCTERR := NA]
data_us_2018[, OCNTYFIPS := NA]
data_us_2018[, OCNTYPOP := NA]
data_us_2018[, MBCNTRY := NA]
data_us_2018[, MRCNTRY := NA]
data_us_2018[, MRTERR := NA]
data_us_2018[, RCNTY := NA]
data_us_2018[, RCNTY_POP := NA]
data_us_2018[, RCITY_POP := NA]
data_us_2018[, RECTYPE := NA]
data_us_2018[, RISK_FACTORS := NA]
data_us_2018[, INFECTIONS_PRESENT := NA]
data_us_2018[, IP_HEPB := NA]
data_us_2018[, IP_HEPC := NA]
data_us_2018[, CONGENITAL_ANOMALIES_OF_THE_NEWBORN_ := NA]

#Forms full dataset with all 3 years (2018-2020)
full_dataset <- rbind(combined_2019_2020_data, data_us_2018)

write.csv(full_dataset, "/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay's Practicum /Data/2018_2020_combined_data.csv ")

