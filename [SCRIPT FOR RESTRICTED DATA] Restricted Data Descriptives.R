#Load packages
library(easypackages)
libraries("car", "crosstable", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "pander", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")

#Combining Datasets
data_2018_2019_intermediate <- rbind(data_2018_restricted, data_2019_restricted)
data_2018_2020_restricted <- rbind(data_2018_2019_intermediate, data_2020_restricted)
write.csv(data_2018_2020_restricted, '/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/restricted_2018_2020us.csv')

