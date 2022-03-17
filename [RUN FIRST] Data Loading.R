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

data_us_2018_2020_combined <- data.table(data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2018_2020_combined_data.csv'))
data_us_2018_2020_combined_sample <- data.table(head(data_us_2018_2020_combined, 100))

