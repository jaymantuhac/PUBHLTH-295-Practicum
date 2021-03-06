#Load packages
library(easypackages)
libraries("car", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")

#Load datasets
data_us_2018 <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2018 Natality Data/natl2018us.csv')
data_us_2018 <- data.table(data_us_2018)
data_us_2018_sample <- data.table(sample_n(data_us_2018, 100))

data_us_2019 <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2019 Natality Data/birth_2019_nber_us.csv')
data_us_2019 <- data.table(data_us_2019)
data_us_2019_sample <- data.table(sample_n(data_us_2019, 100))

data_us_2020 <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2020 Natality Data/birth_2020_nber_us.csv')
data_us_2020 <- data.table(data_us_2020)
data_us_2020_sample <- data.table(sample_n(data_us_2020, 100))

data_us_2018_2020_combined <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2018_2020_combined_data.csv')
data_us_2018_2020_combined <- data.table(data_us_2018_2020_combined)
data_us_2018_2020_combined_sample <- data.table(sample_n(data_us_2018_2020_combined, 1000))

data_us_2018_2020_combined_PI <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2018_2020_PI_sample.csv')
data_us_2018_2020_combined_PI <- data.table(data_us_2018_2020_combined_PI)

data_2018_restricted <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/natl2018us.csv')
data_2018_restricted <- data.table(data_2018_restricted)

data_2019_restricted <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/natl2019us.csv')
data_2019_restricted <- data.table(data_2019_restricted)

data_2020_restricted <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/natl2020us.csv')
data_2020_restricted <- data.table(data_2020_restricted)

data_2018_restricted_PI <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/natl2018us_PIsample.csv')
data_2019_restricted_PI <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/natl2019us_PIsample.csv')
data_2020_restricted_PI <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Restricted Data/natl2020us_PIsample.csv')
