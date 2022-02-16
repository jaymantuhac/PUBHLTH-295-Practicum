#Load packages
library(easypackages)
libraries("car", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")


#Loading data
data_us_2019 <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2019 Natality Data/birth_2019_nber_us.csv')
data_us_2019 <- data.table(data_us_2019)

data_us_2020 <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2020 Natality Data/birth_2020_nber_us.csv')
data_us_2020 <- data.table(data_us_2020)

data_ter_2020 <- data.table::fread('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mMR8JnKiUsIMKqX4IrbhCiI5mv5tvNEu/Jay\'s Practicum /Data/2020 Natality Data/birth_2020_nber_ps.csv')
data_ter_2020 <- data.table(data_ter_2020)


#View first 100 rows for viewing purposes (due to how large dataset is - limits what we can see)
data_us_2019_sample <- head(data_us_2019, 100)
data_us_2020_sample <- head(data_us_2020, 5000) #Use for exporting dataset into SAS

#Race Coding (6 categories)
data_us_2019[, race_cat :=
               ifelse(MRACEHISP == 1, "Non-Hispanic White",
                      ifelse(MRACEHISP == 2, "Non-Hispanic Black",
                             ifelse(MRACEHISP == 3, "Non-Hispanic AIAN",
                                    ifelse(MRACEHISP == 4, "Non-Hispanic Asian",
                                           ifelse(MRACEHISP == 5, "Non-Hispanic Native Hawaiian/Pacific Islander",
                                                  ifelse(MRACEHISP == 7, "Hispanic", "Other"))))))]

#Write 2020 sample data as a SAS dataset

write.foreign(data_us_2020_sample, "/Users/jaymantuhac/Downloads/data_us_2020_sample.txt", "/Users/jaymantuhac/Downloads/data_us_2020_sample.sas", package="SAS")
