#Load packages
library(easypackages)
libraries("car", "data.table", "dplyr", "formattable", "foreign", "forcats", "formattable", 
          "ggplot2", "here", "knitr", "qwraps2", "RcmdrMisc", "tableone", "tidyr", 
          "sjPlot", "sjmisc", "sjlabelled", "tidyverse", "weights", "tableone")

dataset <- data_us_2020_sample
# *-----------------------------------------------------------------------*
#   * PART 1: VARIABLE DEFINITIONS *
#   *-----------------------------------------------------------------------*
#   * *
#   * FROM BIRTH CERTIFICATE DATA FILE: *
#   * *
#   * NPCVBC Number of prenatal care visits, from birth certificate *
#   * MPCBBC Month prenatal care visits began, from birth *
#   * certificate *
#   * SEXBC Sex of infant, from birth certificate *
#   * GAGEBC Gestational age, from birth certificate *
#   * BWGRAMS Birth weight in grams *
#   
#   * *
#   * CALCULATED WITHIN THIS SAS PROGRAM: *
#   * *
#   * UEXPVIS Unadjusted expected prenatal care visits *
#   * EXPVIS Expected prenatal care visits *
#   * EVRATIO Expected visit ratio (observed/expected ratio) *
#   * EVINDEX Expected visit index (received PNC service index) *
#   * MOINDEX4 Month prenatal care initiation index *
#   * INDEXSUM Two factor summary index *
#   * NOPNC No prenatal care received *
#   * *
#   * IMPUTED WITHIN THIS SAS PROGRAM WHEN NECESSARY DATA ARE
# MISSING *
#   * *
#   * GESTIMP Gestational age imputation marker *
#   * *
#   *-----------------------------------------------------------------------*;
# *-----------------------------------------------------------------------*
#   * PART 2: DATA INPUT *
#   *-----------------------------------------------------------------------*
#   * *
#   * This initial part of the program receives the input data to be *
#   * analyzed. Each user of this program must adapt this section to *
#   * his/her own data set and data input files. *
#   * *
#   * The critical variables needed to be imputed for this program *
#   * include: number of prenatal care visits, month of first prenatal *
#   * care visit, gestational age (date of birth minus date of last *
#                                    * menstrual period / 7 days/wk), birth weight (in grams), and sex. *
#   * Other variables may be added to allow for analysis of adequacy of *
#   * prenatal care utilization results stratified by those variables. *
#   * *
#   * Specifically set the following variables to be equal to your *
#   * data base's variable names. *
# * *
# *-----------------------------------------------------------------------*;
# *** Substitute your equivalent variable name here;

NPCVBC <- dataset$PREVIS
MPCBBC <- dataset$PRECARE
SEXBC <- dataset$SEX
GAGEBC <- dataset$COMBGEST
BWGRAMS <- dataset$DBWT

# *-----------------------------------------------------------------------*
#   * PART 3: ACCEPTABLE DATA VALUES *
#   *-----------------------------------------------------------------------*
#   * PART 3a: RANGE EDITS *
#   *-----------------------------------------------------------------------*
#   * *
#   * This section sets limits on the acceptable values of the *
#   
#   * above variables. All unacceptable values are made blank. *
#   * This program assumes all missing and incorrect data are *
#   * blank ( . ) *
#   * *
#   *-----------------------------------------------------------------------*;

dataset[, NPCVBC :=
          ifelse(PREVIS < 0 | PREVIS > 90, NA, PREVIS)] #Acceptable Values 0-90

dataset[, MPCBBC :=
          ifelse(PRECARE < 0 | PRECARE > 10, NA, PRECARE)] #Acceptable Values 0-10

dataset[, GAGEBC :=
          ifelse(COMBGEST < 18 | COMBGEST > 50, NA, COMBGEST)] #Acceptable values 18-50

dataset[, MPCBBC :=
          ifelse(is.null(GAGEBC) & (MPCBBC > (GAGEBC/4)), NA, MPCBBC)] #Omits inconsistent values of MPCBBC OR GAGEBC

dataset[, SEXBC :=
          ifelse(SEX == 'M', 1, 2)] #Acceptable values 1 (MALE),2 (FEMALE) - Unknown SEX = FEMALE (2)

dataset[, BWGRAMS :=
          ifelse(DBWT < 400 | DBWT > 6000, NA, DBWT)] #Acceptable values 400-6000

#Note (2/15): Resume code translation from Part 3b of document

