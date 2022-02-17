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

#Note: This code creates columns named NPCVBC, MPCBBC, etc. 

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

# *-----------------------------------------------------------------------*
#   * PART 3b: NO PRENATAL CARE CODES *
#   *-----------------------------------------------------------------------*
#   * *
#   * Distinguishing between no prenatal care (0) and missing prenatal *
#   * care (.) is difficult because of known inconsistent coding *
#   * practices used on various state and national data sets. The *
#   * difficulty is most evident in the coding of month prenatal care *
#   * visits began, wherein the code "0" (MPCBBC=0) may have multiple *
#   * meanings. By contrast, the code "0" for number of PNC visits *
#   * (NPCVBC=0) generally means no PNC. *
#   * *
#   * From this revision (3) onward, the default assumption *
#   * of the APNCU Index is that no PNC will be assumed if: *
#   * 1. the number of PNC visits (NPCVBC) = 0 AND month PNC *
#   * began (MPCBBC) = 0 or . (missing), OR *
#   * 2. the month PNC began (MPCBBC) = 0 AND number of visits *
#   * (NPCVBC) = 0 or . (missing) *
#   * *
#   * The default assumptions are written into the program. *
#   * If the default assumptions are not valid for your data set, *
#   * change the SAS code below to accommodate the distinctive coding *
#   * pattern for NO PNC in your data set. *
#   * *
#   *-----------------------------------------------------------------------*
#   
#   *** Each of the next 4 questions is followed by a line of SAS code.
# Edit each line of SAS code as follows:
#   If the answer is YES- remove asterisk from SAS code for the item.
# If the answer is NO- do not remove asterisk from SAS code for the item.
# *------------------------------------------------------------------------*
#   * (1) Does the coding MPCBBC = 0 indicate that PNC began in the zero *
#   * month of PNC (e.g., PNC began before month one)? * #Note: Yes
#   *------------------------------------------------------------------------*
dataset[, MPCBBC :=
          ifelse(MPCBBC == 0, 1, MPCBBC)] #Turns all 0s into 1s

# *------------------------------------------------------------------------*
#   * (2) Does the coding MPCBBC = 0 indicate that PNC began in month 10 or *
#   * later? * #Note: No
#   *------------------------------------------------------------------------*
#   *** Edit next line as needed;
# * IF MPCBBC = 0 THEN MPCBBC = 10;
# *------------------------------------------------------------------------*
#   * (3) Does the coding MPCBBC = 0 indicate that data for month PNC began *
#   * are unknown or missing? * #Note: No
#   *------------------------------------------------------------------------*
#   *** Edit next line as needed;
# * IF MPCBBC = 0 THEN MPCBBC = . ;
# *------------------------------------------------------------------------*
#   * (4) Does the coding NPCVBC = 0 indicate that the number of PNC *
#   * visits = unknown or missing? *
#   *------------------------------------------------------------------------*
#   *** Edit next line as needed;
# * IF NPCVBC = 0 THEN NPCVBC = . ;
# *** End of user edits;

#Note: All missing values were defined in Step 3A of code translation

# *-----------------------------------------------------------------------*
#   * RECODES FOR NO PRENATAL CARE CODES *
#   *-----------------------------------------------------------------------*

dataset[, NPCVBC :=
          ifelse((NPCVBC == 0 & MPCBBC >= 1) | (MPCBBC == 0 & NPCVBC >= 1), NA, NPCVBC)]

dataset[, MPCBBC :=
          ifelse((NPCVBC == 0 & MPCBBC >= 1) | (MPCBBC == 0 & NPCVBC >= 1), NA, MPCBBC)]

#Note: Code takes care of invalid combinations of number of PNC visits and month PNC began, vars recoded to NA

dataset[, MPCBBC :=
          ifelse(NPCVBC == 0 & is.null(MPCBBC), 0, MPCBBC)] #If number of visits = 0 and month PNC began is NA, PNC recoded as 0

dataset[, NPCVBC :=
          ifelse(MPCBBC == 0 & is.null(NPCVBC), 0, NPCVBC)] #Number of visits is NA, number of visits recoded to 0
