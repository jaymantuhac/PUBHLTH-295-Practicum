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
          ifelse(is.na(GAGEBC) & (MPCBBC > (GAGEBC/4)), NA, MPCBBC)] #Omits inconsistent values of MPCBBC OR GAGEBC

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
          ifelse(NPCVBC == 0 & is.na(MPCBBC), 0, MPCBBC)] #If number of visits = 0 and month PNC began is NA, PNC recoded as 0

dataset[, NPCVBC :=
          ifelse(MPCBBC == 0 & is.na(NPCVBC), 0, NPCVBC)] #Number of visits is NA, number of visits recoded to 0

# *-----------------------------------------------------------------------*
#   * PART 4: GESTATIONAL AGE EQUIVALENCE *
#   *-----------------------------------------------------------------------*
#   * *
#   * Because gestational age is so frequently missing from birth *
#   * certificates, a gestational age equivalent is imputed from *
#   * birth weight (only for those records with gestational age *
#                     * missing). *
#   * *
#   * The following table shows the sex-linked conversion. A *
#   * variable called GESTIMP has been created to indicate when *
#   * a substitution is made (GESTATION IMPUTED 1=NO/2=YES). *
#   * *
#   *-----------------------------------------------------------------------*

dataset[, GESTIMP :=
          ifelse(GAGEBC >= 18 & GAGEBC <= 50, 1, 2)] # 1 = NOT IMPUTED, 2 = IMPUTED

#Note: This section of code is broken down into 4 sections for cleanliness, readability, and easier debugging 
#Note: Original SAS code displayed the following code as 1 long section

#Imputes GAGEBC when SEXBC == 1 for GAGEBC 22-30
dataset[, GAGEBC :=
          ifelse(SEXBC == 1 & BWGRAMS >= 530 & BWGRAMS <= 608, 22,
                 ifelse(SEXBC == 1 & BWGRAMS >= 609 & BWGRAMS <= 698, 23,
                        ifelse(SEXBC == 1 & BWGRAMS >= 699 & BWGRAMS <= 799, 24,
                               ifelse(SEXBC == 1 & BWGRAMS >= 800 & BWGRAMS <= 912, 25,
                                      ifelse(SEXBC == 1 & BWGRAMS >= 913 & BWGRAMS <= 1040, 26,
                                             ifelse(SEXBC == 1 & BWGRAMS >= 1041 & BWGRAMS <= 1183, 27,
                                                    ifelse(SEXBC == 1 & BWGRAMS >= 1184 & BWGRAMS <= 1342, 28,
                                                           ifelse(SEXBC == 1 & BWGRAMS >= 1343 & BWGRAMS <= 1536, 29,
                                                                  ifelse(SEXBC == 1 & BWGRAMS <= 1537 & BWGRAMS <= 1751, 30, NA)))))))))]

#Imputes GAGEBC when SEXBC == 1 for GAGEBC 31-40
dataset[, GAGEBC :=
          ifelse(SEXBC == 1 & BWGRAMS >= 1752 & BWGRAMS <= 1978, 31,
                 ifelse(SEXBC == 1 & BWGRAMS >= 1979 & BWGRAMS <= 2219, 32,
                        ifelse(SEXBC == 1 & BWGRAMS >= 2200 & BWGRAMS <= 2458, 33,
                               ifelse(SEXBC == 1 & BWGRAMS >= 2459 & BWGRAMS <= 2693, 34,
                                      ifelse(SEXBC == 1 & BWGRAMS >= 2694 & BWGRAMS <= 2909, 35,
                                             ifelse(SEXBC == 1 & BWGRAMS >= 2910 & BWGRAMS <= 3111, 36,
                                                    ifelse(SEXBC == 1 & BWGRAMS >= 3112 & BWGRAMS <= 3291, 37,
                                                           ifelse(SEXBC == 1 & BWGRAMS >= 3292 & BWGRAMS <= 3433, 38,
                                                                  ifelse(SEXBC == 1 & BWGRAMS >= 3434 & BWGRAMS <= 3533, 39,
                                                                         ifelse(SEXBC == 1 & BWGRAMS >= 3534 & BWGRAMS <= 6000, 40, GAGEBC))))))))))]

#Imputes GAGEBC when SEXBC == 2 for GAGEBC 22-30
dataset[, GAGEBC :=
          ifelse(SEXBC == 2 & BWGRAMS >= 496 & BWGRAMS <= 568, 22,
                 ifelse(SEXBC == 2 & BWGRAMS >= 569 & BWGRAMS <= 650, 23,
                        ifelse(SEXBC == 2 & BWGRAMS >= 651 & BWGRAMS <= 744, 24,
                               ifelse(SEXBC == 2 & BWGRAMS >= 745 & BWGRAMS <= 849, 25,
                                      ifelse(SEXBC == 2 & BWGRAMS >= 850 & BWGRAMS <= 968, 26,
                                             ifelse(SEXBC == 2 & BWGRAMS >= 969 & BWGRAMS <= 1101, 27,
                                                    ifelse(SEXBC == 2 & BWGRAMS >= 1102 & BWGRAMS <= 1251, 28,
                                                           ifelse(SEXBC == 2 & BWGRAMS >= 1252 & BWGRAMS <= 1429, 29,
                                                                  ifelse(SEXBC == 2 & BWGRAMS >= 1430 & BWGRAMS <= 1636, 30, GAGEBC)))))))))]

#Imputes GAGEBC when SEXBC == 2 for GAGEBC 31-40
dataset[, GAGEBC :=
          ifelse(SEXBC == 2 & BWGRAMS >= 1637 & BWGRAMS <= 1860, 31,
                 ifelse(SEXBC == 2 & BWGRAMS >= 1861 & BWGRAMS <= 2089, 32,
                        ifelse(SEXBC == 2 & BWGRAMS >= 2090 & BWGRAMS <= 2328, 33,
                               ifelse(SEXBC == 2 & BWGRAMS >= 2329 & BWGRAMS <= 2561, 34,
                                      ifelse(SEXBC == 2 & BWGRAMS >= 2562 & BWGRAMS <= 2787, 35,
                                             ifelse(SEXBC == 2 & BWGRAMS >= 2788 & BWGRAMS <= 2991, 36,
                                                    ifelse(SEXBC == 2 & BWGRAMS >= 2992 & BWGRAMS <= 3160, 37,
                                                           ifelse(SEXBC == 2 & BWGRAMS >= 3161 & BWGRAMS <= 3293, 38,
                                                                  ifelse(SEXBC == 2 & BWGRAMS >= 3294 & BWGRAMS <= 3388, 39,
                                                                         ifelse(SEXBC == 2 & BWGRAMS >= 3389 & BWGRAMS <= 6000, 40, GAGEBC))))))))))]

# *-----------------------------------------------------------------------*
#   * PART 5: CALCULATION OF ADEQUACY OF INITIATION *
#   * OF PRENATAL CARE INDEX *
#   *-----------------------------------------------------------------------*
#   * *
#   * This section calculates the adequacy of initiation of *
#   * prenatal care. This section is basically a straightforward *
#   * recoding of month prenatal care began. *
#   * *
#   * Coding: 1=INADEQUATE 2=INTERMEDIATE 3=ADEQUATE 4=ADEQUATE
# PLUS *
#   * 0=MISSING INFORMATION *
#   * *
#   *-----------------------------------------------------------------------*

dataset[, MOINDEX4 := 
          ifelse(is.na(MPCBBC) | is.na(NPCVBC) | NPCVBC == 0, 0, 0)]

dataset[, MOINDEX4 :=
          ifelse(MPCBBC >= 1 & MPCBBC <= 2, 4,
                 ifelse(MPCBBC >= 3 & MPCBBC <= 4, 3,
                        ifelse(MPCBBC >= 5 & MPCBBC <= 6, 2, 1)))]

#Note: Some MOINDEX values show up as NA, ideally should show up as 0s. May need to debug if absolutely necessary.

# *-----------------------------------------------------------------------*
#   * PART 6: EXPECTED VISITS CALCULATIONS AND RECEIVED PRENATAL
# CARE *
#   * SERVICES INDEX *
#   *-----------------------------------------------------------------------*
#   * *
#   * This section calculates the Adequacy of Received Prenatal Care *
#   * Services Index. *
#   * *
#   * Two principal steps are involved. The first step determines the *
#   * EXPECTED VISITS for each pregnancy (which requires establishing *
#                                           
#                                           * the recommended visits for the gestational age and then an *
#                                           * adjustment for the timing of the initiation of care). The second *
#   * step calculates the EXPECTED VISIT RATIO (OBSERVED / EXPECTED) *
#   * which is then directly converted to the Received Prenatal Care *
#   * Services Index. *
#   * *
#   *-----------------------------------------------------------------------*

#Initialize expected and unadjusted expected visits
dataset[, `:=` (
  EXPVIS = NA,
  UEXPVIS = NA
)]

dataset[, UEXPVIS:=
          ifelse(GAGEBC >= 35, (GAGEBC-35)+9,
                 ifelse(GAGEBC == 34, 9,
                        ifelse(GAGEBC >= 32, 8, 
                               ifelse(GAGEBC >= 30, 7,
                                      ifelse(GAGEBC >= 26, 6,
                                             ifelse(GAGEBC >= 22, 5, 
                                                    ifelse(GAGEBC >= 18, 4,
                                                           ifelse(GAGEBC >= 14, 3,
                                                                  ifelse(GAGEBC >= 10, 2,
                                                                         ifelse(GAGEBC >= 6, 1, 0))))))))))]

#Final step of expected visits calculation adjusts for month of prenatal care initiation
dataset[, EXPVIS := 
          ifelse(is.na(MPCBBC) | MPCBBC ==0, UEXPVIS,
                 ifelse(MPCBBC == 10, UEXPVIS - 17,
                        ifelse(MPCBBC == 9, UEXPVIS - 13,
                               ifelse(MPCBBC == 8, UEXPVIS - 9,
                                      ifelse(MPCBBC == 7, UEXPVIS - 7,
                                             ifelse(MPCBBC == 6, UEXPVIS - 6,
                                                    ifelse(MPCBBC == 5, UEXPVIS - 5,
                                                           ifelse(MPCBBC == 4, UEXPVIS - 3,
                                                                  ifelse(MPCBBC == 3, UEXPVIS - 2,
                                                                         ifelse(MPCBBC == 2, UEXPVIS - 1,
                                                                                ifelse(MPCBBC == 1, UEXPVIS, 1)))))))))))]

#Calculation of observed/expected ratio (expected visits ratio)
dataset[, EVRATIO := (NPCVBC/EXPVIS) * 100]

# *** Calculation of adequacy of received service (expected visits) index
# Coding: 1=INADEQUATE 2=INTERMEDIATE 3=ADEQUATE 4=ADEQUATE
# PLUS
# 0=MISSING INFORMATION;

dataset[, EVINDEX := 
          ifelse(is.na(EVRATIO), 0,
                 ifelse(is.na(MPCBBC), 0,
                        ifelse(EVRATIO > 109.99, 4,
                               ifelse(EVRATIO > 79.99 & EVRATIO <= 109.99, 3,
                                      ifelse(EVRATIO > 49.99 & EVRATIO <= 79.99, 2,
                                             ifelse(EVRATIO <= 49.99, 1, NA))))))]

# *-----------------------------------------------------------------------*
#   * PART 7: CALCULATION OF SUMMATIVE TWO FACTOR ADEQUACY OF
# *
#   * PRENATAL CARE UTILIZATION INDEX *
#   *-----------------------------------------------------------------------*
#   * *
#   * This section combines the two previously derived factors, *
#   * Adequacy of Initiation of Prenatal Care Index (MOINDEX4) and *
#   * Adequacy of Received Prenatal Care Services Index (EVINDEX) *
#   * into a single summative Adequacy of Prenatal Care Utilization *
#   * Index. *
#   * *
#   * Coding: 1=INADEQUATE 2=INTERMEDIATE 3=ADEQUATE 4=ADEQUATE
# PLUS *
#   * 0=MISSING INFORMATION *
#   * *
#   *-----------------------------------------------------------------------*


dataset[, INDEXSUM := NA]
dataset[, INDEXSUM :=
          ifelse(EVINDEX == 0 | MOINDEX4 == 0, 0,
                 ifelse(EVINDEX == 1 | (MOINDEX4 >= 1 & MOINDEX4 <= 2), 1,
                        ifelse(EVINDEX == 3 & (MOINDEX4 >= 3 & MOINDEX4 <= 4), 3,
                               ifelse(EVINDEX == 4 & (MOINDEX4 >= 3 & MOINDEX4 <= 4), 4, 2))))]

# *---------------------------------------------------------------------*
#   * PART 8: ADDITIONAL STATISTICAL CALCULATIONS WOULD GO IN *
#   * THIS SECTION *
#   *---------------------------------------------------------------------*

#Create variable to flag observations with no PNC
#Coding: 1 = no PNC, 2 = some PNC, NA = no PNC
dataset[, NOPNC :=
          ifelse((NPCVBC == 0 & (MPCBBC == 0 | is.na(MPCBBC))) | (MPCBBC == 0 & (NPCVBC == 0 | is.na(NPCVBC))), 1,
                 ifelse(is.na(NPCVBC) | is.na(MPCBBC), NA, 2))]

