###########################
# INITIAL DATA ORGANIZATION SCRIPT
#
# Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
# Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
# Date: 24/11/2015
#
# The script lists all .txt files in the input director, reads them and:
# 1) attachs all rows into a new, big data frame.
# 2) Survey point, age group and sex are then extracted from each speaker's code and
# put into new "ARXIPRESTAT", "EDAT", and "SEXE" columns, using RegEx.
# 3) Rows with results from 10% and 90% intervals are removed.
# 4) All variables are put back together into a big data frame.
#
# Input (INPUT.dir): output of all 'EBR_semiautomatic_extractor.praat' (https://osf.io/zqb9a/), revised
# - ("ARXIPRESTAT_formants_revisat.txt")
#
# Output (OUTPUT.dir):
# - "DATAFRAME_formants_revisats.txt": all files from 'EBR_semiautomatic_extractor.praat' (https://osf.io/zqb9a/),
#   revised, merged into a single file (data frame), with ARXIPRESTAT, EDAT and SEXE variables extracted from
#   INFORMANT and put into new columns, INTERVALS "0.10" and "0.90" removed, and all variables that
#   will not be treated as numerical converted into factor vectors with levels.
#
###############################

## CLEAN UP
rm(list=ls())

################################
## STUFF THAT NEEDS TO BE SET ##
################################

  ## SET DIRECTORY OF THE FILES TO BE PROCESSED
  INPUT.dir <- "/Users/pathtofolder/whereyourinputis/"

  ## SET OUTPUT DIRECTORY AND FILE NAME FOR THE FINAL BIG DATA FRAME FILE
  OUTPUT.dir <- "/Users/pathtofolder/whereyouroutput/willgo/"
  OUTPUT.filename <- "TOT_formants_revisats.txt"

################################
## 1. LIST                    ##
################################

  ## PRODUCE CHARACTER VECTOR (LIST) OF THE NAMES OF THE .txt FILES (PATTERN) IN THE DIRECTORY (PATH)
  file_list <- list.files(path=INPUT.dir, pattern="*.txt")


################################
## 2. CREATE BIG DATA FRAME   ##
################################

  ## READ IN EACH .TXT FILE IN file_list AND rbind THEM INTO A DATA FRAME CALLED DATAFRAME
  DATAFRAME <-
    do.call("rbind",
            lapply(file_list,
                   function(x)
                     read.delim(paste(INPUT.dir, x, sep=""))))

######################################################
## 3. EXTRACT SURVEY POINT, AGE AND SEX VARIABLES   ##
######################################################

  # LOAD PACKAGES
  library(dplyr)
  library(tidyr)

  ## EXTRACT SURVEY POINT, AGE AND SEX VARIABLES FROM COLUMN "$INFORMANT" OF THE "DATAFRAME" DATA
  ## FRAME USING THE REGEX WITH THE INFORMANT CODE, AND ADD THE RESULTS FROM EACH SEARCH GROUP TO THE
  ## NEW COLUMNS "ARXIPRESTAT", "EDAT" i "SEXE"; DO NOT REMOVE THE "$INFORMANT" COLUMN (remove=FALSE).

  DATAFRAME <- extract(DATAFRAME, "INFORMANT", c("ARXIPRESTAT", "EDAT", "SEXE"), regex = "(AEI|AEM|AF|BA|CBC|FM|GS|MA|MB|SE|TB|TO)-(FE1|FE2)-(D|H)[1|2|3|4]", remove=FALSE, convert=FALSE)


  ## CONVERT ALL NEW COLUMNS FROM CHARACTER VECTORS INTO FACTOR VECTORS WITH LEVELS
  DATAFRAME$EDAT <- as.factor(DATAFRAME$EDAT)
  DATAFRAME$SEXE <- as.factor(DATAFRAME$SEXE)
  DATAFRAME$ARXIPRESTAT <- as.factor(DATAFRAME$ARXIPRESTAT)


###########################################
## 4. REMOVE ROWS THAT WILL NOT BE USED  ##
###########################################

  ## REMOVE ALL ROWS WITH 10% and 90% INTERVAL RESULTS, SINCE WE DON'T WANT TO USE THEM
  FINAL.DATAFRAME <- DATAFRAME[!grepl(c("0.1|0.9"), DATAFRAME$INTERVAL),]


#################
## 4. TIDY UP  ##
#################

    ## SUBSTITUTE "T3" FOR "T2v" FOR ALL MOT=="sola" (N.MOT=="97") ROWS, TO SIMPLIFY FURTHER SUBSETTING
    index <- grep("TEST", colnames(FINAL.DATAFRAME)) # create index of the columns in which values are to be changed
    FINAL.DATAFRAME$TEST <- as.character(FINAL.DATAFRAME$TEST) #convert TEST column to character vector (factor levels are hard to look into)
    FINAL.DATAFRAME[grepl("sola", FINAL.DATAFRAME$MOT),index]<- "T2" # substitute value in index (FINAL.DATAFRAME$TEST), "T3", for "T2v"
    FINAL.DATAFRAME$TEST <- as.factor(FINAL.DATAFRAME$TEST) #reconvert to factor vector

    ## CONVERT ALL NUM. VARIABLES THAT DO NOT NEED TO BE TREATED AS NUMERICAL INTO FACTOR VECTORS
    FINAL.DATAFRAME$N.MOT <- as.factor(FINAL.DATAFRAME$N.MOT)
    FINAL.DATAFRAME$TOKEN <- as.factor(FINAL.DATAFRAME$TOKEN)
    FINAL.DATAFRAME$INTERVAL <- as.factor(FINAL.DATAFRAME$INTERVAL)
    FINAL.DATAFRAME$LÍMIT.Hz.FORMANTS <- as.factor(FINAL.DATAFRAME$LÍMIT.Hz.FORMANTS)
    FINAL.DATAFRAME$N.FORMANTS <- as.factor(FINAL.DATAFRAME$N.FORMANTS)


###################################################
## 6. WRITE .TXT FILE WITH RESULTING DATA FRAME  ##
###################################################

  write.table(FINAL.DATAFRAME, file=paste(OUTPUT.dir, OUTPUT.filename, sep = ""), quote=FALSE, sep="\t", row.names=FALSE)
