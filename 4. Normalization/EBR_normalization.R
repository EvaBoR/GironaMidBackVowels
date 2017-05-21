#############################
### NORMALIZATION PROTOCOL
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 24/11/2015
###
### The script takes the file obtained from 'EBR_initial_organization.R', containing the raw formant values obtained
### from all survey points, after outlier removal, and:
### 1) creates a subset data frame in the NORM required format
### 2) performs normalization using the Bark Difference, the Lobanov, the Nearey1, and the modified
### Watt and Fabricius methods from the package 'vowels'
### (see http://ncslaap.lib.ncsu.edu/tools/norm/ and http://blogs.uoregon.edu/vowels/)
### 3) creates 5 files: a file with the normalized values for each method and a file where
### the new columns are added to the initial file, so that all unnormalized and normalized values are
### in a single file.
###
### Input (INPUT): output of 'EBR_initial_organization.R' ("TOT_formants_revisats.txt") once outliers are removed
###
### Output:
### - "TOT_bark.txt": output from norm.bark, the normalized values in the NORM file format
### - "TOT_lobanov.txt": output from norm.lobanov, the normalized values in the NORM file format
### - "TOT_nearey1.txt": output from norm.nearey, the normalized values in the NORM file format
### - "TOT_mWattFabricius.txt": output from norm.wattfabricius, the normalized v. in the NORM file f.
### - "TOT_normalitzat.txt": Bark, Lobanov, Nearey1 and modified Watt and Fabricius normalized values
### added to tab-separated file obtained from 'EBR_initial_organization.R'
###
#################################

## CLEAN UP
rm(list=ls(all=TRUE)) # clean work space
library(pacman) #unload loaded packages, to prevent incompatibilities
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)


################################
## STUFF THAT NEEDS TO BE SET ##
################################

  ## COMPLETE PATH TO THE FILE TO BE PROCESSED
  INPUT = "/path/to/input/file/formants_nooutliers.txt"

  ## SET OUTPUT DIRECTORY AND FILE NAMES
  OUTPUT = "/path/where/output/files/willgo/" # directory
  OUTPUT.bark.filename = "TOT_bark.txt" # filenames
  OUTPUT.lobanov.filename = "TOT_lobanov.txt"
  OUTPUT.mWF.filename = "TOT_mWattFabricius.txt"
  OUTPUT.nearey1.filename = "TOT_nearey1.txt"
  OUTPUT.all.filename = "TOT_normalitzat.txt"

###########
## START ##
###########

## LOAD "VOWELS" R PACKAGE
library(vowels)

source("/path/to/FUNCTION_normformat.R") # function to create data frame following NORM formant requirements

    ## read INPUT file
    form.data = read.delim(INPUT, header=TRUE, sep = "\t")
    form.data = form.data[order(form.data$INFORMANT),]


    ## CREATE DATA FRAME THAT CAN BE USED WITH THE "VOWELS" PACKAGE:
      form.subdata = normformat(form.data, values="Hz")

        ###########################
        #### BARK NORMALIZATION ###
        ###########################

        ## NORMALIZE USING THE BARK DIFFERENCE METHOD
        bark.form.subdata =norm.bark(form.subdata)

        ## CHANGE COLUMN NAMES TO MAKE THEM MORE READABLE
        colnames(bark.form.subdata)[(names(bark.form.subdata) == "Z3-Z1")] = "Bark.Z3-Z1"
        colnames(bark.form.subdata)[(names(bark.form.subdata) == "Z3-Z2")] = "Bark.Z3-Z2"
        colnames(bark.form.subdata)[(names(bark.form.subdata) == "Z2-Z1")] = "Bark.Z2-Z1"

        ## WRITE RESULTS TABLE
        write.table(bark.form.subdata, file=paste(OUTPUT, OUTPUT.bark.filename, sep = ""), quote=FALSE, sep="\t", row.names=FALSE)

        ## SUBSET COLUMNS WITH NORMALIZED VALUES
        bark.normcols = bark.form.subdata[,4:6]

        ##############################
        #### LOBANOV NORMALIZATION ###
        ##############################

        ## NORMALIZE USING THE LOBANOV METHOD
        lobanov.form.subdata = norm.lobanov(form.subdata)

        ## CHANGE COLUMN NAMES TO MAKE THEM MORE READABLE
        colnames(lobanov.form.subdata)[(names(lobanov.form.subdata) == "F*1")] = "Lobanov.F1"
        colnames(lobanov.form.subdata)[(names(lobanov.form.subdata) == "F*2")] = "Lobanov.F2"

        ## WRITE RESULTS TABLE
        write.table(lobanov.form.subdata, file=paste(OUTPUT, OUTPUT.lobanov.filename, sep = ""), quote=FALSE, sep="\t", row.names=FALSE)

        ## SUBSET COLUMNS WITH NORMALIZED VALUES
        lobanov.normcols = lobanov.form.subdata[,4:5]

        ################################################
        #### MODIFIED WATT & FABRICIUS NORMALIZATION ###
        ################################################

        ## NORMALIZE USING THE MODIFIED WATT AND FABRICIUS METHOD (Fabricius, Watt & Johnson, 2009)
        mWattFabricius.form.subdata = norm.wattfabricius(form.subdata, norm.means=FALSE, mod.WF=TRUE)

        ## CHANGE COLUMN NAMES TO MAKE THEM MORE READABLE
        colnames(mWattFabricius.form.subdata)[(names(mWattFabricius.form.subdata) == "F1/S(F1)")] = "mW&F.F1"
        colnames(mWattFabricius.form.subdata)[(names(mWattFabricius.form.subdata) == "F2/S(F2)")] = "mW&F.F2"

        ## WRITE RESULTS TABLE
        write.table(mWattFabricius.form.subdata, file=paste(OUTPUT, OUTPUT.mWF.filename, sep = ""), quote=FALSE, sep="\t", row.names=FALSE)

        ## SUBSET COLUMNS WITH mW&F NORMALIZED VALUES
        mWattFabricius.normcols = mWattFabricius.form.subdata[,4:5]


        ##############################
        #### NEAREY1 NORMALIZATION ###
        ##############################

        ## NORMALIZE USING THE NEAREY1 METHOD (Adank et al., 2004) WITH F3 VALUES (removing them does not seem to have any impact)
        nearey1.form.subdata =norm.nearey(form.subdata, formant.int=TRUE, use.f3=TRUE)

        ## CHANGE COLUMN NAMES TO MAKE THEM MORE READABLE
        colnames(nearey1.form.subdata)[(names(nearey1.form.subdata) == "F*1")] = "Nearey1.F1"
        colnames(nearey1.form.subdata)[(names(nearey1.form.subdata) == "F*2")] = "Nearey1.F2"
        colnames(nearey1.form.subdata)[(names(nearey1.form.subdata) == "F*3")] = "Nearey1.F3"

        ## WRITE RESULTS TABLE
        write.table(nearey1.form.subdata, file=paste(OUTPUT, OUTPUT.nearey1.filename, sep = ""), quote=FALSE, sep="\t", row.names=FALSE)

        ## SUBSET COLUMNS WITH NEAREY1 NORMALIZED VALUES
        nearey1.normcols = nearey1.form.subdata[,4:6]


  ## ADD NORMALIZED F. VALUES TO INITIAL DATAFRAME (keep columns 1 to 25, add 3 f.norm columns for Bark values, 2 f.norm columns for Lobanov values, 2 f.norm columns for mW&F values, 3 f.norm columns for Nearey1 values, and add the rest of the columns, 26 to 31)
  norm.data = cbind(form.data[,1:25], bark.normcols, lobanov.normcols, mWattFabricius.normcols, nearey1.normcols, form.data[,26:31])

  ## CORRECTIONS: CORRECT N.CONTEXT FOR WORDS BELONGING TO TWO TESTS (bota, T1&T2, and pobre, popa, febre, and pipa, T1&T3)
  #norm.data[norm.data$N.CONTEXT=="1+14" & norm.data$TEST=="T1",]$N.CONTEXT = "1"
  #norm.data[norm.data$N.CONTEXT=="1+14" & norm.data$TEST=="T3",]$N.CONTEXT = "14"
  #norm.data[norm.data$N.CONTEXT=="3+10",]$N.CONTEXT = "3"
  #norm.data[norm.data$SÍL.posició.=="SI/SM" & norm.data$FONEMA_ANT.=="tɾ", ]$SÍL.posició. = "SI"

  ## WRITE THE NEW "ALL DATA" FILE
  write.table(norm.data, file=paste(OUTPUT, OUTPUT.all.filename, sep = ""), quote=FALSE, sep="\t", row.names=FALSE)


