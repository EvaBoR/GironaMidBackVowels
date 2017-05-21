###########################
# VOWEL DESCRIPTION AND VISUALIZATION
#
# Author: Eva Bosch-Roura (eva.bosch.roura@gmail.com)
# Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
# Date: 26/01/2016
#
# The script loops through the formant data for the whole vowel system in each survey area, computes
# summary statistics with normalized data and unnormalized data by gender, boxplots by gender, and F1xF2 contour
# plots for all speakers in each.
#
# Input (INPUT): data file with all unnormalized and normalized formant values obtained from
# 'EBR_normalization.R' ("TOT_normalitzat.txt")
#
# Output: txt and tex tables of statistics summaries, boxplots, and F1xF2 contour plots for the vowel
# systems of each survey area.
#
###############################

## CLEAN UP
rm(list=ls(all=TRUE)) # clean work space
library(pacman) #unload loaded packages, to prevent incompatibilities
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

################################
## STUFF THAT NEEDS TO BE SET ##
################################

## COMPLETE PATH TO THE FILE TO BE PROCESSED
INPUT = "/path/to/normalizeddata/TOT_normalitzat.txt"

## SET OUTPUT DIRECTORY AND FILE NAMES

  # SUMMARY STATISTICS TABLES
  OUTPUT_MID_T1T3_STATS_ARXIPRESTAT = "/path/to/output/summarystats/"
  OUTPUT_MID_T1T3_STATS_ARXIPRESTAT_LATEX = "/path/to/output/summarystatslatex/"
  OUTPUT_MID_T1T3_STATS_ARXIPRESTAT_SEXE = "/path/to/output/summarystats/bygender/"
  OUTPUT_MID_T1T3_STATS_ARXIPRESTAT_SEXE_LATEX = "/path/to/output/summarystats/bygenderlatex/"

  # F1 & F2 BOXPLOTS
  OUTPUT_MID_T1T3_BOXPLOTS_EBRtesi = "/path/to/boxplots/"

  OUTPUT_MID_T1T3_KDEPLOTS_EBRtesi = "/path/to/kdeplots/"


## LOAD PACKAGES AND FUNCTIONS
library(ggplot2) ## plots (though it's called again within the functions)
library(Cairo) ## through device=cairo_pdf, pdfs with IPA symbols & diachritics are correctly created
library(xtable) # Export Tables to LaTeX or HTML
source("/path/to/EBR_FUNCTION_summarystats.R")
source("/path/to/EBR_FUNCTION_F1F2boxplot.R")
source("/path/to/EBR_FUNCTION_F1F2contourplots.R")


###########
## START ##
###########

## read INPUT file
form.data <- read.delim(INPUT, header=TRUE, sep = "\t")
form.data$N.CONTEXT = as.factor(form.data$N.CONTEXT)

  ####################################################
  ## LOOP THROUGH REGIONS (form.data$ARXIPRESTAT)   ##
  ####################################################

  # get levels in region column to prepare for loop
  ARX <- levels(form.data$ARXIPRESTAT)

  for (y in seq(along=ARX)) {

    ARX.data <- form.data[which(form.data$ARXIPRESTAT==ARX[y]),]

    ####################################################
    ## 1. CREATE TABLES OF BASIC SUMMARY STATS        ##
    ## USING THE "EBR_FUNCTION_summarystats.R"        ##
    ####################################################

    # CREATE A TABLE WITH F1 AND F2 MEAN, MEDIAN, AND STANDARD DEVIATIONS
    # PER REGION AND VOWEL USING LOBANOV-NORMALIZED VALUES

    mid_T1T3stats_arxiprestat = summarystats(ARX.data,
                                             INTsumm = 0.5,
                                             TESTsumm = "T1-T3",
                                             GROUP = "Arxiprestat",
                                             LEVEL = "Vocal")


    statstable = mid_T1T3stats_arxiprestat[, c(1:9)]

    print(xtable(statstable[,c(2:4,6,5,7,9,8)], # median columns after means
                 digits=c(0,# set number of digits per column; this is rowname column ("INFORMANT"), it'll be removed later
                          0,# column "VOCAL"
                          0,# column "N" (n of items)
                          3,# column "Lobanov.F1.mean"
                          3,# column "Lobanov.F1.median"
                          3,# column "Lobanov.F1.sd"
                          3,# column "Lobanov.F2.mean"
                          3,# column "Lobanov.F2.median"
                          3), # column "Lobanov.F2.sd"
                 align = c("r", #set alignment within column; this is column "INFORMANT", will be removed later
                           "c", #column "VOCAL",
                           "c", #column "N"
                           "c@{\hspace{.7em}}", "c@{\hspace{.7em}}", ">{\\itshape}c", # columns F1 mean, F1 median, F1 sd / @{\hspace{.7em}} reduces space between columns
                           "c@{\hspace{.7em}}", "c@{\hspace{.7em}}", ">{\\itshape}c"), # columns F2 mean, F2 median, F2 sd
                 caption=c(paste("Mitjanes ($\\overline{x}$), medianes ($\\tilde{x}$) i desviacions estàndard (\\emph{s}) dels valors de F1 i F2, normalitzats segons el mètode \\textsc{Lobanov} i obtinguts al 50~\\% de l'interval, de totes les vocals produïdes pels informants de l'arxiprestat ",
                                 ARX[y], " durant els testos T1 i T3", sep=""), #long caption
                           paste("Resum descriptiu de les vocals produïdes pels informants de l'arxiprestat ",
                                 ARX[y], sep="")),#short caption
                 label=paste("statsumm_", ARX[y], sep="")), #table label
          file=paste(OUTPUT_MID_T1T3_STATS_ARXIPRESTAT_LATEX,
                     ARX[y], "_50_T1-T3_estadistiques", ".tex", sep = ""),
          floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
          booktabs=TRUE, # enable toprule, midrule, bottomrule from booktabs package
          caption.placement = "top",
          table.placement="!htb",
          format.args=list(big.mark = "", decimal.mark = ","), # use nothing in numbers such as "2364", use a comma for decimals
          include.rownames=FALSE, #do not include name of rows (INFORMANT id)
          include.colnames=FALSE,
          #size="small" # add \begingroup \small at beginning of float, make text smaller
          add.to.row = list( #add rows for header
            pos = list(0,0,0), # position of the rows to add (0=header)
            #command = c(" VOCAL & N & F1\\textsuperscript{L} & \\normalfont{DE\\textsuperscript{L}} & F2\\textsuperscript{L} & \\normalfont{DE\\textsuperscript{L}} \\\\\n")) # rows added
            command = c("\\multicolumn{2}{c}{} & \\multicolumn{3}{c}{F1 (Lobanov)} & \\multicolumn{3}{c}{F2 (Lobanov)} \\\\\n",
                        "\\cmidrule(lr){3-5} \\cmidrule(lr){6-8}\n",
                        "\\textsc{Vocal} & (N) & $\\overline{x}$ & $\\tilde{x}$ & s & $\\overline{x}$ & $\\tilde{x}$ & s \\\\\n")) # rows added
    ) # finish print

    # CREATE A TABLE WITH F1 AND F2 MEAN, MEDIAN, AND STANDARD DEVIATIONS
    # PER SEX AND VOWEL WITHIN REGION USING UNNORMALIZED VALUES
    mid_T1T3stats_compsexesarx = summarystats(ARX.data,
                                              INTsumm = 0.5,
                                              TESTsumm = "T1-T3",
                                              GROUP = "SexesArxiprestat",
                                              LEVEL = "Vocal")

    statstable2 = mid_T1T3stats_compsexesarx

    comparisontable <- cbind(statstable2[statstable2$SEXE=="D",], statstable2[statstable2$SEXE=="H",])

    print(xtable(comparisontable[,c(3:10,14:20)], # remove repeated AREA, SEX, VOWEL COLUMNS
                 digits=c(0,# set number of digits per column; this is rowname column ("INFORMANT"), it'll be removed later
                          0,# column "VOCAL"
                          0,# column "N" (n of items) = FEMALE
                          0,# column "F1.mean" = FEMALE
                          0,# column "F1.median" = FEMALE
                          1,# column "F1.sd" = FEMALE
                          0,# column "F2.mean" = FEMALE
                          0,# column "median" = FEMALE
                          1,# column "F2.sd" = FEMALE
                          0,# column "N" (n of items) = MALE
                          0,# column "F1.mean" = MALE
                          0,# column "F1.median" = MALE
                          1,# column "F1.sd" = MALE
                          0,# column "F2.mean" = MALE
                          0,# column "median" = MALE
                          1),# column "F2.sd" = MALE
                 align = c("r", #set alignment within column; this is column "INFORMANT", will be removed later
                           "c", #column "VOCAL",
                           "c", #column "N" = FEMALE
                           "@{\hspace{.8em}}c@{\hspace{.7em}}", "c@{\hspace{.7em}}", ">{\\itshape}c", # columns F1 mean, F1 median, F1 sd = FEMALE / @{\hspace{.7em}} reduces space between vowels
                           "c@{\hspace{.7em}}", "c@{\hspace{.7em}}", ">{\\itshape}c", # columns F2 mean, F2 median, F2 sd = FEMALE
                           "c", #column "N" = MALE
                           "@{\hspace{.8em}}c@{\hspace{.7em}}", "c@{\hspace{.7em}}", ">{\\itshape}c", # columns F1 mean, F1 median, F1 sd = MALE
                           "c@{\hspace{.7em}}", "c@{\hspace{.7em}}", ">{\\itshape}c"), # columns F2 mean, F2 median, F2 sd = MALE
                 caption = c(paste("Mitjanes ($\\overline{x}$), medianes ($\\tilde{x}$) i desviacions estàndard (\\emph{s}) dels valors no normalitzats de F1 i F2, obtinguts al 50~\\% de l'interval, de totes les vocals produïdes pels informants de l'arxiprestat ",
                                   ARX[y], " durant els testos T1 i T3, separats per sexes", sep=""), #long caption
                             paste("Resum descriptiu de les vocals produïdes pels informants de l'arxiprestat ",
                                   ARX[y], ", separats per sexes", sep="")),#short caption
                 label=paste("statsumm_compsexes", ARX[y], sep="")), #table label
          file = paste(OUTPUT_MID_T1T3_STATS_ARXIPRESTAT_SEXE_LATEX,
                     ARX[y], "_50_T1-T3_estadistiques_compsexes", ".tex", sep = ""),
          floating = TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
          booktabs = TRUE, # enable toprule, midrule, bottomrule from booktabs package
          caption.placement = "top",
          table.placement = "!htb",
          format.args = list(big.mark = "", decimal.mark = ","), # use nothing in numbers such as "2364", use a comma for decimals
          include.rownames = FALSE, #do not include name of rows (INFORMANT id)
          include.colnames = FALSE,
          size = "scriptsize", # add \begingroup \small at beginning of float, make text smaller
          add.to.row = list( #add rows for header
            pos = list(0,0,0,0,0), # position of the rows to add (0=header)
            command = c(" & \\multicolumn{7}{c}{\\textsc{Dones}} & \\multicolumn{7}{c}{\\textsc{Homes}} \\\\\n",
                        "\\cmidrule(lr){2-8} \\cmidrule(lr){9-15}\n",
                        " &  & \\multicolumn{3}{c}{F1 (Hz)} & \\multicolumn{3}{c}{F2 (Hz)} &  & \\multicolumn{3}{c}{F1 (Hz)} & \\multicolumn{3}{c}{F2 (Hz)} \\\\\n",
                        "\\cmidrule(lr){3-5} \\cmidrule(lr){6-8} \\cmidrule(lr){10-12} \\cmidrule(lr){13-15}\n ",
                        "\\textsc{Vocal} & (N) & $\\overline{x}$ & $\\tilde{x}$ & s & $\\overline{x}$ & $\\tilde{x}$ & s & (N) & $\\overline{x}$ & $\\tilde{x}$ & s & $\\overline{x}$ & $\\tilde{x}$ & s \\\\\n")) # rows added
    ) # finish print

    ####################################################
    ## 2. CREATE BOXPLOTS OF UNNORMALIZED F1 AND F2   ##
    ## VALUES PER SEX AND VOWEL WITHIN EACH REGION,   ##
    ## USING "EBR_FUNCTION_F1F2boxplot.R"             ##
    ####################################################
    bxp = F1F2boxplot(ARX.data,
                      TESTbxp = "T1-T3",
                      INTbxp = 0.5,
                      NORMbxp = "Hz",
                      SEXbxp = "ALL",
                      COMPARE = "sex")

    library(Cairo)
    ggsave(filename=paste(ARX[y], "_50_T1-T3_boxplot_Hz_comparaciosexes.pdf", sep=""),
           plot=bxp, device=cairo_pdf, path=OUTPUT_MID_T1T3_BOXPLOTS_EBRtesi,
           width=13, height=9, units="cm", dpi=500)


    ####################################################
    ## 2. CREATE A CONTOUR PLOT OF LOBANOV-NORMALIZED ##
    ## F1 AND F2 VALUES PER REGION, USING             ##
    ## "EBR_FUNCTION_F1F2contourplots.R"              ##
    ####################################################
    F1F2kdecontour = F1F2contourplots(ARX.data,
                                     TESTkde = "T1-T3",
                                     INTkde = 0.5,
                                     NORMkde = "Lobanov",
                                     GROUPsumm_kde = "Arxiprestat") ## indication to FUNCTION_summarystats within the plot function

    ggsave(filename=paste(ARX[y], "_50_T1-T3_MEANF1F2_DENSITY_Lobanov.pdf", sep=""),
           plot=F1F2kdecontour, device=cairo_pdf, path=OUTPUT_MID_T1T3_KDEPLOTS_EBRtesi,
           height=13, width=13, units="cm", dpi=500)

  } # end ARX loop


