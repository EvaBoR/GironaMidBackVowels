###########################
# VISUALIZATION FOR INITIAL OUTLIER DETECTION
#
# Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
# Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
# Date: 10/05/2016
#
# The script computes atypical values for formant values for all vowels of each speaker,
# and draws an F1xF2 vowel dispersion plot with 95% CI ellipses for each speaker's vowel system.
#
# Input (INPUT): data file with all unnormalized formant values obtained from
# 'EBR_initial_organization.R' ("TOT_formants_revisats.txt")
#
# Output: data file with all computed outliers (outliers.txt) plus an F1xF2 vowel dispersion plot
# with ellipses for each vowel of each speaker
#
###############################

## CLEAN UP
rm(list=ls(all=TRUE)) # clean work space
library(pacman) #unload loaded packages, to prevent incompatibilities
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)


##########################################################
## FUNCTION: DISPERSION GRAPH, WITH MEANS AND ELLIPSES  ##
## (ACTUAL DATA POINTS CAN BE REMOVED)                  ##
##########################################################

plotF1F2ellipses_outliers = function(df,
                                     OUTPUT=NULL,
                                     INT=c("20", "30", "40", "50",
                                           "60", "70", "80", "TRAJ")) {

  vowel.colours <- c("a" = "grey28",  #gris fosc
                     "e" = "#D55E00", #taronja
                     "ɛ" = "#009E73", #verd
                     "i" = "#56B4E9", #blau clar
                     "o" = "#0072B2", #blau fosc
                     "ɔ" = "#CC79A7", #rosa
                     "u" = "#F0E442") #groc

  require(ggplot2)
  require(scales) # pretty_breaks (scale_y_reverse, etc.)

  # subset interval
  if(INT=="20") {dfint = df[df$INTERVAL==0.2,]}
    else if(INT=="30") {dfint = df[df$INTERVAL==0.3,]}
    else if(INT=="40") {dfint = df[df$INTERVAL==0.4,]}
    else if(INT=="50") {dfint = df[df$INTERVAL==0.5,]}
    else if(INT=="60") {dfint = df[df$INTERVAL==0.6,]}
    else if(INT=="70") {dfint = df[df$INTERVAL==0.7,]}
    else if(INT=="80") {dfint = df[df$INTERVAL==0.8,]}
    else if(INT=="TRAJ") {dfint = df}

  # variable F1/F2 (no normalitzat)
    FORMANT2 = dfint$F2
    FORMANT1 = dfint$F1

  # plot
    theme_opts <- list(theme_bw(base_size=6, base_family="Helvetica"), #"Helvetica" / "Linux Libertine O"
                       theme(panel.border = element_blank(),
                             axis.line.x = element_line(size = 0.1, colour = "gray41"),
                             axis.line.y = element_line(size = 0.1, colour = "gray41"),
                             axis.ticks = element_line(colour = "gray41"),
                             axis.text = element_text(colour = "gray41")),
                       labs(x="F2 (Hz)", y="F1 (Hz)"),
                       scale_y_reverse(breaks = pretty_breaks(n=8)),  # pretty_breaks -- require(scales)
                       scale_x_reverse(breaks = pretty_breaks(n=10)),
                       expand_limits(x=c(max(FORMANT2)+70,min(FORMANT2)-70),
                                     y=c(max(FORMANT1)+70,min(FORMANT1)-70)),
                       guides(colour=FALSE, fill=FALSE))


  F1F2ellipsesplot <- ggplot(dfint, aes(x=FORMANT2, y=FORMANT1, colour=factor(VOCAL))) +
    geom_text(aes(label=VOCAL), size=3, family="Helvetica") +
    scale_colour_manual(values = vowel.colours) +
    scale_fill_manual(values = vowel.colours) +
    stat_ellipse(type="t", geom="polygon", alpha=0.20, aes(fill=factor(VOCAL)), size = 0.03) + # també: type="norm"
    theme_opts
    #ggtitle(paste(dfint$INFORMANT, " (", INT, "%)", sep=""))

    #print(F1F2ellipsesplot)

  library(Cairo)
  ggsave(filename=paste(dfint$INFORMANT, "_", INT, "_F1F2_ELLIPSES_", "Hz", ".pdf", sep=""),
         plot=F1F2ellipsesplot, device=cairo_pdf, path=OUTPUT,
         height=6.5, width=6.5, units="cm", dpi=500)

}


################################
## STUFF THAT NEEDS TO BE SET ##
################################

## COMPLETE PATH TO THE FILE TO BE PROCESSED
INPUT <- "/Users/pathtofolder/whereyourinputis/TOT_formants_revisats.txt"

## OUTPUT DIRECTORIES
OUTPUT_GRAPHS <- "/Users/pathtofolder/whereyouroutputgraphs/willgo/"
OUTPUT <- "/Users/pathtofolder/whereyouroutput/willgo/"

###########
## START ##
###########

## LOAD PACKAGES AND FUNCTIONS
library(ggplot2)
library(plyr)
library(dplyr)

## read INPUT file
form.data <- read.delim(INPUT, header=TRUE, sep = "\t")


########################################################
## FOR LOOP, COMPUTE ATYPICAL VALUES FOR EACH SPEAKER ##
########################################################

  INF <- levels(form.data$INFORMANT)

  ## CREATE EMPTY LISTS TO STORE OUTLIERS ##
  datalisty=list()
  datalistz=list()

  ## LOOP THROUGH SPEAKERS ##
  for (y in seq(along=INF)) {

    PARLANT <- form.data[which(form.data$INFORMANT==INF[y]),]

    VOC <- levels(PARLANT$VOCAL)

    ## LOOP THROUGH VOWELS FOR SPEAKER y ##
    for (z in seq(along=VOC)) {

      CADAVOCAL <- PARLANT[which(PARLANT$VOCAL==VOC[z]),]

      ## CALCULATE OUTLIERS IN F1 AND F2 VALUES FOR VOWEL z
      llistaoutliers <- CADAVOCAL[CADAVOCAL$F1 %in% boxplot.stats(CADAVOCAL$F1)$out |
                                  CADAVOCAL$F2 %in% boxplot.stats(CADAVOCAL$F2)$out,]

      # CREATE EMPTY ROW
      emptyrow <- matrix(c(rep.int(NA,length(CADAVOCAL))),nrow=1,ncol=length(CADAVOCAL))

      ## IF THERE ARE NO OUTLIERS FOR VOWEL z, SPEAKER y, ADD EMPTY ROW TO LIST
      if(nrow(llistaoutliers)==0) {
        llistaoutliers <- data.frame(emptyrow)
        colnames(llistaoutliers) <- colnames(CADAVOCAL)
        datalistz[[z]] <- llistaoutliers
      }
      ## IF THERE ARE OUTLIERS FOR VOWEL z, SPEAKER y, ADD THE ROWS W/ OUTLIERS TO THE LIST
      else if(nrow(llistaoutliers)>0) {
        datalistz[[z]] <- llistaoutliers
      }
      ## ADD LIST OF VOWEL OUTLIERS (datalistz) TO LIST OF ALL SPEAKERS (datalisty)
      datalisty[[y]] <- datalistz
    }

  }
  ## ADD ROWS TO BIG LIST, UNDO LISTS INTO DATA.FRAME, REORDER COLUMNS OF NEW DATA.FRAME
  library(data.table)
  big_data = do.call(rbind, datalisty)
  big_data = rbindlist(big_data)
  ind <- apply(big_data, 1, function(x) all(is.na(x)))
  big_data <- big_data[!ind,]
  big_data = big_data[order(big_data$INFORMANT, big_data$VOCAL, big_data$TOKEN),]

  ## WRITE THE DATA.FRAME OF OUTLIERS INTO A TEXT FILE
  write.table(big_data,
              file=paste(OUTPUT, "outliers.txt", sep = ""),
              quote=FALSE,
              sep="\t",
              row.names=FALSE)

  ###########################################################################
  ## FOR LOOP, TO DRAW F1xF2 WITH ELLIPSES FOR ALL VOWELS FOR EACH SPEAKER ##
  ###########################################################################

  INF <- levels(form.data$INFORMANT)

  SP_COUNT_F1F2ELLIPSES_50 = 0

  for (y in seq(along=INF)) {

    PARLANT <- form.data[which(form.data$INFORMANT==INF[y]),]
    ############################################
    ## DISPERSION GRAPH, WITH 95% CI ELLIPSES ##
    ############################################

    ## MIDPOINT ##
    F1F2ellipses50 <- plotF1F2ellipses_outliers(PARLANT,
                                                OUTPUT=OUTPUT_GRAPHS,
                                                INT="50")

    SP_COUNT_F1F2ELLIPSES_50 = SP_COUNT_F1F2ELLIPSES_50  + 1

}

# Print ""SP_COUNT_F1F2ELLIPSES_50" dispersion graphs with 95% CI ellipses drawn at the 50% interval"
print(paste("S'han processat", SP_COUNT_F1F2ELLIPSES_50, "gràfics de dispersió amb el·lipses (CI 95%) per a l'interval 50%"), quote=FALSE)