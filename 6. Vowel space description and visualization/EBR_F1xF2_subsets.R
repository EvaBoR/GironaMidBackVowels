#############################
### MID VOWELS F1xF2 DIAGRAMS
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 26/01/2016
###
### The script creates an F1xF2 contour plot of vowel subsets (/o/ and /ɔ/, /e/ and /ɛ/, and /o/, /ɔ/, and /u/)
###  of each speaker, and F1xF2 occurrences plots of /o/ and /ɔ/ and /o/, /ɔ/, and /u/.
###
### Input (INPUT): data file with all unnormalized and normalized formant values obtained from
### 'EBR_normalization.R' ("TOT_normalitzat.txt")
###
#################################

## CLEAN UP
library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
rm(list=ls(all=TRUE))
options(scipen=999) ## disable scientific notation (instead of "3.225594e-06" => "0.000003225594")

################################
## STUFF THAT NEEDS TO BE SET ##
################################

## COMPLETE PATH TO THE FILE TO BE PROCESSED
INPUT = "/path/to/input/TOT_normalitzat.txt"


## SET OUTPUT DIRECTORY AND FILE NAMES
  OUTPUT_OS = "/output/to/os/F1xF2/"
  OUTPUT_OS_T2 = "/output/to/os/F1xF2_T2/"
  OUTPUT_ES = "/output/to/es/12_F1F2_ES_contour/"
  OUTPUT_F1F2ocurrencies = "/output/to/F1F2ocurrencies/"
  OUTPUT_F1F2_uocurrencies = "/output/to//F1F2_uocurrencies/"
  OUTPUT_F1F2_OS_U_contour = "/output/to/F1F2_Os_U_contour/"
  OUTPUT_F1F2_sistemasencer = "/output/to/F1F2_sistemasencer/"

## LOAD PACKAGES AND FUNCTIONS
library(ggplot2) ## plots (though it's called again within the functions)
library(Cairo) ## through device=cairo_pdf, pdfs with IPA symbols & diachritics are correctly created
  library(tidyr)
  library(dplyr)

source("/path/to/EBR_FUNCTION_F1F2contourplots.R")


  ###########
  ## START ##
  ###########

  ## read INPUT file
  form.data <- read.delim(INPUT, header=TRUE, sep = "\t")

  F1xF2plots = lapply(unique(form.data$INFORMANT), function(i) {

    df = form.data[form.data$INFORMANT==i,]

    plotOs = F1F2contourplots(df[df$VOCAL=="o" | df$VOCAL=="ɔ",],
                              TESTkde = "T1-T3",
                              INTkde = 0.5,
                              NORMkde = "Lobanov",
                              GROUPsumm_kde = "Individual",
                              PAIR = "o-ɔ",
                              AXISTEXT = "PETIT") ## indication to FUNCTION_summarystats within the plot function

    ggsave(filename=paste(i, "_50_T1-T3_MEANF1F2_DENSITY_Lobanov_OS.pdf", sep=""),
           plot=plotOs, device=cairo_pdf, path=OUTPUT_OS,
           height=4, width=4, units="cm", dpi=600)

    plotEs = F1F2contourplots(df[df$VOCAL=="e" | df$VOCAL=="ɛ",],
                              TESTkde = "T1-T3",
                              INTkde = 0.5,
                              NORMkde = "Lobanov",
                              GROUPsumm_kde = "Individual",
                              PAIR = "e-ɛ",
                              AXISTEXT = "PETIT") ## indication to FUNCTION_summarystats within the plot function

    ggsave(filename=paste(i, "_50_T1-T3_MEANF1F2_DENSITY_Lobanov_ES.pdf", sep=""),
           plot=plotEs, device=cairo_pdf, path=OUTPUT_ES,
           height=4, width=4, units="cm", dpi=600)

          ## uncomment to add T2 occurrences as density group
          # df$VOCAL <- as.character(df$VOCAL)
          # if(nrow(df[df$TEST=="T2" & df$VOCAL=="o",])!=0){df[df$TEST=="T2" & df$VOCAL=="o",]$VOCAL = "oT2"}
          # df[df$TEST=="T2" & df$VOCAL=="ɔ",]$VOCAL = "ɔT2"

    vowel.colours <- c("a" = "grey28",  #dark grey
                     "e" = "#D55E00", #orange
                     "ɛ" = "#009E73", #green
                     "i" = "#56B4E9", #light blue
                     "o" = "#0072B2", #dark blue
                     "ɔ" = "#CC79A7", #pink
                     "u" = "#F0E442") #yellow
                          # "ɔT2" = "red",  ## add colours to add T2 occurrences as density
                          # "oT2" = "green"

    theme_opts = list(theme_bw(base_size=6, base_family="Helvetica"), #"Linux Biolinum O"/"Helvetica" / "Linux Libertine O"
                      theme(panel.border = element_blank(),
                            axis.line.x = element_line(size = 0.1, colour = "gray41"),
                            axis.line.y = element_line(size = 0.1, colour = "gray41"),
                            axis.ticks = element_line(colour = "gray41"),
                            axis.text = element_text(colour = "gray41")),
                      labs(x="F2 (Lobanov)", y="F1 (Lobanov)"),
                      scale_y_reverse(limits = c(2.35, -2)),
                      scale_x_reverse(limits = c(1, -2.5)),
                      guides(colour=FALSE, fill=FALSE))


    plotOs_T2 = ggplot(df[df$INTERVAL==0.5 & df$TEST!="T2" & (df$VOCAL=="o" | df$VOCAL=="ɔ"),],
                          aes(x = Lobanov.F2,
                              y = Lobanov.F1,
                              colour = factor(VOCAL))) +
      stat_density2d(aes(fill = factor(VOCAL),
                         alpha = ..level..,
                         size = ..level..),
                     geom = "polygon",
                     contour=TRUE) +
      scale_size(range = c(0.01, 0.03)) + # line width
      scale_alpha(range = c(0.1, 0.8)) + # color intensity
      scale_colour_manual(values=vowel.colours) + # line colour
      scale_fill_manual(values=vowel.colours) + # filling colour
      geom_text(data = df[df$INTERVAL==0.5 & df$TEST=="T2",],
                aes(x = Lobanov.F2,
                    y = Lobanov.F1,
                    label = MOT), # if label = VOCAL, uncomment following line, and comment out the one after that
                #color = ifelse(df[df$INTERVAL==0.5 & df$TEST=="T2",]$VOCAL=="o", "grey41", "white"), # T2-"o" = grey41, T2-"ɔ" = "white"
                color = "grey41",
                size = 1.5,
                family = "Helvetica") +
      guides(alpha = FALSE, # remove all legends
             size = FALSE,
             colour = FALSE,
             fill = FALSE,
             scale_size = FALSE,
             scale_alpha = FALSE) +
      theme_opts
    #print(plotOs_T2)
    ggsave(filename=paste(i, "_50_T1-T3_MEANF1F2_DENSITY_Lobanov_OS_mesT2.pdf", sep=""),
           plot=plotOs_T2, device=cairo_pdf, path=OUTPUT_OS_T2,
           height=4, width=4, units="cm", dpi=600)

    plotF1F2_ocurrencies = ggplot(df[df$INTERVAL==0.5 & df$TEST!="T2" & (df$VOCAL=="o" | df$VOCAL=="ɔ"),],
                      aes(x=Lobanov.F2,
                          y=Lobanov.F1,
                          colour=factor(VOCAL))) +
      geom_text(aes(label=VOCAL), size=2) +
      scale_colour_manual(values = vowel.colours) +
      scale_fill_manual(values = vowel.colours) +
      theme_opts

    ggsave(filename=paste(i, "_50_T1-T3_MEANF1F2_DENSITY_Lobanov_OS_ocurrencies.pdf", sep=""),
           plot=plotF1F2_ocurrencies, device=cairo_pdf, path=OUTPUT_F1F2ocurrencies,
           height=4, width=4, units="cm", dpi=600)

    plot_u_F1F2_ocurrencies = ggplot(df[df$INTERVAL==0.5 & df$TEST!="T2" & (df$VOCAL=="o" | df$VOCAL=="ɔ" | df$VOCAL=="u"),],
                                  aes(x=Lobanov.F2,
                                      y=Lobanov.F1,
                                      colour=factor(VOCAL))) +
      geom_text(aes(label=VOCAL), size=2) +
      scale_colour_manual(values = vowel.colours) +
      scale_fill_manual(values = vowel.colours) +
      theme_opts

    ggsave(filename=paste(i, "_50_T1-T3_MEANF1F2_DENSITY_Lobanov_OSiU_ocurrencies.pdf", sep=""),
           plot=plot_u_F1F2_ocurrencies, device=cairo_pdf, path=OUTPUT_F1F2_uocurrencies,
           height=4, width=4, units="cm", dpi=600)

    plotOs_U = F1F2contourplots(df[df$VOCAL=="o" | df$VOCAL=="ɔ" | df$VOCAL=="u",],
                              TESTkde = "T1-T3",
                              INTkde = 0.5,
                              NORMkde = "Lobanov",
                              GROUPsumm_kde = "Individual",
                              PAIR = "o-ɔ",
                              AXISTEXT = "PETIT") ## indication to FUNCTION_summarystats within the plot function

    ggsave(filename=paste(i, "_50_T1-T3_MEANF1F2_DENSITY_Lobanov_OS_U.pdf", sep=""),
           plot=plotOs_U, device=cairo_pdf, path=OUTPUT_F1F2_OS_U_contour,
           height=4, width=4, units="cm", dpi=600)



    plot_F1F2_sistemasencer = F1F2contourplots(df,
                                TESTkde = "T1-T3",
                                INTkde = 0.5,
                                NORMkde = "Lobanov",
                                GROUPsumm_kde = "Individual",
                                AXISTEXT = "PETIT") #+ ## indication to FUNCTION_summarystats within the plot function

    ggsave(filename=paste(i, "_50_T1-T3_MEANF1F2_DENSITY_Lobanov_sistemasencer.pdf", sep=""),
           plot=plot_F1F2_sistemasencer, device=cairo_pdf, path=OUTPUT_F1F2_sistemasencer,
           height=4, width=4, units="cm", dpi=600)

    })
