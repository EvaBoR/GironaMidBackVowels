#############################
### NORMALIZATION COMPARISON
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 26/01/2016
###
### The script computes mean values of F1 and F2 unnormalized and normalized values
### for a subset of 5 speakers, creates F1xF2 plots and arranges them in 4 png files,
### one for each group of 2 speakers, to allow for comparison of normalisation methods.
###
### Input (INPUT): output of 'EBR_normalization.R' ("TOT_normalitzat.txt")
###
### Output (OUTPUT.PATH): 4 png files ("COMP1/2/3/4_norm.png") with mean F1xF2 plots for
### sets of two speakers and all possible normalization methods.
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
INPUT <- "/path/to/input/TOT_normalitzat.txt"

## SET OUTPUT DIRECTORY AND FILE NAMES
OUTPUT.PATH <- "/path/to/output/directory/" # directory

###########
## START ##
###########

## LOAD NECESSARY PACKAGES AND FUNCTIONS
library(vowels)
library(ggplot2)
library(plyr)
library(dplyr)
library(grid)
library(gridExtra)

source("/path/to/EBR_FUNCTION_comparative_norm_plot.R") # function to create mean F1xF2 plots and a function to arrange them in a single png file
# BEWARE: EBR_FUNCTION_comparative_norm_plot.R sources "EBR_FUNCTION_summarystats.R"

## read INPUT file
form.data <- read.delim(INPUT, header=TRUE, sep = "\t")

## CREATE MEAN F1xF2 PLOTS FOR EACH GROUP OF TWO SPEAKERS AND EACH NORMALIZATION METHOD
plot_COMP1_Hz <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="AEI-FE1-H2", INT=0.5, TEST="TOT", NORM="Hz")
plot_COMP1_L <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="AEI-FE1-H2", INT=0.5, TEST="TOT", NORM="Lobanov")
plot_COMP1_N1 <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="AEI-FE1-H2", INT=0.5, TEST="TOT", NORM="Nearey1")
plot_COMP1_mWF <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="AEI-FE1-H2", INT=0.5, TEST="TOT", NORM="mWF")
plot_COMP1_B <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="AEI-FE1-H2", INT=0.5, TEST="TOT", NORM="Bark")

plot_COMP2_Hz <- comp_norm_plot(form.data, INFORMANT1="GS-FE1-D3", INFORMANT2="GS-FE2-D3", INT=0.5, TEST="TOT", NORM="Hz")
plot_COMP2_L <- comp_norm_plot(form.data, INFORMANT1="GS-FE1-D3", INFORMANT2="GS-FE2-D3", INT=0.5, TEST="TOT", NORM="Lobanov")
plot_COMP2_N1 <- comp_norm_plot(form.data, INFORMANT1="GS-FE1-D3", INFORMANT2="GS-FE2-D3", INT=0.5, TEST="TOT", NORM="Nearey1")
plot_COMP2_mWF <- comp_norm_plot(form.data, INFORMANT1="GS-FE1-D3", INFORMANT2="GS-FE2-D3", INT=0.5, TEST="TOT", NORM="mWF")
plot_COMP2_B <- comp_norm_plot(form.data, INFORMANT1="GS-FE1-D3", INFORMANT2="GS-FE2-D3", INT=0.5, TEST="TOT", NORM="Bark")

plot_COMP3_Hz <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="TO-FE2-H2", INT=0.5, TEST="TOT", NORM="Hz")
plot_COMP3_L <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="TO-FE2-H2", INT=0.5, TEST="TOT", NORM="Lobanov")
plot_COMP3_N1 <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="TO-FE2-H2", INT=0.5, TEST="TOT", NORM="Nearey1")
plot_COMP3_mWF <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="TO-FE2-H2", INT=0.5, TEST="TOT", NORM="mWF")
plot_COMP3_B <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="TO-FE2-H2", INT=0.5, TEST="TOT", NORM="Bark")

plot_COMP4_Hz <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="GS-FE1-D3", INT=0.5, TEST="TOT", NORM="Hz")
plot_COMP4_L <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="GS-FE1-D3", INT=0.5, TEST="TOT", NORM="Lobanov")
plot_COMP4_N1 <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="GS-FE1-D3", INT=0.5, TEST="TOT", NORM="Nearey1")
plot_COMP4_mWF <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="GS-FE1-D3", INT=0.5, TEST="TOT", NORM="mWF")
plot_COMP4_B <- comp_norm_plot(form.data, INFORMANT1="AEI-FE1-D2", INFORMANT2="GS-FE1-D3", INT=0.5, TEST="TOT", NORM="Bark")


library(Cairo)

#COMP1 <- grid_arrange_shared_legend(plot_COMP1_Hz, plot_COMP1_B, plot_COMP1_L, plot_COMP1_mWF, plot_COMP1_N1)
  # ggsave(filename=paste("COMP1_norm", ".pdf", sep=""),
  #        plot=COMP1, device=cairo_pdf, path=OUTPUT.PATH,
  #        height=30, width=30, units="cm", dpi=600)
#
# # png(filename = paste(OUTPUT.PATH, "COMP2_norm", ".png", sep=""), width=30, height=30, unit="cm", res=600)
# COMP2 <- grid_arrange_shared_legend(plot_COMP2_Hz, plot_COMP2_B,  plot_COMP2_L, plot_COMP2_mWF, plot_COMP2_N1)
#   ggsave(filename=paste("COMP2_norm", ".pdf", sep=""),
#          plot=COMP2, device=cairo_pdf, path=OUTPUT.PATH,
#          height=30, width=30, units="cm", dpi=600)
#
# #dev.off()
#
# #png(filename = paste(OUTPUT.PATH, "COMP3_norm", ".png", sep=""), width=30, height=30, unit="cm", res=600)
# COMP3 <- grid_arrange_shared_legend(plot_COMP3_Hz,  plot_COMP3_B,  plot_COMP3_L, plot_COMP3_mWF, plot_COMP3_N1)
#   ggsave(filename=paste("COMP3_norm", ".pdf", sep=""),
#          plot=COMP3, device=cairo_pdf, path=OUTPUT.PATH,
#          height=30, width=30, units="cm", dpi=600)
#
# #dev.off()
#
# #png(filename = paste(OUTPUT.PATH, "COMP4_norm", ".png", sep=""), width=30, height=30, unit="cm", res=600)
# COMP4 <- grid_arrange_shared_legend(plot_COMP4_Hz,  plot_COMP4_B,  plot_COMP4_L, plot_COMP4_mWF, plot_COMP4_N1)
#   ggsave(filename=paste("COMP4_norm", ".pdf", sep=""),
#          plot=COMP4, device=cairo_pdf, path=OUTPUT.PATH,
#          height=30, width=30, units="cm", dpi=600)
#
# #dev.off()

ggsave(filename=paste("COMP1_Hz", ".pdf", sep=""),
         plot=plot_COMP1_Hz, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP1_L", ".pdf", sep=""),
         plot=plot_COMP1_L, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP1_N1", ".pdf", sep=""),
         plot=plot_COMP1_N1, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP1_mWF", ".pdf", sep=""),
         plot=plot_COMP1_mWF, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP1_B", ".pdf", sep=""),
       plot=plot_COMP1_B, device=cairo_pdf, path=OUTPUT.PATH,
       height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP2_Hz", ".pdf", sep=""),
         plot=plot_COMP2_Hz, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP2_L", ".pdf", sep=""),
         plot=plot_COMP2_L, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP2_N1", ".pdf", sep=""),
         plot=plot_COMP2_N1, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP2_mWF", ".pdf", sep=""),
         plot=plot_COMP2_mWF, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP2_B", ".pdf", sep=""),
       plot=plot_COMP2_B, device=cairo_pdf, path=OUTPUT.PATH,
       height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP3_Hz", ".pdf", sep=""),
         plot=plot_COMP3_Hz, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

# legend <- g_legend(plot_COMP3_Hz) ## to use, commment out "theme(legend.position="none")," in FUNCTION_comparative_norm_plot.R
# ggsave(filename=paste("legend_COMP3", ".pdf", sep=""),
#        plot=legend, device=cairo_pdf, path=OUTPUT.PATH,
#        height=3, width=3, units="cm", dpi=600)

ggsave(filename=paste("COMP3_L", ".pdf", sep=""),
         plot=plot_COMP3_L, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP3_N1", ".pdf", sep=""),
         plot=plot_COMP3_N1, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP3_mWF", ".pdf", sep=""),
         plot=plot_COMP3_mWF, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP3_B", ".pdf", sep=""),
       plot=plot_COMP3_B, device=cairo_pdf, path=OUTPUT.PATH,
       height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP4_Hz", ".pdf", sep=""),
         plot=plot_COMP4_Hz, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP4_L", ".pdf", sep=""),
         plot=plot_COMP4_L, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP4_N1", ".pdf", sep=""),
         plot=plot_COMP4_N1, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP4_mWF", ".pdf", sep=""),
         plot=plot_COMP4_mWF, device=cairo_pdf, path=OUTPUT.PATH,
         height=6, width=6, units="cm", dpi=600)

ggsave(filename=paste("COMP4_B", ".pdf", sep=""),
       plot=plot_COMP4_B, device=cairo_pdf, path=OUTPUT.PATH,
       height=6, width=6, units="cm", dpi=600)

