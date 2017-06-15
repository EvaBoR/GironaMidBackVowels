#############################
### STATISTICAL ANALYSIS OF THE DISTRIBUTION OF THE /o/-/ɔ/ MERGER
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 01/06/2017
###
### 
###
#################################

# CLEAN UP ------------------------------------------------------------------
rm(list=ls(all=TRUE))
library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
options(scipen=999) ## disable scientific notation (instead of "3.225594e-06" => "0.000003225594")


# LOAD LIBRARIES AND FUNCTIONS ------------------------------------------------------------------

library(dplyr)
library(plyr)
library(tidyr)
library(tibble)
library(reshape2) ## melt()
library(lme4)
library(lmerTest)
library(ggplot2)
library(car)
library(xtable)
library(effects)
library(RColorBrewer)
library(scales)

# RECONTRAST FUNCTION
# recontrast.R, adapted from "http://www.ling.upenn.edu/~joseff/scripts/recontrast.R" (Josef Fruehwald, 2009)
# Function created to sum code categorical variables but give significant names to columns in
# the matrix so coefficients are easily interpreted (see http://www.ling.upenn.edu/~joseff/scripts/recontrast.html)
#
# Adapted to include DEVIATION CODING: (-.5, .5) instead of (-1,1)
# (See Mirman, D., 2014. Growth Curve Analysis and Visualization Using R, Chapman and Hall / CRC. and
# http://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/.)

  recontrast<-function(data,type = "sum"){
    data.type <-class(data)
    if(data.type == "factor"&!is.ordered(data)&nlevels(data)>1&nlevels(data)<1000){
      if(type == "sum"){
        contrasts(data)<-contr.sum(levels(data))
        colnames(contrasts(data))<-levels(data)[-nlevels(data)]
      }else if(type == "treatment"){
        contrasts(data)<-contr.treatment(levels(data))
      }else if(type == "deviation"){
        contrasts(data)<-contr.sum(levels(data))/2
        colnames(contrasts(data))<-levels(data)[-nlevels(data)]
      }
    }else if(data.type == "data.frame"){
      for(i in 1:ncol(data)){
        if(is.factor(data[,i]) & !is.ordered(data[,i])&nlevels(data[,i])>1&nlevels(data[,i])<1000){
          if(type == "sum"){
            contrasts(data[,i])<-contr.sum(levels(data[,i]))
            colnames(contrasts(data[,i]))<-levels(data[,i])[-nlevels(data[,i])]
          }else if(type == "treatment"){
            contrasts(data[,i])<-contr.treatment(levels(data[,i]))
          }else if(type == "deviation"){
            contrasts(data[,i])<-contr.sum(levels(data[,i]))/2
            colnames(contrasts(data[,i]))<-levels(data[,i])[-nlevels(data[,i])]
          }
        }
      }
    }
    return(data)
  }

# LOAD DATA, SUBSET, CODE, ARRANGE ----------------------------------------------

  ## COMPLETE PATH TO THE FILE(S) TO BE PROCESSED

    INPUT_RESULTS = "/path/to/resultatsquant_classificat.txt"
    results = read.delim(INPUT_RESULTS, header=TRUE, blank.lines.skip = TRUE, sep = "\t")
    resultsos = droplevels(results[results$Parell.vocals=="o-ɔ",])
     
    resultsos$ZONA = "NA"
    costa = c("AEM", "MB", "CBC", "TO", "MA")
    interior = c("AEI", "AF", "BA", "FM", "GS", "SE", "TB")
    for (y in seq(along=costa)) {resultsos[resultsos$ARXIPRESTAT==costa[y],]$ZONA = "Costa"}
    for (y in seq(along=interior)) {resultsos[resultsos$ARXIPRESTAT==interior[y],]$ZONA = "Interior"}
    resultsos$ZONA = as.factor(resultsos$ZONA)
    contr = c("ZONA", "EDAT", "SEXE", "FUSIO", "ARXIPRESTAT")
    resultsos[,contr] = recontrast(resultsos[,contr], type="deviation")
    
  ## SET OUTPUT DIRECTORY AND FILE NAMES

    OUTPUT = "/path/to/output/"

# ANALYSIS ------------------------------------------------------------------
  
    ## Correlation between Euclidean distances and Bhattacharyya Coefficients
    corr_eqn <- function(x,y, digits = 2) {
      corr_coef <- round(cor(x, y), digits = digits)
      paste("italic(r) == ", corr_coef)
    }
    labels = data.frame(x = 0.25, y = 0.6, label = corr_eqn(resultsos$BhaCoeff, resultsos$meanDE))
    
    corCB_DEplot =   ggplot(resultsos, aes(x = meanDE, y = BhaCoeff)) +
                      geom_point(aes(color=FUSIO), size=2) +
                      geom_smooth(method = "lm", color="black")  + 
                      geom_text(data = labels, aes(x = x, y = y, label = label), 
                                parse = TRUE, size=3) +
                      scale_color_manual(values=c("#D55E00", "#009E73", "#F0E442"),
                                                  name=element_blank(),
                                                  breaks=c("F", "FP", "D"),
                                                  labels=c("Fusionadors", "Fusionadors\n parcials", "Distingidors")) + 
                      theme_bw(base_size=8, base_family="Helvetica") + #"Helvetica" / "Linux Libertine O"
                      theme(panel.border = element_blank(), 
                            axis.line.x = element_line(size = 0.1, colour = "gray41"),
                            axis.line.y = element_line(size = 0.1, colour = "gray41"),
                            axis.ticks = element_line(colour = "gray41"),
                            axis.text = element_text(colour = "gray41"),
                            legend.margin=margin(0,0,0,0, unit='cm'), 
                            legend.position = "top",
                            legend.box.margin = margin(0, 0, 0, 0),
                            legend.key.height = unit(0.3, "cm")) +
                          scale_y_continuous(limits=c(NA,1)) + 
                      xlab("Distància euclidiana") + ylab("Coeficient de Bhattacharyya")
    
    ggsave(filename="corCB_DE.pdf",
           plot=corCB_DEplot, device=cairo_pdf, path=OUTPUT,
           height=6, width=8, units="cm", dpi=600)
    
    
    
    
  ## Mixed-effects model for Bhattacharyya Coefficients as a function of Age, Gender and Area, with Deaneries as random effects
  
  CB_m = lmer(BhaCoeff ~ EDAT + SEXE + EDAT:SEXE + ZONA + ZONA:EDAT + ZONA:SEXE + (1|ARXIPRESTAT), data=resultsos, REML=TRUE)
    
    # Check normality and heteroscedasticity of residuals, this looks okay.
    # qqp(resid(CB_m))
    # hist(resid(CB_m))
    # plot(scale(resid(CB_m)) ~ fitted(CB_m), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity
  
  DE_m = lmer(meanDE ~ EDAT + SEXE + EDAT:SEXE + ZONA + EDAT:ZONA + SEXE:ZONA + (1|ARXIPRESTAT), data=resultsos, REML=TRUE)
  
  # Check normality and heteroscedasticity of residuals, this looks okay.
  # qqp(resid(DE_m))
  # hist(resid(DE_m))
  # plot(scale(resid(DE_m)) ~ fitted(DE_m), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity
  
  ## plot region x age effects
  m_DE_eff_ZONAEDAT = as.data.frame(effect("EDAT:ZONA", DE_m, se=TRUE))
  
  # PLOT INTERACTION
  
  intPlot =   ggplot(data=m_DE_eff_ZONAEDAT, aes(x=EDAT, y=fit, group=ZONA)) +
    geom_point(size=1.5,aes(color=ZONA)) +
    geom_path(size=0.8, aes(color=ZONA)) +
    geom_errorbar(aes(ymin=lower, ymax=upper, color=ZONA), width=0.2) +
    scale_color_manual(values=c("#56B4E9", "#009E73"), guide=FALSE) + #guide = guide_legend(title = "Zona")) + ## blue = Costa, green = Interior
    scale_fill_manual(values=c("#56B4E9", "#009E73"), guide=FALSE) +
    ylab("Distància euclidiana") + xlab("Edat") +
    theme_bw(base_size=8, base_family="Helvetica") + #"Helvetica" / "Linux Libertine O"
    theme(panel.border = element_blank(), 
          axis.line.x = element_line(size = 0.1, colour = "gray41"),
          axis.line.y = element_line(size = 0.1, colour = "gray41"),
          axis.ticks = element_line(colour = "gray41"),
          axis.text = element_text(colour = "gray41")) +
          #legend.margin=margin(0,0,0,0, unit='cm'), 
          #legend.title.align=0.5,
          #legend.position = "top",
          #legend.box.margin = margin(0, 0, 0, 0),
          #legend.key.height = unit(0.3, "cm")) +
    scale_x_discrete(expand=c(0.1,0.1)) + scale_y_continuous(limits = c(0,1), expand=c(-0.1,0.1))
    
  
  ggsave(filename="dist_DE_EDAT_ZONA_intplot.pdf",
         plot=intPlot, device=cairo_pdf, path=OUTPUT,
         height=6, width=5, units="cm", dpi=600)
