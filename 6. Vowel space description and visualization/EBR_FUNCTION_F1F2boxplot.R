#############################################################################
## VOWEL BOXPLOT FUNCTION
##
## Author: Eva Bosch-Roura (eva.bosch.roura@gmail.com)
## Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
## Date: 26/01/2016
##
## Used by EBR_vowel_space_description.R
############################################################################

F1F2boxplot = function(df,
                       OUTPUT = NULL,
                       TESTbxp = c("TOT", "T1", "T2", "T3", "T1-T3"),
                       INTbxp = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, "ALL"),
                       NORMbxp = c("Hz", "Lobanov", "Nearey1", "Bark", "mWF"),
                       SEXbxp = c("M", "F", "ALL"),
                       COMPARE = c("sex", "age", "none")) {

  require(reshape2) ## enable function melt()
  require(ggplot2)

  #subset sex
    if(SEXbxp=="M") {dfsex = df[df$SEXE=="H",]}
    if(SEXbxp=="F") {dfsex = df[df$SEXE=="D",]}
    if(SEXbxp=="ALL") {dfsex = df}

  #subset intervals
    if(INTbxp==0.2) {
      dfint = dfsex[dfsex$INTERVAL==0.2,]
      INTLABEL = "20"
      }
    else if(INTbxp==0.3) {
      dfint = dfsex[dfsex$INTERVAL==0.3,]
      INTLABEL = "30"
      }
    else if(INTbxp==0.4) {
      dfint = dfsex[dfsex$INTERVAL==0.4,]
      INTLABEL = "40"
      }
    else if(INTbxp==0.5) {
      dfint = dfsex[dfsex$INTERVAL==0.5,]
      INTLABEL = "50"
      }
    else if(INTbxp==0.6) {
      dfint = dfsex[dfsex$INTERVAL==0.6,]
      INTLABEL = "60"
      }
    else if(INTbxp==0.7) {
      dfint = dfsex[dfsex$INTERVAL==0.7,]
      INTLABEL = "70"
      }
    else if(INTbxp==0.8) {
      dfint = dfsex[dfsex$INTERVAL==0.8,]
      INTLABEL = "80"
      }
    else if(INTbxp=="ALL") {
      dfint = dfsex
      INTLABEL = "ALL"
      }

  # subset test
    if(TESTbxp=="T1") {dftest = dfint[dfint$TEST=="T1",]}
    else if(TESTbxp=="T2") {dftest = dfint[dfint$TEST=="T2",]}
    else if(TESTbxp=="T3") {dftest = dfint[dfint$TEST=="T3",]}
    else if(TESTbxp=="T1-T3") {dftest = dfint[dfint$TEST!="T2",]}
    else if(TESTbxp=="TOT") {dftest = dfint}

  # ORDER OF THE VOWELS IN THE TABLES
  vowel.order <- c("i", "e", "ɛ", "a", "ɔ", "o", "u")
  dftest$VOCAL <- factor(dftest$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
  dftest <- dftest[order(dftest$VOCAL),] # reorder by SPEAKER first, then VOWEL

  # subset formant values by normalization method and "melt" them into a long-format data table,
  # which has a column "variable" that stores, in this case, "F1" and "F2", and a column "values"
  # with, precisely, F1 and F2 values.
    if(NORMbxp=="Hz") {formants = melt(dftest, measure.vars = c("F2", "F1"))}
    else if(NORMbxp=="Lobanov") {formants = melt(dftest, measure.vars = c("Lobanov.F2", "Lobanov.F1"))}
    else if(NORMbxp=="Nearey1") {formants = melt(dftest, measure.vars = c("Nearey1.F2", "Nearey1.F1"))}
    else if(NORMbxp=="mWF") {formants = melt(dftest, measure.vars = c("mw.F.F2", "mW.F.F1"))}
    else if(NORMbxp=="Bark") {formants = melt(dftest, measure.vars = c("Bark.Z3.Z2", "Bark.Z3.Z1"))}

  # create facet_grid command to add to plot below
  if(COMPARE=="sex"){
    addgrid <- list(facet_grid(. ~ SEXE, # if chosen, create a grid with two side by side graphs with productions of female and male speakers
                               labeller = labeller(SEXE = c(D = "DONES", H = "HOMES"))))
    COMPARELABEL = "comparaciosexes"
    }
  if(COMPARE=="age"){
    addgrid <- list(facet_grid(. ~ EDAT, # if chosen, create a grid with two side by side graphs with productions of young and elder speakers
                               labeller = labeller(EDAT = c(FE1 = "Franja d'edat 1 (FE1): joves", FE2 = "Franja d'edat 2 (FE2): grans"))))
    COMPARELABEL = "comparacioedats"}
  if(COMPARE=="none"){
    addgrid = list() # do not make any comparison facet
    COMPARELABEL = ""
    }

  ## set plot options
  theme_opts <- list(theme_bw(base_size=8, base_family="Helvetica"), #"Helvetica" / "Linux Libertine O"
                     theme(panel.border = element_blank(),
                           axis.line.x = element_line(size = 0.1, colour = "gray41"),
                           axis.line.y = element_line(size = 0.1, colour = "gray41"),
                           legend.position = c(-0.058, 0.8),
                           legend.title = element_blank(),
                           legend.text = element_text(size=5),
                           legend.key = element_blank(),
                           axis.ticks = element_line(colour = "gray41"),
                           axis.text.x = element_text(colour = "black", size=8),
                           axis.text.y = element_text(colour = "gray41", size=5),
                           axis.title.x = element_blank(),
                           strip.text.x = element_text(face = "bold"), # text in each facet plot title
                           strip.background = element_blank()),
                     coord_cartesian(ylim = c(0, 3050))) # box for each facet plot title


  ## create plot
    F1F2boxplot = ggplot(formants,
                         aes(x=VOCAL,
                             y=value,  # F1 & F2
                             fill=factor(variable), colour=factor(variable))) +
                             #colour=factor(variable))) + # use fill=factor(variable) instead of colour to fill boxes
                  scale_fill_manual(values=c("grey71", "white")) +
                  scale_colour_manual(values=c("grey31", "black")) +
                  geom_boxplot(varwidth=TRUE, # "If TRUE, boxes are drawn with widths proportional to the square-roots of the number of observations in the groups"
                               position="identity", # so F2 boxes are above F1's
                               outlier.colour=NULL, # so the colour of outliers matches the colour of the box's line
                               outlier.size=0.5,
                               lwd=0.3) + # line width of the boxes
                  stat_summary(fun.y=mean, ## add mean to the box
                               geom="point", ## as a shape
                               shape=8, ## type 5 = rhombe
                               size=1) + ## small
                  labs(y=paste("Freqüència (", NORMbxp, ")", sep="")) + # add; , colour = "Formant:") + to add title to legend
                  theme_opts + ## add settings (above)
                  addgrid ## add facet for comparisons, if needed (otherwise, list addgrid is empty so nothing is added)

  #print(F1F2boxplot)

  # library(Cairo)
  # ggsave(filename=paste(formants$ARXIPRESTAT, "_", INTLABEL, "_", TESTbxp, "_boxplot_", NORMbxp, "_", COMPARELABEL, ".pdf", sep=""),
  #        plot=F1F2boxplot, device=cairo_pdf, path=OUTPUT,
  #        width=13.5, height=9, units="cm", dpi=600)

}
