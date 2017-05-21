#############################################################################
## VOWEL BOXPLOT FUNCTION
##
## Author: Eva Bosch-Roura (eva.bosch.roura@gmail.com)
## Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
## Date: 26/01/2016
##
## Used by EBR_vowel_space_description.R and EBR_F1xF2.R
############################################################################

F1F2contourplots = function(df,
                           OUTPUT = NULL,
                           TESTkde = c("TOT", "T1", "T2", "T3", "T1-T3"),
                           INTkde = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, "ALL"),
                           NORMkde = c("Hz", "Lobanov", "Nearey1", "mWF"),
                           GROUPsumm_kde = c("Individual","Arxiprestat", "SexesArxiprestat", "FUSIOOS"),
                           PAIR = FALSE,
                           AXISTEXT = FALSE) {

  require(ggplot2)

  vowel.colours <- c("a" = "grey28",  #dark grey
                     "e" = "#D55E00", #orange
                     "ɛ" = "#009E73", #green
                     "i" = "#56B4E9", #light blue
                     "o" = "#0072B2", #dark blue
                     "ɔ" = "#CC79A7", #pink
                     "u" = "#F0E442") #yellow

  source("/path/to/EBR_FUNCTION_summarystats.R")

  #subset intervals
    if(INTkde==0.2) {
      dfint = df[df$INTERVAL==0.2,]
      INTLABEL = "20"
    }
    else if(INTkde==0.3) {
      dfint = df[df$INTERVAL==0.3,]
      INTLABEL = "30"
    }
    else if(INTkde==0.4) {
      dfint = df[df$INTERVAL==0.4,]
      INTLABEL = "40"
    }
    else if(INTkde==0.5) {
      dfint = df[df$INTERVAL==0.5,]
      INTLABEL = "50"
    }
    else if(INTkde==0.6) {
      dfint = df[df$INTERVAL==0.6,]
      INTLABEL = "60"
    }
    else if(INTkde==0.7) {
      dfint = df[df$INTERVAL==0.7,]
      INTLABEL = "70"
    }
    else if(INTkde==0.8) {
      dfint = df[df$INTERVAL==0.8,]
      INTLABEL = "80"
    }
    else if(INTkde=="ALL") {
      dfint = df
      INTLABEL = "ALL"
    }

  # subset test
    if(TESTkde=="T1") {dftest = dfint[dfint$TEST=="T1",]}
    else if(TESTkde=="T2") {dftest = dfint[dfint$TEST=="T2",]}
    else if(TESTkde=="T3") {dftest = dfint[dfint$TEST=="T3",]}
    else if(TESTkde=="T1-T3") {dftest = dfint[dfint$TEST!="T2",]}
    else if(TESTkde=="TOT") {dftest = dfint}

  # compute summary statistics
    summ_df = summarystats(dftest, INTsumm = INTkde, TESTsumm = TESTkde, GROUP = GROUPsumm_kde, LEVEL = "Vocal")
    #summ_df2 = summarystats(dftest, INTsumm = INTkde, TESTsumm = TESTkde, GROUP = GROUPsumm_kde, LEVEL = "VocalContext")


  # locate F1 and F2 columns
    if(NORMkde=="Hz") {
      FORMANT2 = dftest$F2
      FORMANT1 = dftest$F1
      FORMANT2mean = summ_df$F2.mean
      FORMANT1mean = summ_df$F1.mean
      F2labs = "F2 (Hz)"
      F1labs = "F1 (Hz)"
      limits.y = c(1200, 190)
      limits.x = c(3050, 450)
    }

    else if(NORMkde=="Lobanov") {
      FORMANT2 = dftest$Lobanov.F2
      FORMANT1 = dftest$Lobanov.F1
      FORMANT2mean = summ_df$Lobanov.F2.mean
      FORMANT1mean = summ_df$Lobanov.F1.mean
      # FORMANT2mean2 = summ_df2$Lobanov.F2.mean
      # FORMANT1mean2 = summ_df2$Lobanov.F1.mean
      F2labs = "F2 (Lobanov)"
      F1labs = "F1 (Lobanov)"
      limits.y = c(4.5, -2.5)
      limits.x = c(2.75, -2.5)
      if(PAIR=="o-ɔ") {
        limits.y = c(2.35, -2)
        limits.x = c(1, -2.5)
        #center = c(max(dftest$Lobanov.F1),min(dftest$Lobanov.F1))
      }
      else if(PAIR=="e-ɛ") {
        limits.y = c(2.8, -2.25)
        limits.x = c(2.5, -0.7)
      }
      else if(PAIR=="MV") {
        limits.y = c(1.7, -2.25)
        limits.x = c(2.5, 0.25)
      }
    }

    else if(NORMkde=="Nearey1") {
      FORMANT2 = dftest$Nearey1.F2
      FORMANT1 = dftest$Nearey1.F1
      FORMANT2mean = summ_df$Nearey1.F2.mean
      FORMANT1mean = summ_df$Nearey1.F1.mean
      F2labs = "F2 (Nearey 1)"
      F1labs = "F1 (Nearey 1)"
      limits.y = c(2.25, 0.5)
      limits.x = c(2.1, 0.25)
    }

    else if(NORMkde=="mWF") {
      FORMANT2 = dftest$mW.F.F2
      FORMANT1 = dftest$mW.F.F1
      FORMANT2mean = summ_df$mW.F.F2.mean
      FORMANT1mean = summ_df$mW.F.F1.mean
      F2labs = "F2 (mW&F)"
      F1labs = "F1 (mW&F)"
      limits.y = c(2.15, 0.5)
      limits.x = c(2.15, 0.3)
    }


  ## configure plot basic theme settings

  if(is.null(AXISTEXT)) {theme_opts = list(theme_bw(base_size=8, base_family="Helvetica"), #"Helvetica" / "Linux Libertine O"
                                         theme(panel.border = element_blank(),
                                               axis.line.x = element_line(size = 0.1, colour = "gray41"),
                                               axis.line.y = element_line(size = 0.1, colour = "gray41"),
                                               axis.ticks = element_line(colour = "gray41"),
                                               axis.text = element_text(colour = "gray41")),
                                         labs(x = F2labs, y = F1labs),
                                         scale_y_reverse(limits = limits.y),  # pretty_breaks -- require(scales)
                                         scale_x_reverse(limits = limits.x))}

    else if(!is.null(AXISTEXT)) { theme_opts = list(theme_bw(base_size=6, base_family="Helvetica"), #"Helvetica" / "Linux Libertine O"
                                                   theme(panel.border = element_blank(),
                                                         # legend.key.size = unit(0.3, "cm"),
                                                         # legend.text = element_text(size = 5),
                                                         # legend.title = element_text(size = 7),#
                                                         axis.line.x = element_line(size = 0.1, colour = "gray41"),
                                                         axis.line.y = element_line(size = 0.1, colour = "gray41"),
                                                         axis.ticks = element_line(colour = "gray41"),
                                                         axis.text = element_text(colour = "gray41")),
                                                   labs(x = F2labs, y = F1labs),
                                                   scale_y_reverse(limits = limits.y),  # pretty_breaks -- require(scales)
                                                   scale_x_reverse(limits = limits.x))}


  ## create plot
    densityplot = ggplot(dftest,
                         aes(x = FORMANT2,
                             y = FORMANT1,
                             colour = factor(VOCAL))) +
                  stat_density2d(aes(fill = factor(VOCAL),
                                     alpha = ..level.., ## transparency depends on density
                                     size = ..level..), ## line width depends on density
                                 geom = "polygon",
                                 contour=TRUE) +
                  geom_text(data = summ_df,
                            aes(x = FORMANT2mean,
                                y = FORMANT1mean,
                                label = VOCAL),
                            colour = "black",
                            size = 5,
                            family = "Helvetica") +
                  # geom_point(data = summ_df2,
                  #           aes(x = FORMANT2mean2,
                  #               y = FORMANT1mean2,
                  #               shape = factor(N.CONTEXT)),
                  #           colour = "white",
                  #           size = 3) +
                  # scale_shape_manual(values=c(1:12)) +
                  scale_size(range = c(0.01, 0.03)) + # line width
                  scale_alpha(range = c(0.1, 0.8)) + # color transparency
                  scale_colour_manual(values=vowel.colours, guide = guide_legend(title = "Vocal")) + # line colour
                  scale_fill_manual(values=vowel.colours, guide = guide_legend(title = "Vocal")) + # filling colour
                  guides(alpha = FALSE, # remove all legends
                         size = FALSE,
                         colour = FALSE,
                         fill = FALSE,
                         scale_size = FALSE,
                         scale_alpha = FALSE) +
                  theme_opts

}