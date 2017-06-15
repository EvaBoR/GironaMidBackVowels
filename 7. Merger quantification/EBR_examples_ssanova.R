#############################
### EXAMPLE SSANOVA
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 23/07/2016
###
#################################

## CLEAN UP
rm(list=ls(all=TRUE)) # clean work space
library(pacman) #unload loaded packages, to prevent incompatibilities
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
options(scipen=999) ## disable scientific notation (instead of "3.225594e-06" => "0.000003225594")

################################
## STUFF THAT NEEDS TO BE SET ##
################################

## COMPLETE PATH TO THE FILE TO BE PROCESSED
INPUT = "/path/to/input/TOT_normalitzat.txt"

vowel.colours <- c("a" = "grey28",  #dark grey
                     "e" = "#D55E00", #orange
                     "ɛ" = "#009E73", #green
                     "i" = "#56B4E9", #light blue
                     "o" = "#0072B2", #dark blue
                     "ɔ" = "#CC79A7", #pink
                     "u" = "#F0E442") #yellow

## SET OUTPUT DIRECTORY AND FILE NAMES
OUTPUT = "/path/to/output/"


## LOAD PACKAGES AND FILES
library(gss) ## ssanova
library(gridExtra)
library(ggplot2)
library(Cairo) ## through device=cairo_pdf, pdfs with IPA symbols & diachritics are correctly created


###########
## START ##
###########

  ## read INPUT file
  form.data <- read.delim(INPUT, header=TRUE, sep = "\t")

  df = form.data[form.data$TEST!="T2" & form.data$INFORMANT=="BA-FE1-D1",]

    f1.model = ssanova(F1 ~ VOCAL + INTERVAL + VOCAL:INTERVAL, data = df)
    f2.model = ssanova(F2 ~ VOCAL + INTERVAL + VOCAL:INTERVAL, data = df)


    # CALCULATE CONFIDENCE INTERVALS OF 95% (2 STANDARD DEVIATIONS) (IF YOU CHANGED DEFAULTS ABOVE, CHANGE HERE TO MATCH)
    grid = expand.grid(INTERVAL = seq(min(df$INTERVAL), max(df$INTERVAL), length = 100), VOCAL = c("o", "ɔ"))

    grid$F1.Fit = predict(f1.model, newdata = grid, se = T)$fit
    grid$F1.SE = predict(f1.model, newdata = grid, se = T)$se.fit
    grid$F2.Fit = predict(f2.model, newdata = grid, se = T)$fit
    grid$F2.SE = predict(f2.model, newdata = grid, se = T)$se.fit

    # PLOT SSANOVA CURVES AND CONFIDENCE INTERVALS

    ## configure plot basic theme settings
    theme_opts = list(theme_bw(base_size=8, base_family="Helvetica"), #"Linux Biolinum O" / "Helvetica" / "Linux Libertine O"
                      theme(panel.border = element_blank(),
                            axis.line.x = element_line(size = 0.1, colour = "gray41"),
                            axis.line.y = element_line(size = 0.1, colour = "gray41"),
                            axis.ticks = element_line(colour = "gray41"),
                            axis.text = element_text(colour = "gray41"),
                            axis.text.x = element_text(angle = 45),
                            legend.key.size = unit(0.2, "cm"),
                            legend.text = element_text(size = 5),
                            legend.title = element_text(size = 7),
                            legend.position = "top",
                            legend.margin = unit(0, "cm")),
                      #labs(y = paste("F1", "Hz", "F2", sep="                        "),
                      labs(y = "Hz",
                           x = "Punt d'anàlisi"),
                      scale_x_continuous(breaks=c(0.2,0.3,0.4,0.5,0.6,0.7,0.8),
                                         labels = c("20 %","30 %","40 %","50 %","60 %","70 %","80 %")),
                      scale_y_continuous(limits = c(350,2550)),
                      scale_colour_manual(values=vowel.colours),
                      guides(colour = guide_legend("Vocal"), fill = guide_legend("Vocal"), linetype = guide_legend("Formant")))

    ssanova.noic = ggplot(grid, aes(x = INTERVAL, colour = VOCAL, fill = VOCAL)) +
                        geom_line(aes(y = F1.Fit, colour = VOCAL), lwd = 0.75, alpha = 0.8, linetype = "solid") +
                        geom_line(aes(y = F2.Fit, colour = VOCAL), lwd = 0.75, alpha = 0.8, linetype = "dotted") +
                        theme_opts

    ssanova.ic = ggplot(grid, aes(x = INTERVAL, colour = VOCAL, fill = VOCAL)) +
                        geom_ribbon(aes(ymin = F1.Fit-(1.96*F1.SE),
                                        ymax = F1.Fit+(1.96*F1.SE)),
                                    alpha = 0.2,
                                    colour = "NA") +
                        geom_ribbon(aes(ymin = F2.Fit-(1.96*F2.SE),
                                        ymax = F2.Fit+(1.96*F2.SE)),
                                    alpha = 0.2,
                                    colour = "NA") +
                        scale_fill_manual(values=vowel.colours) +
                        geom_line(aes(y = F1.Fit, colour = VOCAL), lwd = 0.75, alpha = 0.8, linetype = "solid") +
                        geom_line(aes(y = F2.Fit, colour = VOCAL), lwd = 0.75, alpha = 0.8, linetype = "dotted") +
                        theme_opts

    ggsave(filename="exemple_SSANOVA_sense_IC.pdf",
           plot=ssanova.noic, device=cairo_pdf, path=OUTPUT,
           height=7, width=5, units="cm", dpi=600)

    ggsave(filename="exemple_SSANOVA_IC.pdf",
           plot=ssanova.ic, device=cairo_pdf, path=OUTPUT,
           height=7, width=5, units="cm", dpi=600)



