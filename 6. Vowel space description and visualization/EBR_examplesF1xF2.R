#############################
### MID VOWELS F1xF2 DIAGRAMS
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 26/01/2016
###
#################################

## CLEAN UP
rm(list=ls(all=TRUE)) # clean work space
library(pacman) #unload loaded packages, to prevent incompatibilities
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

INPUT <- "/path/to/input/TOT_normalitzat.txt"
OUTPUT <- "/path/to/output/"

## FUNCTIONS & LIBRARIES
source("/path/to/EBR_FUNCTION_summarystats.R")
library(ggplot2) ## plots
library(Cairo) ## printing pdf with IPA symbols

vowel.colours <- c("a" = "grey28",  #dark grey
                     "e" = "#D55E00", #orange
                     "ɛ" = "#009E73", #green
                     "i" = "#56B4E9", #light blue
                     "o" = "#0072B2", #dark blue
                     "ɔ" = "#CC79A7", #pink
                     "u" = "#F0E442") #yellow


## read INPUT file
form.data <- read.delim(INPUT, header=TRUE, sep = "\t")

## create subset for a single area
ex.sub = form.data[form.data$INFORMANT=="BA-FE1-D1",]

## get means
summ_df = summarystats(ex.sub, TESTsumm = "TOT", INTsumm = 0.5, GROUP = "Individual", LEVEL = "Vocal")

## configure plot basic theme settings
theme_opts = list(theme_bw(base_size=6, base_family="Helvetica"), #"Linux Biolinum O"/"Helvetica" / "Linux Libertine O"
                  theme(panel.border = element_blank(),
                        axis.line.x = element_line(size = 0.1, colour = "gray41"),
                        axis.line.y = element_line(size = 0.1, colour = "gray41"),
                        axis.ticks = element_line(colour = "gray41"),
                        axis.text = element_text(colour = "gray41")),
                  labs(x="F2 (Lobanov)", y="F1 (Lobanov)"),
                  scale_y_reverse(limits = c(4.5, -2.5)),
                  scale_x_reverse(limits = c(2.75, -2.5)),
                  guides(colour=FALSE, fill=FALSE))

## 1. MEAN F1xF2 PLOT
  meanF1F2plot <- ggplot(summ_df,
                         aes(x=Lobanov.F2.mean,
                             y=Lobanov.F1.mean,
                             colour=factor(VOCAL),
                             label=VOCAL)) +
                  scale_colour_manual(values=vowel.colours) +
                  scale_fill_manual(values=vowel.colours) +
                  geom_path(color="lightgrey") +
                  geom_point(size=1) +
                  geom_text(aes(label=VOCAL),
                            color="black",
                            hjust=-1.5, vjust=0.5,
                            size=3,
                            family="Helvetica") +
                  theme_opts +
                  theme(axis.title.x = element_text(colour="white"),
                        axis.line.x = element_line(size = 0.1, colour = "gray41"),
                        axis.line.y = element_line(size = 0.1, colour = "gray41"))

    ggsave(filename="example_meanF1F2plot.pdf",
          plot=meanF1F2plot, device=cairo_pdf, path=OUTPUT,
          height=4.5, width=4.5, units="cm", dpi=600)


## 2. DISPERSION F1xF2 PLOT
  F1F2plot = ggplot(ex.sub,
                    aes(x=Lobanov.F2,
                        y=Lobanov.F1,
                        colour=factor(VOCAL))) +
                    geom_text(aes(label=VOCAL), size=2) +
                    scale_colour_manual(values = vowel.colours) +
                    scale_fill_manual(values = vowel.colours) +
                    theme_opts +
                    theme(axis.title.y = element_blank(),
                          axis.text.y = element_blank(),
                          axis.line.x = element_line(size = 0.1, colour = "gray41"),
                          axis.line.y = element_line(size = 0.1, colour = "gray41"))

    ggsave(filename="example_allF1F2plot.pdf",
           plot=F1F2plot, device=cairo_pdf, path=OUTPUT,
           height=4.5, width=4, units="cm", dpi=600)


## 3. F1xF2 ELLIPSE PLOT
    F1F2ellipseplot <-ggplot(ex.sub,
                      aes(x=Lobanov.F2,
                          y=Lobanov.F1,
                          colour=factor(VOCAL))) +
                      stat_ellipse(type="t",
                                   geom="polygon",
                                   alpha=0.25,
                                   aes(fill=factor(VOCAL)),
                                   size = 0.03) +
                      geom_text(data=summ_df, # means obtained through FUNCTION_summarystats.R, at the end so it's not covered by the ellipse
                                aes(x=Lobanov.F2.mean,
                                    y=Lobanov.F1.mean,
                                    colour=factor(VOCAL),
                                    label=VOCAL),
                                size=3,
                                family="Helvetica") +
                      scale_colour_manual(values = vowel.colours) +
                      scale_fill_manual(values = vowel.colours) +
                      theme_opts +
                      theme(axis.title.y = element_blank(),
                            axis.text.y = element_blank(),
                            axis.title.x = element_text(colour="white"),
                            axis.line.x = element_line(size = 0.1, colour = "gray41"),
                            axis.line.y = element_line(size = 0.1, colour = "gray41"))

    ggsave(filename="example_F1F2ellipseplot.pdf",
           plot=F1F2ellipseplot, device=cairo_pdf, path=OUTPUT,
           height=4.5, width=4, units="cm", dpi=600)


### BAD ELLIPSES ##
    ## create subset for speaker GS-FE2-H2
    ex.sub2 = form.data[form.data$INFORMANT=="GS-FE2-H2",]
    summ_df2 = summarystats(ex.sub2, TESTsumm = "TOT", INTsumm = 0.5, GROUP = "Individual", LEVEL = "Vocal")

    ## ellipse plot with means GS-FE2-H2
    F1F2badmeanellipseplot = ggplot(ex.sub2,
                             aes(x=Lobanov.F2,
                                 y=Lobanov.F1,
                                 colour=factor(VOCAL))) +
                            stat_ellipse(type="t",
                                         geom="polygon",
                                         alpha=0.25,
                                         aes(fill=factor(VOCAL)),
                                         size = 0.03) +
                            geom_text(data=summ_df2,
                                      aes(x=Lobanov.F2.mean,
                                          y=Lobanov.F1.mean,
                                          colour=factor(VOCAL),
                                          label=VOCAL),
                                      #colour="black",
                                      size=3,
                                      family="Helvetica") + # mitjanes obtingudes amb summarystats, al final perquè les lletres no quedin tapades per les el·lipses.
                            scale_colour_manual(values = vowel.colours) +
                            scale_fill_manual(values = vowel.colours) +
                            theme_opts

            ggsave(filename="example_badmeanF1F2ellipseplot.pdf",
                   plot=F1F2badmeanellipseplot, device=cairo_pdf, path=OUTPUT,
                   height=6.5, width=6.5, units="cm", dpi=600)

    ## ellipse plot with all items, GS-FE2-H2
    F1F2badallellipseplot = ggplot(ex.sub2,
                                    aes(x = Lobanov.F2,
                                    y = Lobanov.F1,
                                    colour = factor(VOCAL))) +
                            geom_text(aes(label = VOCAL), size=1.5, family="Linux Biolinum O") +
                            stat_ellipse(type = "t",
                                         geom = "polygon",
                                         alpha = 0.25,
                                         aes(fill = factor(VOCAL)),
                                         size = 0.03) +
                            scale_colour_manual(values = vowel.colours) +
                            scale_fill_manual(values = vowel.colours) +
                            theme_opts

            ggsave(filename="example_badallF1F2ellipseplot.pdf",
                   plot=F1F2badallellipseplot, device=cairo_pdf, path=OUTPUT,
                   height=6.5, width=6.5, units="cm", dpi=600)

    ## F1xF2 density plot GS-FE2-H2 ##
    densityplot1 = ggplot(ex.sub2,
                         aes(x = Lobanov.F2,
                             y = Lobanov.F1,
                             colour = factor(VOCAL))) +
                  stat_density2d(aes(fill = factor(VOCAL),
                                     alpha = ..level..,
                                     size = ..level..),
                                 geom = "polygon",
                                 contour=TRUE) +
                  geom_text(data = summ_df2,
                            aes(x = Lobanov.F2.mean,
                                y = Lobanov.F1.mean,
                                label = VOCAL),
                            colour = "black",
                            size = 3,
                            family = "Helvetica") +
                  scale_size(range = c(0.01, 0.03)) + # line width
                  scale_alpha(range = c(0.1, 0.8)) + # color intensity
                  scale_colour_manual(values=vowel.colours) + # line colour
                  scale_fill_manual(values=vowel.colours) + # filling colour
                  guides(alpha = FALSE, # remove all legends
                         size = FALSE,
                         colour = FALSE,
                         fill = FALSE,
                         scale_size = FALSE,
                         scale_alpha = FALSE) +
                  theme_opts

    library(Cairo)
    ggsave(filename=paste(ex.sub2$INFORMANT, "_example_F1F2kdecontourplot.pdf", sep=""),
           plot=densityplot1, device=cairo_pdf, path=OUTPUT,
           height=6.5, width=6.5, units="cm", dpi=600)

    ##################################
    ## F1xF2 density plot BA-FE1-D1 ##
    ##################################
    densityplot2 = ggplot(ex.sub,
                          aes(x = Lobanov.F2,
                              y = Lobanov.F1,
                              colour = factor(VOCAL))) +
      stat_density2d(aes(fill = factor(VOCAL),
                         alpha = ..level..,
                         size = ..level..),
                     geom = "polygon",
                     contour=TRUE) +
      geom_text(data = summ_df,
                aes(x = Lobanov.F2.mean,
                    y = Lobanov.F1.mean,
                    label = VOCAL),
                colour = "black",
                size = 3,
                family = "Helvetica") +
      scale_size(range = c(0.01, 0.03)) + # line width
      scale_alpha(range = c(0.1, 0.8)) + # color intensity
      scale_colour_manual(values=vowel.colours) + # line colour
      scale_fill_manual(values=vowel.colours) + # filling colour
      guides(alpha = FALSE, # remove all legends
             size = FALSE,
             colour = FALSE,
             fill = FALSE,
             scale_size = FALSE,
             scale_alpha = FALSE) +
      theme_opts

    #print(densityplot)
    library(Cairo)
    ggsave(filename=paste(ex.sub$INFORMANT, "_example_F1F2kdecontourplot.pdf", sep=""),
           plot=densityplot2, device=cairo_pdf, path=OUTPUT,
           height=6.5, width=6.5, units="cm", dpi=600)
