#############################
### BOXPLOT example
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

## SET UP
INPUT <- "/path/to/input/TOT_normalitzat.txt"
OUTPUT <- "/path/to/output/"

## read INPUT file
form.data <- read.delim(INPUT, header=TRUE, sep = "\t")


library(ggplot2)

ex.sub = form.data[form.data$VOCAL=="a" & form.data$INFORMANT=="AEI-FE1-D1",]

theme_opts <- list(theme_bw(base_size=8, base_family="Helvetica"), #"Helvetica" / "Linux Libertine O"
                   theme(panel.border = element_blank(),
                         axis.line.x = element_line(size = 0.1, colour = "gray41"),
                         axis.line.y = element_line(size = 0.1, colour = "gray41"),
                         #axis.line = element_line(size = 0.1, colour = "gray41"),
                         axis.ticks = element_line(colour = "gray41"),
                         axis.text = element_blank(),
                         #axis.text = element_text(colour = "gray41"),
                         axis.title = element_blank()))

exampleF1boxplot <- ggplot(ex.sub, aes(x=VOCAL, y=F1)) +
                    #stat_boxplot(geom="errorbar", width=0.2) +
                    geom_boxplot(varwidth = TRUE) +
                    stat_summary(fun.y=mean, geom="point", shape=8, size=3) +
                    theme_opts

print(exampleF1boxplot)
ggsave(filename="example_boxplot.pdf",
       plot=exampleF1boxplot, device=cairo_pdf, path=OUTPUT,
       width=3, height=7, units="cm", dpi=600)
