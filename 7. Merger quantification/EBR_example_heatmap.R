#############################
### HEATMAP example
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
options(scipen=999) ## disable scientific notation (instead of "3.225594e-06" => "")

## SET UP
INPUT <- "/path/to/input/TOT_normalitzat.txt"
OUTPUT <- "/path/to/output/"

## LOAD PACKAGES AND FUNCTIONS
library(ggplot2) ## plots (though it's called again within the functions)
library(Cairo) ## through device=cairo_pdf, pdfs with IPA symbols & diachritics are correctly created
library(reshape2) ## melt()
library(tibble)
library(tidyr)
library(dplyr)

source("/path/to/FUNCTION_summarystats.R")

###########
## START ##
###########

## read INPUT file
form.data <- read.delim(INPUT, header=TRUE, sep = "\t")

## create subset for a single speaker
ex.sub = form.data[form.data$INFORMANT=="BA-FE1-D1",]

# DISTANCE ----------------------------------------------------------------

#### EUCLIDEAN DISTANCES ####
#############################

## set theme options
theme_opts = list(theme_minimal(base_size=12, base_family="Helvetica"), #"Linux Biolinum O"/"Helvetica" / "Linux Libertine O"
                  theme(axis.text = element_text(colour = "gray41"),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        panel.grid = element_blank(),
                        legend.key.size = unit(0.3, "cm"),
                        legend.text = element_text(size = 6),
                        legend.title = element_text(size = 8)#
                  ),
                  coord_fixed(ratio=1),
                  #guides(fill = guide_colourbar(title.hjust = 0.2))
                  guides(fill = FALSE)
)



### Get individual distance matrices
indsumm = summarystats(ex.sub, INTsumm = 0.5, TESTsumm = "T1-T3", GROUP = "Individual", LEVEL = "Vocal")

mat = as.matrix(indsumm[, c("Lobanov.F1.mean","Lobanov.F2.mean")])
rownames(mat) = indsumm$VOCAL

isflipflop = indsumm[, c("VOCAL", "F1.mean","F2.mean")] ## data.frame used by geom_text to identify flip-flops

# Distance matrix
d = as.matrix(dist(mat, method = "euclidean"))

# Keep only lower triangle
d[upper.tri(d, diag=FALSE)] = NA

melteddistmeans = melt(d,
                       varnames = c("VOCAL", "VOCAL2"),
                       value.name = "DE",
                       na.rm = TRUE)

melteddistmeans$DE = round(melteddistmeans$DE, 2) ## round distance values to two digits

# Text means: get [o]-[ɔ], [e]-[ɛ] and maximum distance values, to add them to heatmap
meanED.Os = melteddistmeans[melteddistmeans$VOCAL=="o" & melteddistmeans$VOCAL2=="ɔ",]
meanED.Es = melteddistmeans[melteddistmeans$VOCAL=="ɛ" & melteddistmeans$VOCAL2=="e",]
meanmax.ED = melteddistmeans[which.max(melteddistmeans[,"DE"]),]

## means heatmap
vowelnames1 = subset(melteddistmeans, VOCAL==VOCAL2) ## get vowel labels to add as geom_text on plot

meansEDheatmap =  ggplot(data = melteddistmeans,
                         aes(VOCAL, VOCAL2, fill = DE)) +
                  geom_tile(color = "white") + ## add heatmap
                  scale_fill_gradient2(low = "#4F535F", ## dark grey
                                       mid = "#CAF0C7", ## green
                                       high = "#EAFCE8", ## very light green
                                       midpoint = max(melteddistmeans$DE)/2,
                                       guide = "colourbar") +
                  geom_text(data=vowelnames1,
                            aes(label=VOCAL2),
                            family="Helvetica",
                            color = "white",
                            size = 3) +
                  geom_text(data=meanED.Os, ## add value of mean [o]-[ɔ] ED
                            aes(VOCAL,
                                VOCAL2,
                                label = ifelse(isflipflop[isflipflop$VOCAL=="ɔ","F1.mean"]<isflipflop[isflipflop$VOCAL=="o","F1.mean"], ## if [ɔ] has a lower F1 than [o]
                                               format(-DE,decimal.mark=","), ## print a negative Bhattacharyya value
                                               format(DE, decimal.mark=","))), ## otherwise, just print the Bhattacharyya value
                            color = "white",
                            family="Helvetica",
                            fontface = "bold",
                            size = 3) +
                  geom_text(data=meanED.Es, ## add value of mean [e]-[ɛ] ED
                            aes(VOCAL,
                                VOCAL2,
                                label = format(DE, decimal.mark=",")),
                            color = ifelse(meanED.Es$DE < 0.8, "grey81", "grey41"), ## conditional color, so it stands out from background
                            family="Helvetica Neue",
                            fontface = "italic",
                            size = 3) +
                  geom_text(data=meanmax.ED, ## add value of maximum ED
                            aes(VOCAL,
                                VOCAL2,
                                label = format(DE, decimal.mark=",")),
                            color = "grey41",
                            family="Helvetica",
                            size = 3) +
                  theme_opts +
                  theme(#legend.position = c(0.1, 0.8),
                    axis.text.y = element_blank(),
                    axis.text.x = element_text(vjust = 0.5, margin = unit(c(t = -0.1, r = 0, b = 0, l = 0),"cm"))) +
                  guides(fill = guide_colourbar(reverse = TRUE, title.hjust = 0.3))

ggsave(filename="BA-FE1-D1_example_meansED_heatmap.pdf",
       plot=meansEDheatmap, device=cairo_pdf, path=OUTPUT,
       height=5, width=7, units="cm", dpi=600)


