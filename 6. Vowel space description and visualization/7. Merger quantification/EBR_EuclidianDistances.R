#############################
### EUCLIDEAN DISTANCES
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 26/01/2016
###
### The script computes the Euclidean distances between all pairs of vowels in the system of each speaker
### and creates a heat map with each matrix.
###
### Input (INPUT): data file with all unnormalized and normalized formant values obtained from
### 'EBR_normalization.R' ("TOT_normalitzat.txt")
###
### Output: a heatmap of Euclidean distances of all vowel pairs in the vowel system of each speaker, and a table
### with all ED.
###
#################################

## CLEAN UP
library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
rm(list=ls(all=TRUE))
options(scipen=999) ## disable scientific notation (instead of "3.225594e-06" => "")

################################
## STUFF THAT NEEDS TO BE SET ##
################################

## COMPLETE PATH TO THE FILE TO BE PROCESSED
INPUT = "/path/to/input/TOT_normalitzat.txt"

## SET OUTPUT DIRECTORY AND FILE NAMES
OUTPUT_MID_T1T3_HEATMAPS_EBRtesi = "/path/to/output/heatmaps/deuclidianes/"
OUTPUT_MID_T1T3_IND_ED = "/path/to/output/tables/deuclidianes/"

## LOAD PACKAGES AND FUNCTIONS
library(ggplot2) ## plots (though it's called again within the functions)
library(Cairo) ## through device=cairo_pdf, pdfs with IPA symbols & diachritics are correctly created
library(reshape2) ## melt()
library(tibble)
library(tidyr)
library(dplyr)

source("/path/to/EBR_FUNCTION_summarystats.R")

###########
## START ##
###########

## read INPUT file
form.data <- read.delim(INPUT, header=TRUE, sep = "\t")

# DISTANCE ----------------------------------------------------------------

#### EUCLIDEAN DISTANCES ####
#############################

## set theme options
theme_opts = list(theme_minimal(base_size=10, base_family="Helvetica"), #"Linux Biolinum O"/"Helvetica" / "Linux Libertine O"
                  theme(axis.text = element_text(colour = "gray41"),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        panel.grid = element_blank()#,
                        # legend.key.size = unit(0.2, "cm"),
                        # legend.text = element_text(size = 5),
                        # legend.title = element_text(size = 7)#
                        ),
                  coord_fixed(ratio=1),
                  #guides(fill = guide_colourbar(title.hjust = 0.2))
                  guides(fill = FALSE)
)

### Get individual distance matrices
indsumm = summarystats(form.data, INTsumm = 0.5, TESTsumm = "T1-T3", GROUP = "Individual", LEVEL = "Vocal")

inddistmeanmatrix = lapply(unique(indsumm$INFORMANT), function(i) {

  mat = as.matrix(indsumm[indsumm$INFORMANT==i, c("Lobanov.F1.mean","Lobanov.F2.mean")])
  rownames(mat) = indsumm$VOCAL[indsumm$INFORMANT==i]

  isflipflop = indsumm[indsumm$INFORMANT==i, c("VOCAL", "F1.mean","F2.mean")] ## data.frame used by geom_text to identify flip-flops

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
                # hjust=7,
                family="Helvetica",
                color = "white",
                size = 2) +
      geom_text(data=meanED.Os, ## add value of mean [o]-[ɔ] ED
                aes(VOCAL,
                    VOCAL2,
                    label = ifelse(isflipflop[isflipflop$VOCAL=="ɔ","F1.mean"]<isflipflop[isflipflop$VOCAL=="o","F1.mean"], ## if [ɔ] has a lower F1 than [o]
                                   format(-DE,decimal.mark=","), ## print a negative Euclidean Distance value
                                   format(DE, decimal.mark=","))), ## otherwise, just print the Euclidean distance value
                color = "white",
                family="Helvetica",
                fontface = "bold",
                size = 2) +
      geom_text(data=meanED.Es, ## add value of mean [e]-[ɛ] ED
                aes(VOCAL,
                    VOCAL2,
                    label = format(DE, decimal.mark=",")),
                #label = paste("DE [e]-[ɛ]\n= ", format(value, decimal.mark=","), sep="")),
                color = ifelse(meanED.Es$DE < 0.8, "grey81", "grey41"), ## conditional color, so it stands out from background
                family="Helvetica Neue",
                fontface = "italic",
                size = 2) +
      geom_text(data=meanmax.ED, ## add value of maximum ED
                aes(VOCAL,
                    VOCAL2,
                    label = format(DE, decimal.mark=",")),
                #label = paste("DE màx.\n= ", format(value, decimal.mark=","), sep="")),
                color = "grey41",
                family="Helvetica",
                size = 2) +
      theme_opts +
      theme(#legend.position = c(0.1, 0.8),
            axis.text.y = element_blank(),
            axis.text.x = element_text(vjust = 0.5, margin = unit(c(t = -0.1, r = 0, b = 0, l = 0),"cm"))) +
      #guides(fill = guide_colourbar(reverse = TRUE, title.hjust = 0.3))
      guides(fill=FALSE)

    ggsave(filename=paste(i, "_50_T1-T3_meansEDmatrix_Lobanov.pdf", sep=""),
           plot=meansEDheatmap, device=cairo_pdf, path=OUTPUT_MID_T1T3_HEATMAPS_EBRtesi,
           height=4.2, width=4.2, units="cm", dpi=600)

  # Return data frame with distances, time and participants
  data.frame(INFORMANT=i, d) %>% rownames_to_column("vocal1")

  })

# Combine all time points into single long data frame of distances
Taula.dist.means =  bind_rows(inddistmeanmatrix) %>%
  melt(id.var=c("INFORMANT","vocal1"), variable.name="vocal2", value.name="meanDE") %>%
  filter(!is.na(meanDE)) %>%
  rowwise %>%
  mutate(meanDE = round(meanDE, digits=2)) %>%
  mutate(Parell.vocals = paste(sort(c(as.character(vocal1), as.character(vocal2))), collapse="-")) %>%
  select(INFORMANT, Parell.vocals, meanDE) %>%
  arrange(INFORMANT, Parell.vocals)

write.table(Taula.dist.means,
            file=paste(OUTPUT_MID_T1T3_IND_ED,"DE_individuals.txt", sep=""),
            quote=FALSE, sep="\t", row.names=FALSE)

