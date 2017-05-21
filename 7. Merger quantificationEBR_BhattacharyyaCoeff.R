#############################
### BHATTACHARRYA COEFFICIENTS
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 26/01/2016
###
### The script computes the Bhattacharyya Coefficients between all pairs of vowels in the system of each speaker
### and creates a heat map with each matrix.
###
### Input (INPUT): data file with all unnormalized and normalized formant values obtained from
### 'EBR_normalization.R' ("TOT_normalitzat.txt")
###
### Output: a heatmap of Bhattacharyya Coefficients of all vowel pairs in the vowel system of each speaker, and a table
### with all coefficients.
###
#################################

## CLEAN UP
rm(list=ls(all=TRUE))
library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
options(scipen=999) ## disable scientific notation (instead of "3.225594e-06" => "")

################################
## STUFF THAT NEEDS TO BE SET ##
################################

## COMPLETE PATH TO THE FILE TO BE PROCESSED
INPUT = "/path/to/input/TOT_normalitzat.txt"

## SET OUTPUT DIRECTORY AND FILE NAMES
OUTPUT = "/path/to/output/bhattacharyya/"
OUTPUT_MID_T1T3_IND_CB = "path/to/table/output/bhattacharyya/"

## LOAD PACKAGES AND FUNCTIONS
library(ggplot2) ## plots (though it's called again within the functions)
library(Cairo) ## through device=cairo_pdf, pdfs with IPA symbols & diachritics are correctly created
library(tibble)
library(reshape2) ## melt()
library(adehabitatHR)
library(tidyr)
library(dplyr)

###########
## START ##
###########

## read INPUT file
form.data <- read.delim(INPUT, header=TRUE, sep = "\t")

subdf = form.data[form.data$INTERVAL==0.5 & form.data$TEST!="T2",]

  ## set plot theme options
  theme_opts = list(theme_minimal(base_size=10, base_family="Helvetica"), #"Helvetica" / "Linux Libertine O"
                    theme(axis.text = element_text(colour = "gray41"),
                          axis.title = element_blank(),
                          axis.ticks = element_blank(),
                          panel.grid = element_blank()#,
                          # legend.key.size = unit(0.2, "cm"),
                          # legend.text = element_text(size = 5),
                          # legend.title = element_text(size = 7)
                          ),
                    coord_fixed(ratio=1),
                    #guides(fill = guide_colourbar(title.hjust = 0.2))
                    guides(fill = FALSE)
  )

## BHATTACHARYYA COEFFICIENT ##
###############################

bhattacharyyacf = lapply(unique(subdf$INFORMANT), function(i) {

  df = subdf[subdf$INFORMANT==i,]

  vowel.order <- c("i", "e", "ɛ", "a", "ɔ", "o", "u")
  df$VOCAL <- factor(df$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
  df <- df[order(df$VOCAL),] # reorder by VOWEL

  spdf = SpatialPointsDataFrame(cbind(df$Lobanov.F1,df$Lobanov.F2), data.frame(VOWEL=df$VOCAL))
  ba = kerneloverlap(spdf, method="BA") # kern="epa"

  ba[upper.tri(ba, diag=FALSE)] = NA

  meltedba = melt(ba,
                 varnames = c("VOCAL", "VOCAL2"),
                 value.name = "BA",
                 na.rm = TRUE)

  meltedba$BA = round(meltedba$BA, 2) ## round distance values to two digits

  # Text means: get [o]-[ɔ], [e]-[ɛ] and maximum distance values, to add them to heatmap
  BA.Os = meltedba[meltedba$VOCAL=="o" & meltedba$VOCAL2=="ɔ",]
  BA.Es = meltedba[meltedba$VOCAL=="ɛ" & meltedba$VOCAL2=="e",]
  minBA = meltedba[which.min(meltedba[,"BA"]),]

  vowelnames = subset(meltedba, VOCAL==VOCAL2) ## get vowel labels to add as geom_text on plot

  BAheatmap =  ggplot(data = meltedba,
                             aes(VOCAL, VOCAL2, fill = BA)) +
              geom_tile(color = "white") +
              scale_fill_gradient2(high = "#4F535F", ## dark grey
                                   mid = "#98A2AA", ## bluish grey
                                   low = "#E1F2F5", ## very light blue
                                   midpoint = 0.5, #max(meltedba$BA)/2,
                                   name = "CB",
                                   breaks = c(1, 0.75, 0.50, 0.25, 0),
                                   labels = c("1", "0.75", "0.50", "0.25", "0")) +
              geom_text(data=vowelnames,
                        aes(label=VOCAL),
                        #vjust=3.2,
                        family="Helvetica",
                        color = "white",
                        size = 2) +
              geom_text(data=BA.Os, ## add value of mean [o]-[ɔ] ED
                        aes(VOCAL,
                            VOCAL2,
                            label = format(BA, decimal.mark=",")),
                        color = "white",
                        size = 2,
                        family="Helvetica",
                        fontface = "bold") +
              geom_text(data=BA.Es, ## add value of mean [e]-[ɛ] ED
                                  aes(VOCAL,
                                      VOCAL2,
                                      label = format(BA, decimal.mark=",")),
                                  #label = paste("DE [e]-[ɛ]\n= ", format(value, decimal.mark=","), sep="")),
                                  color = ifelse(BA.Es$BA > 0.5, "grey81", "grey41"), ## conditional color, so it stands out from background
                                  size = 2,
                                  family="Helvetica Neue",# "Helvetica",
                                  fontface = "italic") +
              geom_text(data=minBA, ## add value of maximum ED
                        aes(VOCAL,
                            VOCAL2,
                            label = format(BA, decimal.mark=",")),
                        #label = paste("DE màx.\n= ", format(value, decimal.mark=","), sep="")),
                        color = "grey41",
                        size = 2,
                        family="Helvetica") +
              theme_opts +
              theme(#legend.position = c(0.1, 0.8),
                     axis.text.y = element_blank(),
                     axis.text.x = element_text(vjust = 0, margin = unit(c(t = -0.1, r = 0, b = 0, l = 0),"cm"))) +
              #guides(fill = guide_colourbar(reverse = FALSE, title.hjust = 0.3))
              guides(fill = FALSE)

  ggsave(filename=paste(i, "_50_T1-T3_Bhattacharyya_Lobanov.pdf", sep=""),
         plot=BAheatmap, device=cairo_pdf, path=OUTPUT,
         height=4.2, width=4.2, units="cm", dpi=600)

  # Return data frame with BHA. COEFF., VOWEL and participants
  data.frame(INFORMANT=i, ba) %>% rownames_to_column("vocal1")

})


Taula.bhacf =   bind_rows(bhattacharyyacf) %>%
  melt(id.var=c("INFORMANT","vocal1"), variable.name="vocal2", value.name="BhaCoeff") %>%
  filter(!is.na(BhaCoeff)) %>%
  rowwise %>%
  mutate(BhaCoeff = round(BhaCoeff, digits=2)) %>%
  mutate(Parell.vocals = paste(sort(c(as.character(vocal1), as.character(vocal2))), collapse="-")) %>%
  select(INFORMANT, Parell.vocals, vocal1, vocal2, BhaCoeff) %>%
  arrange(INFORMANT, Parell.vocals)

write.table(Taula.bhacf,
            file=paste(OUTPUT_MID_T1T3_IND_CB,"CB_individuals.txt", sep=""),
            quote=FALSE, sep="\t", row.names=FALSE)

