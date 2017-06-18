#############################
### PROCESSING OF PERCEPTION TEST TA
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 22/08/2016
### 
###
################################# 

# CLEAN UP ------------------------------------------------------------------
rm(list=ls(all=TRUE))
library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
options(scipen=999) ## disable scientific notation (instead of "3.225594e-06" => "")

################################
## STUFF THAT NEEDS TO BE SET ##
################################

# LOAD LIBRARIES AND FUNCTIONS ------------------------------------------------------------------


library(ggplot2)
library(dplyr)
library(reshape)
library(reshape2)
library(tidyr)

## SET OUTPUT DIRECTORY AND FILE NAME FOR THE FINAL BIG DATA FRAME FILE
OUTPUT = "/Users/evaboschroura/Desktop/PERCEPCIO/RESULTATS/TA_plots/"

# LOAD DATA, SUBSET, CODE, ARRANGE ----------------------------------------------

    INPUT = "/Users/evaboschroura/Desktop/PERCEPCIO/RESULTATS/OS_TA_revisat.txt"
    TA = read.delim(INPUT, header=TRUE, blank.lines.skip = TRUE, sep = "\t")
    
    TA = TA[!TA$INFORMANT %in% c("AEM-FE1-H2", "GS-FE1-H1"),] ## these speakers did not complete TB, remove him from data
    
    TA$RESP = TA$SCORE
    TA = TA %>% mutate(RESP = factor(RESP, levels = c("1", "u", "x", "2", "0"), 
                                     labels = c("Mot esperat", "Mot amb /u/", "Cap", "Tots dos", "Mot\ninesperat")))
    
    answer.colour <- c("Mot esperat" = "white",  # Speaker chose expected word
                       "Mot\ninesperat" = "#252525", # almost black, speaker chose opposite member of the minimal pair/triplet
                       "Mot amb /u/" = "#cccccc", # very light grey, speaker chose member of the minimal triplet with /u/
                       "Cap" = "#636363", # slightly dark grey, speaker didn't chose either word in the minimal pair
                       "Tots dos" = "#969696") # dark grey, speaker chose both words in the minimal pair
    
    # ERRORS = c("0", "u", "x", "2")
    # for (y in seq(along=ERRORS)) {TA[TA$SCORE==ERRORS[y],]$SCORE = "0"}
    # 
    # TA = TA %>% mutate(SCORE = factor(SCORE, levels = c("1", "0"), 
    #                                  labels = c("Resposta\ncorrecta", "Resposta\nincorrecta")))
    # 
    f_TA = droplevels(TA[TA$FUSIO=="F",])
    fp_TA = droplevels(TA[TA$FUSIO=="FP",])
    d_TA = droplevels(TA[TA$FUSIO=="D",])
    
    as.data.frame(TA %>% group_by(FUSIO, ARXIPRESTAT) %>% summarise(number_of_rows = length(unique(INFORMANT))))

# PLOT SETTINGS ----------------------------------------------
    
    theme_opts = list(theme_bw(base_size=8, base_family="Helvetica"), #"Helvetica" / "Linux Libertine O"
                      theme(panel.border = element_blank(),
                            panel.grid = element_blank(),
                            #legend.position = "top",
                            legend.margin=margin(0,0,0,0, unit='cm'), 
                            legend.box.margin = margin(0, 0, 0, 0),
                            legend.key.height = unit(0.5, "cm"),
                            legend.key.width = unit(0.4, "cm"),
                            axis.line.x = element_line(size = 0.1, colour = "gray41"),
                            axis.line.y = element_line(size = 0.1, colour = "gray41"),
                            axis.ticks = element_line(colour = "gray41"),
                            axis.text = element_text(colour = "gray41")),
                      scale_y_continuous(labels=scales::percent_format(),expand=c(0,0)))
    
# PLOTS ----------------------------------------------

f_TAplotARX = lapply(unique(f_TA$ARXIPRESTAT), function(i) {
  
  df = f_TA[f_TA$ARXIPRESTAT==i,] 
  
  ## force all bars to be equally wide (tall, since coords reversed), 
  ## by setting plot height as a multiple of the number of speakers x area
  plotheight = 1*nlevels(droplevels(df$INFORMANT))+1.5 
  
  ## set order of speakers, FE1 first 
  df$INFORMANT = factor(df$INFORMANT, levels = rev(unique(df$INFORMANT))) 
  
  plot =  ggplot(df,  aes(x=INFORMANT, fill=RESP, order=RESP)) + 
              geom_bar(position="fill", color="#969696", width=0.6) +
              scale_fill_manual(values=answer.colour, guide=FALSE) +         
              #scale_fill_grey(start = 1, end=0.1, guide=FALSE) + ## start=white, right answers, end=dark grey, wrong mid back vowel         
              theme_opts + coord_flip() + 
              guides(fill = guide_legend(reverse=T, title = NULL)) +
              xlab(NULL) +  ylab("Respostes")
  
  
  ggsave(filename=paste("f_", i, "_perception_TA.pdf", sep=""),
         plot=plot, device=cairo_pdf, path=OUTPUT,
         height=plotheight, width=13, units="cm", dpi=600)

})



fp_TA = droplevels(TA[TA$FUSIO=="FP",])

fp_TAplotARX = lapply(unique(fp_TA$ARXIPRESTAT), function(i) {
  
  df = fp_TA[fp_TA$ARXIPRESTAT==i,] 
  
  ## force all bars to be equally wide (tall, since coords reversed), 
  ## by setting plot height as a multiple of the number of speakers x area
  plotheight = 1*nlevels(droplevels(df$INFORMANT))+1.5 
  
  ## set order of speakers, FE1 first 
  df$INFORMANT = factor(df$INFORMANT, levels = rev(unique(df$INFORMANT))) 
  
  plot =  ggplot(df,  aes(x=INFORMANT, fill=RESP, order=RESP)) + 
    geom_bar(position="fill", color="#969696", width=0.6) +
    scale_fill_manual(values=answer.colour, guide=FALSE) +          
    #scale_fill_grey(start = 1, end=0.1, guide=FALSE) + ## start=white, right answers, end=dark grey, wrong mid back vowel         
    theme_opts + coord_flip() + 
    guides(fill = guide_legend(reverse=T, title = NULL)) +
    xlab(NULL) +  ylab("Respostes")
  
  
  ggsave(filename=paste("fp_", i, "_perception_TA.pdf", sep=""),
         plot=plot, device=cairo_pdf, path=OUTPUT,
         height=plotheight, width=13, units="cm", dpi=600)
  
})


d_TA = droplevels(TA[TA$FUSIO=="D",])

d_TAplotARX = lapply(unique(d_TA$ARXIPRESTAT), function(i) {
  
  df = d_TA[d_TA$ARXIPRESTAT==i,] 
  
  ## force all bars to be equally wide (tall, since coords reversed), 
  ## by setting plot height as a multiple of the number of speakers x area
  plotheight = 1*nlevels(droplevels(df$INFORMANT))+1.5 
  
  ## set order of speakers, FE1 first 
  df$INFORMANT = factor(df$INFORMANT, levels = rev(unique(df$INFORMANT))) 
  
  plot =  ggplot(df,  aes(x=INFORMANT, fill=RESP, order=RESP)) + 
    geom_bar(position="fill", color="#969696", width=0.6) +
    scale_fill_manual(values=answer.colour, guide=FALSE) +      
    #scale_fill_grey(start = 1, end=0.1, guide=FALSE) + ## start=white, right answers, end=dark grey, wrong mid back vowel         
    theme_opts + coord_flip() + 
    guides(fill = guide_legend(reverse=T, title = NULL)) +
    xlab(NULL) +  ylab("Respostes")
  
  
  ggsave(filename=paste("d_", i, "_perception_TA.pdf", sep=""),
         plot=plot, device=cairo_pdf, path=OUTPUT,
         height=plotheight, width=13, units="cm", dpi=600)
  
})

TA_ans = dcast(TA, formula = INFORMANT+FUSIO+ARXIPRESTAT+EDAT+SEXE ~ RESP)
TA_ans$ERRORS = rowSums(TA_ans[,c("Mot amb /u/", "Cap", "Tots dos", "Mot\ninesperat")], na.rm = TRUE)
TA_ans$TOTAL = rowSums(TA_ans[,c("ERRORS", "Mot esperat")], na.rm = TRUE)

