#############################
### PROCESSING OF PERCEPTION TEST TB
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 22/08/2016
### 
################################# 

# CLEAN UP ------------------------------------------------------------------
rm(list=ls(all=TRUE))
library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
options(scipen=999) ## disable scientific notation (instead of "3.225594e-06" => "0.000003225594")

################################
## STUFF THAT NEEDS TO BE SET ##
################################

# LOAD LIBRARIES AND FUNCTIONS ------------------------------------------------------------------


library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)

## SET OUTPUT DIRECTORY AND FILE NAME FOR THE FINAL BIG DATA FRAME FILE
OUTPUT = "/Users/evaboschroura/Desktop/PERCEPCIO/RESULTATS/TB_plots/"

# LOAD DATA, SUBSET, CODE, ARRANGE ----------------------------------------------

    INPUT = "/Users/evaboschroura/Desktop/PERCEPCIO/RESULTATS/OS_TB_revisat.txt"
    TB = read.delim(INPUT, header=TRUE, blank.lines.skip = TRUE, sep = "\t")
    TB$SCORE = as.factor(TB$SCORE)
    TB = TB[TB$INFORMANT!="AEM-FE1-H2",] ## speaker AEM-FE1-H2 did not complete the test, remove him from data
    
    f_TB = droplevels(TB[TB$FUSIO=="F",])
      f_TB_good = droplevels(f_TB[f_TB$ESTIMUL=="bo",])
      f_TB_unexpected = droplevels(f_TB[f_TB$ESTIMUL!="bo",])
    fp_TB = droplevels(TB[TB$FUSIO=="FP",])
      fp_TB_good = droplevels(fp_TB[fp_TB$ESTIMUL=="bo",])
      fp_TB_unexpected = droplevels(fp_TB[fp_TB$ESTIMUL!="bo",])
    d_TB = droplevels(TB[TB$FUSIO=="D",])
      d_TB_good = droplevels(d_TB[d_TB$ESTIMUL=="bo",])
      d_TB_unexpected = droplevels(d_TB[d_TB$ESTIMUL!="bo",])
    
    #as.data.frame(TB %>% group_by(FUSIO, ARXIPRESTAT) %>% summarise(number_of_rows = length(unique(INFORMANT))))

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

f_TBplotARX_good = lapply(unique(f_TB_good$ARXIPRESTAT), function(i) {
  
  df = f_TB_good[f_TB_good$ARXIPRESTAT==i,] 
  df$RESP = df$SCORE
  df = df %>% mutate(RESP = factor(RESP, levels = c("1", "0"),
                                   labels = c("No", "Sí")))
  
  answer.colour <- c("Sí" = "#252525",  # it shouldn't sound weird, it'll probably sound weird if speakers are trying to hypercorrect
                     "No" = "white") # expected answer, there's theoretically nothing weird about the sentence
  
  ## force all bars to be equally wide (tall, since coords reversed), 
  ## by setting plot height as a multiple of the number of speakers x area
  plotheight = 1*nlevels(droplevels(df$INFORMANT))+1.5 
  
  ## set order of speakers, FE1 first 
  df$INFORMANT = factor(df$INFORMANT, levels = rev(unique(df$INFORMANT))) 
  
  plot =  ggplot(df,  aes(x=INFORMANT, fill=RESP, order=RESP)) + 
    geom_bar(position="fill", color="#969696", width=0.6) +
    scale_fill_manual(values = answer.colour) +
    theme_opts + coord_flip() + 
    guides(fill = guide_legend(reverse=T, title = "Estrany")) +
    xlab(NULL) +  ylab("Respostes")
  
  
  ggsave(filename=paste("f_", i, "_perception_TBgood.pdf", sep=""),
         plot=plot, device=cairo_pdf, path=OUTPUT,
         height=plotheight, width=13, units="cm", dpi=600)
  
})

f_TBplotARX_unexepected = lapply(unique(f_TB_unexpected$ARXIPRESTAT), function(i) {
  
  df = f_TB_unexpected[f_TB_unexpected$ARXIPRESTAT==i,] 
  df$RESP = df$SCORE
  df = df %>% mutate(RESP = factor(RESP, levels = c("1", "0"),
                                   labels = c("Sí", "No")))
  
  answer.colour <- c("Sí" = "white",  #expected answer, if speakers distinguish it should sound weird
                     "No" = "#252525") #unexpected answer, it won't sound weird if speakers do not distinguish
                     
  
  ## force all bars to be equally wide (tall, since coords reversed), 
  ## by setting plot height as a multiple of the number of speakers x area
  plotheight = 1*nlevels(droplevels(df$INFORMANT))+1.5 
  
  ## set order of speakers, FE1 first 
  df$INFORMANT = factor(df$INFORMANT, levels = rev(unique(df$INFORMANT))) 
  
  plot =  ggplot(df,  aes(x=INFORMANT, fill=RESP, order=RESP)) + 
    geom_bar(position="fill", color="#969696", width=0.6) +
    scale_fill_manual(values = answer.colour) + ## start=white, right answers, end=dark grey, wrong mid back vowel         
    theme_opts + coord_flip() + 
    guides(fill = guide_legend(reverse=T, title = "Estrany")) +
    xlab(NULL) +  ylab("Respostes")
  
  
  ggsave(filename=paste("f_", i, "_perception_TBunexpected.pdf", sep=""),
         plot=plot, device=cairo_pdf, path=OUTPUT,
         height=plotheight, width=13, units="cm", dpi=600)
  
})



fp_TBplotARX_good = lapply(unique(fp_TB_good$ARXIPRESTAT), function(i) {
  
  df = fp_TB_good[fp_TB_good$ARXIPRESTAT==i,] 
  df$RESP = df$SCORE
  df = df %>% mutate(RESP = factor(RESP, levels = c("1", "0"),
                                   labels = c("No", "Sí")))
  
  answer.colour <- c("Sí" = "#252525",  # it shouldn't sound weird, it'll probably sound weird if speakers are trying to hypercorrect
                     "No" = "white") # expected answer, there's theoretically nothing weird about the sentence
  
  ## force all bars to be equally wide (tall, since coords reversed), 
  ## by setting plot height as a multiple of the number of speakers x area
  plotheight = 1*nlevels(droplevels(df$INFORMANT))+1.5
  
  ## set order of speakers, FE1 first 
  df$INFORMANT = factor(df$INFORMANT, levels = rev(unique(df$INFORMANT))) 
  
  plot =  ggplot(df,  aes(x=INFORMANT, fill=RESP, order=RESP)) + 
    geom_bar(position="fill", color="#969696", width=0.6) +
    scale_fill_manual(values = answer.colour) +
    theme_opts + coord_flip() + 
    guides(fill = guide_legend(reverse=T, title = "Estrany")) +
    xlab(NULL) +  ylab("Respostes")
  
  
  ggsave(filename=paste("fp_", i, "_perception_TBgood.pdf", sep=""),
         plot=plot, device=cairo_pdf, path=OUTPUT,
         height=plotheight, width=13, units="cm", dpi=600)
  
})

fp_TBplotARX_unexepected = lapply(unique(fp_TB_unexpected$ARXIPRESTAT), function(i) {
  
  df = fp_TB_unexpected[fp_TB_unexpected$ARXIPRESTAT==i,] 
  df$RESP = df$SCORE
  df = df %>% mutate(RESP = factor(RESP, levels = c("1", "0"),
                                   labels = c("Sí", "No")))
  
  answer.colour <- c("Sí" = "white",  #expected answer, if speakers distinguish it should sound weird
                     "No" = "#252525") #unexpected answer, it won't sound weird if speakers do not distinguish
  
  
  ## force all bars to be equally wide (tall, since coords reversed), 
  ## by setting plot height as a multiple of the number of speakers x area
  plotheight = 1*nlevels(droplevels(df$INFORMANT))+1.5 
  
  ## set order of speakers, FE1 first 
  df$INFORMANT = factor(df$INFORMANT, levels = rev(unique(df$INFORMANT))) 
  
  plot =  ggplot(df,  aes(x=INFORMANT, fill=RESP, order=RESP)) + 
    geom_bar(position="fill", color="#969696", width=0.6) +
    scale_fill_manual(values = answer.colour) + ## start=white, right answers, end=dark grey, wrong mid back vowel         
    theme_opts + coord_flip() + 
    guides(fill = guide_legend(reverse=T, title = "Estrany")) +
    xlab(NULL) +  ylab("Respostes")
  
  
  ggsave(filename=paste("fp_", i, "_perception_TBunexpected.pdf", sep=""),
         plot=plot, device=cairo_pdf, path=OUTPUT,
         height=plotheight, width=13, units="cm", dpi=600)
  
})

d_TBplotARX_good = lapply(unique(d_TB_good$ARXIPRESTAT), function(i) {
  
  df = d_TB_good[d_TB_good$ARXIPRESTAT==i,] 
  df$RESP = df$SCORE
  df = df %>% mutate(RESP = factor(RESP, levels = c("1", "0"),
                                   labels = c("No", "Sí")))
  
  answer.colour <- c("Sí" = "#252525",  # it shouldn't sound weird, it'll probably sound weird if speakers are trying to hypercorrect
                     "No" = "white") # expected answer, there's theoretically nothing weird about the sentence
  
  ## force all bars to be equally wide (tall, since coords reversed), 
  ## by setting plot height as a multiple of the number of speakers x area
  plotheight = 1*nlevels(droplevels(df$INFORMANT))+1.5 
  
  ## set order of speakers, FE1 first 
  df$INFORMANT = factor(df$INFORMANT, levels = rev(unique(df$INFORMANT))) 
  
  plot =  ggplot(df,  aes(x=INFORMANT, fill=RESP, order=RESP)) + 
    geom_bar(position="fill", color="#969696", width=0.6) +
    scale_fill_manual(values = answer.colour) +
    theme_opts + coord_flip() + 
    guides(fill = guide_legend(reverse=T, title = "Estrany")) +
    xlab(NULL) +  ylab("Respostes")
  
  
  ggsave(filename=paste("d_", i, "_perception_TBgood.pdf", sep=""),
         plot=plot, device=cairo_pdf, path=OUTPUT,
         height=plotheight, width=13, units="cm", dpi=600)
  
})

d_TBplotARX_unexepected = lapply(unique(d_TB_unexpected$ARXIPRESTAT), function(i) {
  
  df = d_TB_unexpected[d_TB_unexpected$ARXIPRESTAT==i,] 
  df$RESP = df$SCORE
  df = df %>% mutate(RESP = factor(RESP, levels = c("1", "0"),
                                   labels = c("Sí", "No")))
  
  answer.colour <- c("Sí" = "white",  #expected answer, if speakers distinguish it should sound weird
                     "No" = "#252525") #unexpected answer, it won't sound weird if speakers do not distinguish
  
  
  ## force all bars to be equally wide (tall, since coords reversed), 
  ## by setting plot height as a multiple of the number of speakers x area
  plotheight = 1*nlevels(droplevels(df$INFORMANT))+1.5
  
  ## set order of speakers, FE1 first 
  df$INFORMANT = factor(df$INFORMANT, levels = rev(unique(df$INFORMANT))) 
  
  plot =  ggplot(df,  aes(x=INFORMANT, fill=RESP, order=RESP)) + 
    geom_bar(position="fill", color="#969696", width=0.6) +
    scale_fill_manual(values = answer.colour) + ## start=white, right answers, end=dark grey, wrong mid back vowel         
    theme_opts + coord_flip() + 
    guides(fill = guide_legend(reverse=T, title = "Estrany")) +
    xlab(NULL) +  ylab("Respostes")
  
  
  ggsave(filename=paste("d_", i, "_perception_TBunexpected.pdf", sep=""),
         plot=plot, device=cairo_pdf, path=OUTPUT,
         height=plotheight, width=13, units="cm", dpi=600)
  
})

TB_good = droplevels(TB[TB$ESTIMUL=="bo",])
TB_unexpected = droplevels(TB[TB$ESTIMUL!="bo",])
TB_un_ans = dcast(TB_unexpected, formula = INFORMANT+FUSIO+ARXIPRESTAT+EDAT+SEXE ~ SCORE)

