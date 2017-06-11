#############################
### STATISTICAL ANALYSIS OF THE SIGNIFICANCE OF THE /o/-/ɔ/ MERGER
###
### Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
### Affiliation: Departament de Filologia Catalana - Universitat de Barcelona
### Date: 01/06/2017
###
### 
###
#################################

# CLEAN UP ------------------------------------------------------------------
rm(list=ls(all=TRUE))
library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
options(scipen=999) ## disable scientific notation (instead of "3.225594e-06" => "")


# LOAD LIBRARIES AND FUNCTIONS ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tibble)
library(reshape2) ## melt()
library(lme4)
library(lmerTest)
library(ggplot2)
library(car)
library(xtable)
library(effects)
library(RColorBrewer)
library(scales)

# RECONTRAST FUNCTION
# recontrast.R, adapted from "http://www.ling.upenn.edu/~joseff/scripts/recontrast.R" (Josef Fruehwald, 2009)
# Function created to sum code categorical variables but give significant names to columns in
# the matrix so coefficients are easily interpreted (see http://www.ling.upenn.edu/~joseff/scripts/recontrast.html)
#
# Adapted to include DEVIATION CODING: (-.5, .5) instead of (-1,1)
# (See Mirman, D., 2014. Growth Curve Analysis and Visualization Using R, Chapman and Hall / CRC. and
# http://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/.)

  recontrast<-function(data,type = "sum"){
    data.type <-class(data)
    if(data.type == "factor"&!is.ordered(data)&nlevels(data)>1&nlevels(data)<1000){
      if(type == "sum"){
        contrasts(data)<-contr.sum(levels(data))
        colnames(contrasts(data))<-levels(data)[-nlevels(data)]
      }else if(type == "treatment"){
        contrasts(data)<-contr.treatment(levels(data))
      }else if(type == "deviation"){
        contrasts(data)<-contr.sum(levels(data))/2
        colnames(contrasts(data))<-levels(data)[-nlevels(data)]
      }
    }else if(data.type == "data.frame"){
      for(i in 1:ncol(data)){
        if(is.factor(data[,i]) & !is.ordered(data[,i])&nlevels(data[,i])>1&nlevels(data[,i])<1000){
          if(type == "sum"){
            contrasts(data[,i])<-contr.sum(levels(data[,i]))
            colnames(contrasts(data[,i]))<-levels(data[,i])[-nlevels(data[,i])]
          }else if(type == "treatment"){
            contrasts(data[,i])<-contr.treatment(levels(data[,i]))
          }else if(type == "deviation"){
            contrasts(data[,i])<-contr.sum(levels(data[,i]))/2
            colnames(contrasts(data[,i]))<-levels(data[,i])[-nlevels(data[,i])]
          }
        }
      }
    }
    return(data)
  }

# LEGEND
  g_legend<-function(plot){
    tmp <- ggplot_gtable(ggplot_build(plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}

  # use with:
  # legend <- g_legend(plot_x) ## to use, commment out "theme(legend.position="none")," in FUNCTION_comparative_norm_plot.R
  # ggsave(filename=paste("legend_x", ".pdf", sep=""),
  #        plot=legend, device=cairo_pdf, path=OUTPUT.PATH,
  #        height=3, width=3, units="cm", dpi=600)



# LOAD DATA, SUBSET, CODE, ARRANGE ----------------------------------------------

  ## COMPLETE PATH TO THE FILE(S) TO BE PROCESSED

    INPUT_RAWCLASS = "/path/to/TOT_normalitzat_classificat.txt"
    
  ## SET OUTPUT DIRECTORY AND FILE NAMES

    OUTPUT = "/path/to/output/"
    OUTPUT_intPlots = "/path/to/output/"
    OUTPUT_intPlotsV = "/path/to/output/"
    OUTPUT_lsmPlots = "/path/to/output/"
    OUTPUT_taulaanova = "/path/to/output/"

  ## read INPUT file, create subsets

    class.form.data = read.delim(INPUT_RAWCLASS, header=TRUE, blank.lines.skip = TRUE, sep = "\t")
      names(class.form.data)[names(class.form.data) == "SÍL.posició."] <- "SILposicio"
      names(class.form.data)[names(class.form.data) == "SÍL.tipus."] <- "SILtipus"
      names(class.form.data)[names(class.form.data) == "DURADA.ms."] <- "DURADAms"
      class.form.data$repeticio = as.character(class.form.data$repeticio)
      class.form.data[class.form.data$repeticio=="1",]$repeticio = "primera"
      class.form.data[class.form.data$repeticio=="2",]$repeticio = "segona"
      class.form.data[class.form.data$repeticio=="3",]$repeticio = "tercera"
      class.form.data[class.form.data$repeticio=="4",]$repeticio = "quarta"
      class.form.data[class.form.data$repeticio=="5",]$repeticio = "cinquena"
      class.form.data[class.form.data$repeticio=="6",]$repeticio = "sisena"


      ## set as factors
      cols <- c("N.MOT", "TOKEN", "N.CONTEXT", "repeticio", "VOCAL", "FUSIOOS")
      class.form.data[,cols] <- lapply(class.form.data[,cols], factor)

    ## subset

    df = droplevels(class.form.data[class.form.data$INTERVAL==0.5,])
      dfT1 = droplevels(df[df$TEST=="T1",])
      dfT3 = droplevels(df[df$TEST=="T3",])
      dfT1T3 = rbind(dfT1,dfT3)

      ## o-ɔ in T1 and T3
      dfos = droplevels(dfT1T3[dfT1T3$VOCAL=="o" | dfT1T3$VOCAL=="ɔ",])
        dfosf = droplevels(dfos[dfos$FUSIOOS=="F",]) # merged speakers
        dfosfp = droplevels(dfos[dfos$FUSIOOS=="FP",]) # partially merged speakers
        dfosd = droplevels(dfos[dfos$FUSIOOS=="D",]) # distinct speakers

        ## deviation code constrasts, using function recontrast() in line XXX so matrix has column names
        contrcols = c("VOCAL","SILposicio","FONANT", "ARXIPRESTAT", "EDAT","SEXE","repeticio", "INFORMANT")
        dfosf[,contrcols] = recontrast(dfosf[,contrcols], type="deviation")
        dfosfp[,contrcols] = recontrast(dfosfp[,contrcols], type="deviation")
        dfosd[,contrcols] = recontrast(dfosd[,contrcols], type="deviation")

      ## o-ɔ in T1
      dfosT1 = droplevels(dfT1[dfT1$VOCAL=="o" | dfT1$VOCAL=="ɔ",])
        dfosT1f = droplevels(dfosT1[dfosT1$FUSIOOS=="F",]) # merged speakers
        dfosT1fp = droplevels(dfosT1[dfosT1$FUSIOOS=="FP",]) # partially merged speakers
        dfosT1d = droplevels(dfosT1[dfosT1$FUSIOOS=="D",]) # distinct speakers

        ## deviation code constrasts, using function recontrast() in line XXX so matrix has column names
        contrcols = c("VOCAL","SILposicio","FONANT", "ARXIPRESTAT", "EDAT","SEXE","repeticio", "INFORMANT")
        dfosT1f[,contrcols] = recontrast(dfosT1f[,contrcols], type="deviation")
        dfosT1fp[,contrcols] = recontrast(dfosT1fp[,contrcols], type="deviation")
        dfosT1d[,contrcols] = recontrast(dfosT1d[,contrcols], type="deviation")

      ## o-ɔ in T3
      dfosT3 = droplevels(dfT3[dfT3$VOCAL=="o" | dfT3$VOCAL=="ɔ",])
        dfosT3f = droplevels(dfosT3[dfosT3$FUSIOOS=="F",]) # merged speakers
        dfosT3fp = droplevels(dfosT3[dfosT3$FUSIOOS=="FP",]) # partially merged speakers
        dfosT3d = droplevels(dfosT3[dfosT3$FUSIOOS=="D",]) # distinct speakers

        ## deviation code constrasts, using function recontrast() in line XXX so matrix has column names
        contrcols = c("VOCAL","SILposicio","FONANT", "ARXIPRESTAT", "EDAT","SEXE","repeticio", "INFORMANT")
        dfosT3f[,contrcols] = recontrast(dfosT3f[,contrcols], type="deviation")
        dfosT3fp[,contrcols] = recontrast(dfosT3fp[,contrcols], type="deviation")
        dfosT3d[,contrcols] = recontrast(dfosT3d[,contrcols], type="deviation")

        # ## NROWS + summary stats per LEVEL (http://stackoverflow.com/a/26114852)
        # as.data.frame(dfosT1f %>% group_by(VOCAL) %>%
        #                  summarise(number_of_rows = length(VOCAL), meanF1Lob = round(mean(Lobanov.F1),3), sdF1Lob = round(sd(Lobanov.F1),3),meanF2Lob = round(mean(Lobanov.F2),3),sdF2Lob = round(sd(Lobanov.F2),3)))

# PLOT SETTINGS ------------------------------------------------------------------

  vowel.colours <- c("a" = "grey28",  #gris fosc
                     "e" = "#D55E00", #taronja
                     "ɛ" = "#009E73", #verd
                     "i" = "#56B4E9", #blau clar
                     "o" = "#0072B2", #blau fosc
                     "ɔ" = "#CC79A7", #rosa
                     "u" = "#F0E442") #groc

  # From cbPalette = color-blind-friendly palette, http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/:
   cbPalette <- c("#000000", # black,
                  "#009E73", # green
                  "#D55E00", # reddish
                  "#F0E442") # yellow



  # theme options for interaction plots
  theme_opts = list(theme_bw(base_size=6, base_family="Helvetica"), #"Helvetica" / "Linux Libertine O"
                    theme(panel.border = element_blank(),
                          axis.line.x = element_line(size = 0.1, colour = "gray41"),
                          axis.line.y = element_line(size = 0.1, colour = "gray41"),
                          axis.ticks = element_line(colour = "gray41"),
                          axis.text = element_text(colour = "gray41")),
                    scale_y_reverse(limits=c(1.2,-1.5), breaks = pretty_breaks(n=6)),
                    scale_x_discrete(expand=c(0.1,0.1)))

# ANALYSIS ------------------------------------------------------------------

  ## Notation expansive (i.e., "VOCAL + SILposicio + SILposicio:VOCAL" instead of "VOCAL*SILposicio") to make parameters clearer.

  # FUSIÓ =================================

    # > T1 -------------------------------------------------------------------
      # >> F1 MODEL -------------------------------------------------------------------

        mf_T1_F1  = lmer(Lobanov.F1 ~ SILposicio + FONANT + EDAT + SEXE + VOCAL +
                                   SILposicio:VOCAL + FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                                   (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|N.MOT), data=dfosT1f, REML=TRUE)

          # Check normality and heteroscedasticity of residuals, this looks pretty okay. (According to Gelman, A. & Hill, J., 2007. Data Analysis Using Regression and
          # Multilevel/Hierarchical Models, Cambridge: Cambridge University Press. p. 46, normality is not that relevant, and I already checked for outliers due to errors.)
            # qqp(resid(mf_T1_F1)) # qqnorm+qqline function from car package
            # hist(resid(mf_T1_F1)) # # histogram
            # plot(scale(resid(mf_T1_F1)) ~ fitted(mf_T1_F1), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

          # ANOVA
            anova(mf_T1_F1)
            mf_T1_F1_anovatb = rownames_to_column(as.data.frame(anova(mf_T1_F1)))

          # SUMMARY
            summary(mf_T1_F1)
            mf_T1_F1_fixef = rownames_to_column(as.data.frame(coef(summary(mf_T1_F1))))
            mf_T1_F1_randef = as.data.frame(VarCorr(mf_T1_F1), comp=c("Variance","Std.Dev."))[c(1,2,4:6,8),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
            names(mf_T1_F1_randef) = c("Group","Name","Variance","StdDev","X","Y")
            mf_T1_F1_randef[c(2,5),1] = NA


      # >> F2 MODEL -------------------------------------------------------------------

        mf_T1_F2  = lmer(Lobanov.F2 ~ SILposicio + FONANT + EDAT + SEXE + VOCAL +
                           SILposicio:VOCAL + FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                           (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|N.MOT), data=dfosT1f, REML=TRUE)

        # Check normality and heteroscedasticity of residuals, this looks pretty okay. (According to Gelman, A. & Hill, J., 2007. Data Analysis Using Regression and
        # Multilevel/Hierarchical Models, Cambridge: Cambridge University Press. p. 46, normality is not that relevant, and I already checked for outliers due to errors.)
        # qqp(resid(mf_T1_F2)) # qqnorm+qqline function from car package
        # hist(resid(mf_T1_F2)) # # histogram
        # plot(scale(resid(mf_T1_F2)) ~ fitted(mf_T1_F2), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        # ANOVA
          anova(mf_T1_F2)
          mf_T1_F2_anovatb = rownames_to_column(as.data.frame(anova(mf_T1_F2)))

        # SUMMARY
          summary(mf_T1_F2)
          mf_T1_F2_fixef = rownames_to_column(as.data.frame(coef(summary(mf_T1_F2))))
          mf_T1_F2_randef = as.data.frame(VarCorr(mf_T1_F2), comp=c("Variance","Std.Dev."))[c(1,2,4:6,8),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
          names(mf_T1_F2_randef) = c("Group","Name","Variance","StdDev","X","Y")
          mf_T1_F2_randef[c(2,5),1] = NA

        # >>> SILposicio -------------------------------------------------------------------

          ## INTERACTION EFFECTS

            # DATA FRAME
            mf_T1_F1_VSILpos_eff = as.data.frame(effect("SILposicio:VOCAL", mf_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
            mf_T1_F2_VSILpos_eff = as.data.frame(effect("SILposicio:VOCAL", mf_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
            mf_T1_VSILpos_eff = merge(mf_T1_F1_VSILpos_eff, mf_T1_F2_VSILpos_eff, all = TRUE) %>%
                                mutate(SILposicio = factor(SILposicio, levels = c("SI", "SM", "SF"), labels = c("Inicial", "Medial", "Final")),
                                       Formant = factor(Formant)) %>%
                                arrange(Formant,SILposicio)

            # PLOT INTERACTION

              mf_T1_SILposV_intPlot =   ggplot(data=mf_T1_VSILpos_eff,
                                               aes(x=VOCAL, y=fit, group=Formant:SILposicio)) +
                #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=SILposicio), alpha=.3) +
								geom_point(size=1.5,aes(color=SILposicio),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=SILposicio, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=SILposicio), width=0.2,position=position_dodge(0.05)) +
                scale_color_manual(values=cbPalette, guide = FALSE) + # guide = guide_legend(title = "Posició de la\nsíl·laba tònica"), breaks=c("SI", "SM", "SF"), labels=c("Inicial", "Medial", "Final")) +
                scale_fill_manual(values=cbPalette, guide=FALSE) +
                scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
                ylab("Hz (Lobanov)") + xlab("Vocal") +
                theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

              ggsave(filename="f_T1_SILposV_intPlot.pdf",
                     plot=mf_T1_SILposV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
                     height=6, width=5, units="cm", dpi=600)

        # >>> FONANT -------------------------------------------------------------------

            ## INTERACTION EFFECTS

            # DATA FRAME
            mf_T1_F1_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", mf_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
            mf_T1_F2_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", mf_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
            mf_T1_VFONANT_eff = merge(mf_T1_F1_VFONANT_eff, mf_T1_F2_VFONANT_eff, all = TRUE) %>%
              mutate(FONANT = factor(FONANT, levels = c("Labial", "Dental", "Velar")),
                     Formant = factor(Formant)) %>%
              arrange(Formant,FONANT)

            # PLOT INTERACTION

            mf_T1_FONANTV_intPlot =   ggplot(data=mf_T1_VFONANT_eff,
                                             aes(x=VOCAL, y=fit, group=Formant:FONANT)) +
              geom_path(size=0.8, aes(color=FONANT, linetype=Formant)) +
              #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=FONANT), alpha=.3) +
								geom_point(size=1.5,aes(color=FONANT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=FONANT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=FONANT), width=0.2,position=position_dodge(0.05)) +
              scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Segment\nanterior")) +
              scale_fill_manual(values=cbPalette, guide=FALSE) +
              scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
              ylab("Hz (Lobanov)") + xlab("Vocal") +
              theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

            #mf_T1_FONANTV_intPlot

            # legend <- g_legend(mf_T1_FONANTV_intPlot) ## to use, commment out "theme(legend.position="none")," in FUNCTION_comparative_norm_plot.R
            # ggsave(filename=paste("legend_FONANTV_intPlot", ".pdf", sep=""),plot=legend, device=cairo_pdf, path=OUTPUT_intPlots,height=1.5, width=1.5, units="cm", dpi=600)

            ggsave(filename="f_T1_FONANTV_intPlot.pdf",
                   plot=mf_T1_FONANTV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
                   height=6, width=5, units="cm", dpi=600)

        # >>> EDAT -------------------------------------------------------------------

              ## INTERACTION EFFECTS

              # DATA FRAME

                mf_T1_F1_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", mf_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
                mf_T1_F2_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", mf_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
                mf_T1_VEDAT_eff = merge(mf_T1_F1_VEDAT_eff, mf_T1_F2_VEDAT_eff, all = TRUE) %>%
                                  mutate(EDAT = factor(EDAT, levels = c("FE1", "FE2")), Formant = factor(Formant)) %>%
                                  arrange(Formant,EDAT)

                # PLOT INTERACTION

                mf_T1_EDATV_intPlot =   ggplot(data=mf_T1_VEDAT_eff,
                                                 aes(x=VOCAL, y=fit, group=Formant:EDAT)) +
                  geom_path(size=0.8, aes(color=EDAT, linetype=Formant)) +
                  #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=EDAT), alpha=.3) +
								geom_point(size=1.5,aes(color=EDAT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=EDAT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=EDAT), width=0.2,position=position_dodge(0.05)) +
                  scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Edat")) +
                  scale_fill_manual(values=cbPalette, guide=FALSE) +
                  scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
                  ylab("Hz (Lobanov)") + xlab("Vocal") +
                  theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

                #mf_T1_EDATV_intPlot

                # legend <- g_legend(mf_T1_EDATV_intPlot) ## to use, commment out "theme(legend.position="none")," in FUNCTION_comparative_norm_plot.R
                # ggsave(filename=paste("legend_EDATV_intPlot", ".pdf", sep=""),plot=legend, device=cairo_pdf, path=OUTPUT_intPlots,height=1.5, width=1.5, units="cm", dpi=600)

                ggsave(filename="f_T1_EDATV_intPlot.pdf",
                       plot=mf_T1_EDATV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
                       height=6, width=5, units="cm", dpi=600)


        # >>> SEXE -------------------------------------------------------------------

          ## INTERACTION EFFECTS

            # DATA FRAME
            mf_T1_F1_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", mf_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
            mf_T1_F2_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", mf_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
            mf_T1_VSEXE_eff = merge(mf_T1_F1_VSEXE_eff, mf_T1_F2_VSEXE_eff, all = TRUE) %>%
              mutate(SEXE = factor(SEXE, levels = c("D", "H"), labels=c("Dones", "Homes")),
                     Formant = factor(Formant)) %>%
              arrange(Formant,SEXE)

            # PLOT INTERACTION
            #
            mf_T1_SEXEV_intPlot =   ggplot(data=mf_T1_VSEXE_eff,
                                           aes(x=VOCAL, y=fit, group=Formant:SEXE)) +
              geom_path(size=0.8, aes(color=SEXE, linetype=Formant)) +
              #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=SEXE), alpha=.3) +
								geom_point(size=1.5,aes(color=SEXE),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=SEXE, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=SEXE), width=0.2,position=position_dodge(0.05)) +
              scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Gènere")) +
              scale_fill_manual(values=cbPalette, guide=FALSE) +
              scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
              ylab("Hz (Lobanov)") + xlab("Vocal") +
              theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

            #mf_T1_SEXEV_intPlot

            # legend <- g_legend(mf_T1_SEXEV_intPlot) ## to use, commment out "theme(legend.position="none")," in FUNCTION_comparative_norm_plot.R
            # ggsave(filename=paste("legend_SEXEV_intPlot", ".pdf", sep=""),plot=legend, device=cairo_pdf, path=OUTPUT_intPlots,height=1.5, width=1.5, units="cm", dpi=600)

            ggsave(filename="f_T1_SEXEV_intPlot.pdf",
                   plot=mf_T1_SEXEV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
                   height=6, width=5, units="cm", dpi=600)



    # > T3 -------------------------------------------------------------------

      # >> F1 MODEL -------------------------------------------------------------------

        mf_T3_F1prev  = lmer(Lobanov.F1 ~ FONANT + EDAT + SEXE + VOCAL +
                           FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                           (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|repeticio), data=dfosT3f, REML=TRUE)

        # Check normality and heteroscedasticity of residuals. There's a few very extreme values that might cause problems.
        # qqp(resid(mf_T3_F1)) # qqnorm+qqline function from car package
        # hist(resid(mf_T3_F1)) # # histogram
        # plot(scale(resid(mf_T3_F1)) ~ fitted(mf_T3_F1), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        dfosT3f_trimmed1 = dfosT3f[abs(scale(resid(mf_T3_F1prev))) < 2.5, ]
        #(1-(nrow(dfosT3f_trimmed1))/nrow(dfosT3f))*100 # 2.06 % removed

        mf_T3_F1  = lmer(Lobanov.F1 ~ FONANT + EDAT + SEXE + VOCAL +
                           FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                           (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|repeticio), data=dfosT3f_trimmed1, REML=TRUE)

        # ANOVA
        anova(mf_T3_F1)
        mf_T3_F1_anovatb = rownames_to_column(as.data.frame(anova(mf_T3_F1)))

        # SUMMARY
        summary(mf_T3_F1)
        mf_T3_F1_fixef = rownames_to_column(as.data.frame(coef(summary(mf_T3_F1))))
        mf_T3_F1_randef = as.data.frame(VarCorr(mf_T3_F1), comp=c("Variance","Std.Dev."))[c(1,2,4,5,7,8),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
        names(mf_T3_F1_randef) = c("Group","Name","Variance","StdDev","X","Y")
        mf_T3_F1_randef[c(2,4),1] = NA

      # >> F2 MODEL -------------------------------------------------------------------

        mf_T3_F2prev  = lmer(Lobanov.F2 ~ FONANT + EDAT + SEXE + VOCAL +
                           FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                           (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|repeticio), data=dfosT3f, REML=TRUE)

        # Check normality and heteroscedasticity of residuals, there's some extreme results.
        # qqp(resid(mf_T3_F2prev)) # qqnorm+qqline function from car package
        # hist(resid(mf_T3_F2prev)) # # histogram
        # plot(scale(resid(mf_T3_F2prev)) ~ fitted(mf_T3_F2prev), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        ## trimm data
        dfosT3f_trimmed2 = dfosT3f[abs(scale(resid(mf_T3_F2prev))) < 2.5, ]
        #(1-(nrow(dfosT3f_trimmed2))/nrow(dfosT3f))*100 # 2.26 % removed

        ## refit
        require(optimx)
        mf_T3_F2  = lmer(Lobanov.F2 ~ FONANT + EDAT + SEXE + VOCAL +
                           FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                           (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|repeticio), data=dfosT3f_trimmed2, REML=TRUE,
                            control = lmerControl(optimizer= "optimx",  optCtrl = list(method="nlminb")))

        # Re-check normality and heteroscedasticity of residuals, this looks much better.
        # qqp(resid(mf_T3_F2)) # qqnorm+qqline function from car package
        # hist(resid(mf_T3_F2)) # # histogram
        # plot(scale(resid(mf_T3_F2)) ~ fitted(mf_T3_F2), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        # ANOVA
        anova(mf_T3_F2)
        mf_T3_F2_anovatb = rownames_to_column(as.data.frame(anova(mf_T3_F2)))

        # SUMMARY
        summary(mf_T3_F2)
        mf_T3_F2_fixef = rownames_to_column(as.data.frame(coef(summary(mf_T3_F2))))
        mf_T3_F2_randef = as.data.frame(VarCorr(mf_T3_F2), comp=c("Variance","Std.Dev."))[c(1,2,4,5,7,8),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
        names(mf_T3_F2_randef) = c("Group","Name","Variance","StdDev","X","Y")
        mf_T3_F2_randef[c(2,4),1] = NA


        # >>> FONANT -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        mf_T3_F1_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", mf_T3_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        mf_T3_F2_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", mf_T3_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        mf_T3_VFONANT_eff = merge(mf_T3_F1_VFONANT_eff, mf_T3_F2_VFONANT_eff, all = TRUE) %>%
          mutate(FONANT = factor(FONANT, levels = c("Alveolopalatal", "lr", "Labial", "Dentalveolar"), labels = c("Alveolopalatal", "/l,r/", "Labial", "Dentalveolar")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)

        # PLOT INTERACTION

        mf_T3_FONANTV_intPlot =   ggplot(data=mf_T3_VFONANT_eff,
                                         aes(x=VOCAL, y=fit, group=Formant:FONANT)) +
          geom_path(size=0.8, aes(color=FONANT, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=FONANT), alpha=.3) +
								geom_point(size=1.5,aes(color=FONANT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=FONANT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=FONANT), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide = FALSE) +# guide = guide_legend(title = "Segment anterior")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mf_T3_FONANTV_intPlot

        # legend <- g_legend(mf_T3_FONANTV_intPlot) ## to use, commment out "theme(legend.position="none")," in FUNCTION_comparative_norm_plot.R
        # ggsave(filename=paste("legend_FONANTV_intPlot", ".pdf", sep=""),plot=legend, device=cairo_pdf, path=OUTPUT_intPlots,height=2, width=2, units="cm", dpi=600)

        ggsave(filename="f_T3_FONANTV_intPlot.pdf",
               plot=mf_T3_FONANTV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

        ## LSMEANS

        # DATA FRAME
        mf_T3_F1_lsm_FONANT = lsmeans::lsmeans(mf_T3_F1,  ~ VOCAL|FONANT)
        mf_T3_F1_lsmeans_FONANT = data.frame(summary(update(mf_T3_F1_lsm_FONANT, by = NULL))) %>% add_column(Formant="F1",.before=1)
        mf_T3_F1_lsmCont_FONANT = data.frame(summary(update(pairs(mf_T3_F1_lsm_FONANT, by = "FONANT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F1",.before=1)

        mf_T3_F2_lsm_FONANT = lsmeans::lsmeans(mf_T3_F2,  ~ VOCAL|FONANT)
        mf_T3_F2_lsmeans_FONANT = data.frame(summary(update(mf_T3_F2_lsm_FONANT, by = NULL))) %>% add_column(Formant="F2",.before=1)
        mf_T3_F2_lsmCont_FONANT = data.frame(summary(update(pairs(mf_T3_F2_lsm_FONANT, by = "FONANT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F2",.before=1)

        mf_T3_F1F2_lsmeans_FONANT_df = merge(mf_T3_F1_lsmeans_FONANT, mf_T3_F2_lsmeans_FONANT, all = TRUE) %>%
          select(Formant, FONANT, VOCAL, lsmean, SE, df, lower.CL, upper.CL) %>%
          mutate(FONANT = factor(FONANT, levels = c("Alveolopalatal", "lr", "Labial", "Dentalveolar"), labels = c("Alveolopalatal", "/l,r/", "Labial", "Dentalveolar")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)

        mf_T3_VFONANT_lsmPlot =  ggplot(data=mf_T3_F1F2_lsmeans_FONANT_df, aes(x=FONANT, y=lsmean, group=Formant:VOCAL)) +
          geom_path(size=0.8, aes(color=VOCAL, linetype=Formant)) +
          geom_point(size=1.5, aes(color=VOCAL)) +
          geom_errorbar(width=0.2, aes(ymin=lower.CL, ymax=upper.CL, color=VOCAL)) +
          scale_colour_manual(values=vowel.colours, guide=FALSE) + #guide = guide_legend(title = "Vocal")) +
          scale_fill_manual(values=vowel.colours, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE) +
          ylab("Hz (Lobanov)") +
          xlab("Segment anterior") +
          theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        mf_T3_VFONANT_lsmPlot

        ggsave(filename="f_T3_VFONANT_lsmPlot.pdf",
               plot=mf_T3_VFONANT_lsmPlot, device=cairo_pdf, path=OUTPUT_lsmPlots,
               height=6, width=5, units="cm", dpi=600)

        mf_T3_F1F2_lsmCont_FONANT_df = merge(mf_T3_F1_lsmCont_FONANT,mf_T3_F2_lsmCont_FONANT, all=TRUE) %>%
          select(contrast, Formant, FONANT, estimate, SE, df, t.ratio, p.value) %>%
          mutate(Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)
        
        ### MODEL F1 of /e/-/ɛ/ produced in T3, to compare differences between the vowels
          
        dfesT3 = droplevels(dfT1T3[(dfT1T3$VOCAL=="e" | dfT1T3$VOCAL=="ɛ") & dfT1T3$TEST=="T3" & dfT1T3$FUSIOOS=="F",]) ## we want to check with same speakers
        dfesT3[,contrcols] = recontrast(dfesT3[,contrcols], type="deviation")
        m_T3_F1es  = lmer(Lobanov.F1 ~ FONANT + EDAT + SEXE + VOCAL +
                            FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                            (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|repeticio), data=dfesT3, REML=TRUE)
        
        # anova(m_T3_F1es)
        # Analysis of Variance Table of type III  with  Satterthwaite 
        # approximation for degrees of freedom (MODEL m_T3_F1es)
        # Sum Sq Mean Sq NumDF   DenDF F.value                Pr(>F)    
        # FONANT       85.441  28.480     3 2081.18  247.64 < 0.00000000000000022 ***
        #   EDAT          4.334   4.334     1   82.79   37.68      0.00000002751843 ***
        #   SEXE          0.000   0.000     1   82.79    0.00             0.9762671    
        # VOCAL        96.722  96.722     1    9.76  841.01      0.00000000008528 ***
        #   FONANT:VOCAL  2.183   0.728     3 2081.15    6.33             0.0002867 ***
        #   EDAT:VOCAL    0.112   0.112     1   82.55    0.98             0.3256841    
        # SEXE:VOCAL    1.054   1.054     1   82.55    9.16             0.0032910 ** 
        #   ---
        #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
        #   
        # LSMEANS CONTRASTS:
        #es_m_T3_F1_lsmCont_FONANT = data.frame(summary(update(pairs(lsmeans::lsmeans(m_T3_F1es, ~VOCAL|FONANT), by = "FONANT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F1",.before=1)
        
        es_m_T3_F1_lsm_FONANT = lsmeans::lsmeans(m_T3_F1es,  ~ VOCAL|FONANT)
        es_mf_T3_F1_lsmeans_FONANT = data.frame(summary(update(es_m_T3_F1_lsm_FONANT, by = NULL))) %>% add_column(Formant="F1",.before=1)
        es_mf_T3_F1_lsmCont_FONANT = data.frame(summary(update(pairs(es_m_T3_F1_lsm_FONANT, by = "FONANT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F1",.before=1)
        
        ## UNCOMMENT TO CREATE CORRESPONDING PLOT
        # es_mf_T3_F1_lsmeans_FONANT_df = es_mf_T3_F1_lsmeans_FONANT %>%
        #   select(Formant, FONANT, VOCAL, lsmean, SE, df, lower.CL, upper.CL) %>%
        #   mutate(FONANT = factor(FONANT, levels = c("Alveolopalatal", "lr", "Labial", "Dentalveolar"), labels = c("Alveolopalatal", "/l,r/", "Labial", "Dentalveolar")),
        #          Formant = factor(Formant)) %>%
        #   arrange(Formant,FONANT)
        # 
        # es_mf_T3_VFONANT_lsmPlot =  ggplot(data=es_mf_T3_F1_lsmeans_FONANT_df, aes(x=FONANT, y=lsmean, group=VOCAL)) +
        #   geom_path(size=0.8, aes(color=VOCAL)) +
        #   geom_point(size=1.5, aes(color=VOCAL)) +
        #   geom_errorbar(width=0.2, aes(ymin=lower.CL, ymax=upper.CL, color=VOCAL)) +
        #   scale_colour_manual(values=vowel.colours, guide_legend(title = "Vocal")) + #guide = guide_legend(title = "Vocal")) +
        #   scale_fill_manual(values=vowel.colours, guide=FALSE) +
        #   #scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE) +
        #   ylab("Hz (Lobanov)") +
        #   xlab("Segment anterior") +
        #   theme_opts + theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm")) +
        #   scale_y_reverse(limits=c(1.3,-1.5), breaks = pretty_breaks(n=6)) ## ɛ is lower than any lsmean for ɔ
        # 
        # es_mf_T3_VFONANT_lsmPlot
        
        
        # >>> EDAT -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME

        mf_T3_F1_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", mf_T3_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        mf_T3_F2_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", mf_T3_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        mf_T3_VEDAT_eff = merge(mf_T3_F1_VEDAT_eff, mf_T3_F2_VEDAT_eff, all = TRUE) %>%
          mutate(EDAT = factor(EDAT, levels = c("FE1", "FE2")), Formant = factor(Formant)) %>%
          arrange(Formant,EDAT)

        # PLOT INTERACTION

        mf_T3_EDATV_intPlot =   ggplot(data=mf_T3_VEDAT_eff,
                                       aes(x=VOCAL, y=fit, group=Formant:EDAT)) +
          geom_path(size=0.8, aes(color=EDAT, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=EDAT), alpha=.3) +
								geom_point(size=1.5,aes(color=EDAT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=EDAT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=EDAT), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Edat")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mf_T3_EDATV_intPlot

        # legend <- g_legend(mf_T3_EDATV_intPlot) ## to use, commment out "theme(legend.position="none")," in FUNCTION_comparative_norm_plot.R
        # ggsave(filename=paste("legend_EDATV_intPlot", ".pdf", sep=""),plot=legend, device=cairo_pdf, path=OUTPUT_intPlots,height=1.5, width=1.5, units="cm", dpi=600)

        ggsave(filename="f_T3_EDATV_intPlot.pdf",
               plot=mf_T3_EDATV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

        # ## LSMEANS
        # 
        # # DATA FRAME
        # mf_T3_F1_lsm_EDAT = lsmeans::lsmeans(mf_T3_F1,  ~ VOCAL|EDAT)
        # mf_T3_F1_lsmeans_EDAT = data.frame(summary(update(mf_T3_F1_lsm_EDAT, by = NULL))) %>% add_column(Formant="F1",.before=1)
        # mf_T3_F1_lsmCont_EDAT = data.frame(summary(update(pairs(mf_T3_F1_lsm_EDAT, by = "EDAT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F1",.before=1)
        # 
        # mf_T3_F2_lsm_EDAT = lsmeans::lsmeans(mf_T3_F2,  ~ VOCAL|EDAT)
        # mf_T3_F2_lsmeans_EDAT = data.frame(summary(update(mf_T3_F2_lsm_EDAT, by = NULL))) %>% add_column(Formant="F2",.before=1)
        # mf_T3_F2_lsmCont_EDAT = data.frame(summary(update(pairs(mf_T3_F2_lsm_EDAT, by = "EDAT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F2",.before=1)
        # 
        # mf_T3_F1F2_lsmeans_EDAT_df = merge(mf_T3_F1_lsmeans_EDAT, mf_T3_F2_lsmeans_EDAT, all = TRUE) %>%
        #   select(Formant, EDAT, VOCAL, lsmean, SE, df, lower.CL, upper.CL) %>%
        #   mutate(Formant = factor(Formant)) %>%
        #   arrange(Formant,EDAT)
        # 
        # mf_T3_VEDAT_lsmPlot =  ggplot(data=mf_T3_F1F2_lsmeans_EDAT_df, aes(x=EDAT, y=lsmean, group=Formant:VOCAL)) +
        #   geom_path(size=0.8, aes(color=VOCAL, linetype=Formant)) +
        #   geom_point(size=1.5, aes(color=VOCAL)) +
        #   geom_errorbar(width=0.2, aes(ymin=lower.CL, ymax=upper.CL, color=VOCAL)) +
        #   scale_colour_manual(values=vowel.colours, guide=FALSE) + #guide = guide_legend(title = "Vocal")) +
        #   scale_fill_manual(values=vowel.colours, guide=FALSE) +
        #   scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE) +
        #   ylab("Hz (Lobanov)") +
        #   xlab("Edat") +
        #   theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))
        # 
        # #mf_T3_VEDAT_lsmPlot
        # 
        # ggsave(filename="f_T3_VEDAT_lsmPlot.pdf",
        #        plot=mf_T3_VEDAT_lsmPlot, device=cairo_pdf, path=OUTPUT_lsmPlots,
        #        height=6, width=4, units="cm", dpi=600)
        # 
        # mf_T3_F1F2_lsmCont_EDAT_df = merge(mf_T3_F1_lsmCont_EDAT,mf_T3_F2_lsmCont_EDAT, all=TRUE) %>%
        #   select(contrast, Formant, EDAT, estimate, SE, df, t.ratio, p.value) %>%
        #   mutate(Formant = factor(Formant)) %>%
        #   arrange(Formant,EDAT)

        # >>> SEXE -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        mf_T3_F1_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", mf_T3_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        mf_T3_F2_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", mf_T3_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        mf_T3_VSEXE_eff = merge(mf_T3_F1_VSEXE_eff, mf_T3_F2_VSEXE_eff, all = TRUE) %>%
          mutate(SEXE = factor(SEXE, levels = c("D", "H"), labels=c("Dones", "Homes")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,SEXE)

        # PLOT INTERACTION
        #
        mf_T3_SEXEV_intPlot =   ggplot(data=mf_T3_VSEXE_eff,
                                       aes(x=VOCAL, y=fit, group=Formant:SEXE)) +
          geom_path(size=0.8, aes(color=SEXE, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=SEXE), alpha=.3) +
								geom_point(size=1.5,aes(color=SEXE),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=SEXE, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=SEXE), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Gènere")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mf_T3_SEXEV_intPlot

        # legend <- g_legend(mf_T3_SEXEV_intPlot) ## to use, commment out "theme(legend.position="none")," in FUNCTION_comparative_norm_plot.R
        # ggsave(filename=paste("legend_SEXEV_intPlot", ".pdf", sep=""),plot=legend, device=cairo_pdf, path=OUTPUT_intPlots,height=1.5, width=1.5, units="cm", dpi=600)

        ggsave(filename="f_T3_SEXEV_intPlot.pdf",
               plot=mf_T3_SEXEV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)


  # FUSIÓ PARCIAL =================================

    # > T1 -------------------------------------------------------------------
      # >> F1 MODEL -------------------------------------------------------------------

        mfp_T1_F1  = lmer(Lobanov.F1 ~ SILposicio + FONANT + EDAT + SEXE + VOCAL +
                            SILposicio:VOCAL + FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                            (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|N.MOT), data=dfosT1fp, REML=TRUE)

        # Check normality and heteroscedasticity of residuals, this looks pretty okay. (According to Gelman, A. & Hill, J., 2007. Data Analysis Using Regression and
        # Multilevel/Hierarchical Models, Cambridge: Cambridge University Press. p. 46, normality is not that relevant, and I already checked for outliers due to errors.)
        # qqp(resid(mfp_T1_F1)) # qqnorm+qqline function from car package
        # hist(resid(mfp_T1_F1)) # # histogram
        # plot(scale(resid(mfp_T1_F1)) ~ fitted(mfp_T1_F1), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        # ANOVA
        anova(mfp_T1_F1)
        mfp_T1_F1_anovatb = rownames_to_column(as.data.frame(anova(mfp_T1_F1)))

        # SUMMARY
        summary(mfp_T1_F1)
        mfp_T1_F1_fixef = rownames_to_column(as.data.frame(coef(summary(mfp_T1_F1))))
        mfp_T1_F1_randef = as.data.frame(VarCorr(mfp_T1_F1), comp=c("Variance","Std.Dev."))[c(1,2,4:6,8),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
        names(mfp_T1_F1_randef) = c("Group","Name","Variance","StdDev","X","Y")
        mfp_T1_F1_randef[c(2,5),1] = NA


      # >> F2 MODEL -------------------------------------------------------------------

        mfp_T1_F2  = lmer(Lobanov.F2 ~ SILposicio + FONANT + EDAT + SEXE + VOCAL +
                            SILposicio:VOCAL + FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                            (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|N.MOT), data=dfosT1fp, REML=TRUE)

        # Check normality and heteroscedasticity of residuals, this looks pretty okay. (According to Gelman, A. & Hill, J., 2007. Data Analysis Using Regression and
        # Multilevel/Hierarchical Models, Cambridge: Cambridge University Press. p. 46, normality is not that relevant, and I already checked for outliers due to errors.)
        # qqp(resid(mfp_T1_F2)) # qqnorm+qqline function from car package
        # hist(resid(mfp_T1_F2)) # # histogram
        # plot(scale(resid(mfp_T1_F2)) ~ fitted(mfp_T1_F2), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        # ANOVA
        anova(mfp_T1_F2)
        mfp_T1_F2_anovatb = rownames_to_column(as.data.frame(anova(mfp_T1_F2)))

        # SUMMARY
        summary(mfp_T1_F2)
        mfp_T1_F2_fixef = rownames_to_column(as.data.frame(coef(summary(mfp_T1_F2))))
        mfp_T1_F2_randef = as.data.frame(VarCorr(mfp_T1_F2), comp=c("Variance","Std.Dev."))[c(1,2,4:6,8),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
        names(mfp_T1_F2_randef) = c("Group","Name","Variance","StdDev","X","Y")
        mfp_T1_F2_randef[c(2,5),1] = NA

        # >>> SILposicio -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        mfp_T1_F1_VSILpos_eff = as.data.frame(effect("SILposicio:VOCAL", mfp_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        mfp_T1_F2_VSILpos_eff = as.data.frame(effect("SILposicio:VOCAL", mfp_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        mfp_T1_VSILpos_eff = merge(mfp_T1_F1_VSILpos_eff, mfp_T1_F2_VSILpos_eff, all = TRUE) %>%
          mutate(SILposicio = factor(SILposicio, levels = c("SI", "SM", "SF")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,SILposicio)

        # PLOT INTERACTION

        mfp_T1_SILposV_intPlot =   ggplot(data=mfp_T1_VSILpos_eff,
                                          aes(x=VOCAL, y=fit, group=Formant:SILposicio)) +
          geom_path(size=0.8, aes(color=SILposicio, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=SILposicio), alpha=.3) +
								geom_point(size=1.5,aes(color=SILposicio),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=SILposicio, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=SILposicio), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide = FALSE) + # guide = guide_legend(title = "Posició de la\nsíl·laba tònica"), breaks=c("SI", "SM", "SF"), labels=c("Inicial", "Medial", "Final")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mfp_T1_SILposV_intPlot

        ggsave(filename="fp_T1_SILposV_intPlot.pdf",
               plot=mfp_T1_SILposV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

        # >>> FONANT -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        mfp_T1_F1_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", mfp_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        mfp_T1_F2_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", mfp_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        mfp_T1_VFONANT_eff = merge(mfp_T1_F1_VFONANT_eff, mfp_T1_F2_VFONANT_eff, all = TRUE) %>%
          mutate(FONANT = factor(FONANT, levels = c("Labial", "Dental", "Velar")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)

        # PLOT INTERACTION

        mfp_T1_FONANTV_intPlot =   ggplot(data=mfp_T1_VFONANT_eff,
                                          aes(x=VOCAL, y=fit, group=Formant:FONANT)) +
          geom_path(size=0.8, aes(color=FONANT, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=FONANT), alpha=.3) +
								geom_point(size=1.5,aes(color=FONANT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=FONANT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=FONANT), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Segment\nanterior")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mfp_T1_FONANTV_intPlot

        ggsave(filename="fp_T1_FONANTV_intPlot.pdf",
               plot=mfp_T1_FONANTV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

        # >>> EDAT -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME

        mfp_T1_F1_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", mfp_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        mfp_T1_F2_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", mfp_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        mfp_T1_VEDAT_eff = merge(mfp_T1_F1_VEDAT_eff, mfp_T1_F2_VEDAT_eff, all = TRUE) %>%
          mutate(EDAT = factor(EDAT, levels = c("FE1", "FE2")), Formant = factor(Formant)) %>%
          arrange(Formant,EDAT)

        # PLOT INTERACTION

        mfp_T1_EDATV_intPlot =   ggplot(data=mfp_T1_VEDAT_eff,
                                        aes(x=VOCAL, y=fit, group=Formant:EDAT)) +
          geom_path(size=0.8, aes(color=EDAT, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=EDAT), alpha=.3) +
								geom_point(size=1.5,aes(color=EDAT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=EDAT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=EDAT), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Edat")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mfp_T1_EDATV_intPlot

        ggsave(filename="fp_T1_EDATV_intPlot.pdf",
               plot=mfp_T1_EDATV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

        # >>> SEXE -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        mfp_T1_F1_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", mfp_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        mfp_T1_F2_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", mfp_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        mfp_T1_VSEXE_eff = merge(mfp_T1_F1_VSEXE_eff, mfp_T1_F2_VSEXE_eff, all = TRUE) %>%
          mutate(SEXE = factor(SEXE, levels = c("D", "H"), labels=c("Dones", "Homes")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,SEXE)

        # PLOT INTERACTION

        mfp_T1_SEXEV_intPlot =   ggplot(data=mfp_T1_VSEXE_eff,
                                        aes(x=VOCAL, y=fit, group=Formant:SEXE)) +
          geom_path(size=0.8, aes(color=SEXE, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=SEXE), alpha=.3) +
								geom_point(size=1.5,aes(color=SEXE),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=SEXE, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=SEXE), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Gènere")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mfp_T1_SEXEV_intPlot

        ggsave(filename="fp_T1_SEXEV_intPlot.pdf",
               plot=mfp_T1_SEXEV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

    # > T3 -------------------------------------------------------------------

      # >> F1 MODEL -------------------------------------------------------------------

        mfp_T3_F1prev  = lmer(Lobanov.F1 ~ FONANT + EDAT + SEXE + VOCAL +
                            FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                            (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|repeticio), data=dfosT3fp, REML=TRUE)

        # Check normality and heteroscedasticity of residuals, there's a few considerably extreme values.
        # qqp(resid(mfp_T3_F1prev)) # qqnorm+qqline function from car package
        # hist(resid(mfp_T3_F1prev)) # # histogram
        # plot(scale(resid(mfp_T3_F1prev)) ~ fitted(mfp_T3_F1prev), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        dfosT3fp_trimmed1 = dfosT3fp[abs(scale(resid(mfp_T3_F1prev))) < 2.5, ]
        #(1-(nrow(dfosT3fp_trimmed1))/nrow(dfosT3fp))*100 # 2.24 % removed

        mfp_T3_F1  = lmer(Lobanov.F1 ~ FONANT + EDAT + SEXE + VOCAL +
                            FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                            (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|repeticio), data=dfosT3fp_trimmed1, REML=TRUE)

        # Re-check. Much better
        # qqp(resid(mfp_T3_F1)) # qqnorm+qqline function from car package
        # hist(resid(mfp_T3_F1)) # # histogram
        # plot(scale(resid(mfp_T3_F1)) ~ fitted(mfp_T3_F1), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        # ANOVA
        anova(mfp_T3_F1)
        mfp_T3_F1_anovatb = rownames_to_column(as.data.frame(anova(mfp_T3_F1)))

        # SUMMARY
        summary(mfp_T3_F1)
        mfp_T3_F1_fixef = rownames_to_column(as.data.frame(coef(summary(mfp_T3_F1))))
        mfp_T3_F1_randef = as.data.frame(VarCorr(mfp_T3_F1), comp=c("Variance","Std.Dev."))[c(1,2,4,5,7,8),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
        names(mfp_T3_F1_randef) = c("Group","Name","Variance","StdDev","X","Y")
        mfp_T3_F1_randef[c(2,4),1] = NA

      # >> F2 MODEL -------------------------------------------------------------------

        mfp_T3_F2prev  = lmer(Lobanov.F2 ~ FONANT + EDAT + SEXE + VOCAL +
                            FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                            (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|repeticio), data=dfosT3fp, REML=TRUE)

        # Check normality and heteroscedasticity of residuals, there's some extreme results.
        # qqp(resid(mfp_T3_F2)) # qqnorm+qqline function from car package
        # hist(resid(mfp_T3_F2)) # # histogram
        # plot(scale(resid(mfp_T3_F2)) ~ fitted(mfp_T3_F2), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        ## trimm data
        dfosT3fp_trimmed2 = dfosT3fp[abs(scale(resid(mfp_T3_F2prev))) < 2.5, ]
        #(1-(nrow(dfosT3fp_trimmed2))/nrow(dfosT3fp))*100 # 2.24 % removed

        ## refit
        mfp_T3_F2  = lmer(Lobanov.F2 ~ FONANT + EDAT + SEXE + VOCAL +
                            FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                            (1+VOCAL|ARXIPRESTAT/INFORMANT) + (1|repeticio), data=dfosT3fp_trimmed2, REML=TRUE)

        # Re-check normality and heteroscedasticity of residuals, this looks much better.
        # qqp(resid(mfp_T3_F2)) # qqnorm+qqline function from car package
        # hist(resid(mfp_T3_F2)) # # histogram
        # plot(scale(resid(mfp_T3_F2)) ~ fitted(mfp_T3_F2), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        # ANOVA
        anova(mfp_T3_F2)
        mfp_T3_F2_anovatb = rownames_to_column(as.data.frame(anova(mfp_T3_F2)))

        # SUMMARY
        summary(mfp_T3_F2)
        mfp_T3_F2_fixef = rownames_to_column(as.data.frame(coef(summary(mfp_T3_F2))))
        mfp_T3_F2_randef = as.data.frame(VarCorr(mfp_T3_F2), comp=c("Variance","Std.Dev."))[c(1,2,4,5,7,8),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
        names(mfp_T3_F2_randef) = c("Group","Name","Variance","StdDev","X","Y")
        mfp_T3_F2_randef[c(2,4),1] = NA


        # >>> FONANT -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        mfp_T3_F1_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", mfp_T3_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        mfp_T3_F2_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", mfp_T3_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        mfp_T3_VFONANT_eff = merge(mfp_T3_F1_VFONANT_eff, mfp_T3_F2_VFONANT_eff, all = TRUE) %>%
          mutate(FONANT = factor(FONANT, levels = c("Alveolopalatal", "lr", "Labial", "Dentalveolar"), labels = c("Alveolopalatal", "/l,r/", "Labial", "Dentalveolar")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)

        # PLOT INTERACTION

        mfp_T3_FONANTV_intPlot =   ggplot(data=mfp_T3_VFONANT_eff,
                                          aes(x=VOCAL, y=fit, group=Formant:FONANT)) +
          geom_path(size=0.8, aes(color=FONANT, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=FONANT), alpha=.3) +
								geom_point(size=1.5,aes(color=FONANT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=FONANT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=FONANT), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide = FALSE) +# guide = guide_legend(title = "Segment anterior")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mfp_T3_FONANTV_intPlot

        ggsave(filename="fp_T3_FONANTV_intPlot.pdf",
               plot=mfp_T3_FONANTV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

        ## LSMEANS

        # DATA FRAME
        mfp_T3_F1_lsm_FONANT = lsmeans::lsmeans(mfp_T3_F1,  ~ VOCAL|FONANT)
        mfp_T3_F1_lsmeans_FONANT = data.frame(summary(update(mfp_T3_F1_lsm_FONANT, by = NULL))) %>% add_column(Formant="F1",.before=1)
        mfp_T3_F1_lsmCont_FONANT = data.frame(summary(update(pairs(mfp_T3_F1_lsm_FONANT, by = "FONANT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F1",.before=1)

        mfp_T3_F2_lsm_FONANT = lsmeans::lsmeans(mfp_T3_F2,  ~ VOCAL|FONANT)
        mfp_T3_F2_lsmeans_FONANT = data.frame(summary(update(mfp_T3_F2_lsm_FONANT, by = NULL))) %>% add_column(Formant="F2",.before=1)
        mfp_T3_F2_lsmCont_FONANT = data.frame(summary(update(pairs(mfp_T3_F2_lsm_FONANT, by = "FONANT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F2",.before=1)

        mfp_T3_F1F2_lsmeans_FONANT_df = merge(mfp_T3_F1_lsmeans_FONANT, mfp_T3_F2_lsmeans_FONANT, all = TRUE) %>%
          select(Formant, FONANT, VOCAL, lsmean, SE, df, lower.CL, upper.CL) %>%
          mutate(FONANT = factor(FONANT, levels = c("Alveolopalatal", "lr", "Labial", "Dentalveolar"), labels = c("Alveolopalatal", "/l,r/", "Labial", "Dentalveolar")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)

        mfp_T3_VFONANT_lsmPlot =  ggplot(data=mfp_T3_F1F2_lsmeans_FONANT_df, aes(x=FONANT, y=lsmean, group=Formant:VOCAL)) +
          geom_path(size=0.8, aes(color=VOCAL, linetype=Formant)) +
          geom_point(size=1.5, aes(color=VOCAL)) +
          geom_errorbar(width=0.2, aes(ymin=lower.CL, ymax=upper.CL, color=VOCAL)) +
          scale_colour_manual(values=vowel.colours, guide=FALSE) + #guide = guide_legend(title = "Vocal")) +
          scale_fill_manual(values=vowel.colours, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE) +
          ylab("Hz (Lobanov)") +
          xlab("Segment anterior") +
          theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mfp_T3_VFONANT_lsmPlot

        ggsave(filename="fp_T3_VFONANT_lsmPlot.pdf",
               plot=mfp_T3_VFONANT_lsmPlot, device=cairo_pdf, path=OUTPUT_lsmPlots,
               height=6, width=5, units="cm", dpi=600)

        mfp_T3_F1F2_lsmCont_FONANT_df = merge(mfp_T3_F1_lsmCont_FONANT,mfp_T3_F2_lsmCont_FONANT, all=TRUE) %>%
          select(contrast, Formant, FONANT, estimate, SE, df, t.ratio, p.value) %>%
          mutate(Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)

        # >>> EDAT -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME

        mfp_T3_F1_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", mfp_T3_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        mfp_T3_F2_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", mfp_T3_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        mfp_T3_VEDAT_eff = merge(mfp_T3_F1_VEDAT_eff, mfp_T3_F2_VEDAT_eff, all = TRUE) %>%
          mutate(EDAT = factor(EDAT, levels = c("FE1", "FE2")), Formant = factor(Formant)) %>%
          arrange(Formant,EDAT)

        # PLOT INTERACTION

        mfp_T3_EDATV_intPlot =   ggplot(data=mfp_T3_VEDAT_eff,
                                        aes(x=VOCAL, y=fit, group=Formant:EDAT)) +
          geom_path(size=0.8, aes(color=EDAT, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=EDAT), alpha=.3) +
								geom_point(size=1.5,aes(color=EDAT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=EDAT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=EDAT), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Edat")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mfp_T3_EDATV_intPlot

        ggsave(filename="fp_T3_EDATV_intPlot.pdf",
               plot=mfp_T3_EDATV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

        # >>> SEXE -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        mfp_T3_F1_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", mfp_T3_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        mfp_T3_F2_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", mfp_T3_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        mfp_T3_VSEXE_eff = merge(mfp_T3_F1_VSEXE_eff, mfp_T3_F2_VSEXE_eff, all = TRUE) %>%
          mutate(SEXE = factor(SEXE, levels = c("D", "H"), labels=c("Dones", "Homes")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,SEXE)

        # PLOT INTERACTION

        mfp_T3_SEXEV_intPlot =   ggplot(data=mfp_T3_VSEXE_eff,
                                        aes(x=VOCAL, y=fit, group=Formant:SEXE)) +
          geom_path(size=0.8, aes(color=SEXE, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=SEXE), alpha=.3) +
								geom_point(size=1.5,aes(color=SEXE),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=SEXE, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=SEXE), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Gènere")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #mfp_T3_SEXEV_intPlot

        ggsave(filename="fp_T3_SEXEV_intPlot.pdf",
               plot=mfp_T3_SEXEV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

  # DISTINCIÓ =================================

    # > T1 -------------------------------------------------------------------
      # >> F1 MODEL -------------------------------------------------------------------

        md_T1_F1  = lmer(Lobanov.F1 ~ SILposicio + FONANT + EDAT + SEXE + VOCAL +
                           SILposicio:VOCAL + FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                           (1+VOCAL|INFORMANT) + (1|N.MOT), data=dfosT1d, REML=TRUE)

        # Check normality and heteroscedasticity of residuals, this looks pretty okay. (According to Gelman, A. & Hill, J., 2007. Data Analysis Using Regression and
        # Multilevel/Hierarchical Models, Cambridge: Cambridge University Press. p. 46, normality is not that relevant, and I already checked for outliers due to errors.)
        # qqp(resid(md_T1_F1)) # qqnorm+qqline function from car package
        # hist(resid(md_T1_F1)) # # histogram
        # plot(scale(resid(md_T1_F1)) ~ fitted(md_T1_F1), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        # ANOVA
        anova(md_T1_F1)
        md_T1_F1_anovatb = rownames_to_column(as.data.frame(anova(md_T1_F1)))

        # SUMMARY
        summary(md_T1_F1)
        md_T1_F1_fixef = rownames_to_column(as.data.frame(coef(summary(md_T1_F1))))
        md_T1_F1_randef = as.data.frame(VarCorr(md_T1_F1), comp=c("Variance","Std.Dev."))[c(1,2,3,5),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
        names(md_T1_F1_randef) = c("Group","Name","Variance","StdDev","X","Y")
        md_T1_F1_randef[3,1] = NA


      # >> F2 MODEL -------------------------------------------------------------------

        md_T1_F2prev  = lmer(Lobanov.F2 ~ SILposicio + FONANT + EDAT + SEXE + VOCAL +
                           SILposicio:VOCAL + FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                           (1+VOCAL|INFORMANT) + (1|N.MOT), data=dfosT1d, REML=TRUE)

        # Check normality and heteroscedasticity of residuals, there is one extreme outlier.
        # qqp(resid(md_T1_F2)) # qqnorm+qqline function from car package
        # hist(resid(md_T1_F2)) # # histogram
        # plot(scale(resid(md_T1_F2)) ~ fitted(md_T1_F2), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        dfosT1d_trimmed = dfosT1d[abs(scale(resid(md_T1_F2prev))) < 2.5, ]
        #(1-(nrow(dfosT1d_trimmed))/nrow(dfosT1d))*100 # 0.8 % removed

        md_T1_F2  = lmer(Lobanov.F2 ~ SILposicio + FONANT + EDAT + SEXE + VOCAL +
                           SILposicio:VOCAL + FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                           (1+VOCAL|INFORMANT) + (1|N.MOT), data=dfosT1d_trimmed, REML=TRUE)

        # Re-check. Much better
        # qqp(resid(md_T1_F2)) # qqnorm+qqline function from car package
        # hist(resid(md_T1_F2)) # # histogram
        # plot(scale(resid(md_T1_F2)) ~ fitted(md_T1_F2), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        # ANOVA
        anova(md_T1_F2)
        md_T1_F2_anovatb = rownames_to_column(as.data.frame(anova(md_T1_F2)))

        # SUMMARY
        summary(md_T1_F2)
        md_T1_F2_fixef = rownames_to_column(as.data.frame(coef(summary(md_T1_F2))))
        md_T1_F2_randef = as.data.frame(VarCorr(md_T1_F2), comp=c("Variance","Std.Dev."))[c(1,2,3,5),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
        names(md_T1_F2_randef) = c("Group","Name","Variance","StdDev","X","Y")
        md_T1_F2_randef[3,1] = NA

        # >>> SILposicio -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        md_T1_F1_VSILpos_eff = as.data.frame(effect("SILposicio:VOCAL", md_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        md_T1_F2_VSILpos_eff = as.data.frame(effect("SILposicio:VOCAL", md_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        md_T1_VSILpos_eff = merge(md_T1_F1_VSILpos_eff, md_T1_F2_VSILpos_eff, all = TRUE) %>%
          mutate(SILposicio = factor(SILposicio, levels = c("SI", "SM", "SF")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,SILposicio)

        # PLOT INTERACTION

        md_T1_SILposV_intPlot =   ggplot(data=md_T1_VSILpos_eff,
                                         aes(x=VOCAL, y=fit, group=Formant:SILposicio)) +
          geom_path(size=0.8, aes(color=SILposicio, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=SILposicio), alpha=.3) +
								geom_point(size=1.5,aes(color=SILposicio),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=SILposicio, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=SILposicio), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide = FALSE) + # guide = guide_legend(title = "Posició de la\nsíl·laba tònica"), breaks=c("SI", "SM", "SF"), labels=c("Inicial", "Medial", "Final")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #md_T1_SILposV_intPlot

        ggsave(filename="d_T1_SILposV_intPlot.pdf",
               plot=md_T1_SILposV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)


        # >>> FONANT -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        md_T1_F1_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", md_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        md_T1_F2_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", md_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        md_T1_VFONANT_eff = merge(md_T1_F1_VFONANT_eff, md_T1_F2_VFONANT_eff, all = TRUE) %>%
          mutate(FONANT = factor(FONANT, levels = c("Labial", "Dental", "Velar")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)

        # PLOT INTERACTION

        md_T1_FONANTV_intPlot =   ggplot(data=md_T1_VFONANT_eff,
                                         aes(x=VOCAL, y=fit, group=Formant:FONANT)) +
          geom_path(size=0.8, aes(color=FONANT, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=FONANT), alpha=.3) +
								geom_point(size=1.5,aes(color=FONANT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=FONANT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=FONANT), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Segment\nanterior")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #md_T1_FONANTV_intPlot

        # legend <- g_legend(md_T1_FONANTV_intPlot) ## to use, commment out "theme(legend.position="none")," in FUNCTION_comparative_norm_plot.R
        # ggsave(filename=paste("legend_FONANTV_intPlot", ".pdf", sep=""),plot=legend, device=cairo_pdf, path=OUTPUT_intPlots,height=1.5, width=1.5, units="cm", dpi=600)

        ggsave(filename="d_T1_FONANTV_intPlot.pdf",
               plot=md_T1_FONANTV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)


        # >>> EDAT -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME

        md_T1_F1_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", md_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        md_T1_F2_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", md_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        md_T1_VEDAT_eff = merge(md_T1_F1_VEDAT_eff, md_T1_F2_VEDAT_eff, all = TRUE) %>%
          mutate(EDAT = factor(EDAT, levels = c("FE1", "FE2")), Formant = factor(Formant)) %>%
          arrange(Formant,EDAT)

        # PLOT INTERACTION

        md_T1_EDATV_intPlot =   ggplot(data=md_T1_VEDAT_eff,
                                       aes(x=VOCAL, y=fit, group=Formant:EDAT)) +
          geom_path(size=0.8, aes(color=EDAT, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=EDAT), alpha=.3) +
								geom_point(size=1.5,aes(color=EDAT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=EDAT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=EDAT), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Edat")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #md_T1_EDATV_intPlot

        ggsave(filename="d_T1_EDATV_intPlot.pdf",
               plot=md_T1_EDATV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

        # >>> SEXE -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        md_T1_F1_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", md_T1_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        md_T1_F2_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", md_T1_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        md_T1_VSEXE_eff = merge(md_T1_F1_VSEXE_eff, md_T1_F2_VSEXE_eff, all = TRUE) %>%
          mutate(SEXE = factor(SEXE, levels = c("D", "H"), labels=c("Dones", "Homes")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,SEXE)

        # PLOT INTERACTION
        #
        md_T1_SEXEV_intPlot =   ggplot(data=md_T1_VSEXE_eff,
                                       aes(x=VOCAL, y=fit, group=Formant:SEXE)) +
          geom_path(size=0.8, aes(color=SEXE, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=SEXE), alpha=.3) +
								geom_point(size=1.5,aes(color=SEXE),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=SEXE, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=SEXE), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Gènere")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #md_T1_SEXEV_intPlot

        # legend <- g_legend(md_T1_SEXEV_intPlot) ## to use, commment out "theme(legend.position="none")," in FUNCTION_comparative_norm_plot.R
        # ggsave(filename=paste("legend_SEXEV_intPlot", ".pdf", sep=""),plot=legend, device=cairo_pdf, path=OUTPUT_intPlots,height=1.5, width=1.5, units="cm", dpi=600)

        ggsave(filename="d_T1_SEXEV_intPlot.pdf",
               plot=md_T1_SEXEV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)




    # > T3 -------------------------------------------------------------------

      # >> F1 MODEL -------------------------------------------------------------------

        md_T3_F1  = lmer(Lobanov.F1 ~ FONANT + EDAT + SEXE + VOCAL +
                           FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                           (1+VOCAL|INFORMANT) + (1|repeticio), data=dfosT3d, REML=TRUE)

        # Check normality and heteroscedasticity of residuals, there's a few considerably extreme values.
        # qqp(resid(md_T3_F1)) # qqnorm+qqline function from car package
        # hist(resid(md_T3_F1)) # # histogram
        # plot(scale(resid(md_T3_F1)) ~ fitted(md_T3_F1), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        # ANOVA
        anova(md_T3_F1)
        md_T3_F1_anovatb = rownames_to_column(as.data.frame(anova(md_T3_F1)))

        # SUMMARY
        summary(md_T3_F1)
        md_T3_F1_fixef = rownames_to_column(as.data.frame(coef(summary(md_T3_F1))))
        md_T3_F1_randef = as.data.frame(VarCorr(md_T3_F1), comp=c("Variance","Std.Dev."))[c(1,2,4,5),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
        names(md_T3_F1_randef) = c("Group","Name","Variance","StdDev","X","Y")
        md_T3_F1_randef[2,1] = NA

      # >> F2 MODEL -------------------------------------------------------------------

        md_T3_F2  = lmer(Lobanov.F2 ~ FONANT + EDAT + SEXE + VOCAL +
                           FONANT:VOCAL + EDAT:VOCAL + SEXE:VOCAL +
                           (1+VOCAL|INFORMANT) + (1|repeticio), data=dfosT3d, REML=TRUE)

        # Check normality and heteroscedasticity of residuals, looks alright.
        # qqp(resid(md_T3_F2)) # qqnorm+qqline function from car package
        # hist(resid(md_T3_F2)) # # histogram
        # plot(scale(resid(md_T3_F2)) ~ fitted(md_T3_F2), pch = ".", cex = 2, ylim = c(-5, 5)) + abline(h = c(-2.5, 2.5))# plot to check for heteroscedasticity

        # ANOVA
        anova(md_T3_F2)
        md_T3_F2_anovatb = rownames_to_column(as.data.frame(anova(md_T3_F2)))

        # SUMMARY
        summary(md_T3_F2)
        md_T3_F2_fixef = rownames_to_column(as.data.frame(coef(summary(md_T3_F2))))
        md_T3_F2_randef = as.data.frame(VarCorr(md_T3_F2), comp=c("Variance","Std.Dev."))[c(1,2,4,5),c(1,2,4,5)] %>% add_column(X=NA,Y=NA) # added columns to make it same length as fixef table
        names(md_T3_F2_randef) = c("Group","Name","Variance","StdDev","X","Y")
        md_T3_F2_randef[2,1] = NA


        # >>> FONANT -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        md_T3_F1_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", md_T3_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        md_T3_F2_VFONANT_eff = as.data.frame(effect("FONANT:VOCAL", md_T3_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        md_T3_VFONANT_eff = merge(md_T3_F1_VFONANT_eff, md_T3_F2_VFONANT_eff, all = TRUE) %>%
          mutate(FONANT = factor(FONANT, levels = c("Alveolopalatal", "lr", "Labial", "Dentalveolar"), labels = c("Alveolopalatal", "/l,r/", "Labial", "Dentalveolar")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)

        # PLOT INTERACTION

        md_T3_FONANTV_intPlot =   ggplot(data=md_T3_VFONANT_eff,
                                         aes(x=VOCAL, y=fit, group=Formant:FONANT)) +
          geom_path(size=0.8, aes(color=FONANT, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=FONANT), alpha=.3) +
								geom_point(size=1.5,aes(color=FONANT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=FONANT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=FONANT), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide = FALSE) +# guide = guide_legend(title = "Segment anterior")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts #+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #md_T3_FONANTV_intPlot

        ggsave(filename="d_T3_FONANTV_intPlot.pdf",
               plot=md_T3_FONANTV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

        ## LSMEANS

        # DATA FRAME
        md_T3_F1_lsm_FONANT = lsmeans::lsmeans(md_T3_F1,  ~ VOCAL|FONANT)
        md_T3_F1_lsmeans_FONANT = data.frame(summary(update(md_T3_F1_lsm_FONANT, by = NULL))) %>% add_column(Formant="F1",.before=1)
        md_T3_F1_lsmCont_FONANT = data.frame(summary(update(pairs(md_T3_F1_lsm_FONANT, by = "FONANT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F1",.before=1)

        md_T3_F2_lsm_FONANT = lsmeans::lsmeans(md_T3_F2,  ~ VOCAL|FONANT)
        md_T3_F2_lsmeans_FONANT = data.frame(summary(update(md_T3_F2_lsm_FONANT, by = NULL))) %>% add_column(Formant="F2",.before=1)
        md_T3_F2_lsmCont_FONANT = data.frame(summary(update(pairs(md_T3_F2_lsm_FONANT, by = "FONANT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F2",.before=1)

        md_T3_F1F2_lsmeans_FONANT_df = merge(md_T3_F1_lsmeans_FONANT, md_T3_F2_lsmeans_FONANT, all = TRUE) %>%
          select(Formant, FONANT, VOCAL, lsmean, SE, df, lower.CL, upper.CL) %>%
          mutate(FONANT = factor(FONANT, levels = c("Alveolopalatal", "lr", "Labial", "Dentalveolar"), labels = c("Alveolopalatal", "/l,r/", "Labial", "Dentalveolar")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)

        md_T3_VFONANT_lsmPlot =  ggplot(data=md_T3_F1F2_lsmeans_FONANT_df, aes(x=FONANT, y=lsmean, group=Formant:VOCAL)) +
          geom_path(size=0.8, aes(color=VOCAL, linetype=Formant)) +
          geom_point(size=1.5, aes(color=VOCAL)) +
          geom_errorbar(width=0.2, aes(ymin=lower.CL, ymax=upper.CL, color=VOCAL)) +
          scale_colour_manual(values=vowel.colours, guide=FALSE) + #guide = guide_legend(title = "Vocal")) +
          scale_fill_manual(values=vowel.colours, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE) +
          ylab("Hz (Lobanov)") +
          xlab("Segment anterior") +
          theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #md_T3_VFONANT_lsmPlot

        ggsave(filename="d_T3_VFONANT_lsmPlot.pdf",
               plot=md_T3_VFONANT_lsmPlot, device=cairo_pdf, path=OUTPUT_lsmPlots,
               height=6, width=5, units="cm", dpi=600)

        md_T3_F1F2_lsmCont_FONANT_df = merge(md_T3_F1_lsmCont_FONANT,md_T3_F2_lsmCont_FONANT, all=TRUE) %>%
          select(contrast, Formant, FONANT, estimate, SE, df, t.ratio, p.value) %>%
          mutate(Formant = factor(Formant)) %>%
          arrange(Formant,FONANT)

        # >>> EDAT -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME

        md_T3_F1_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", md_T3_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        md_T3_F2_VEDAT_eff = as.data.frame(effect("EDAT:VOCAL", md_T3_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        md_T3_VEDAT_eff = merge(md_T3_F1_VEDAT_eff, md_T3_F2_VEDAT_eff, all = TRUE) %>%
          mutate(EDAT = factor(EDAT, levels = c("FE1", "FE2")), Formant = factor(Formant)) %>%
          arrange(Formant,EDAT)

        # PLOT INTERACTION

        md_T3_EDATV_intPlot =   ggplot(data=md_T3_VEDAT_eff,
                                       aes(x=VOCAL, y=fit, group=Formant:EDAT)) +
          geom_path(size=0.8, aes(color=EDAT, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=EDAT), alpha=.3) +
								geom_point(size=1.5,aes(color=EDAT),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=EDAT, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=EDAT), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Edat")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #md_T3_EDATV_intPlot

        ggsave(filename="d_T3_EDATV_intPlot.pdf",
               plot=md_T3_EDATV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)

        ## LSMEANS

        # DATA FRAME
        md_T3_F1_lsm_EDAT = lsmeans::lsmeans(md_T3_F1,  ~ VOCAL|EDAT)
        md_T3_F1_lsmeans_EDAT = data.frame(summary(update(md_T3_F1_lsm_EDAT, by = NULL))) %>% add_column(Formant="F1",.before=1)
        md_T3_F1_lsmCont_EDAT = data.frame(summary(update(pairs(md_T3_F1_lsm_EDAT, by = "EDAT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F1",.before=1)

        md_T3_F2_lsm_EDAT = lsmeans::lsmeans(md_T3_F2,  ~ VOCAL|EDAT)
        md_T3_F2_lsmeans_EDAT = data.frame(summary(update(md_T3_F2_lsm_EDAT, by = NULL))) %>% add_column(Formant="F2",.before=1)
        md_T3_F2_lsmCont_EDAT = data.frame(summary(update(pairs(md_T3_F2_lsm_EDAT, by = "EDAT", adjust="Bonferroni"), by = NULL))) %>% add_column(Formant="F2",.before=1)

        md_T3_F1F2_lsmeans_EDAT_df = merge(md_T3_F1_lsmeans_EDAT, md_T3_F2_lsmeans_EDAT, all = TRUE) %>%
          select(Formant, EDAT, VOCAL, lsmean, SE, df, lower.CL, upper.CL) %>%
          mutate(Formant = factor(Formant)) %>%
          arrange(Formant,EDAT)

        md_T3_VEDAT_lsmPlot =  ggplot(data=md_T3_F1F2_lsmeans_EDAT_df, aes(x=EDAT, y=lsmean, group=Formant:VOCAL)) +
          geom_path(size=0.8, aes(color=VOCAL, linetype=Formant)) +
          geom_point(size=1.5, aes(color=VOCAL)) +
          geom_errorbar(width=0.2, aes(ymin=lower.CL, ymax=upper.CL, color=VOCAL)) +
          scale_colour_manual(values=vowel.colours, guide=FALSE) + #guide = guide_legend(title = "Vocal")) +
          scale_fill_manual(values=vowel.colours, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE) +
          ylab("Hz (Lobanov)") +
          xlab("Edat") +
          theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #md_T3_VEDAT_lsmPlot

        ggsave(filename="d_T3_VEDAT_lsmPlot.pdf",
               plot=md_T3_VEDAT_lsmPlot, device=cairo_pdf, path=OUTPUT_lsmPlots,
               height=6, width=5, units="cm", dpi=600)

        md_T3_F1F2_lsmCont_EDAT_df = merge(md_T3_F1_lsmCont_EDAT,md_T3_F2_lsmCont_EDAT, all=TRUE) %>%
          select(contrast, Formant, EDAT, estimate, SE, df, t.ratio, p.value) %>%
          mutate(Formant = factor(Formant)) %>%
          arrange(Formant,EDAT)

        # >>> SEXE -------------------------------------------------------------------

        ## INTERACTION EFFECTS

        # DATA FRAME
        md_T3_F1_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", md_T3_F1, se=TRUE)) %>% add_column(Formant="F1",.before=1)
        md_T3_F2_VSEXE_eff = as.data.frame(effect("SEXE:VOCAL", md_T3_F2, se=TRUE)) %>% add_column(Formant="F2",.before=1)
        md_T3_VSEXE_eff = merge(md_T3_F1_VSEXE_eff, md_T3_F2_VSEXE_eff, all = TRUE) %>%
          mutate(SEXE = factor(SEXE, levels = c("D", "H"), labels=c("Dones", "Homes")),
                 Formant = factor(Formant)) %>%
          arrange(Formant,SEXE)

        # PLOT INTERACTION

        md_T3_SEXEV_intPlot =   ggplot(data=md_T3_VSEXE_eff,
                                       aes(x=VOCAL, y=fit, group=Formant:SEXE)) +
          geom_path(size=0.8, aes(color=SEXE, linetype=Formant)) +
          #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=SEXE), alpha=.3) +
								geom_point(size=1.5,aes(color=SEXE),position=position_dodge(0.05)) +
								geom_path(size=0.8, aes(color=SEXE, linetype=Formant),position=position_dodge(0.05)) +
								geom_errorbar(aes(ymin=lower, ymax=upper, color=SEXE), width=0.2,position=position_dodge(0.05)) +
          scale_color_manual(values=cbPalette, guide=FALSE) + #guide = guide_legend(title = "Gènere")) +
          scale_fill_manual(values=cbPalette, guide=FALSE) +
          scale_linetype_manual(values=c("solid", "dotted"), guide=FALSE)+
          ylab("Hz (Lobanov)") + xlab("Vocal") +
          theme_opts#+ theme(legend.margin=margin(0,0,0,0, unit='cm'), legend.key.height = unit(0.2, "cm"))

        #md_T3_SEXEV_intPlot

        ggsave(filename="d_T3_SEXEV_intPlot.pdf",
               plot=md_T3_SEXEV_intPlot, device=cairo_pdf, path=OUTPUT_intPlots,
               height=6, width=5, units="cm", dpi=600)
