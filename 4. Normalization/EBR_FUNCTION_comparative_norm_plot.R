## plot function used by EBR_normalization_comparison.R

comp_norm_plot <- function(df,
                           INFORMANT1=NULL,
                           INFORMANT2=NULL,
                           INT=c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, "ALL"),
                           TEST=c("TOT", "T1", "T2", "T3", "T1-T3"),
                           NORM=c("Hz", "Lobanov", "Nearey1", "Bark", "mWF")) {

  vowel.colours <- c("a" = "grey28",  #dark grey
                     "e" = "#D55E00", #orange
                     "ɛ" = "#009E73", #green
                     "i" = "#56B4E9", #light blue
                     "o" = "#0072B2", #dark blue
                     "ɔ" = "#CC79A7", #pink
                     "u" = "#F0E442") #yellow

  require(ggplot2)
  require(scales) # pretty_breaks (scale_y_reverse, etc.)

  # subset speakers
  dfinformants = df[df$INFORMANT==INFORMANT1 | df$INFORMANT==INFORMANT2,]

  # subset interval
  if(INT==0.2) {dfint = dfinformants[dfinformants$INTERVAL==0.2,]}
  else if(INT==0.3) {dfint = dfinformants[dfinformants$INTERVAL==0.3,]}
  else if(INT==0.4) {dfint = dfinformants[dfinformants$INTERVAL==0.4,]}
  else if(INT==0.5) {dfint = dfinformants[dfinformants$INTERVAL==0.5,]}
  else if(INT==0.6) {dfint = dfinformants[dfinformants$INTERVAL==0.6,]}
  else if(INT==0.7) {dfint = dfinformants[dfinformants$INTERVAL==0.7,]}
  else if(INT==0.8) {dfint = dfinformants[dfinformants$INTERVAL==0.8,]}
  else if(INT=="ALL") {dfint = dfinformants}

  # subset test
  if(TEST=="T1") {dftest = dfint[dfint$TEST=="T1",]}
  else if(TEST=="T2") {dftest = dfint[dfint$TEST=="T2",]}
  else if(TEST=="T3") {dftest = dfint[dfint$TEST=="T3",]}
  else if(TEST=="T1-T3") {dftest = dfint[dfint$TEST!="T2",]}
  else if(TEST=="TOT") {dftest = dfint}

  source("/path/to/EBR_FUNCTION_summarystats.R")
  summ_df <- summarystats(df, INTsumm = INT, TESTsumm = TEST, GROUP = "Individual", LEVEL = "Vocal")

  ## subset data from normalization method
  if(NORM=="Hz") {
    FORMANT2 = dftest$F2
    FORMANT1 = dftest$F1
    F2labs = "F2 (Hz)"
    F1labs = "F1 (Hz)"
    F2summ1 = summ_df[summ_df$INFORMANT==INFORMANT1,]$F2.mean
    F1summ1 = summ_df[summ_df$INFORMANT==INFORMANT1,]$F1.mean
    F2summ2 = summ_df[summ_df$INFORMANT==INFORMANT2,]$F2.mean
    F1summ2 = summ_df[summ_df$INFORMANT==INFORMANT2,]$F1.mean
    subplottitle = "Valors no normalitzats (Hz)"
    theme_norm <- list(scale_y_reverse(),  # pretty_breaks -- require(scales)
                      scale_x_reverse(),
                      expand_limits(x=c(max(FORMANT2)+(max(FORMANT2)/50),min(FORMANT2)-(min(FORMANT2)/50)),
                                    y=c(max(FORMANT1)+(max(FORMANT1)/50),min(FORMANT1)-(max(FORMANT1)/50))))
  }

  else if(NORM=="Lobanov") {
    FORMANT2 = dftest$Lobanov.F2
    FORMANT1 = dftest$Lobanov.F1
    F2labs = "F2 (Lobanov)"
    F1labs = "F1 (Lobanov)"
    F2summ1 = summ_df[summ_df$INFORMANT==INFORMANT1,]$Lobanov.F2.mean
    F1summ1 = summ_df[summ_df$INFORMANT==INFORMANT1,]$Lobanov.F1.mean
    F2summ2 = summ_df[summ_df$INFORMANT==INFORMANT2,]$Lobanov.F2.mean
    F1summ2 = summ_df[summ_df$INFORMANT==INFORMANT2,]$Lobanov.F1.mean
    subplottitle = "Normalització pel mètode Lobanov"
    theme_norm <- list(scale_y_reverse(),  # pretty_breaks -- require(scales)
                       scale_x_reverse(),
                       expand_limits(x=c(max(FORMANT2)+(max(FORMANT2)/50),min(FORMANT2)-(min(FORMANT2)/50)),
                                     y=c(max(FORMANT1)+(max(FORMANT1)/50),min(FORMANT1)-(max(FORMANT1)/50))))
  }

  else if(NORM=="Nearey1") {
    FORMANT2 = dftest$Nearey1.F2
    FORMANT1 = dftest$Nearey1.F1
    F2labs = "F2 (Nearey 2)"
    F1labs = "F1 (Nearey 1)"
    F2summ1 = summ_df[summ_df$INFORMANT==INFORMANT1,]$Nearey1.F2.mean
    F1summ1 = summ_df[summ_df$INFORMANT==INFORMANT1,]$Nearey1.F1.mean
    F2summ2 = summ_df[summ_df$INFORMANT==INFORMANT2,]$Nearey1.F2.mean
    F1summ2 = summ_df[summ_df$INFORMANT==INFORMANT2,]$Nearey1.F1.mean
    subplottitle = "Normalització pel mètode Nearey1"
    theme_norm <- list(scale_y_reverse(),  # pretty_breaks -- require(scales)
                       scale_x_reverse(),
                       expand_limits(x=c(max(FORMANT2)+(max(FORMANT2)/50),min(FORMANT2)-(min(FORMANT2)/50)),
                                     y=c(max(FORMANT1)+(max(FORMANT1)/50),min(FORMANT1)-(max(FORMANT1)/50))))
  }

  else if(NORM=="mWF") {
    FORMANT2 = dftest$mW.F.F2
    FORMANT1 = dftest$mW.F.F1
    F2labs = "F2 (mW&F)"
    F1labs = "F1 (mW&F)"
    F2summ1 = summ_df[summ_df$INFORMANT==INFORMANT1,]$mW.F.F2.mean
    F1summ1 = summ_df[summ_df$INFORMANT==INFORMANT1,]$mW.F.F1.mean
    F2summ2 = summ_df[summ_df$INFORMANT==INFORMANT2,]$mW.F.F2.mean
    F1summ2 = summ_df[summ_df$INFORMANT==INFORMANT2,]$mW.F.F1.mean
    subplottitle = "Normalització pel mètode Watt & Fabricius"
    theme_norm <- list(scale_y_reverse(),  # pretty_breaks -- require(scales)
                       scale_x_reverse(),
                       expand_limits(x=c(max(FORMANT2)+(max(FORMANT2)/50),min(FORMANT2)-(min(FORMANT2)/50)),
                                     y=c(max(FORMANT1)+(max(FORMANT1)/50),min(FORMANT1)-(max(FORMANT1)/50))))
  }

  else if(NORM=="Bark") {
    FORMANT2 = dftest$Bark.Z3.Z2
    FORMANT1 = dftest$Bark.Z3.Z1
    F2labs = "Z3-Z2"
    F1labs = "Z3-Z1"
    F2summ1 = summ_df[summ_df$INFORMANT==INFORMANT1,]$Bark.Z3.Z2.mean
    F1summ1 = summ_df[summ_df$INFORMANT==INFORMANT1,]$Bark.Z3.Z1.mean
    F2summ2 = summ_df[summ_df$INFORMANT==INFORMANT2,]$Bark.Z3.Z2.mean
    F1summ2 = summ_df[summ_df$INFORMANT==INFORMANT2,]$Bark.Z3.Z1.mean
    subplottitle = "Normalització pel Bark Difference Method"
    theme_norm <- list(expand_limits(x=c(max(FORMANT2)+(max(FORMANT2)/70),min(FORMANT2)-(min(FORMANT2)/70)),
                                     y=c(max(FORMANT1)+(max(FORMANT1)/70),min(FORMANT1)-(max(FORMANT1)/70))))
  }


theme_opts <- list(theme_bw(base_size=6, base_family="Helvetica"), #"Linux Biolinum O" / "Linux Libertine O"
                     theme(panel.border = element_blank(),
                           axis.line.x = element_line(size = 0.1, colour = "gray41"),
                           axis.line.y = element_line(size = 0.1, colour = "gray41"),
                           axis.ticks = element_line(colour = "gray41"),
                           axis.text = element_text(colour = "gray41")),
                     labs(x=F2labs, y=F1labs, lty="Informant:"),
                   theme(legend.position="none"),
                     #ggtitle(subplottitle),
                   guides(fill=FALSE,colour=FALSE))

plot = ggplot(dftest, aes(x=FORMANT2, y=FORMANT1, colour=factor(VOCAL))) +
      scale_colour_manual(values = vowel.colours) +
      scale_fill_manual(values = vowel.colours) +
      #stat_ellipse(type="t", level=0.85, geom="polygon", alpha=0.10, aes(lty=factor(INFORMANT), fill=factor(VOCAL))) + # també: type="norm"
      stat_ellipse(type="t", level=0.85, aes(lty=factor(INFORMANT))) + # també: type="norm"
      geom_text(data=summ_df[summ_df$INFORMANT==INFORMANT1,],
                aes(x=F2summ1, y=F1summ1, label=VOCAL),
                size=3,
                family="Helvetica") + # mitjanes obtingudes amb summarystats
      geom_text(data=summ_df[summ_df$INFORMANT==INFORMANT2,],
                aes(x=F2summ2, y=F1summ2, label=VOCAL),
                size=3,
                family="Helvetica",
                fontface="italic",
                lineheight=.8) + # mitjanes obtingudes amb summarystats
      theme_norm + theme_opts

  print(plot)
  return(plot)

}


## Share a legend between multiple plots that do not share axes using grid.arrange
## (https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs)
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] +
                    theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

g_legend<-function(plot){
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}