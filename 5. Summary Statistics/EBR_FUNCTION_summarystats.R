# EVA BOSCH-ROURA
#
# FUNCTION TO CREATE TABLES WITH DESCRIPTIVE STATISTICS SUMMARIES FOR VOWEL F1, F2(, AND F3)
# VALUES IN DATA FRAME WITH VALUES FOR UNNORMALIZED AND Lobanov- Nearey1-, Bark Difference Metric- and
# Modified Watt&Fabricius- NORMALIZED FORMANT VALUES.
#
# Used by several scripts in https://github.com/EvaBoR/GironaMidBackVowels/

summarystats <- function(df,
                         INTsumm = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, "ALL"),
                         TESTsumm = c("TOT", "T1", "T2", "T3", "T1-T3"),
                         GROUP = c("Individual","Arxiprestat", "SexesArxiprestat", "FUSIOOS"),
                         LEVEL = c("Vocal", "VocalContext", "VocalFonpost", "PosicioSil")) {

  # ORDER OF THE VOWELS IN THE TABLES
  vowel.order <- c("i", "e", "ɛ", "a", "ɔ", "o", "u")

  # LOAD PACKAGES
  require(tidyr)

  # subset interval
    if(INTsumm==0.2) {dfint = df[df$INTERVAL==0.2,]}
        else if(INTsumm==0.3) {dfint = df[df$INTERVAL==0.3,]}
        else if(INTsumm==0.4) {dfint = df[df$INTERVAL==0.4,]}
        else if(INTsumm==0.5) {dfint = df[df$INTERVAL==0.5,]}
        else if(INTsumm==0.6) {dfint = df[df$INTERVAL==0.6,]}
        else if(INTsumm==0.7) {dfint = df[df$INTERVAL==0.7,]}
        else if(INTsumm==0.8) {dfint = df[df$INTERVAL==0.8,]}
        else if(INTsumm=="ALL") {dfint = df}

  # subset test
    if(TESTsumm=="T1") {dftest = dfint[dfint$TEST=="T1",]}
      else if(TESTsumm=="T2") {dftest = dfint[dfint$TEST=="T2",]}
      else if(TESTsumm=="T3") {dftest = dfint[dfint$TEST=="T3",]}
      else if(TESTsumm=="T1-T3") {dftest = dfint[dfint$TEST!="T2",]}
      else if(TESTsumm=="TOT") {dftest = dfint}

  # compute summary statistics by group

  if(GROUP=="Individual") {

    if(LEVEL=="Vocal") {
    # COMPUTE BASIC STATISTICS ON UNNORMALIZED FORMANT VALUES
      basicsHz <- do.call(data.frame,
                          aggregate(formula= cbind(F1, F2, F3) ~ INFORMANT + VOCAL, # For columns F1, F2, and F3, for each SPEAKER and VOWEL,
                                    data = dftest, # on data frame "dftest",
                                    FUN=function(x) c(N=length(x),# compute number of items,
                                                      mean=round(as.vector(mean(x))), # (rounded) mean,
                                                      sd=sd(x), # standard deviation,
                                                      median=median(x)))) # and median

      basicsHz <- basicsHz[c(-7,-11)] # remove repeated "number of items" columns
      colnames(basicsHz)[3] <- "N" # rename "number of items" columns
      basicsHz$VOCAL <- factor(basicsHz$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsHz <- basicsHz[order(basicsHz$INFORMANT, basicsHz$VOCAL),] # reorder by SPEAKER first, then VOWEL

    # COMPUTE BASIC STATISTICS ON LOBANOV-NORMALIZED FORMANT VALUES
      basicsLobanov <- do.call(data.frame,
                               aggregate(formula= cbind(Lobanov.F1, Lobanov.F2) ~ INFORMANT + VOCAL, # For columns Lobanov.F1 and Lobanov.F2, for each SPEAKER and VOWEL,
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median


      basicsLobanov$VOCAL <- factor(basicsLobanov$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsLobanov <- basicsLobanov[order(basicsLobanov$INFORMANT, basicsLobanov$VOCAL),] # reorder by SPEAKER first, then VOWEL

    # COMPUTE BASIC STATISTICS ON BARK DIFFERENCE METRIC-NORMALIZED FORMANT VALUES
      basicsBark <- do.call(data.frame,
                            aggregate(formula= cbind(Bark.Z3.Z1, Bark.Z3.Z2) ~ INFORMANT + VOCAL, # For columns Bark.Z3-Z1 and Bark.Z3.Z2, for each SPEAKER and VOWEL,
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsBark$VOCAL <- factor(basicsBark$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsBark <- basicsBark[order(basicsBark$INFORMANT, basicsBark$VOCAL),] # reorder by SPEAKER first, then VOWEL

    # COMPUTE BASIC STATISTICS ON MODIFIED WATT & FABRICIUS-NORMALIZED FORMANT VALUES
      basicsmW.F <- do.call(data.frame,
                            aggregate(formula= cbind(mW.F.F1, mW.F.F2) ~ INFORMANT + VOCAL, # For columns mW.F.F1 and mW.F.F2, for each SPEAKER and VOWEL,
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsmW.F$VOCAL <- factor(basicsmW.F$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsmW.F <- basicsmW.F[order(basicsmW.F$INFORMANT, basicsmW.F$VOCAL),] # reorder by SPEAKER first, then VOWEL

    # COMPUTE BASIC STATISTICS ON NEAREY 1-NORMALIZED FORMANT VALUES
      basicsNearey1 <- do.call(data.frame,
                               aggregate(formula= cbind(Nearey1.F1, Nearey1.F2, Nearey1.F3) ~ INFORMANT + VOCAL, # For columns Nearey1.F1, Nearey1.F2, and Nearey1.F3 for each SPEAKER and VOWEL,
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median


      basicsNearey1$VOCAL <- factor(basicsNearey1$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsNearey1 <- basicsNearey1[order(basicsNearey1$INFORMANT, basicsNearey1$VOCAL),] # reorder by SPEAKER first, then VOWEL

    # CREATE DATA.FRAME WITH ALL DATA.FRAMES OF SUMMARY STATISTICS WE JUST CREATED (removing )
      allsummarystats <- cbind(basicsHz, basicsLobanov[c(-1,-2)], basicsBark[c(-1,-2)], basicsmW.F[c(-1,-2)], basicsNearey1[c(-1,-2)])
    }

      else if(LEVEL=="VocalContext"){

        # COMPUTE BASIC STATISTICS ON UNNORMALIZED FORMANT VALUES
        basicsHz <- do.call(data.frame,
                            aggregate(formula= cbind(F1, F2, F3) ~ INFORMANT + VOCAL + N.CONTEXT, # For columns F1, F2, and F3, for each SPEAKER and VOWEL, by CONTEXT
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(N=length(x),# compute number of items,
                                                        mean=round(as.vector(mean(x))), # (rounded) mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median

        basicsHz <- basicsHz[c(-8,-12)] # remove repeated "number of items" columns
        colnames(basicsHz)[4] <- "N" # rename "number of items" columns
        basicsHz$VOCAL <- factor(basicsHz$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
        basicsHz <- basicsHz[order(basicsHz$INFORMANT, basicsHz$VOCAL, basicsHz$N.CONTEXT),] # reorder by SPEAKER first, then VOWEL, then context

        # COMPUTE BASIC STATISTICS ON LOBANOV-NORMALIZED FORMANT VALUES
        basicsLobanov <- do.call(data.frame,
                                 aggregate(formula= cbind(Lobanov.F1, Lobanov.F2) ~ INFORMANT + VOCAL + N.CONTEXT, # For columns Lobanov.F1 and Lobanov.F2, for each SPEAKER and VOWEL, by CONTEXT
                                           data = dftest, # on data frame "dftest",
                                           FUN=function(x) c(mean=mean(x), # compute mean,
                                                             sd=sd(x), # standard deviation,
                                                             median=median(x)))) # and median


        basicsLobanov$VOCAL <- factor(basicsLobanov$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
        basicsLobanov <- basicsLobanov[order(basicsLobanov$INFORMANT, basicsLobanov$VOCAL, basicsLobanov$N.CONTEXT),] # reorder by SPEAKER first, then VOWEL, then context

        # COMPUTE BASIC STATISTICS ON BARK DIFFERENCE METRIC-NORMALIZED FORMANT VALUES
        basicsBark <- do.call(data.frame,
                              aggregate(formula= cbind(Bark.Z3.Z1, Bark.Z3.Z2) ~ INFORMANT + VOCAL + N.CONTEXT, # For columns Bark.Z3-Z1 and Bark.Z3.Z2, for each SPEAKER and VOWEL by CONTEXT,
                                        data = dftest, # on data frame "dftest",
                                        FUN=function(x) c(mean=mean(x), # compute mean,
                                                          sd=sd(x), # standard deviation,
                                                          median=median(x)))) # and median


        basicsBark$VOCAL <- factor(basicsBark$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
        basicsBark <- basicsBark[order(basicsBark$INFORMANT, basicsBark$VOCAL, basicsBark$N.CONTEXT),] # reorder by SPEAKER first, then VOWEL, then context

        # COMPUTE BASIC STATISTICS ON MODIFIED WATT & FABRICIUS-NORMALIZED FORMANT VALUES
        basicsmW.F <- do.call(data.frame,
                              aggregate(formula= cbind(mW.F.F1, mW.F.F2) ~ INFORMANT + VOCAL + N.CONTEXT, # For columns mW.F.F1 and mW.F.F2, for each SPEAKER and VOWEL, by CONTEXT
                                        data = dftest, # on data frame "dftest",
                                        FUN=function(x) c(mean=mean(x), # compute mean,
                                                          sd=sd(x), # standard deviation,
                                                          median=median(x)))) # and median


        basicsmW.F$VOCAL <- factor(basicsmW.F$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
        basicsmW.F <- basicsmW.F[order(basicsmW.F$INFORMANT, basicsmW.F$VOCAL, basicsmW.F$N.CONTEXT),] # reorder by SPEAKER first, then VOWEL, then context

        # COMPUTE BASIC STATISTICS ON NEAREY 1-NORMALIZED FORMANT VALUES
        basicsNearey1 <- do.call(data.frame,
                                 aggregate(formula= cbind(Nearey1.F1, Nearey1.F2, Nearey1.F3) ~ INFORMANT + VOCAL + N.CONTEXT, # For columns Nearey1.F1, Nearey1.F2, and Nearey1.F3 for each SPEAKER and VOWEL, by context
                                           data = dftest, # on data frame "dftest",
                                           FUN=function(x) c(mean=mean(x), # compute mean,
                                                             sd=sd(x), # standard deviation,
                                                             median=median(x)))) # and median


        basicsNearey1$VOCAL <- factor(basicsNearey1$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
        basicsNearey1 <- basicsNearey1[order(basicsNearey1$INFORMANT, basicsNearey1$VOCAL, basicsNearey1$N.CONTEXT),] # reorder by SPEAKER first, then VOWEL, then CONTEXT

        # CREATE DATA.FRAME WITH ALL DATA.FRAMES OF SUMMARY STATISTICS WE JUST CREATED (removing )
        allsummarystats <- cbind(basicsHz, basicsLobanov[c(-1,-2,-3)], basicsBark[c(-1,-2,-3)], basicsmW.F[c(-1,-2,-3)], basicsNearey1[c(-1,-2,-3)])

      }

    else if(LEVEL=="VocalFonpost"){

      # COMPUTE BASIC STATISTICS ON UNNORMALIZED FORMANT VALUES
      basicsHz <- do.call(data.frame,
                          aggregate(formula= cbind(F1, F2, F3) ~ INFORMANT + VOCAL + FONEMA_POST., # For columns F1, F2, and F3, for each SPEAKER and VOWEL, by POSTERIOR SEGMENT
                                    data = dftest, # on data frame "dftest",
                                    FUN=function(x) c(N=length(x),# compute number of items,
                                                      mean=round(as.vector(mean(x))), # (rounded) mean,
                                                      sd=sd(x), # standard deviation,
                                                      median=median(x)))) # and median

      basicsHz <- basicsHz[c(-8,-12)] # remove repeated "number of items" columns
      colnames(basicsHz)[4] <- "N" # rename "number of items" columns
      basicsHz$VOCAL <- factor(basicsHz$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsHz <- basicsHz[order(basicsHz$INFORMANT, basicsHz$VOCAL, basicsHz$FONEMA_POST.),] # reorder by SPEAKER first, then VOWEL, then posterior segment

      # COMPUTE BASIC STATISTICS ON LOBANOV-NORMALIZED FORMANT VALUES
      basicsLobanov <- do.call(data.frame,
                               aggregate(formula= cbind(Lobanov.F1, Lobanov.F2) ~ INFORMANT + VOCAL + FONEMA_POST., # For columns Lobanov.F1 and Lobanov.F2, for each SPEAKER and VOWEL, by POSTERIOR SEGMENT
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median


      basicsLobanov$VOCAL <- factor(basicsLobanov$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsLobanov <- basicsLobanov[order(basicsLobanov$INFORMANT, basicsLobanov$VOCAL, basicsLobanov$FONEMA_POST.),] # reorder by SPEAKER first, then VOWEL, then POSTERIOR SEGMENT

      # COMPUTE BASIC STATISTICS ON BARK DIFFERENCE METRIC-NORMALIZED FORMANT VALUES
      basicsBark <- do.call(data.frame,
                            aggregate(formula= cbind(Bark.Z3.Z1, Bark.Z3.Z2) ~ INFORMANT + VOCAL + FONEMA_POST., # For columns Bark.Z3-Z1 and Bark.Z3.Z2, for each SPEAKER and VOWEL by POSTERIOR SEGMENT
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsBark$VOCAL <- factor(basicsBark$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsBark <- basicsBark[order(basicsBark$INFORMANT, basicsBark$VOCAL, basicsBark$FONEMA_POST.),] # reorder by SPEAKER first, then VOWEL, then POSTERIOR SEGMENT

      # COMPUTE BASIC STATISTICS ON MODIFIED WATT & FABRICIUS-NORMALIZED FORMANT VALUES
      basicsmW.F <- do.call(data.frame,
                            aggregate(formula= cbind(mW.F.F1, mW.F.F2) ~ INFORMANT + VOCAL + FONEMA_POST., # For columns mW.F.F1 and mW.F.F2, for each SPEAKER and VOWEL, by POSTERIOR SEGMENT
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsmW.F$VOCAL <- factor(basicsmW.F$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsmW.F <- basicsmW.F[order(basicsmW.F$INFORMANT, basicsmW.F$VOCAL, basicsmW.F$FONEMA_POST.),] # reorder by SPEAKER first, then VOWEL, then POSTERIOR SEGMENT

      # COMPUTE BASIC STATISTICS ON NEAREY 1-NORMALIZED FORMANT VALUES
      basicsNearey1 <- do.call(data.frame,
                               aggregate(formula= cbind(Nearey1.F1, Nearey1.F2, Nearey1.F3) ~ INFORMANT + VOCAL + FONEMA_POST., # For columns Nearey1.F1, Nearey1.F2, and Nearey1.F3 for each SPEAKER and VOWEL, by POSTERIOR SEGMENT
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median


      basicsNearey1$VOCAL <- factor(basicsNearey1$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsNearey1 <- basicsNearey1[order(basicsNearey1$INFORMANT, basicsNearey1$VOCAL, basicsNearey1$FONEMA_POST.),] # reorder by SPEAKER first, then VOWEL, then POSTERIOR SEGMENT

      # CREATE DATA.FRAME WITH ALL DATA.FRAMES OF SUMMARY STATISTICS WE JUST CREATED (removing )
      allsummarystats <- cbind(basicsHz, basicsLobanov[c(-1,-2,-3)], basicsBark[c(-1,-2,-3)], basicsmW.F[c(-1,-2,-3)], basicsNearey1[c(-1,-2,-3)])

    }

    else if(LEVEL=="PosicioSil"){

      # COMPUTE BASIC STATISTICS ON UNNORMALIZED FORMANT VALUES
      basicsHz <- do.call(data.frame,
                          aggregate(formula= cbind(F1, F2, F3) ~ INFORMANT + VOCAL + SÍL.posició., # For columns F1, F2, and F3, for each SPEAKER and VOWEL, by SYLLABLE POSITION (initial, mid, final, monosyllabic)
                                    data = dftest, # on data frame "dftest",
                                    FUN=function(x) c(N=length(x),# compute number of items,
                                                      mean=round(as.vector(mean(x))), # (rounded) mean,
                                                      sd=sd(x), # standard deviation,
                                                      median=median(x)))) # and median
      View(basicsHz)
      basicsHz <- basicsHz[c(-8,-12)] # remove repeated "number of items" columns
      colnames(basicsHz)[4] <- "N" # rename "number of items" columns
      basicsHz$VOCAL <- factor(basicsHz$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsHz <- basicsHz[order(basicsHz$INFORMANT, basicsHz$VOCAL, basicsHz$SÍL.posició.),] # reorder by SPEAKER first, then VOWEL, then SYLLABLE POSITION (initial, mid, final, monosyllabic)

      # COMPUTE BASIC STATISTICS ON LOBANOV-NORMALIZED FORMANT VALUES
      basicsLobanov <- do.call(data.frame,
                               aggregate(formula= cbind(Lobanov.F1, Lobanov.F2) ~ INFORMANT + VOCAL + SÍL.posició., # For columns Lobanov.F1 and Lobanov.F2, for each SPEAKER and VOWEL, by SYLLABLE POSITION (initial, mid, final, monosyllabic)
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median


      basicsLobanov$VOCAL <- factor(basicsLobanov$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsLobanov <- basicsLobanov[order(basicsLobanov$INFORMANT, basicsLobanov$VOCAL, basicsLobanov$SÍL.posició.),] # reorder by SPEAKER first, then VOWEL, then SYLLABLE POSITION (initial, mid, final, monosyllabic)

      # COMPUTE BASIC STATISTICS ON BARK DIFFERENCE METRIC-NORMALIZED FORMANT VALUES
      basicsBark <- do.call(data.frame,
                            aggregate(formula= cbind(Bark.Z3.Z1, Bark.Z3.Z2) ~ INFORMANT + VOCAL + SÍL.posició., # For columns Bark.Z3-Z1 and Bark.Z3.Z2, for each SPEAKER and VOWEL by SYLLABLE POSITION (initial, mid, final, monosyllabic)
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsBark$VOCAL <- factor(basicsBark$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsBark <- basicsBark[order(basicsBark$INFORMANT, basicsBark$VOCAL, basicsBark$SÍL.posició.),] # reorder by SPEAKER first, then VOWEL, then SYLLABLE POSITION (initial, mid, final, monosyllabic)

      # COMPUTE BASIC STATISTICS ON MODIFIED WATT & FABRICIUS-NORMALIZED FORMANT VALUES
      basicsmW.F <- do.call(data.frame,
                            aggregate(formula= cbind(mW.F.F1, mW.F.F2) ~ INFORMANT + VOCAL + SÍL.posició., # For columns mW.F.F1 and mW.F.F2, for each SPEAKER and VOWEL, by SYLLABLE POSITION (initial, mid, final, monosyllabic)
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsmW.F$VOCAL <- factor(basicsmW.F$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsmW.F <- basicsmW.F[order(basicsmW.F$INFORMANT, basicsmW.F$VOCAL, basicsmW.F$SÍL.posició.),] # reorder by SPEAKER first, then VOWEL, then SYLLABLE POSITION (initial, mid, final, monosyllabic)

      # COMPUTE BASIC STATISTICS ON NEAREY 1-NORMALIZED FORMANT VALUES
      basicsNearey1 <- do.call(data.frame,
                               aggregate(formula= cbind(Nearey1.F1, Nearey1.F2, Nearey1.F3) ~ INFORMANT + VOCAL + SÍL.posició., # For columns Nearey1.F1, Nearey1.F2, and Nearey1.F3 for each SPEAKER and VOWEL, by SYLLABLE POSITION (initial, mid, final, monosyllabic)
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median


      basicsNearey1$VOCAL <- factor(basicsNearey1$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsNearey1 <- basicsNearey1[order(basicsNearey1$INFORMANT, basicsNearey1$VOCAL, basicsNearey1$SÍL.posició.),] # reorder by SPEAKER first, then VOWEL, then SYLLABLE POSITION (initial, mid, final, monosyllabic)

      # CREATE DATA.FRAME WITH ALL DATA.FRAMES OF SUMMARY STATISTICS WE JUST CREATED (removing )
      allsummarystats = cbind(basicsHz, basicsLobanov[c(-1,-2,-3)], basicsBark[c(-1,-2,-3)], basicsmW.F[c(-1,-2,-3)], basicsNearey1[c(-1,-2,-3)])

    }

    ## get REGION, AGE, and SEX COLUMNS from ID codes
    allsummarystats = extract(allsummarystats, "INFORMANT", c("ARXIPRESTAT", "EDAT", "SEXE"), regex = "(AEI|AEM|AF|BA|CBC|FM|GS|MA|MB|SE|TB|TO)-(FE1|FE2)-(D|H)[1|2|3|4]", remove=FALSE, convert=FALSE)

    return(allsummarystats)
  } # if(GROUP=="Individual)


  if(GROUP=="Arxiprestat") {

    if(LEVEL=="Vocal") {
      # COMPUTE BASIC STATISTICS ON LOBANOV-NORMALIZED FORMANT VALUES
      basicsLobanov <- do.call(data.frame,
                               aggregate(formula= cbind(Lobanov.F1, Lobanov.F2) ~ ARXIPRESTAT + VOCAL, # For columns Lobanov.F1 and Lobanov.F2, for each REGION and VOWEL,
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(N=length(x), # compute number of items,
                                                           mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median

      basicsLobanov <- basicsLobanov[c(-7,-11)] # remove repeated "number of items" column
      colnames(basicsLobanov)[3] <- "N" # rename "number of items" columns
      basicsLobanov$VOCAL <- factor(basicsLobanov$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsLobanov <- basicsLobanov[order(basicsLobanov$ARXIPRESTAT, basicsLobanov$VOCAL),] # reorder by REGION first, then VOWEL

      # COMPUTE BASIC STATISTICS ON BARK DIFFERENCE METRIC-NORMALIZED FORMANT VALUES
      basicsBark <- do.call(data.frame,
                            aggregate(formula= cbind(Bark.Z3.Z1, Bark.Z3.Z2) ~ ARXIPRESTAT + VOCAL, # For columns Bark.Z3-Z1 and Bark.Z3.Z2, for each REGION and VOWEL,
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsBark$VOCAL <- factor(basicsBark$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsBark <- basicsBark[order(basicsBark$ARXIPRESTAT, basicsBark$VOCAL),] # reorder by REGION first, then VOWEL

      # COMPUTE BASIC STATISTICS ON MODIFIED WATT & FABRICIUS-NORMALIZED FORMANT VALUES
      basicsmW.F <- do.call(data.frame,
                            aggregate(formula= cbind(mW.F.F1, mW.F.F2) ~ ARXIPRESTAT + VOCAL, # For columns mW.F.F1 and mW.F.F2, for each REGION and VOWEL,
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsmW.F$VOCAL <- factor(basicsmW.F$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsmW.F <- basicsmW.F[order(basicsmW.F$ARXIPRESTAT, basicsmW.F$VOCAL),] # reorder by REGION first, then VOWEL

      # COMPUTE BASIC STATISTICS ON NEAREY 1-NORMALIZED FORMANT VALUES
      basicsNearey1 <- do.call(data.frame,
                               aggregate(formula= cbind(Nearey1.F1, Nearey1.F2, Nearey1.F3) ~ ARXIPRESTAT + VOCAL, # For columns Nearey1.F1, Nearey1.F2, and Nearey1.F3 for each REGION and VOWEL,
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median


      basicsNearey1$VOCAL <- factor(basicsNearey1$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsNearey1 <- basicsNearey1[order(basicsNearey1$ARXIPRESTAT, basicsNearey1$VOCAL),] # reorder by REGION first, then VOWEL

      # CREATE DATA.FRAME WITH ALL DATA.FRAMES OF SUMMARY STATISTICS WE JUST CREATED (removing )
      allsummarystats <- cbind(basicsLobanov, basicsBark[c(-1,-2)], basicsmW.F[c(-1,-2)], basicsNearey1[c(-1,-2)])
    }

    else if(LEVEL=="VocalContext"){
      # COMPUTE BASIC STATISTICS ON LOBANOV-NORMALIZED FORMANT VALUES
      basicsLobanov <- do.call(data.frame,
                               aggregate(formula= cbind(Lobanov.F1, Lobanov.F2) ~ ARXIPRESTAT + VOCAL + N.CONTEXT, # For columns Lobanov.F1 and Lobanov.F2, for each REGION and VOWEL, by CONTEXT
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(N=length(x), # compute number of items,
                                                           mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median

      basicsLobanov <- basicsLobanov[c(-8)] # remove repeated "number of items" columns
      colnames(basicsLobanov)[4] <- "N" # rename "number of items" columns
      basicsLobanov$VOCAL <- factor(basicsLobanov$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsLobanov <- basicsLobanov[order(basicsLobanov$ARXIPRESTAT, basicsLobanov$VOCAL, basicsLobanov$N.CONTEXT),] # reorder by REGION first, then VOWEL, then context

      # COMPUTE BASIC STATISTICS ON BARK DIFFERENCE METRIC-NORMALIZED FORMANT VALUES
      basicsBark <- do.call(data.frame,
                            aggregate(formula= cbind(Bark.Z3.Z1, Bark.Z3.Z2) ~ ARXIPRESTAT + VOCAL + N.CONTEXT, # For columns Bark.Z3-Z1 and Bark.Z3.Z2, for each REGION and VOWEL by CONTEXT,
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsBark$VOCAL <- factor(basicsBark$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsBark <- basicsBark[order(basicsBark$ARXIPRESTAT, basicsBark$VOCAL, basicsBark$N.CONTEXT),] # reorder by REGION first, then VOWEL, then context

      # COMPUTE BASIC STATISTICS ON MODIFIED WATT & FABRICIUS-NORMALIZED FORMANT VALUES
      basicsmW.F <- do.call(data.frame,
                            aggregate(formula= cbind(mW.F.F1, mW.F.F2) ~ ARXIPRESTAT + VOCAL + N.CONTEXT, # For columns mW.F.F1 and mW.F.F2, for each REGION and VOWEL, by CONTEXT
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsmW.F$VOCAL <- factor(basicsmW.F$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsmW.F <- basicsmW.F[order(basicsmW.F$ARXIPRESTAT, basicsmW.F$VOCAL, basicsmW.F$N.CONTEXT),] # reorder by REGION first, then VOWEL, then context

      # COMPUTE BASIC STATISTICS ON NEAREY 1-NORMALIZED FORMANT VALUES
      basicsNearey1 <- do.call(data.frame,
                               aggregate(formula= cbind(Nearey1.F1, Nearey1.F2, Nearey1.F3) ~ ARXIPRESTAT + VOCAL + N.CONTEXT, # For columns Nearey1.F1, Nearey1.F2, and Nearey1.F3 for each REGION and VOWEL, by context
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median


      basicsNearey1$VOCAL <- factor(basicsNearey1$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsNearey1 <- basicsNearey1[order(basicsNearey1$ARXIPRESTAT, basicsNearey1$VOCAL, basicsNearey1$N.CONTEXT),] # reorder by REGION first, then VOWEL, then CONTEXT

      # CREATE DATA.FRAME WITH ALL DATA.FRAMES OF SUMMARY STATISTICS WE JUST CREATED (removing )
      allsummarystats <- cbind(basicsLobanov, basicsBark[c(-1,-2,-3)], basicsmW.F[c(-1,-2,-3)], basicsNearey1[c(-1,-2,-3)])

    }
    else if(LEVEL=="VocalFonpost"){

      # COMPUTE BASIC STATISTICS ON LOBANOV-NORMALIZED FORMANT VALUES
      basicsLobanov <- do.call(data.frame,
                               aggregate(formula= cbind(Lobanov.F1, Lobanov.F2) ~ ARXIPRESTAT + VOCAL + FONEMA_POST., # For columns Lobanov.F1 and Lobanov.F2, for each REGION and VOWEL, by POSTERIOR SEGMENT
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(N=length(x), # compute number of items,
                                                           mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median

      basicsLobanov <- basicsLobanov[c(-8)] # remove repeated "number of items" columns
      colnames(basicsLobanov)[4] <- "N" # rename "number of items" columns
      basicsLobanov$VOCAL <- factor(basicsLobanov$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsLobanov <- basicsLobanov[order(basicsLobanov$ARXIPRESTAT, basicsLobanov$VOCAL, basicsLobanov$FONEMA_POST.),] # reorder by REGION first, then VOWEL, then POSTERIOR SEGMENT

      # COMPUTE BASIC STATISTICS ON BARK DIFFERENCE METRIC-NORMALIZED FORMANT VALUES
      basicsBark <- do.call(data.frame,
                            aggregate(formula= cbind(Bark.Z3.Z1, Bark.Z3.Z2) ~ ARXIPRESTAT + VOCAL + FONEMA_POST., # For columns Bark.Z3-Z1 and Bark.Z3.Z2, for each REGION and VOWEL by POSTERIOR SEGMENT
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsBark$VOCAL <- factor(basicsBark$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsBark <- basicsBark[order(basicsBark$ARXIPRESTAT, basicsBark$VOCAL, basicsBark$FONEMA_POST.),] # reorder by REGION first, then VOWEL, then POSTERIOR SEGMENT

      # COMPUTE BASIC STATISTICS ON MODIFIED WATT & FABRICIUS-NORMALIZED FORMANT VALUES
      basicsmW.F <- do.call(data.frame,
                            aggregate(formula= cbind(mW.F.F1, mW.F.F2) ~ ARXIPRESTAT + VOCAL + FONEMA_POST., # For columns mW.F.F1 and mW.F.F2, for each REGION and VOWEL, by POSTERIOR SEGMENT
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsmW.F$VOCAL <- factor(basicsmW.F$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsmW.F <- basicsmW.F[order(basicsmW.F$ARXIPRESTAT, basicsmW.F$VOCAL, basicsmW.F$FONEMA_POST.),] # reorder by REGION first, then VOWEL, then POSTERIOR SEGMENT

      # COMPUTE BASIC STATISTICS ON NEAREY 1-NORMALIZED FORMANT VALUES
      basicsNearey1 <- do.call(data.frame,
                               aggregate(formula= cbind(Nearey1.F1, Nearey1.F2, Nearey1.F3) ~ ARXIPRESTAT + VOCAL + FONEMA_POST., # For columns Nearey1.F1, Nearey1.F2, and Nearey1.F3 for each REGION and VOWEL, by POSTERIOR SEGMENT
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median


      basicsNearey1$VOCAL <- factor(basicsNearey1$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsNearey1 <- basicsNearey1[order(basicsNearey1$ARXIPRESTAT, basicsNearey1$VOCAL, basicsNearey1$FONEMA_POST.),] # reorder by REGION first, then VOWEL, then POSTERIOR SEGMENT

      # CREATE DATA.FRAME WITH ALL DATA.FRAMES OF SUMMARY STATISTICS WE JUST CREATED (removing )
      allsummarystats <- cbind(basicsLobanov, basicsBark[c(-1,-2,-3)], basicsmW.F[c(-1,-2,-3)], basicsNearey1[c(-1,-2,-3)])

    }

    else if(LEVEL=="PosicioSil"){

      # COMPUTE BASIC STATISTICS ON LOBANOV-NORMALIZED FORMANT VALUES
      basicsLobanov <- do.call(data.frame,
                               aggregate(formula= cbind(Lobanov.F1, Lobanov.F2) ~ ARXIPRESTAT + VOCAL + SÍL.posició., # For columns Lobanov.F1 and Lobanov.F2, for each REGION and VOWEL, by SYLLABLE POSITION (initial, mid, final, monosyllabic)
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(N=length(x), # compute number of items,
                                                           mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median

      basicsLobanov <- basicsLobanov[c(-8)] # remove repeated "number of items" columns
      colnames(basicsLobanov)[4] <- "N" # rename "number of items" columns
      basicsLobanov$VOCAL <- factor(basicsLobanov$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsLobanov <- basicsLobanov[order(basicsLobanov$ARXIPRESTAT, basicsLobanov$VOCAL, basicsLobanov$SÍL.posició.),] # reorder by REGION first, then VOWEL, then SYLLABLE POSITION (initial, mid, final, monosyllabic)

      # COMPUTE BASIC STATISTICS ON BARK DIFFERENCE METRIC-NORMALIZED FORMANT VALUES
      basicsBark <- do.call(data.frame,
                            aggregate(formula= cbind(Bark.Z3.Z1, Bark.Z3.Z2) ~ ARXIPRESTAT + VOCAL + SÍL.posició., # For columns Bark.Z3-Z1 and Bark.Z3.Z2, for each REGION and VOWEL by SYLLABLE POSITION (initial, mid, final, monosyllabic)
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsBark$VOCAL <- factor(basicsBark$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsBark <- basicsBark[order(basicsBark$ARXIPRESTAT, basicsBark$VOCAL, basicsBark$SÍL.posició.),] # reorder by REGION first, then VOWEL, then SYLLABLE POSITION (initial, mid, final, monosyllabic)

      # COMPUTE BASIC STATISTICS ON MODIFIED WATT & FABRICIUS-NORMALIZED FORMANT VALUES
      basicsmW.F <- do.call(data.frame,
                            aggregate(formula= cbind(mW.F.F1, mW.F.F2) ~ ARXIPRESTAT + VOCAL + SÍL.posició., # For columns mW.F.F1 and mW.F.F2, for each REGION and VOWEL, by SYLLABLE POSITION (initial, mid, final, monosyllabic)
                                      data = dftest, # on data frame "dftest",
                                      FUN=function(x) c(mean=mean(x), # compute mean,
                                                        sd=sd(x), # standard deviation,
                                                        median=median(x)))) # and median


      basicsmW.F$VOCAL <- factor(basicsmW.F$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsmW.F <- basicsmW.F[order(basicsmW.F$ARXIPRESTAT, basicsmW.F$VOCAL, basicsmW.F$SÍL.posició.),] # reorder by REGION first, then VOWEL, then SYLLABLE POSITION (initial, mid, final, monosyllabic)

      # COMPUTE BASIC STATISTICS ON NEAREY 1-NORMALIZED FORMANT VALUES
      basicsNearey1 <- do.call(data.frame,
                               aggregate(formula= cbind(Nearey1.F1, Nearey1.F2, Nearey1.F3) ~ ARXIPRESTAT + VOCAL + SÍL.posició., # For columns Nearey1.F1, Nearey1.F2, and Nearey1.F3 for each REGION and VOWEL, by SYLLABLE POSITION (initial, mid, final, monosyllabic)
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median


      basicsNearey1$VOCAL <- factor(basicsNearey1$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsNearey1 <- basicsNearey1[order(basicsNearey1$ARXIPRESTAT, basicsNearey1$VOCAL, basicsNearey1$SÍL.posició.),] # reorder by REGION first, then VOWEL, then SYLLABLE POSITION (initial, mid, final, monosyllabic)

      # CREATE DATA.FRAME WITH ALL DATA.FRAMES OF SUMMARY STATISTICS WE JUST CREATED (removing )
      allsummarystats = cbind(basicsLobanov, basicsBark[c(-1,-2,-3)], basicsmW.F[c(-1,-2,-3)], basicsNearey1[c(-1,-2,-3)])

    }
    return(allsummarystats)
  } # if (group=="Arxiprestat")


  if(GROUP=="SexesArxiprestat") {

    # COMPUTE BASIC STATISTICS ON LOBANOV-NORMALIZED FORMANT VALUES
    basicsHz <- do.call(data.frame,
                             aggregate(formula= cbind(F1, F2) ~ ARXIPRESTAT + SEXE + VOCAL, # For columns Lobanov.F1 and Lobanov.F2, for each VOWEL by SPEAKER'S SEX in an AREA,
                                       data = dftest, # on data frame "dftest",
                                       FUN=function(x) c(N=length(x), # compute number of items,
                                                         mean=round(as.vector(mean(x))), # (rounded) mean,
                                                         median=round(as.vector(median(x))), # standard deviation,
                                                         sd=sd(x)))) # and median

    basicsHz <- basicsHz[c(-8)] # remove repeated "number of items" column
    colnames(basicsHz)[4] <- "N" # rename "number of items" columns
    basicsHz$VOCAL <- factor(basicsHz$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
    basicsHz <- basicsHz[order(basicsHz$ARXIPRESTAT, basicsHz$SEXE, basicsHz$VOCAL),] # reorder by SPEAKER first, then SEX, then VOWEL

    return(basicsHz)
  } # if (group=="SexesArxiprestat")

  if(GROUP=="FUSIOOS") {

      basicsLobanov <- do.call(data.frame,
                               aggregate(formula= cbind(Lobanov.F1, Lobanov.F2) ~ FUSIOOS + VOCAL, # For columns Lobanov.F1 and Lobanov.F2, for each REGION and VOWEL,
                                         data = dftest, # on data frame "dftest",
                                         FUN=function(x) c(N=length(x), # compute number of items,
                                                           mean=mean(x), # compute mean,
                                                           sd=sd(x), # standard deviation,
                                                           median=median(x)))) # and median

      basicsLobanov <- basicsLobanov[c(-7)] # remove repeated "number of items" column
      colnames(basicsLobanov)[3] <- "N" # rename "number of items" columns
      basicsLobanov$VOCAL <- factor(basicsLobanov$VOCAL, levels = vowel.order) # reorder by "vowel.order" set above
      basicsLobanov <- basicsLobanov[order(basicsLobanov$FUSIOOS, basicsLobanov$VOCAL),] # reorder by REGION first, then VOWEL

    }

} # FUNCIÓ

