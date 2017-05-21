## CREATE DATA FRAME THAT CAN BE USED WITH THE "VOWELS" PACKAGE: (see http://lingtools.uoregon.edu/norm/norm1.php)
## Required data frame of the format:
## speaker_id, vowel_id, context, F1, F2, F3, F1_glide, F2_glide, F3_glide.
## The context column and glide columns must exist but can be empty. Columns can have any name.

normformat <- function(data, values=c("Hz","Bark", "Lobanov", "mWF", "Nearey1"), interval=NULL, test=NULL) {

  ## SET COLUMNS TO SUBSET: RAW HZ VALUES OR VALUES FROM A SPECIFIC NORMALIZATION METHOD
  if(values=="Hz"){
    values <- c("INFORMANT","VOCAL","N.MOT", "F1", "F2", "F3")
  }
  else if(values=="Bark"){
    values <- c("INFORMANT","VOCAL","N.MOT", "Bark.Z3.Z1", "Bark.Z3.Z2", "Bark.Z2.Z1")
  }
  else if(values=="Lobanov"){
    values <- c("INFORMANT","VOCAL","N.MOT", "Lobanov.F1", "Lobanov.F2", "F3")
  }
  else if(values=="mWF"){
    values <- c("INFORMANT","VOCAL","N.MOT", "mW.F.F1", "mW.F.F2", "F3")
  }
  else if(values=="Nearey1"){
    values <- c("INFORMANT","VOCAL","N.MOT", "Nearey1.F1", "Nearey1.F2", "Nearey1.F3")
  }

  ## DEFINE WHAT TO SUBSET:
  if(!is.null(interval) & !is.null(test)) {               ## 1) IF INTERVAL AND TEST ARE SPECIFIED
    form.subdata <- data[data$INTERVAL==interval & data$TEST==test,values] ## EXTRACT NECESSARY ROWS AND COLUMNS

    ## ADD REQUIRED EXTRA COLUMNS
    form.subdata["G1"] <- NA
    form.subdata["G2"] <- NA
    form.subdata["G3"] <- NA
    return(form.subdata)
  }
  else if(!is.null(test)) {     ## 2) IF A TEST BUT NO INTERVAL IS SPECIFIED
    form.subdata <- data[data$TEST==test,values] ## EXTRACT NECESSARY ROWS AND COLUMNS

    ## ADD REQUIRED EXTRA COLUMNS
    form.subdata["G1"] <- NA
    form.subdata["G2"] <- NA
    form.subdata["G3"] <- NA
    return(form.subdata)
  }

  else if(!is.null(interval)) {     ## 3) IF AN INTERVAL BUT NO TEST IS SPECIFIED
    form.subdata <- data[data$INTERVAL==interval,values] ## EXTRACT NECESSARY ROWS AND COLUMNS

    ## ADD REQUIRED EXTRA COLUMNS
    form.subdata["G1"] <- NA
    form.subdata["G2"] <- NA
    form.subdata["G3"] <- NA
    return(form.subdata)
  }

  else {      ## 4) IF NO INTERVAL AND NO TESTS ARE SPECIFIED

  form.subdata <- data[,values] ## EXTRACT NECESSARY COLUMNS

  ## ADD REQUIRED EXTRA COLUMNS
  form.subdata["G1"] <- NA
  form.subdata["G2"] <- NA
  form.subdata["G3"] <- NA
  return(form.subdata)
  }

}

