#! C:/Program Files/R/R-3.3.1/bin RScript
# Script to run R scripts for preparing database for PopSyn3 run

message("=== Running R_Shell_Script.R")

####################################################################################################################
################################          Read arguments and initial setup        ##################################
####################################################################################################################

## Read Command Line Arguments
args            <- commandArgs(trailingOnly = TRUE)
Parameters_File <- args[1]

## Install all required R packages to the user specified directory [Make sure the R library directory has write permissions for the user]
if (!"dplyr" %in% installed.packages()) install.packages("dplyr", repos='http://cran.us.r-project.org')
if (!"stringr" %in% installed.packages()) install.packages("stringr", repos='http://cran.us.r-project.org')
if (!"RMySQL" %in% installed.packages()) install.packages("RMySQL", repos='http://cran.us.r-project.org')
if (!"reshape2" %in% installed.packages()) install.packages("reshape2", repos='http://cran.us.r-project.org')
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2", repos='http://cran.us.r-project.org')
if (!"tidyr" %in% installed.packages()) install.packages("tidyr", repos='http://cran.us.r-project.org')
if (!"scales" %in% installed.packages()) install.packages("scales", repos='http://cran.us.r-project.org')
if (!"hydroGOF" %in% installed.packages()) install.packages("hydroGOF", repos='http://cran.us.r-project.org')

####################################################################################################################
########################################          Set Parameters        ############################################
####################################################################################################################

## Read parameters from Parameters_File
parameters <- read.csv(Parameters_File, header = TRUE)

WORKING_DIR          <- trimws(paste(parameters$Value[parameters$Key=="WORKING_DIR"]))
CENSUS_DATA_DIR      <- trimws(paste(parameters$Value[parameters$Key=="CENSUS_DATA_DIR"]))
INPUT_CONTROLS_DIR   <- trimws(paste(parameters$Value[parameters$Key=="INPUT_CONTROLS_DIR"]))
GEOXWALK_DIR         <- trimws(paste(parameters$Value[parameters$Key=="GEOXWALK_DIR"]))
INTERMEDIATE_DIR     <- trimws(paste(parameters$Value[parameters$Key=="INTERMEDIATE_DIR"]))
MYSQL_SERVER         <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_SERVER"]))
MYSQL_DATABASE       <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_DATABASE"]))
MYSQL_USER_NAME      <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_USER_NAME"]))
MYSQL_PASSWORD_FILE  <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_PASSWORD_FILE"]))
CENSUS_API_KEY_FILE  <- trimws(paste(parameters$Value[parameters$Key=="CENSUS_API_KEY_FILE"]))
YEAR                 <- trimws(paste(parameters$Value[parameters$Key=="PopSyn_YEAR"]))
Run_HH_PopSyn        <- trimws(paste(parameters$Value[parameters$Key=="Run_HH_PopSyn"]))
Run_GQ_PopSyn        <- trimws(paste(parameters$Value[parameters$Key=="Run_GQ_PopSyn"]))
Run_Step_1           <- trimws(paste(parameters$Value[parameters$Key=="Run_Step_1"]))
Run_Step_2           <- trimws(paste(parameters$Value[parameters$Key=="Run_Step_2"]))
Run_Step_3           <- trimws(paste(parameters$Value[parameters$Key=="Run_Step_3"]))
downloadCensus       <- trimws(paste(parameters$Value[parameters$Key=="downloadCensus"]))

setwd(paste(WORKING_DIR, "scripts", sep="/"))

## Set other parameter values

PUMS_0711_HH_FILE    <- file.path(CENSUS_DATA_DIR, "PUMS", "ss11hca.csv")
PUMS_0711_PER_FILE   <- file.path(CENSUS_DATA_DIR, "PUMS", "ss11pca.csv")
PUMS_00_HH_FILE      <- file.path(CENSUS_DATA_DIR, "PUMS", "hbayarea5_2000.csv")
PUMS_00_PER_FILE     <- file.path(CENSUS_DATA_DIR, "PUMS", "pbayarea5_2000.csv")

####################################################################################################################
####################################        Run Data Processing Scripts         ####################################
####################################################################################################################

## Kill open MySQL connections
#killDbConnections()

## Step 01 PUMS to Database
if (Run_Step_1=="YES"){
	source("Step 01 PUMS to Database.R")
}

## Step 02 Build Controls
if (Run_Step_2=="YES"){
  if(YEAR=="year_2000"){
    source("buildControls2000.R")
  }
  if(YEAR=="year_2005"){
    source("buildControls2005.R")
  }
  if(YEAR=="year_2010"){
    source("buildControls2010.R")
  }
  if(YEAR=="year_2015"){
    source("buildControls2015.R")
  }
}

## STEP 3 Controls to Database
if (Run_Step_3=="YES"){
	source("Step 03 Controls to Database.R", local = F)
}

