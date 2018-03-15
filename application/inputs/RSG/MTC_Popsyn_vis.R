#################################################################################################################
# Script for validating PopSyn III convergance
# The following files are produced by the program:
#   stats.csv file has the following statistics to examine convergance
#     controlName - Name of the control
#     geography - Geography at which the control is specified
#     Observed - Regional total specified
#     Predicted - Regional total synthesized
#     Difference - Predicted - Observed
#     pcDifference - Perecntage difference at a regional level
#     N - Number of geographies (MAZ/TAZ/META) with non-zero control
#     RMSE - Percentage root mean square error for the control at the specified geography
#     SDEV - Standard deviation of precentage difference

# Inputs and settings that need to be changed:
#  1. columnMpa needs to be updated
#  2. set the year for the run
#  3. set GQ_RUN == TRUE for a GQ run validation
# 
# PopSyn Convergance - Plot showing mean %age difference across geographies +/- SDEV
#################################################################################################################

## Read Command Line Arguments
args            <- commandArgs(trailingOnly = TRUE)
Parameters_File <- args[1]

#USER INPUTS
##################
## Read parameters from Parameters_File
parameters <- read.csv(Parameters_File, header = TRUE)

WORKING_DIR          <- trimws(paste(parameters$Value[parameters$Key=="WORKING_DIR"]))	
MYSQL_SERVER         <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_SERVER"]))
MYSQL_DATABASE       <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_DATABASE"]))
MYSQL_USER_NAME      <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_USER_NAME"]))
MYSQL_PASSWORD_FILE  <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_PASSWORD_FILE"]))
YEAR                 <- trimws(paste(parameters$Value[parameters$Key=="PopSyn_YEAR"]))
Run_HH_PopSyn        <- trimws(paste(parameters$Value[parameters$Key=="Run_HH_PopSyn"]))
Run_GQ_PopSyn        <- trimws(paste(parameters$Value[parameters$Key=="Run_GQ_PopSyn"]))

setwd(paste(WORKING_DIR, "Validation", sep = "/"))


#Load libraries
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(tidyr))
suppressMessages(library(scales))
suppressMessages(library(hydroGOF))
suppressMessages(library(RMySQL))

# get password
mysql_passes <- read.csv(MYSQL_PASSWORD_FILE, header = TRUE)

mysql_passes <- mysql_passes %>%
  filter(user == MYSQL_USER_NAME) %>%
  mutate(pwd = paste(pwd))



############## HH PopSyn Validation ###################
if(Run_HH_PopSyn=="YES"){
  GQ_RUN <- FALSE #different views read for GQ run
  
  region <- "MTC"                  #Region Name  			   
  metaGeography <- ifelse(GQ_RUN, "region", "mtc_county_id")               	   #Geography designated as meta geography ID
  plotGeographies <- c("maz","taz",metaGeography)	                             #Specify geographies for which to plot the difference frequency
  
  #Function to process each control
  procControl <- function(geography, controlName, controlID, summaryID){
    
    #Set the table name to query
    if(geography == "maz"){
      CtableName = ifelse(GQ_RUN,paste('control_totals_maz_gq',  YEAR, sep = '_'), paste('control_totals_maz',  YEAR, sep = '_'))
      StableName = ifelse(GQ_RUN, "gqmazSummaryView", "mazSummaryView")
    }else if(geography == "taz"){
      CtableName = paste('control_totals_taz',  YEAR, sep = '_')
      StableName = ifelse(GQ_RUN, "gqtazSummaryView", "tazSummaryView")
    }else if(geography == metaGeography){
      CtableName = ifelse(GQ_RUN,paste('control_totals_meta_gq',  YEAR, sep = '_'), paste('control_totals_meta',  YEAR, sep = '_'))
      StableName = ifelse(GQ_RUN, "gqmetaSummaryView", "metaSummaryView")
    }else{
      stop("Incorrect geography specified")
    }
    
    #Constructing MySQL Query
    controlQuery <- gsub("ARG1", geography, "SELECT ARG1 AS GEOGRAPHY, ARG2 AS CONTROL FROM ARG3")
    controlQuery <- gsub("ARG2", controlID, controlQuery)
    controlQuery <- gsub("ARG3", CtableName, controlQuery)
    
    synQuery <- gsub("ARG1", geography, "SELECT ARG1 AS GEOGRAPHY, ARG2 AS SYNTHESIZED FROM ARG3")
    synQuery <- gsub("ARG2", summaryID, synQuery)
    synQuery <- gsub("ARG3", StableName, synQuery)
    
    #Fetching data
    #controls <- sqlQuery(channel,controlQuery)
    #synthesized <- sqlQuery(channel,synQuery)
    
    controls <- dbGetQuery(channel,controlQuery)
    synthesized <- dbGetQuery(channel,synQuery)
    
    #Fetch and process each control for getting convergance statistics
    compareData <- left_join(controls, synthesized, by="GEOGRAPHY") %>%
      mutate(CONTROL = as.numeric(CONTROL)) %>%
      mutate(SYNTHESIZED = ifelse(is.na(SYNTHESIZED), 0, SYNTHESIZED)) %>%
      mutate(DIFFERENCE = SYNTHESIZED - CONTROL) %>%
      mutate(pcDIFFERENCE = ifelse(CONTROL > 0,(DIFFERENCE/CONTROL)*100,NA))
    
    if (summaryID=="POP"){
      write.csv(compareData, "compareData.csv", row.names = TRUE)
    }
    
    #Calculate statistics
    Observed <- sum(compareData$CONTROL)
    Predicted <- sum(compareData$SYNTHESIZED)
    Difference <- Predicted - Observed
    pcDifference <- (Difference/Observed)*100
    N <- sum(compareData$CONTROL > 0)
    PRMSE <- (((sum((compareData$CONTROL - compareData$SYNTHESIZED)^2)/(sum(compareData$CONTROL > 0) - 1))^0.5)/sum(compareData$CONTROL))*sum(compareData$CONTROL > 0)*100
    meanPCDiff <- mean(compareData$pcDIFFERENCE, na.rm=TRUE)
    SDEV <- sd(compareData$pcDIFFERENCE, na.rm=TRUE)
    stats <- data.frame(controlName, geography, Observed, Predicted, Difference, pcDifference, N, PRMSE, meanPCDiff, SDEV)
    
    #Preparing data for difference frequency plot
    freqPlotData <- compareData %>%
      filter(CONTROL > 0) %>%
      group_by(DIFFERENCE) %>%
      summarise(FREQUENCY = n())
    
    if(geography %in% plotGeographies){
      #computing plotting parameters
      xaxisLimit <- max(abs(freqPlotData$DIFFERENCE)) + 10
      plotTitle <- paste("Frequency Plot: Syn - Control totals for", controlName, sep = " ")
      
      #Frequency Plot
      p1 <- ggplot(freqPlotData, aes(x=DIFFERENCE,y=FREQUENCY))+
        geom_point(colour="coral") +
        coord_cartesian(xlim = c(-xaxisLimit, xaxisLimit)) +
        geom_vline(xintercept=c(0), colour = "steelblue")+
        labs(title = plotTitle)
      ggsave(paste(plot_dir_name, "/",controlID,"_", YEAR, ".png",sep=""), width=9,height=6)
    }
    
    cat("\n Processed Control: ", controlName) 
    
    return(stats)
  }
  
  
  myRMSE <- function(FINALEXPANSIONS, AVERAGEEXPANSION, N){
    EXPECTED <- rep(AVERAGEEXPANSION,N)
    ACTUAL <- FINALEXPANSIONS
    return(rmse(ACTUAL, EXPECTED, na.rm=TRUE))
  }
  
  mapFile <- ifelse(GQ_RUN, "columnMapMTCGQ.csv", paste("columnMapMTC_", YEAR,".csv", sep = ""))
  columnMap <- read.csv(mapFile)  	   #Read in column equivalency between control tables and summary tables
  
  ## CREATE VIEWS
  viewScript <- ifelse(GQ_RUN, paste(WORKING_DIR, "/scripts/createSummaryViewsGQ_", YEAR, ".R", sep = ""), 
                       paste(WORKING_DIR, "/scripts/createSummaryViews_", YEAR, ".R", sep = ""))
  source(viewScript)
  
  #Create plot directory
  plot_dir_name <- ifelse(GQ_RUN, 'plots_gq', 'plots')
  dir.create(plot_dir_name, showWarnings = FALSE)
  
  # MySQL connection	
  channel <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)	
  
  #Computing convergance statistics and write out results
  stats <- apply(columnMap, 1, function(x) procControl(x["GEOGRAPHY"], x["NAME"], x["CONTROL"],x["SUMMARY"]))
  #close(channel)
  stats <- do.call(rbind,stats)
  stats_outfile <- ifelse(GQ_RUN, paste("stats_gq", "_", YEAR, ".csv", sep = ""), paste("stats", "_", YEAR, ".csv", sep = ""))
  write.csv(stats, stats_outfile, row.names = FALSE)
  
  #Convergance plot
  p2 <- ggplot(stats, aes(x = controlName, y=meanPCDiff)) +
    geom_point(shape = 15, colour = "steelblue", size = 2)+
    geom_errorbar(data = stats, aes(ymin=-SDEV,ymax=SDEV), width=0.2, colour = "steelblue") +
    scale_x_discrete(limits=rev(levels(stats$controlName))) + 
    geom_hline(yintercept=c(0)) +
    labs(x = NULL, y="Percentage Difference [+/- SDEV]", title = gsub("Region",region,"Region PopSynIII Controls Validation")) +
    coord_flip(ylim = c(-100, 100)) +
    theme_bw() +
    theme(plot.title=element_text(size=12, lineheight=.9, face="bold", vjust=1))
  p2_filename <- ifelse(GQ_RUN, paste("PopSyn Convergance-sdev_GQ", "_", YEAR, ".jpeg"), paste("PopSyn Convergance-sdev", "_", YEAR, ".jpeg"))
  ggsave(file=p2_filename, width=8,height=10)
  
  #Convergance plot
  p3 <- ggplot(stats, aes(x = controlName, y=meanPCDiff)) +
    geom_point(shape = 15, colour = "steelblue", size = 2)+
    geom_errorbar(data = stats, aes(ymin=-PRMSE,ymax=PRMSE), width=0.2, colour = "steelblue") +
    scale_x_discrete(limits=rev(levels(stats$controlName))) + 
    geom_hline(yintercept=c(0)) +
    labs(x = NULL, y="Percentage Difference [+/- PRMSE]", title = gsub("Region",region,"Region PopSynIII Controls Validation")) +
    coord_flip(ylim = c(-100, 100)) +
    theme_bw() +
    theme(plot.title=element_text(size=12, lineheight=.9, face="bold", vjust=1))
  
  p3_filename <- ifelse(GQ_RUN, paste("PopSyn Convergance-PRMSE_GQ", "_", YEAR, ".jpeg"), paste("PopSyn Convergance-PRMSE", "_", YEAR, ".jpeg")) 
  ggsave(file=p3_filename, width=8,height=10)
  
  #Uniformity Analysis
  uniformQuery <- ifelse(GQ_RUN, "SELECT * FROM GQuniformity WHERE WGTP>0", "SELECT * FROM uniformity WHERE WGTP>0")
  uniformity <- dbGetQuery(channel,uniformQuery)
  dbDisconnect(channel)
  uniformity <- uniformity %>%
    mutate(FINALWEIGHT = ifelse(is.na(FINALWEIGHT), 0, FINALWEIGHT)) %>%
    mutate(EXPANSIONFACTOR = FINALWEIGHT/WGTP) %>%
    mutate(EFBIN = cut(EXPANSIONFACTOR,seq(0,max(EXPANSIONFACTOR)+0.5,0.5),right=FALSE, include.lowest=FALSE))
  
  uAnalysisPUMA <- group_by(uniformity, PUMA, EFBIN)
  
  efPlotData <- summarise(uAnalysisPUMA, PC = n()) %>%
    mutate(PC=PC/sum(PC))
  
  ggplot(efPlotData, aes(x=EFBIN, y=PC))  + 
    geom_bar(colour="black", fill="#DD8888", width=.7, stat="identity") + 
    guides(fill=FALSE) +
    xlab("RANGE OF EXPANSION FACTOR") + ylab("PERCENTAGE") +
    ggtitle("EXPANSION FACTOR DISTRIBUTION BY PUMA") + 
    facet_wrap(~PUMA, ncol=6) + 
    theme_bw()+
    theme(axis.title.x = element_text(face="bold"),
          axis.title.y = element_text(face="bold"),
          axis.text.x  = element_text(angle=90, size=5),
          axis.text.y  = element_text(size=5))  +
    scale_y_continuous(labels = percent_format())
  ef_filename <- ifelse(GQ_RUN, paste(plot_dir_name, "/EF-Distribution_GQ", "_", YEAR, ".png"), 
                        paste(plot_dir_name,"/EF-Distribution", "_", YEAR, ".png")) 
  ggsave("plots/EF-Distribution.png", width=15,height=10)
  
  uAnalysisPUMA <- group_by(uniformity, PUMA)
  
  uAnalysisPUMA <- summarize(uAnalysisPUMA
                             ,W = sum(WGTP)
                             ,Z = sum(FINALWEIGHT)
                             ,N = n()
                             ,EXP = Z/W
                             ,EXP_MIN = min(EXPANSIONFACTOR)
                             ,EXP_MAX = max(EXPANSIONFACTOR)
                             ,RMSE = myRMSE(EXPANSIONFACTOR, EXP, N))
  univ_filename <- ifelse(GQ_RUN, paste("uniformity", "_", YEAR, ".csv"), paste("uniformity_GQ", "_", YEAR, ".csv")) 
  write.csv(uAnalysisPUMA, univ_filename, row.names=FALSE)
  
  #fin
}

############## GQ PopSyn Validation ###################
if(Run_GQ_PopSyn=="YES"){
  GQ_RUN <- TRUE #different views read for GQ run
   
  region <- "MTC"                  #Region Name  			   
  metaGeography <- ifelse(GQ_RUN, "region", "mtc_county_id")               	   #Geography designated as meta geography ID
  plotGeographies <- c("maz","taz",metaGeography)	                             #Specify geographies for which to plot the difference frequency
  
  #Function to process each control
  procControl <- function(geography, controlName, controlID, summaryID){
    
    #Set the table name to query
    if(geography == "maz"){
      CtableName = ifelse(GQ_RUN,paste('control_totals_maz_gq',  YEAR, sep = '_'), paste('control_totals_maz',  YEAR, sep = '_'))
      StableName = ifelse(GQ_RUN, "gqmazSummaryView", "mazSummaryView")
    }else if(geography == "taz"){
      CtableName = paste('control_totals_taz',  YEAR, sep = '_')
      StableName = ifelse(GQ_RUN, "gqtazSummaryView", "tazSummaryView")
    }else if(geography == metaGeography){
      CtableName = ifelse(GQ_RUN,paste('control_totals_meta_gq',  YEAR, sep = '_'), paste('control_totals_meta',  YEAR, sep = '_'))
      StableName = ifelse(GQ_RUN, "gqmetaSummaryView", "metaSummaryView")
    }else{
      stop("Incorrect geography specified")
    }
    
    #Constructing MySQL Query
    controlQuery <- gsub("ARG1", geography, "SELECT ARG1 AS GEOGRAPHY, ARG2 AS CONTROL FROM ARG3")
    controlQuery <- gsub("ARG2", controlID, controlQuery)
    controlQuery <- gsub("ARG3", CtableName, controlQuery)
    
    synQuery <- gsub("ARG1", geography, "SELECT ARG1 AS GEOGRAPHY, ARG2 AS SYNTHESIZED FROM ARG3")
    synQuery <- gsub("ARG2", summaryID, synQuery)
    synQuery <- gsub("ARG3", StableName, synQuery)
    
    #Fetching data
    #controls <- sqlQuery(channel,controlQuery)
    #synthesized <- sqlQuery(channel,synQuery)
    
    controls <- dbGetQuery(channel,controlQuery)
    synthesized <- dbGetQuery(channel,synQuery)
    
    #Fetch and process each control for getting convergance statistics
    compareData <- left_join(controls, synthesized, by="GEOGRAPHY") %>%
      mutate(CONTROL = as.numeric(CONTROL)) %>%
      mutate(SYNTHESIZED = ifelse(is.na(SYNTHESIZED), 0, SYNTHESIZED)) %>%
      mutate(DIFFERENCE = SYNTHESIZED - CONTROL) %>%
      mutate(pcDIFFERENCE = ifelse(CONTROL > 0,(DIFFERENCE/CONTROL)*100,NA))
    
    if (summaryID=="POP"){
      write.csv(compareData, "compareData.csv", row.names = TRUE)
    }
    
    #Calculate statistics
    Observed <- sum(compareData$CONTROL)
    Predicted <- sum(compareData$SYNTHESIZED)
    Difference <- Predicted - Observed
    pcDifference <- (Difference/Observed)*100
    N <- sum(compareData$CONTROL > 0)
    PRMSE <- (((sum((compareData$CONTROL - compareData$SYNTHESIZED)^2)/(sum(compareData$CONTROL > 0) - 1))^0.5)/sum(compareData$CONTROL))*sum(compareData$CONTROL > 0)*100
    meanPCDiff <- mean(compareData$pcDIFFERENCE, na.rm=TRUE)
    SDEV <- sd(compareData$pcDIFFERENCE, na.rm=TRUE)
    stats <- data.frame(controlName, geography, Observed, Predicted, Difference, pcDifference, N, PRMSE, meanPCDiff, SDEV)
    
    #Preparing data for difference frequency plot
    freqPlotData <- compareData %>%
      filter(CONTROL > 0) %>%
      group_by(DIFFERENCE) %>%
      summarise(FREQUENCY = n())
    
    if(geography %in% plotGeographies){
      #computing plotting parameters
      xaxisLimit <- max(abs(freqPlotData$DIFFERENCE)) + 10
      plotTitle <- paste("Frequency Plot: Syn - Control totals for", controlName, sep = " ")
      
      #Frequency Plot
      p1 <- ggplot(freqPlotData, aes(x=DIFFERENCE,y=FREQUENCY))+
        geom_point(colour="coral") +
        coord_cartesian(xlim = c(-xaxisLimit, xaxisLimit)) +
        geom_vline(xintercept=c(0), colour = "steelblue")+
        labs(title = plotTitle)
      ggsave(paste(plot_dir_name, "/",controlID,"_", YEAR, ".png",sep=""), width=9,height=6)
    }
    
    cat("\n Processed Control: ", controlName) 
    
    return(stats)
  }
  
  
  myRMSE <- function(FINALEXPANSIONS, AVERAGEEXPANSION, N){
    EXPECTED <- rep(AVERAGEEXPANSION,N)
    ACTUAL <- FINALEXPANSIONS
    return(rmse(ACTUAL, EXPECTED, na.rm=TRUE))
  }
  
  mapFile <- ifelse(GQ_RUN, "columnMapMTCGQ.csv", paste("columnMapMTC_", YEAR,".csv", sep = ""))
  columnMap <- read.csv(mapFile)  	   #Read in column equivalency between control tables and summary tables
  
  ## CREATE VIEWS
  viewScript <- ifelse(GQ_RUN, paste(WORKING_DIR, "/scripts/createSummaryViewsGQ_", YEAR, ".R", sep = ""), 
                       paste(WORKING_DIR, "/scripts/createSummaryViews_", YEAR, ".R", sep = ""))
  source(viewScript)
  
  #Create plot directory
  plot_dir_name <- ifelse(GQ_RUN, 'plots_gq', 'plots')
  dir.create(plot_dir_name, showWarnings = FALSE)
  
  # MySQL connection	
  channel <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)	
  
  #Computing convergance statistics and write out results
  stats <- apply(columnMap, 1, function(x) procControl(x["GEOGRAPHY"], x["NAME"], x["CONTROL"],x["SUMMARY"]))
  #close(channel)
  stats <- do.call(rbind,stats)
  stats_outfile <- ifelse(GQ_RUN, paste("stats_gq", "_", YEAR, ".csv", sep = ""), paste("stats", "_", YEAR, ".csv", sep = ""))
  write.csv(stats, stats_outfile, row.names = FALSE)
  
  #Convergance plot
  p2 <- ggplot(stats, aes(x = controlName, y=meanPCDiff)) +
    geom_point(shape = 15, colour = "steelblue", size = 2)+
    geom_errorbar(data = stats, aes(ymin=-SDEV,ymax=SDEV), width=0.2, colour = "steelblue") +
    scale_x_discrete(limits=rev(levels(stats$controlName))) + 
    geom_hline(yintercept=c(0)) +
    labs(x = NULL, y="Percentage Difference [+/- SDEV]", title = gsub("Region",region,"Region PopSynIII Controls Validation")) +
    coord_flip(ylim = c(-100, 100)) +
    theme_bw() +
    theme(plot.title=element_text(size=12, lineheight=.9, face="bold", vjust=1))
  p2_filename <- ifelse(GQ_RUN, paste("PopSyn Convergance-sdev_GQ", "_", YEAR, ".jpeg"), paste("PopSyn Convergance-sdev", "_", YEAR, ".jpeg"))
  ggsave(file=p2_filename, width=8,height=10)
  
  #Convergance plot
  p3 <- ggplot(stats, aes(x = controlName, y=meanPCDiff)) +
    geom_point(shape = 15, colour = "steelblue", size = 2)+
    geom_errorbar(data = stats, aes(ymin=-PRMSE,ymax=PRMSE), width=0.2, colour = "steelblue") +
    scale_x_discrete(limits=rev(levels(stats$controlName))) + 
    geom_hline(yintercept=c(0)) +
    labs(x = NULL, y="Percentage Difference [+/- PRMSE]", title = gsub("Region",region,"Region PopSynIII Controls Validation")) +
    coord_flip(ylim = c(-100, 100)) +
    theme_bw() +
    theme(plot.title=element_text(size=12, lineheight=.9, face="bold", vjust=1))
  
  p3_filename <- ifelse(GQ_RUN, paste("PopSyn Convergance-PRMSE_GQ", "_", YEAR, ".jpeg"), paste("PopSyn Convergance-PRMSE", "_", YEAR, ".jpeg")) 
  ggsave(file=p3_filename, width=8,height=10)
  
  #Uniformity Analysis
  uniformQuery <- ifelse(GQ_RUN, "SELECT * FROM GQuniformity WHERE WGTP>0", "SELECT * FROM uniformity WHERE WGTP>0")
  uniformity <- dbGetQuery(channel,uniformQuery)
  dbDisconnect(channel)
  uniformity <- uniformity %>%
    mutate(FINALWEIGHT = ifelse(is.na(FINALWEIGHT), 0, FINALWEIGHT)) %>%
    mutate(EXPANSIONFACTOR = FINALWEIGHT/WGTP) %>%
    mutate(EFBIN = cut(EXPANSIONFACTOR,seq(0,max(EXPANSIONFACTOR)+0.5,0.5),right=FALSE, include.lowest=FALSE))
  
  uAnalysisPUMA <- group_by(uniformity, PUMA, EFBIN)
  
  efPlotData <- summarise(uAnalysisPUMA, PC = n()) %>%
    mutate(PC=PC/sum(PC))
  
  ggplot(efPlotData, aes(x=EFBIN, y=PC))  + 
    geom_bar(colour="black", fill="#DD8888", width=.7, stat="identity") + 
    guides(fill=FALSE) +
    xlab("RANGE OF EXPANSION FACTOR") + ylab("PERCENTAGE") +
    ggtitle("EXPANSION FACTOR DISTRIBUTION BY PUMA") + 
    facet_wrap(~PUMA, ncol=6) + 
    theme_bw()+
    theme(axis.title.x = element_text(face="bold"),
          axis.title.y = element_text(face="bold"),
          axis.text.x  = element_text(angle=90, size=5),
          axis.text.y  = element_text(size=5))  +
    scale_y_continuous(labels = percent_format())
  ef_filename <- ifelse(GQ_RUN, paste(plot_dir_name, "/EF-Distribution_GQ", "_", YEAR, ".png"), 
                        paste(plot_dir_name,"/EF-Distribution", "_", YEAR, ".png")) 
  ggsave("plots/EF-Distribution.png", width=15,height=10)
  
  uAnalysisPUMA <- group_by(uniformity, PUMA)
  
  uAnalysisPUMA <- summarize(uAnalysisPUMA
                             ,W = sum(WGTP)
                             ,Z = sum(FINALWEIGHT)
                             ,N = n()
                             ,EXP = Z/W
                             ,EXP_MIN = min(EXPANSIONFACTOR)
                             ,EXP_MAX = max(EXPANSIONFACTOR)
                             ,RMSE = myRMSE(EXPANSIONFACTOR, EXP, N))
  univ_filename <- ifelse(GQ_RUN, paste("uniformity", "_", YEAR, ".csv"), paste("uniformity_GQ", "_", YEAR, ".csv")) 
  write.csv(uAnalysisPUMA, univ_filename, row.names=FALSE)
  
  #fin
}


