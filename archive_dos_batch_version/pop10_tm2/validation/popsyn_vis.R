#'  Script for validating PopSyn III convergence
#'  This script is used to validate convergence characteristics of population synthesis procedure PopSynIII
#'  
#'  The useR has to specify the following parameters
#'  @param region Name of the region - for pretty plotting
#'  @param serverName Name of the SQL server where the population is stored
#'  @param dbName Name of the database
#'  @param metaGeography Geography designated as meta geography ID
#'  @param columnMap Column equivalence between control tables and summary tables
#'  @param plotGeographies Specify geographies for which to plot the difference frequency
#'  @param scenario Specify scenario being validated
#' 
#'  The following files are produced by the program:
#'    stats.csv file has the following statistics to examine convergence
#'      controlName - Name of the control
#'      geography - Geography at which the control is specified
#'      Observed - Regional total specified
#'      Predicted - Regional total synthesized
#'      Difference - Predicted - Observed
#'      pcDifference - Percentage difference at a regional level
#'      N - Number of geographies (MAZ/TAZ/META) with non-zero control
#'      RMSE - Percentage root mean square error for the control at the specified geography
#'      SDEV - Standard deviation of percentage difference
#'  
#'    uniformity.csv file has the following statistics to examine convergence
#'      PUMA - PUMA ID
#'      W - Observed number of households
#'      Z - Expected number of households (from control)
#'      N - Number of non-zero geographies
#'      EXP - Average expansion (Z/W)
#'      EXP_MIN - Minimum expansion factor within PUMA
#'      EXP_MAX - Maximum expansion factor within PUMA
#'      RMSE - Root mean square error
#'      
#'  The following plots are produced by the program in the plots folder:
#'     PopSyn Convergence - Plot showing mean %age difference across geographies +/- SDEV
#'     Difference frequency plot for each control
#'     Expansion factor distribution plot faceted by PUMA
#'        
#'  @date: 2014-11-11
#'  @author: sn, narayanamoorthys AT pbworld DOT com


#UseR inputs
region <- "MTC"                                         #Region Name
serverName <- "W-AMPDX-D-SAG10"                         #SQL Server
dbName <- "MTCPopSynIII"                                #PopSyn Database
metaGeography <- "MTCCountyID"                          #Geography designated as meta geography ID
columnMap <- read.csv("columnMapMTC.csv")               #Read in column equivalence between control tables and summary tables
plotGeographies <- c("MAZ","TAZ",metaGeography)         #Specify geographies for which to plot the difference frequency
scenario <- "TESTRUN"                                   #Specify scenario being validated

#Load libraries
library(RODBC)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(hydroGOF)

#Function to process each control
procControl <- function(geography, controlName, controlID, summaryID){
  
  #Set the table name to query
  if(geography == "MAZ"){
    CtableName = paste(scenario,"control_totals_maz",sep=".")
    StableName = "mazSummaryView"
  }else if(geography == "TAZ"){
    CtableName = paste(scenario,"control_totals_taz",sep=".")
    StableName = "tazSummaryView"
  }else if(geography == metaGeography){
    CtableName = paste(scenario,"control_totals_meta",sep=".")
    StableName = "metaSummaryView"
  }else{
    stop("Incorrect geography specified")
  }
  
  #Constructing SQL Query
  controlQuery <- gsub("ARG1", geography, "SELECT ARG1 AS GEOGRAPHY, ARG2 AS CONTROL FROM ARG3")
  controlQuery <- gsub("ARG2", controlID, controlQuery)
  controlQuery <- gsub("ARG3", CtableName, controlQuery)
  
  synQuery <- gsub("ARG1", geography, "SELECT ARG1 AS GEOGRAPHY, ARG2 AS SYNTHESIZED FROM ARG3")
  synQuery <- gsub("ARG2", summaryID, synQuery)
  synQuery <- gsub("ARG3", StableName, synQuery)
  
  #Fetching data
  controls <- sqlQuery(channel,controlQuery)
  synthesized <- sqlQuery(channel,synQuery)
  
  #Fetch and process each control for getting convergence statistics
  compareData <- left_join(controls, synthesized, by="GEOGRAPHY") %>%
    mutate(CONTROL = as.numeric(CONTROL)) %>%
    mutate(SYNTHESIZED = ifelse(is.na(SYNTHESIZED), 0, SYNTHESIZED)) %>%
    mutate(DIFFERENCE = SYNTHESIZED - CONTROL) %>%
    mutate(pcDIFFERENCE = ifelse(CONTROL > 0,(DIFFERENCE/CONTROL)*100,NA))
  
  #Calculate statistics
  Observed <- sum(compareData$CONTROL)
  Predicted <- sum(compareData$SYNTHESIZED)
  Difference <- Predicted - Observed
  pcDifference <- (Difference/Observed)*100
  N <- sum(compareData$CONTROL > 0)
  
  if (N < 2){
    RMSE <- NA
  } else {
    RMSE <- (((sum((compareData$CONTROL - compareData$SYNTHESIZED)^2)/(sum(compareData$CONTROL > 0) - 1))^0.5)/sum(compareData$CONTROL))*sum(compareData$CONTROL > 0)*100
  }
  
  meanPCDiff <- mean(compareData$pcDIFFERENCE, na.rm=TRUE)
  SDEV <- sd(compareData$pcDIFFERENCE, na.rm=TRUE)
  stats <- data.frame(controlName, geography, Observed, Predicted, Difference, pcDifference, N, RMSE, meanPCDiff, SDEV)
  
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
      ggsave(paste("plots/",controlID,".png",sep=""), width=9,height=6)
    }
  
  cat("\n Processed Control: ", controlName) 

  return(stats)
}

myRMSE <- function(FINALEXPANSIONS, AVERAGEEXPANSION, N){
  EXPECTED <- rep(AVERAGEEXPANSION,N)
  ACTUAL <- FINALEXPANSIONS
  return(rmse(ACTUAL, EXPECTED, na.rm=TRUE))
}

#Create plot directory
dir.create('plots', showWarnings = FALSE)

#Open connection to PopSyn SQL server
connectionstring <- paste("Driver={SQL Server Native Client 11.0};Server=",serverName,";Database=",dbName,";Trusted_Connection=yes;", sep="")
channel <- odbcDriverConnect(connection=connectionstring)

#Computing convergence statistics and write out results
stats <- apply(columnMap, 1, function(x) procControl(x["GEOGRAPHY"], x["NAME"], x["CONTROL"],x["SUMMARY"]))
stats <- do.call(rbind,stats)
write.csv(stats, "stats.csv", row.names = FALSE)

#Convergence plot
p2 <- ggplot(stats, aes(x = controlName, y=meanPCDiff)) +
  geom_point(shape = 15, colour = "steelblue", size = 2)+
  geom_errorbar(data = stats, aes(ymin=-SDEV,ymax=SDEV), width=0.2, colour = "steelblue") +
  scale_x_discrete(limits=rev(levels(stats$controlName))) + 
  geom_hline(xintercept=c(0)) +
  labs(x = NULL, y="Percentage Difference [+/- SDEV]", title = gsub("Region",region,"Region PopSynIII Controls Validation")) +
  coord_flip(ylim = c(-100, 100)) +
  theme_bw() +
  theme(plot.title=element_text(size=12, lineheight=.9, face="bold", vjust=1))

ggsave("plots/PopSyn Convergence - SDEV.png", width=8,height=8)

#Convergence plot
p3 <- ggplot(stats, aes(x = controlName, y=meanPCDiff)) +
  geom_point(shape = 15, colour = "steelblue", size = 2)+
  geom_errorbar(data = stats, aes(ymin=-RMSE,ymax=RMSE), width=0.2, colour = "steelblue") +
  scale_x_discrete(limits=rev(levels(stats$controlName))) + 
  geom_hline(xintercept=c(0)) +
  labs(x = NULL, y="Percentage Difference [+/- RMSE]", title = gsub("Region",region,"Region PopSynIII Controls Validation")) +
  coord_flip(ylim = c(-100, 100)) +
  theme_bw() +
  theme(plot.title=element_text(size=12, lineheight=.9, face="bold", vjust=1))

ggsave("plots/PopSyn Convergence - RMSE.png", width=8,height=8)


#Uniformity Analysis
uniformity <- sqlQuery(channel,"SELECT * FROM uniformity")
close(channel)
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
write.csv(uAnalysisPUMA, "uniformity.csv", row.names=FALSE)

#fin

