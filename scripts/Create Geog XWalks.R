#####################################################################################################
# Script to create geographic crosswalks between MAZ, TAZ and Census 2000 and Census 2010 geographies
# INPUTS:-
#   1. List of MAZ Centroid coordinates is an input
#   2. Shape files for following Census geographies - Block Groups, Census Tracts, PUMA, County
# Based on Ben Stabler's point in polygon script
# Author: binny.mathewpaul@rsginc.com [Feb 2017]
####################################################################################################
message("=== Running Create Geog XWalks.R")

# LIBRARIES
###########
if (!"dplyr" %in% installed.packages()) install.packages("dplyr", repos='http://cran.us.r-project.org')
library(dplyr)
if (!"rgeos" %in% installed.packages()) install.packages("rgeos", repos='http://cran.us.r-project.org')
library(rgeos)
if (!"sp" %in% installed.packages()) install.packages("sp", repos='http://cran.us.r-project.org')
library(sp)
if (!"rgdal" %in% installed.packages()) install.packages("rgdal", repos='http://cran.us.r-project.org')
library(rgdal)
if (!"foreign" %in% installed.packages()) install.packages("foreign", repos='http://cran.us.r-project.org')
library(foreign)


# INPUTS
#########

# Switches for 2000 and 2010
create2000XWalk    <- TRUE
create2010XWalk    <- FALSE

# County List
county_list = c("001","013","041","055","075","081","085","095","097")

WorkingDir   <- "E:\\Projects\\Clients\\mtc\\TO2_Task2\\GeographicXWalk"
censusDir    <- "E:\\Projects\\Clients\\mtc\\TO2_Task2\\CensusGIS"
mazSHPDir    <- "E:\\Projects\\Clients\\mtc\\TO2_Task2\\SHP_MTC\\mtctm2zonesRevised_shapeExport"

mazDF        <- read.csv(paste(WorkingDir, "MAZ_Centroids.csv", sep = "\\"))
mazCentroids <- data.frame(x=mazDF$Longitude/1000000, y=mazDF$Latitude/1000000)
mazCentroids <- SpatialPoints(mazCentroids)
tazDF        <- read.csv(paste(WorkingDir, "TAZ_Centroids.csv", sep = "\\"))
tazCentroids <- data.frame(x=tazDF$Longitude/1000000, y=tazDF$Latitude/1000000)
tazCentroids <- SpatialPoints(tazCentroids)

BG00DF       <- read.dbf(paste(censusDir, "Blocks", "TL_2010_06_TABBLOCK00.DBF", sep = "\\"), as.is = TRUE)
BG00DF       <- filter(BG00DF, COUNTYFP00 %in% county_list)
BG00IntPt    <- data.frame(x=as.numeric(BG00DF$INTPTLON00), y=as.numeric(BG00DF$INTPTLAT00))
BG00IntPt    <- SpatialPoints(BG00IntPt)
BG10DF       <- read.dbf(paste(censusDir, "Blocks", "TL_2010_06_TABBLOCK10.DBF", sep = "\\"), as.is = TRUE)
BG10DF       <- filter(BG10DF, COUNTYFP10 %in% county_list)
BG10IntPt    <- data.frame(x=as.numeric(BG10DF$INTPTLON10), y=as.numeric(BG10DF$INTPTLAT10))
BG10IntPt    <- SpatialPoints(BG10IntPt)

mazTazXWalk  <- read.csv("E:\\Projects\\Clients\\mtc\\TO2_Task2\\MTCPopSynIII\\data\\geographicCWalk.csv")

# Census SHP file
BG2000       <- readOGR(paste(censusDir, "BlockGroups\\2000", sep = "\\"), layer="tl_2010_06_bg00") #block group 2000
BG2010       <- readOGR(paste(censusDir, "BlockGroups\\2010", sep = "\\"), layer="tl_2010_06_bg10") #block group 2010
CT2000       <- readOGR(paste(censusDir, "CensusTracts\\2000", sep = "\\"), layer="tl_2010_06_tract00") #census tract 2000
CT2010       <- readOGR(paste(censusDir, "CensusTracts\\2010", sep = "\\"), layer="tl_2010_06_tract10") #census tract 2010
PM2000       <- readOGR(paste(censusDir, "PUMA\\2000", sep = "\\"), layer="p506_d00") #puma 2000
PM2010       <- readOGR(paste(censusDir, "PUMA\\2010", sep = "\\"), layer="tl_2010_06_puma10") #puma 2010

CRS_Census   <- proj4string(BG2000)

# MAZ SHP file
MAZ_SHP      <- readOGR(mazSHPDir, layer="mazs")
MAZ_SHP      <- spTransform(MAZ_SHP, CRS_Census)

# FUNCTIONS 
############

# Overlay
overlay <- function(points, polygon){
  proj4string(points) <- proj4string(polygon) # use same projection
  pointsDF <- over(points,polygon)
  return(pointsDF)
}


# CREATE CROSSWALK
##################

if(create2000XWalk){
  # 2000 Crosswalk
  #---------------------------
  XWalk2000 <- select(mazDF, MAZSEQ)
  mazTemp1  <- overlay(mazCentroids, BG2000)
  # perfect correspondence between PUMA and County with MTC modeling region
  # Since the two DFs are in the same order of MAZ centroids, they can be appended to generate crosswalk
  
  # TAZ-> CT
  tazXWalk <- select(tazDF, TAZSEQ)
  tazTemp1 <- overlay(tazCentroids, CT2000)
  tazTemp2 <- overlay(tazCentroids, PM2000)
  # perfect correspondence between PUMA and County with MTC modeling region
  # Since the two DFs are in the same order of TAZ centroids, they can be appended to generate crosswalk
  tazXWalk <- tazXWalk %>%
    mutate(TRACTCE00  = tazTemp1[,'TRACTCE00']) %>%
    mutate(CTIDFP00   = tazTemp1[,'CTIDFP00'])  %>%
    mutate(COUNTYFP00 = tazTemp1[,'COUNTYFP00']) %>%
    mutate(STATEFP00  = tazTemp1[,'STATEFP00'])  %>%
    mutate(PUMA00     = tazTemp2[,'PUMA5'])
  
  # Manual fixes required for tazXWalk since some TAZ centroids lie outside PUMA boundaries
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==211] <- "02205"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==725] <- "02303"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==614] <- "02207"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==200] <- "02205"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==4657] <- "01201"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==4499] <- "01202"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==4652] <- "01202"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==4658] <- "01202"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==3070] <- "02401"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==3064] <- "02401"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==2481] <- "02401"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==2526] <- "02405"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==2559] <- "02405"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==2995] <- "02405"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==2204] <- "02405"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==2598] <- "02405"
  tazXWalk$PUMA00[tazXWalk$TAZSEQ==2558] <- "02405"
  
  
  # Assemble final crosswalk
  XWalk2000 <- XWalk2000 %>%
    mutate(BLGRPCE00  = mazTemp1[,'BLKGRPCE00']) %>%
    mutate(BKGPIDFP00 = mazTemp1[,'BKGPIDFP00'])
  
  # Copy MAZ_ORIGINAL, TAZ and TAZ_ORIGINAL from old crosswalk
  XWalk2000$MAZ_ORIGINAL <- mazTazXWalk$MAZ_ORIGINAL[match(XWalk2000$MAZSEQ, mazTazXWalk$MAZ)]
  XWalk2000$TAZSEQ       <- mazTazXWalk$TAZ[match(XWalk2000$MAZSEQ, mazTazXWalk$MAZ)]
  XWalk2000$TAZ_ORIGINAL <- mazTazXWalk$TAZ_ORIGINAL[match(XWalk2000$MAZSEQ, mazTazXWalk$MAZ)]
  
  # Copy TAZ and higher geographies from TAZ crosswalk
  XWalk2000$TRACTCE00    <- tazXWalk$TRACTCE00[match(XWalk2000$TAZSEQ, tazXWalk$TAZSEQ)]
  XWalk2000$CTIDFP00     <- tazXWalk$CTIDFP00[match(XWalk2000$TAZSEQ, tazXWalk$TAZSEQ)]
  XWalk2000$PUMA00       <- tazXWalk$PUMA00[match(XWalk2000$TAZSEQ, tazXWalk$TAZSEQ)]
  XWalk2000$COUNTYFP00   <- tazXWalk$COUNTYFP00[match(XWalk2000$TAZSEQ, tazXWalk$TAZSEQ)]
  XWalk2000$STATEFP00    <- tazXWalk$STATEFP00[match(XWalk2000$TAZSEQ, tazXWalk$TAZSEQ)]
  
  # Select appropriate columns & rearrange columns in order of geography
  XWalk2000_MZ_BG <- XWalk2000[c("MAZSEQ","MAZ_ORIGINAL","BLGRPCE00","BKGPIDFP00")]
  # Write output file
  cat("Number of NAs in MAZ-BLK_GRP 2000 Xwalk: ",sum(is.na(XWalk2000_MZ_BG)), "\n")
  write.csv(XWalk2000_MZ_BG, paste(WorkingDir, "GeogXWalk2000_MAZ_BG.csv", sep = "//"), row.names = F)
  
  XWalk2000_MZ_TZ_CT_PM_CY <- XWalk2000[c("MAZSEQ","MAZ_ORIGINAL","TAZSEQ","TAZ_ORIGINAL",
                                 "TRACTCE00","CTIDFP00","PUMA00","COUNTYFP00","STATEFP00")]
  # Write output file
  cat("Number of NAs in MAZ-TAZ-CT-PUMA-CNTY 2000 Xwalk: ",sum(is.na(XWalk2000_MZ_TZ_CT_PM_CY)), "\n")
  write.csv(XWalk2000_MZ_TZ_CT_PM_CY, paste(WorkingDir, "GeogXWalk2000_MAZ_TAZ_CT_PUMA_CY.csv", sep = "//"), row.names = F)
  write.csv(tazXWalk, paste(WorkingDir, "GeogXWalk2000_TAZ_CT_PUMA_CY.csv", sep = "//"), row.names = F)
  
  rm(list = c("mazTemp1","tazTemp1","tazTemp2","tazXWalk","XWalk2000","BG2000","CT2000","PM2000", "XWalk2000_MZ_BG", "XWalk2000_MZ_TZ_CT_PM_CY"))
  
  # Blocks -> MAZ
  blockXWalk <- select(BG00DF, STATEFP00,COUNTYFP00,TRACTCE00,BLOCKCE00,BLKIDFP00)
  blkTemp1   <- overlay(BG00IntPt, MAZ_SHP)
  # Since the two DFs are in the same order of TAZ centroids, they can be appended to generate crosswalk
  blockXWalk <- blockXWalk %>%
    mutate(MAZ_ORIGINAL = as.numeric(as.character(blkTemp1[,'MAZ_ORIGIN']))) %>%
    mutate(MAZ_ORIGINAL = ifelse(!is.na(MAZ_ORIGINAL), MAZ_ORIGINAL, 0)) %>%
    left_join(mazTazXWalk[,c('MAZ_ORIGINAL', 'TAZ_ORIGINAL')], by = "MAZ_ORIGINAL") %>%
    mutate(TAZ_ORIGINAL = ifelse(!is.na(TAZ_ORIGINAL), TAZ_ORIGINAL, 0))
  write.csv(blockXWalk, paste(WorkingDir, "GeogXWalk2000_Blocks_MAZ_TAZ.csv", sep = "//"), row.names = F)
  
}

if(create2010XWalk){
  # 2010 Crosswalk
  #---------------------------
  XWalk2010 <- select(mazDF, MAZSEQ)
  mazTemp1 <- overlay(mazCentroids, BG2010)
  # perfect correspondence between PUMA and County with MTC modeling region
  # Since the two DFs are in the same order of MAZ centroids, they can be appended to generate crosswalk
  
  # TAZ-> CT
  tazXWalk <- select(tazDF, TAZSEQ)
  tazTemp1 <- overlay(tazCentroids, CT2010)
  tazTemp2 <- overlay(tazCentroids, PM2010)
  # perfect correspondence between PUMA and County with MTC modeling region
  # Since the two DFs are in the same order of TAZ centroids, they can be appended to generate crosswalk
  tazXWalk <- tazXWalk %>%
    mutate(TRACTCE10  = tazTemp1[,'TRACTCE10']) %>%
    mutate(CTIDFP10   = tazTemp1[,'GEOID10'])  %>%
    mutate(COUNTYFP10 = tazTemp1[,'COUNTYFP10']) %>%
    mutate(STATEFP10  = tazTemp1[,'STATEFP10'])  %>%
    mutate(PUMA10     = tazTemp2[,'PUMACE10'])
  
  # Assemble final crosswalk
  XWalk2010 <- XWalk2010 %>%
    mutate(BLGRPCE10  = mazTemp1[,'BLKGRPCE10']) %>%
    mutate(BKGPIDFP10 = mazTemp1[,'GEOID10'])
  
  # Copy MAZ_ORIGINAL, TAZ and TAZ_ORIGINAL from old crosswalk
  XWalk2010$MAZ_ORIGINAL <- mazTazXWalk$MAZ_ORIGINAL[match(XWalk2010$MAZSEQ, mazTazXWalk$MAZ)]
  XWalk2010$TAZSEQ       <- mazTazXWalk$TAZ[match(XWalk2010$MAZSEQ, mazTazXWalk$MAZ)]
  XWalk2010$TAZ_ORIGINAL <- mazTazXWalk$TAZ_ORIGINAL[match(XWalk2010$MAZSEQ, mazTazXWalk$MAZ)]
  
  # Copy TAZ and higher geographies from TAZ crosswalk
  XWalk2010$TRACTCE10    <- tazXWalk$TRACTCE10[match(XWalk2010$TAZSEQ, tazXWalk$TAZSEQ)]
  XWalk2010$CTIDFP10     <- tazXWalk$CTIDFP10[match(XWalk2010$TAZSEQ, tazXWalk$TAZSEQ)]
  XWalk2010$PUMA10       <- tazXWalk$PUMA10[match(XWalk2010$TAZSEQ, tazXWalk$TAZSEQ)]
  XWalk2010$COUNTYFP10   <- tazXWalk$COUNTYFP10[match(XWalk2010$TAZSEQ, tazXWalk$TAZSEQ)]
  XWalk2010$STATEFP10    <- tazXWalk$STATEFP10[match(XWalk2010$TAZSEQ, tazXWalk$TAZSEQ)]
  
  # Select appropriate columns & rearrange columns in order of geography
  XWalk2010_MZ_BG <- XWalk2010[c("MAZSEQ","MAZ_ORIGINAL","BLGRPCE10","BKGPIDFP10")]
  # Write output file
  cat("Number of NAs in MAZ-BLK_GRP 2010 Xwalk: ",sum(is.na(XWalk2010_MZ_BG)), "\n")
  write.csv(XWalk2010_MZ_BG, paste(WorkingDir, "GeogXWalk2010_MAZ_BG.csv", sep = "//"), row.names = F)
  
  XWalk2010_MZ_TZ_CT_PM_CY <- XWalk2010[c("MAZSEQ","MAZ_ORIGINAL","TAZSEQ","TAZ_ORIGINAL",
                                          "TRACTCE10","CTIDFP10","PUMA10","COUNTYFP10","STATEFP10")]
  # Write output file
  cat("Number of NAs in MAZ-TAZ-CT-PUMA-CNTY 2010 Xwalk: ",sum(is.na(XWalk2010_MZ_TZ_CT_PM_CY)), "\n")
  write.csv(XWalk2010_MZ_TZ_CT_PM_CY, paste(WorkingDir, "GeogXWalk2010_MAZ_TAZ_CT_PUMA_CY.csv", sep = "//"), row.names = F)
  write.csv(tazXWalk, paste(WorkingDir, "GeogXWalk2010_TAZ_CT_PUMA_CY.csv", sep = "//"), row.names = F)
  
  rm(list = c("mazTemp1","tazTemp1","tazTemp2","tazXWalk","XWalk2010","BG2010","CT2010","PM2010", "XWalk2010_MZ_BG", "XWalk2010_MZ_TZ_CT_PM_CY"))

  # Blocks -> MAZ
  blockXWalk <- select(BG10DF, STATEFP10,COUNTYFP10,TRACTCE10,BLOCKCE10,GEOID10)
  blkTemp1   <- overlay(BG10IntPt, MAZ_SHP)
  # Since the two DFs are in the same order of TAZ centroids, they can be appended to generate crosswalk
  blockXWalk <- blockXWalk %>%
    mutate(MAZ_ORIGINAL = as.numeric(as.character(blkTemp1[,'MAZ_ORIGIN']))) %>%
    mutate(MAZ_ORIGINAL = ifelse(!is.na(MAZ_ORIGINAL), MAZ_ORIGINAL, 0)) %>%
    left_join(mazTazXWalk[,c('MAZ_ORIGINAL', 'TAZ_ORIGINAL')], by = "MAZ_ORIGINAL") %>%
    mutate(TAZ_ORIGINAL = ifelse(!is.na(TAZ_ORIGINAL), TAZ_ORIGINAL, 0))
  write.csv(blockXWalk, paste(WorkingDir, "GeogXWalk2010_Blocks_MAZ_TAZ.csv", sep = "//"), row.names = F)
  
}
  
  
