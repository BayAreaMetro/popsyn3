############################################################################################################
# Script to download BKGP and Tract level Decennial Censusand ACS data
#
# By default the script reads the Census data from the Census download directory. If the CensusDownload 
# switch is set to "TRUE" or if the files don't exist in the specified directory, Census data is downloaded
# from the web via Census API.
#
# Need to specify Census API KEY
#
# Allocate Block Group data to MAZs proportional to number of HHs in each MAZ
# Allocate Census Tract data to TAZs proportional to number of HHs in each TAZ
#
# Author: binny.mathewpaul@rsginc.com Feb 2017
###########################################################################################################

# LIBRARIES
############
source("E:\\Projects\\Clients\\mtc\\TO2_Task2\\RScripts\\downloadCensusData.R")
library(dplyr)

# INPUTS
#########

# Census download switch
downloadCensus <- FALSE

# Working directory
workingDir <- "E:\\Projects\\Clients\\mtc\\TO2_Task2"

outputDir <- paste(workingDir, "CensusData", sep = "\\")
censusDownloadDir <- paste(workingDir, "CensusData\\Downloads", sep = "\\")

CENSUS_API_KEY_FILE <- paste(workingDir, "CensusData\\census_api_key.csv", sep = "\\")
STATE_FIPS = "06"	
COUNTY_FIPS_STRING = c("01","13","41","55","75","81","85","95","97")

# Get the Census API key	
api_key <- read.csv(CENSUS_API_KEY_FILE, header = TRUE)	
api_key <- api_key %>%	
  mutate(key = paste(key))

# Read HHs by MAZ and TAZ
mazData <- read.csv(paste(workingDir, "MTCPopSynIII\\data\\2010\\mazData.csv", sep = "\\"))
tazData <- read.csv(paste(workingDir, "MTCPopSynIII\\data\\2010\\tazData.csv", sep = "\\"))

# Read PopSyn XWalk
popsyn_xwalk <- read.csv(paste(workingDir, "MTCPopSynIII\\data\\geographicCWalk.csv", sep = "\\"))

# Read MAZ-BLK_GRP crosswalk
MAZ_BG10 <- read.csv(paste(workingDir, "GeographicXWalk\\GeogXWalk2010_MAZ_BG.csv", sep = "\\"))

# Read TAZ-CT XWalk
TAZ_CT10 <- read.csv(paste(workingDir, "GeographicXWalk\\GeogXWalk2010_TAZ_CT_PUMA_CY.csv", sep = "\\"))
TAZ_CT00 <- read.csv(paste(workingDir, "GeographicXWalk\\GeogXWalk2000_TAZ_CT_PUMA_CY.csv", sep = "\\"))


# DOWNLOAD DATA
################

if(downloadCensus==TRUE |!file.exists(paste(censusDownloadDir, "hhsize_BG_2010_sf1.csv", sep = "\\"))){
  # HHs by HHSize at Block Group Level [2010 decennial, SF1	]
  #----------------------------------------------------------
  hhsize_BG <- getCensusData(name = "sf1", 	
                             vintage = 2010, 	
                             key = api_key$key, 	
                             vars=c("H0130001", "H0130002", "H0130003", "H0130004", "H0130005", "H0130006", "H0130007", "H0130008"), 	
                             region = paste("block+group:", "*", sep = ""),	
                             regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "sf1", 	
                               vintage = 2010, 	
                               key = api_key$key, 	
                               vars=c("H0130001", "H0130002", "H0130003", "H0130004", "H0130005", "H0130006", "H0130007", "H0130008"), 	
                               region = paste("block+group:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    hhsize_BG <- rbind(hhsize_BG, temp_data)
  }
  # Create blcok group ID
  hhsize_BG$BKGPIDFP10 <- hhsize_BG$state * 10000000000 + hhsize_BG$county * 10000000 + hhsize_BG$tract * 10 + hhsize_BG$`block group`
  write.csv(hhsize_BG, paste(censusDownloadDir, "hhsize_BG_2010_sf1.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "hhtype_BG_2010_sf1.csv", sep = "\\"))){
  # HHs by HH Type at Block Group Level [2010 decennial, SF1	]
  #----------------------------------------------------------
  hhtype_BG <- getCensusData(name = "sf1", 	
                             vintage = 2010, 	
                             key = api_key$key, 	
                             vars=c("P0180001", "P0180002", "P0180003", "P0180004", "P0180005", "P0180006", "P0180007", "P0180008", "P0180009"), 	
                             region = paste("block+group:", "*", sep = ""),	
                             regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "sf1", 	
                               vintage = 2010, 	
                               key = api_key$key, 	
                               vars=c("P0180001", "P0180002", "P0180003", "P0180004", "P0180005", "P0180006", "P0180007", "P0180008", "P0180009"), 	
                               region = paste("block+group:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    hhtype_BG <- rbind(hhtype_BG, temp_data)
  }
  # Create blcok group ID
  hhtype_BG$BKGPIDFP10 <- hhtype_BG$state * 10000000000 + hhtype_BG$county * 10000000 + hhtype_BG$tract * 10 + hhtype_BG$`block group`
  write.csv(hhtype_BG, paste(censusDownloadDir, "hhtype_BG_2010_sf1.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "hhpkid_CT_2010_sf1.csv", sep = "\\"))){
  # HHs with no children under 18 at Census Tract Level [2010 decennial, SF1	]
  #----------------------------------------------------------
  hhpkid_CT <- getCensusData(name = "sf1", 	
                             vintage = 2010, 	
                             key = api_key$key, 	
                             vars=c("PCT0160001","PCT0160004", "PCT0160010", "PCT0160016"), 	
                             region = paste("tract:", "*", sep = ""),	
                             regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "sf1", 	
                               vintage = 2010, 	
                               key = api_key$key, 	
                               vars=c("PCT0160001","PCT0160004", "PCT0160010", "PCT0160016"), 	
                               region = paste("tract:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    hhpkid_CT <- rbind(hhpkid_CT, temp_data)
  }
  # Create census tract ID
  hhpkid_CT$CTIDFP10 <- hhpkid_CT$state * 1000000000 + hhpkid_CT$county * 1000000 + hhpkid_CT$tract
  write.csv(hhpkid_CT, paste(censusDownloadDir, "hhpkid_CT_2010_sf1.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "pop_BG_2010_sf1.csv", sep = "\\"))){
  # Persons by Gender and Age at Block Group Level [2010 decennial, SF1	]
  #----------------------------------------------------------
  pop_BG <- getCensusData(name = "sf1", 	
                          vintage = 2010, 	
                          key = api_key$key, 	
                          vars=c("P0120001", "P0120002", "P0120003", "P0120004", "P0120005", "P0120006", "P0120007", "P0120008", 
                                 "P0120009", "P0120010", "P0120011", "P0120012", "P0120013", "P0120014", "P0120015", "P0120016",
                                 "P0120017", "P0120018", "P0120019", "P0120020", "P0120021", "P0120022", "P0120023", "P0120024",
                                 "P0120025", "P0120026", "P0120027", "P0120028", "P0120029", "P0120030", "P0120031", "P0120032",
                                 "P0120033", "P0120034", "P0120035", "P0120036", "P0120037", "P0120038", "P0120039", "P0120040",
                                 "P0120041", "P0120042", "P0120043", "P0120044", "P0120045", "P0120046", "P0120047", "P0120048","P0120049"), 	
                          region = paste("block+group:", "*", sep = ""),	
                          regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "sf1", 	
                               vintage = 2010, 	
                               key = api_key$key, 	
                               vars=c("P0120001", "P0120002", "P0120003", "P0120004", "P0120005", "P0120006", "P0120007", "P0120008", 
                                      "P0120009", "P0120010", "P0120011", "P0120012", "P0120013", "P0120014", "P0120015", "P0120016",
                                      "P0120017", "P0120018", "P0120019", "P0120020", "P0120021", "P0120022", "P0120023", "P0120024",
                                      "P0120025", "P0120026", "P0120027", "P0120028", "P0120029", "P0120030", "P0120031", "P0120032",
                                      "P0120033", "P0120034", "P0120035", "P0120036", "P0120037", "P0120038", "P0120039", "P0120040",
                                      "P0120041", "P0120042", "P0120043", "P0120044", "P0120045", "P0120046", "P0120047", "P0120048","P0120049"), 	
                               region = paste("block+group:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    pop_BG <- rbind(pop_BG, temp_data)
  }
  # Create blcok group ID
  pop_BG$BKGPIDFP10 <- pop_BG$state * 10000000000 + pop_BG$county * 10000000 + pop_BG$tract * 10 + pop_BG$`block group`
  write.csv(pop_BG, paste(censusDownloadDir, "pop_BG_2010_sf1.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "hhworker_CT_2011_acs5.csv", sep = "\\"))){
  # HHs by number of workers at Census Tract Level [2007-11 5 year ACS, http://api.census.gov/data/2011/acs5/variables.html]
  #----------------------------------------------------------
  hhworker_CT <- getCensusData(name = "acs5", 	
                               vintage = 2011, 	
                               key = api_key$key, 	
                               vars=c("B08202_001E", "B08202_002E", "B08202_003E", "B08202_004E", "B08202_005E"), 	
                               region = paste("tract:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "acs5", 	
                               vintage = 2011, 	
                               key = api_key$key, 	
                               vars=c("B08202_001E", "B08202_002E", "B08202_003E", "B08202_004E", "B08202_005E"), 	
                               region = paste("tract:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    hhworker_CT <- rbind(hhworker_CT, temp_data)
  }
  # Create census tract ID
  hhworker_CT$CTIDFP10 <- hhworker_CT$state * 1000000000 + hhworker_CT$county * 1000000 + hhworker_CT$tract
  write.csv(hhworker_CT, paste(censusDownloadDir, "hhworker_CT_2011_acs5.csv", sep = "\\"), row.names = F)
}

# PROCESS DATA
###############

# read Census data from Census download directory
hhsize_BG   <- read.csv(paste(censusDownloadDir, "hhsize_BG_2010_sf1.csv", sep = "\\"))
hhtype_BG   <- read.csv(paste(censusDownloadDir, "hhtype_BG_2010_sf1.csv", sep = "\\"))
hhpkid_CT   <- read.csv(paste(censusDownloadDir, "hhpkid_CT_2010_sf1.csv", sep = "\\"))
pop_BG      <- read.csv(paste(censusDownloadDir, "pop_BG_2010_sf1.csv", sep = "\\"))
hhworker_CT <- read.csv(paste(censusDownloadDir, "hhworker_CT_2011_acs5.csv", sep = "\\"))

# Total households in Census
totHHs <- sum(hhsize_BG$H0130001)
# Total population as per Census
totPop <- sum(pop_BG$P0120001)

# **************************
# Create MAZ control file
# **************************

mazControl <- MAZ_BG10
mazControl$HH <- mazData$X2010.HH[match(mazControl$MAZ_ORIGINAL, mazData$MAZ)]

# HH Size
#--------
mazControl <- mazControl %>% 
  group_by(BKGPIDFP10) %>%
  mutate(BKGP_HH = sum(HH)) %>%
  mutate(MAZ_Percent = ifelse(BKGP_HH>0,HH/BKGP_HH,0)) %>%  # compute proportions by hh across MAZ within ech block group
  left_join(hhsize_BG, by = "BKGPIDFP10") %>% 
  mutate(TOT_HH   = MAZ_Percent*H0130001) %>%                 # allocate to each MAZ from block group based on proportion
  mutate(HHSize1  = MAZ_Percent*H0130002) %>% 
  mutate(HHSize2  = MAZ_Percent*H0130003) %>% 
  mutate(HHSize3  = MAZ_Percent*H0130004) %>% 
  mutate(HHSize4p = MAZ_Percent*(H0130005 + H0130006 + H0130007 + H0130008))

# rescale to match total HHs in Census
totHHs_temp <- sum(mazControl[,c("HHSize1", "HHSize2", "HHSize3", "HHSize4p")])
mazControl <- mazControl %>% 
  mutate(TOT_HH   = TOT_HH  *totHHs/totHHs_temp) %>% 
  mutate(HHSize1  = HHSize1 *totHHs/totHHs_temp) %>% 
  mutate(HHSize2  = HHSize2 *totHHs/totHHs_temp) %>% 
  mutate(HHSize3  = HHSize3 *totHHs/totHHs_temp) %>% 
  mutate(HHSize4p = HHSize4p*totHHs/totHHs_temp)

# remove redundant fields
mazControl <- mazControl %>% 
  select(-H0130001, -H0130002, -H0130003, -H0130004, -H0130005, -H0130006, -H0130007, -H0130008)

# HH Type
#--------
# HHType1: Family households: !! Husband-wife family
# HHType2: Family households: !! Other family: !! Male householder, no wife present
# HHtype3: Family households: !! Other family: !! Female householder, no husband present
# HHType4: Nonfamily households: !! Householder living alone !! Female householder, no husband present
# HHType5: Nonfamily households: !! Householder not living alone !! Female householder, no husband present
mazControl <- mazControl %>%
  left_join(hhtype_BG, by = "BKGPIDFP10") %>%
  mutate(HHType1 = MAZ_Percent * P0180003) %>%
  mutate(HHType2 = MAZ_Percent * P0180005) %>%
  mutate(HHType3 = MAZ_Percent * P0180006) %>%
  mutate(HHType4 = MAZ_Percent * P0180008) %>%
  mutate(HHType5 = MAZ_Percent * P0180009)
# rescale to match total HHs in Census
totHHs_temp <- sum(mazControl[,c("HHType1", "HHType2", "HHType3", "HHType4", "HHType5")])
mazControl <- mazControl %>% 
  mutate(HHType1  = HHType1 * totHHs/totHHs_temp) %>% 
  mutate(HHType2  = HHType2 * totHHs/totHHs_temp) %>% 
  mutate(HHType3  = HHType3 * totHHs/totHHs_temp) %>% 
  mutate(HHType4  = HHType4 * totHHs/totHHs_temp) %>% 
  mutate(HHType5  = HHType5 * totHHs/totHHs_temp) 


# Persons by Gender and Age
#--------------------------
mazControl <- mazControl %>%
  left_join(pop_BG, by = "BKGPIDFP10") %>%
  mutate(Tot_Pop   = MAZ_Percent * P0120001) %>%
  mutate(Male      = MAZ_Percent * P0120002) %>%
  mutate(Female    = (Tot_Pop - Male)) %>%
  mutate(Age_00_05 = MAZ_Percent * (P0120003 + P0120027)) %>%
  mutate(Age_05_09 = MAZ_Percent * (P0120004 + P0120028)) %>%	
  mutate(Age_10_14 = MAZ_Percent * (P0120005 + P0120029)) %>%	
  mutate(Age_15_17 = MAZ_Percent * (P0120006 + P0120030)) %>%	
  mutate(Age_18_19 = MAZ_Percent * (P0120007 + P0120031)) %>%	
  mutate(Age_20_20 = MAZ_Percent * (P0120008 + P0120032)) %>%	
  mutate(Age_21_21 = MAZ_Percent * (P0120009 + P0120033)) %>%	
  mutate(Age_22_24 = MAZ_Percent * (P0120010 + P0120034)) %>%	
  mutate(Age_25_29 = MAZ_Percent * (P0120011 + P0120035)) %>%	
  mutate(Age_30_34 = MAZ_Percent * (P0120012 + P0120036)) %>%	
  mutate(Age_35_39 = MAZ_Percent * (P0120013 + P0120037)) %>%	
  mutate(Age_40_44 = MAZ_Percent * (P0120014 + P0120038)) %>%	
  mutate(Age_45_49 = MAZ_Percent * (P0120015 + P0120039)) %>%	
  mutate(Age_50_54 = MAZ_Percent * (P0120016 + P0120040)) %>%	
  mutate(Age_55_59 = MAZ_Percent * (P0120017 + P0120041)) %>%	
  mutate(Age_60_61 = MAZ_Percent * (P0120018 + P0120042)) %>%	
  mutate(Age_62_64 = MAZ_Percent * (P0120019 + P0120043)) %>%	
  mutate(Age_65_66 = MAZ_Percent * (P0120020 + P0120044)) %>%	
  mutate(Age_67_69 = MAZ_Percent * (P0120021 + P0120045)) %>%	
  mutate(Age_70_74 = MAZ_Percent * (P0120022 + P0120046)) %>%	
  mutate(Age_75_79 = MAZ_Percent * (P0120023 + P0120047)) %>%	
  mutate(Age_80_84 = MAZ_Percent * (P0120024 + P0120048)) %>%	
  mutate(Age_85_up = MAZ_Percent * (P0120025 + P0120049)) %>%	
  mutate(Age_00_18 = Age_00_05 + Age_05_09 + Age_10_14 + Age_15_17 + round(0.5 * Age_18_19)) %>%	
  mutate(Age_19_64 = round(0.5 * Age_18_19) + Age_20_20 + Age_21_21 + Age_22_24 + Age_25_29 + Age_30_34 + 
           Age_35_39 + Age_40_44 + Age_45_49 + Age_50_54 + Age_55_59 + Age_60_61 + Age_62_64) %>%	
  mutate(Age_65_up = Age_65_66 + Age_67_69 + Age_70_74 + Age_75_79 + Age_80_84 + Age_85_up)

# rescale to match total pop in Census
totPop_temp <- sum(mazControl$Tot_Pop)
totPop_temp1 <- sum(mazControl$Age_00_18 + mazControl$Age_19_64 + mazControl$Age_65_up) # to fix rounding errors
mazControl <- mazControl %>% 
  mutate(Tot_Pop   = Tot_Pop   * totPop/totPop_temp) %>%
  mutate(Male      = Male      * totPop/totPop_temp) %>%
  mutate(Female    = Female    * totPop/totPop_temp) %>%
  mutate(Age_00_05 = Age_00_05 * totPop/totPop_temp) %>%
  mutate(Age_05_09 = Age_05_09 * totPop/totPop_temp) %>%	
  mutate(Age_10_14 = Age_10_14 * totPop/totPop_temp) %>%	
  mutate(Age_15_17 = Age_15_17 * totPop/totPop_temp) %>%	
  mutate(Age_18_19 = Age_18_19 * totPop/totPop_temp) %>%	
  mutate(Age_20_20 = Age_20_20 * totPop/totPop_temp) %>%	
  mutate(Age_21_21 = Age_21_21 * totPop/totPop_temp) %>%	
  mutate(Age_22_24 = Age_22_24 * totPop/totPop_temp) %>%	
  mutate(Age_25_29 = Age_25_29 * totPop/totPop_temp) %>%	
  mutate(Age_30_34 = Age_30_34 * totPop/totPop_temp) %>%	
  mutate(Age_35_39 = Age_35_39 * totPop/totPop_temp) %>%	
  mutate(Age_40_44 = Age_40_44 * totPop/totPop_temp) %>%	
  mutate(Age_45_49 = Age_45_49 * totPop/totPop_temp) %>%	
  mutate(Age_50_54 = Age_50_54 * totPop/totPop_temp) %>%	
  mutate(Age_55_59 = Age_55_59 * totPop/totPop_temp) %>%	
  mutate(Age_60_61 = Age_60_61 * totPop/totPop_temp) %>%	
  mutate(Age_62_64 = Age_62_64 * totPop/totPop_temp) %>%	
  mutate(Age_65_66 = Age_65_66 * totPop/totPop_temp) %>%	
  mutate(Age_67_69 = Age_67_69 * totPop/totPop_temp) %>%	
  mutate(Age_70_74 = Age_70_74 * totPop/totPop_temp) %>%	
  mutate(Age_75_79 = Age_75_79 * totPop/totPop_temp) %>%	
  mutate(Age_80_84 = Age_80_84 * totPop/totPop_temp) %>%	
  mutate(Age_85_up = Age_85_up * totPop/totPop_temp) %>%	
  mutate(Age_00_18 = Age_00_18 * totPop/totPop_temp1) %>%	
  mutate(Age_19_64 = Age_19_64 * totPop/totPop_temp1) %>%	
  mutate(Age_65_up = Age_65_up * totPop/totPop_temp1)

# select variables to be included in final TAZ control file
mazControl <- mazControl %>%
  select(MAZSEQ, MAZ_ORIGINAL, HH, BKGPIDFP10, MAZ_Percent, TOT_HH, HHSize1, HHSize2, HHSize3, HHSize4p, 
         HHType1, HHType2, HHType3, HHType4, HHType5, Tot_Pop, Male, Female, Age_00_05, Age_05_09, Age_10_14, 
         Age_15_17, Age_18_19, Age_20_20, Age_21_21, Age_22_24, Age_25_29, Age_30_34, Age_35_39, Age_40_44, 
         Age_45_49, Age_50_54, Age_55_59, Age_60_61, Age_62_64, Age_65_66, Age_67_69, Age_70_74, 
         Age_75_79, Age_80_84, Age_85_up, Age_00_18, Age_19_64, Age_65_up)

# write outputs
write.csv(mazControl, paste(outputDir, "mazAllocatedCensus.csv", sep = "\\"), row.names = F)


# **************************
# Create TAZ control file
# **************************

#tazControl <- TAZ_CT10
tazControl <- mazControl[,c("MAZSEQ", "MAZ_ORIGINAL", "HH")] %>%
  left_join(popsyn_xwalk[,c("MAZ_ORIGINAL", "TAZ", "TAZ_ORIGINAL")], by = "MAZ_ORIGINAL") %>%
  group_by(TAZ) %>%
  summarise(TAZSEQ = max(TAZ), TAZ_ORIGINAL = max(TAZ_ORIGINAL), TAZ_HH = sum(HH)) %>%
  select(-TAZ) %>%
  left_join(TAZ_CT10, by = "TAZSEQ") %>%
  group_by(CTIDFP10) %>%
  mutate(CT10_HH = sum(TAZ_HH)) %>%
  mutate(TAZ_Percent10 = ifelse(CT10_HH>0,TAZ_HH/CT10_HH,0)) %>%
  ungroup() %>%
  left_join(TAZ_CT00, by = "TAZSEQ") %>%
  group_by(CTIDFP00) %>%
  mutate(CT00_HH = sum(TAZ_HH)) %>%
  mutate(TAZ_Percent00 = ifelse(CT00_HH>0,TAZ_HH/CT00_HH,0)) %>%
  ungroup()

# Presence of Children
#---------------------
tazControl <- tazControl %>%
  left_join(hhpkid_CT, by = "CTIDFP10") %>%
  mutate(TOT_HH  = TAZ_Percent10 * PCT0160001) %>%
  mutate(WI_KIDS = TAZ_Percent10 * (PCT0160004 + PCT0160010 + PCT0160016)) %>%
  mutate(WO_KIDS = TOT_HH - WI_KIDS)

# rescale to match total HHs in Census
totHHs_temp <- sum(tazControl$TOT_HH)
tazControl <- tazControl %>% 
  mutate(TOT_HH  = TOT_HH  * totHHs/totHHs_temp) %>%
  mutate(WI_KIDS = WI_KIDS * totHHs/totHHs_temp) %>%
  mutate(WO_KIDS = WO_KIDS * totHHs/totHHs_temp)
  
# Number of Workers [ACS 2008-12, Census Tracts in Census 2010 geography]
#---------------------
tazControl <- tazControl %>%
  left_join(hhworker_CT, by = "CTIDFP10") %>%
  mutate(TOT_HH   = TAZ_Percent10 * (B08202_001E)) %>%
  mutate(WORKERS0 = TAZ_Percent10 * (B08202_002E)) %>%
  mutate(WORKERS1 = TAZ_Percent10 * (B08202_003E)) %>%
  mutate(WORKERS2 = TAZ_Percent10 * (B08202_004E)) %>%
  mutate(WORKERS3 = TAZ_Percent10 * (B08202_005E))

# rescale to match total HHs in Census
totHHs_temp <- sum(tazControl$TOT_HH)
tazControl <- tazControl %>% 
  mutate(WORKERS0 = WORKERS0 * totHHs/totHHs_temp) %>%
  mutate(WORKERS1 = WORKERS1 * totHHs/totHHs_temp) %>%
  mutate(WORKERS2 = WORKERS2 * totHHs/totHHs_temp) %>%
  mutate(WORKERS3 = WORKERS3 * totHHs/totHHs_temp) 

# set NAs to zero
#tazControl$WORKERS[is.na(tazControl$WORKERS)] <- 0

# select variables to be included in final TAZ control file
tazControl <- tazControl %>%
  select(TAZSEQ, TAZ_ORIGINAL, TAZ_HH, CTIDFP10, TAZ_Percent10,  
         TOT_HH, WI_KIDS, WO_KIDS, WORKERS0, WORKERS1, WORKERS2, WORKERS3)

# write outputs
write.csv(tazControl, paste(outputDir, "tazAllocatedCensus.csv", sep = "\\"), row.names = F)



