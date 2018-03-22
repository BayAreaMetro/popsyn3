############################################################################################################
# Script to download BKGP and Tract level Decennial Census and ACS data and build controls for year 2000
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
message("=== Running buildControls2000.R")

# LIBRARIES
############
source(paste(WORKING_DIR, "scripts\\downloadCensusData.R", sep = "\\"))
suppressMessages(library(dplyr))
suppressMessages(library(RMySQL))	
suppressMessages(library(stringr))	
suppressMessages(library(reshape2))	

# INPUTS
#########

outputDir         <- file.path(CENSUS_DATA_DIR, "CensusData2000")
censusDownloadDir <- file.path(outputDir, "Downloads")
dir.create(censusDownloadDir, showWarnings = F)

STATE_FIPS = "06"	
COUNTY_FIPS_STRING = c("01","13","41","55","75","81","85","95","97")

county_codes <- data.frame(county_name = c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma"),	
                           county = c("001","013","041","055","075","081","085","095","097"), 	
                           mtc_county_id = c(4,5,9,7,1,2,3,6,8),	
                           stringsAsFactors = FALSE)	

occupation_df = data.frame(occupation = c(1,2,3,4,5,6), 	
                           occupation_category = c("Management", "Professional", "Services", "Retail", "Manual", "Military"),	
                           stringsAsFactors = FALSE)
# Get the Census API key	
api_key <- read.csv(CENSUS_API_KEY_FILE, header = TRUE)	
api_key <- api_key %>%	
  mutate(key = paste(key))

# get password	
mysql_passes <- read.csv(MYSQL_PASSWORD_FILE, header = TRUE)	

mysql_passes <- mysql_passes %>%	
  filter(user == MYSQL_USER_NAME) %>%	
  mutate(pwd = paste(pwd))

# Read HHs by MAZ and TAZ
mazData    <- read.csv(file.path(INPUT_CONTROLS_DIR, "2000", "mazData.csv"))
tazData    <- read.csv(file.path(INPUT_CONTROLS_DIR, "2000", "tazData.csv"))
colnames(tazData) <- c("TAZ_ORIGINAL", "inc_00_30", "inc_30_60", "inc_60_100", "inc_100p")
countyData <- read.csv(file.path(INPUT_CONTROLS_DIR, "2000", "countyData.csv"))
ctpp2000   <- read.csv(file.path(CENSUS_DATA_DIR, "CensusData2000", "CTPP2000", "491968900_P1-062.csv"))

### Read PopSyn XWalk
popsyn_xwalk <- read.csv(file.path(GEOXWALK_DIR, "geographicCWalk.csv"))
### Build the PUMA to County cross-walk	
popsyn_xwalk <- popsyn_xwalk %>%	
  rename(county_name = COUNTYNAME)

puma_to_county <- popsyn_xwalk %>%	
  select(PUMA = PUMA5CE00, county_name) %>%	
  group_by(PUMA, county_name) %>%	
  summarise(count = n()) %>%	
  ungroup() %>%	
  select(-count)

relevant_pumas <- unique(popsyn_xwalk$PUMA5CE00)	

# read 2000 PUMS person and HH data
mysql_connection <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)

pums_pop_00 <- dbReadTable(conn = mysql_connection, name = paste('person_table', "2000", sep = '_'))	
pums_pop_00 <- pums_pop_00[pums_pop_00$PUMA %in% relevant_pumas,]
pums_pop <- pums_pop_00

pums_hh_00 <- dbReadTable(conn = mysql_connection, name = paste('household_table', "2000", sep = '_'))	
pums_hh_00 <- pums_hh_00[pums_hh_00$PUMA %in% relevant_pumas,]
pums_hh <- pums_hh_00

dbDisconnect(mysql_connection)	

#createPUMS_Dist <- F
#if(createPUMS_Dist){
#  # Create distribution of GQ population by age for each PUMA [run only once]
#  # GQ population
#  pums_pop <- pums_pop %>%
#    left_join(pums_hh[,c("serialno", "WGTP", "hht")],  by = c("serialno")) %>%
#    filter(hht==0) %>%  # GQ population
#    mutate(age_group3 = ifelse(age>=35 & age<=64, 3, 0)) %>%
#    mutate(age_group2 = ifelse(age>=20 & age<=34, 2, 0)) %>%
#    mutate(age_group1 = ifelse(age>=18 & age<=19, 1, 0)) %>%
#    group_by(PUMA)  %>%
#    summarise(age_group1 = sum(age_group1 * pweight), age_group2 = sum(age_group2 * pweight), age_group3 = sum(age_group3 * pweight)) %>%
#    mutate(percent_18_19 = age_group1/(age_group1+age_group2+age_group3)) %>%
#    mutate(percent_20_34 = age_group2/(age_group1+age_group2+age_group3)) %>%
#    mutate(percent_35_64 = age_group3/(age_group1+age_group2+age_group3)) %>%
#    ungroup()
#  pums_hh <- pums_hh %>%
#    filter(!(hht==0)) %>%
#    mutate(hhsize_4p = ifelse(persons>=4, 1, 0)) %>%
#    group_by(PUMA)  %>%
#    summarise(avg_size_4p = sum(hhsize_4p*WGTP*persons)/sum(hhsize_4p*WGTP)) %>%
#    ungroup()
#  
#  pums_pop <- pums_pop %>%
#    left_join(pums_hh, by = "PUMA")
#  
#  write.csv(pums_pop, file.path(CENSUS_DATA_DIR,"CensusData2000","PUMA_Distributions.csv"), row.names = F)
#}
PUMA_Dist <- read.csv(file.path(CENSUS_DATA_DIR, "CensusData2000","PUMA_Distributions.csv"))

# Read MAZ-BLK_GRP crosswalk
MAZ_BG00 <- read.csv(file.path(GEOXWALK_DIR, "GeogXWalk2000_MAZ_BG.csv"))
# Read TAZ-CT XWalk
TAZ_CT10 <- read.csv(file.path(GEOXWALK_DIR, "GeogXWalk2010_TAZ_CT_PUMA_CY.csv"))
TAZ_CT00 <- read.csv(file.path(GEOXWALK_DIR, "GeogXWalk2000_TAZ_CT_PUMA_CY.csv"))


# DOWNLOAD DATA
################

if(downloadCensus==TRUE |!file.exists(paste(censusDownloadDir, "hhsize_BG_2000_sf1.csv", sep = "\\"))){
  # HHs by HHSize at Block Group Level [2010 decennial, SF1	]
  #----------------------------------------------------------
  hhsize_BG <- getCensusData(name = "sf1", 	
                             vintage = 2000, 	
                             key = api_key$key, 	
                             vars=c("H013001", "H013002", "H013003", "H013004", "H013005", "H013006", "H013007", "H013008"), 	
                             region = paste("block+group:", "*", sep = ""),	
                             regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "sf1", 	
                               vintage = 2000, 	
                               key = api_key$key, 	
                               vars=c("H013001", "H013002", "H013003", "H013004", "H013005", "H013006", "H013007", "H013008"), 	
                               region = paste("block+group:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    hhsize_BG <- rbind(hhsize_BG, temp_data)
  }
  # Create blcok group ID
  hhsize_BG$BKGPIDFP00 <- hhsize_BG$state * 10000000000 + hhsize_BG$county * 10000000 + hhsize_BG$tract * 10 + hhsize_BG$`block group`
  write.csv(hhsize_BG, paste(censusDownloadDir, "hhsize_BG_2000_sf1.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "hhpkid_CT_2000_sf1.csv", sep = "\\"))){
  # HHs with no children under 18 at Census Tract Level [2010 decennial, SF1	]
  #----------------------------------------------------------
  hhpkid_CT <- getCensusData(name = "sf1", 	
                             vintage = 2000, 	
                             key = api_key$key, 	
                             vars=c("P019002","P019011"), 	
                             region = paste("tract:", "*", sep = ""),	
                             regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "sf1", 	
                               vintage = 2000, 	
                               key = api_key$key, 	
                               vars=c("P019002","P019011"), 	
                               region = paste("tract:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    hhpkid_CT <- rbind(hhpkid_CT, temp_data)
  }
  # Create census tract ID
  hhpkid_CT$CTIDFP00 <- hhpkid_CT$state * 1000000000 + hhpkid_CT$county * 1000000 + hhpkid_CT$tract
  write.csv(hhpkid_CT, paste(censusDownloadDir, "hhpkid_CT_2000_sf1.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "pop_BG_2000_sf1.csv", sep = "\\"))){
  # Persons by Gender and Age at Block Group Level [2010 decennial, SF1	]
  #----------------------------------------------------------
  pop_BG <- getCensusData(name = "sf1", 	
                          vintage = 2000, 	
                          key = api_key$key, 	
                          vars=c("P012001", "P012002", "P012003", "P012004", "P012005", "P012006", "P012007", "P012008", 
                                 "P012009", "P012010", "P012011", "P012012", "P012013", "P012014", "P012015", "P012016",
                                 "P012017", "P012018", "P012019", "P012020", "P012021", "P012022", "P012023", "P012024",
                                 "P012025", "P012026", "P012027", "P012028", "P012029", "P012030", "P012031", "P012032",
                                 "P012033", "P012034", "P012035", "P012036", "P012037", "P012038", "P012039", "P012040",
                                 "P012041", "P012042", "P012043", "P012044", "P012045", "P012046", "P012047", "P012048",
                                 "P012049"), 	
                          region = paste("block+group:", "*", sep = ""),	
                          regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "sf1", 	
                               vintage = 2000, 	
                               key = api_key$key, 	
                               vars=c("P012001", "P012002", "P012003", "P012004", "P012005", "P012006", "P012007", "P012008", 
                                      "P012009", "P012010", "P012011", "P012012", "P012013", "P012014", "P012015", "P012016",
                                      "P012017", "P012018", "P012019", "P012020", "P012021", "P012022", "P012023", "P012024",
                                      "P012025", "P012026", "P012027", "P012028", "P012029", "P012030", "P012031", "P012032",
                                      "P012033", "P012034", "P012035", "P012036", "P012037", "P012038", "P012039", "P012040",
                                      "P012041", "P012042", "P012043", "P012044", "P012045", "P012046", "P012047", "P012048",
                                      "P012049"), 	
                               region = paste("block+group:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    pop_BG <- rbind(pop_BG, temp_data)
  }
  # Create blcok group ID
  pop_BG$BKGPIDFP00 <- pop_BG$state * 10000000000 + pop_BG$county * 10000000 + pop_BG$tract * 10 + pop_BG$`block group`
  write.csv(pop_BG, paste(censusDownloadDir, "pop_BG_2000_sf1.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "GQPop_CT_2000_sf1.csv", sep = "\\"))){
  # GQ Population by Gender and Age at Census Tract Level [2010 decennial, SF1	]
  #----------------------------------------------------------
  gqPop_CT <- getCensusData(name = "sf1", 	
                             vintage = 2000, 	
                             key = api_key$key, 	
                             vars=c("PCT017003", "PCT017015", "PCT017027", "PCT017040", "PCT017052", "PCT017064"), 	
                             region = paste("tract:", "*", sep = ""),	
                             regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "sf1", 	
                               vintage = 2000, 	
                               key = api_key$key, 	
                               vars=c("PCT017003", "PCT017015", "PCT017027", "PCT017040", "PCT017052", "PCT017064"), 	
                               region = paste("tract:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    gqPop_CT <- rbind(gqPop_CT, temp_data)
  }
  # Create census tract ID
  gqPop_CT$CTIDFP00 <- gqPop_CT$state * 1000000000 + gqPop_CT$county * 1000000 + gqPop_CT$tract
  write.csv(gqPop_CT, paste(censusDownloadDir, "GQPop_CT_2000_sf1.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "hhworker_tract_2000.csv", sep = "\\"))){
  # HHs by number of workers at County level [CTPP 2000]
  #----------------------------------------------------------
  hh_workers_df <- ctpp2000 %>%
    select(STATE, COUNTY, TRACT, hh_workers_0=TAB62X2, hh_workers_1=TAB62X3, hh_workers_2=TAB62X4, 
           hh_workers_3=TAB62X5, hh_workers_4_plus=TAB62X6) %>%
    mutate(hh_workers_3_plus=hh_workers_3+hh_workers_4_plus) %>%
    mutate(CTIDFP00 = STATE * 1000000000 + COUNTY * 1000000 + TRACT) %>%
    select(-hh_workers_3, -hh_workers_4_plus)
  
  write.csv(hh_workers_df, paste(censusDownloadDir, "hhworker_tract_2000.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "occupation_county_2000.csv", sep = "\\"))){
  # Persons by occupation type at County Level [PUMS data]
  #----------------------------------------------------------
  # TODO fix the NA removal -- why are weights NA?	
  working <- pums_pop_00 %>%	
    select(PUMA, occupation, WGTP) %>%	
    filter(!(is.na(WGTP))) %>%	
    group_by(PUMA, occupation) %>%	
    summarise(persons = sum(WGTP)) %>%	
    ungroup() %>%	
    filter(!(is.na(occupation)))	
  
  sum(working$persons)	
  
  working <- left_join(working, occupation_df, by = c("occupation"))	
  working <- left_join(working, puma_to_county, by = c("PUMA"))	
  
  working_sum <- working %>%	
    mutate(occupation_category = paste("occupation", str_to_lower(occupation_category), sep = "_")) %>%	
    group_by(county_name, occupation_category) %>%	
    summarise(persons = sum(persons))	
  
  persons_occupation_df <- dcast(working_sum, county_name ~ occupation_category, sum, value.var = c("persons"))	
  
  persons_occupation_df <- left_join(persons_occupation_df, county_codes, by = c("county_name"))	
  
  persons_occupation_df <- persons_occupation_df %>%	
    select(-county)
 
  # Create census tract ID
  write.csv(persons_occupation_df, paste(censusDownloadDir, "occupation_county_2000.csv", sep = "\\"), row.names = F)
}


# PROCESS DATA
###############

# read Census data from Census download directory
hhsize_BG   <- read.csv(paste(censusDownloadDir, "hhsize_BG_2000_sf1.csv", sep = "\\"))
#hhtype_BG   <- read.csv(paste(censusDownloadDir, "hhtype_BG_2000_sf1.csv", sep = "\\"))
hhpkid_CT   <- read.csv(paste(censusDownloadDir, "hhpkid_CT_2000_sf1.csv", sep = "\\"))
pop_BG      <- read.csv(paste(censusDownloadDir, "pop_BG_2000_sf1.csv", sep = "\\"))
#hhincome_CT <- read.csv(paste(censusDownloadDir, "hhincome_CT_2011_acs5.csv", sep = "\\"))
gqPop_CT    <- read.csv(paste(censusDownloadDir, "GQPop_CT_2000_sf1.csv", sep = "\\"))
hhworker_CT <- read.csv(paste(censusDownloadDir, "hhworker_tract_2000.csv", sep = "\\"))
occupation_CY <- read.csv(paste(censusDownloadDir, "occupation_county_2000.csv", sep = "\\"))

# get relevant census tract ID
relevant_tracts <- unique(hhpkid_CT$CTIDFP00)

hhworker_CT <- hhworker_CT[hhworker_CT$CTIDFP00 %in% relevant_tracts,]
hhworker_CT[is.na(hhworker_CT)] <- 0

# Total households in Census
totHHs <- sum(hhsize_BG$H013001)
# Total population as per Census
totPop <- sum(pop_BG$P012001)


# **************************
# Create MAZ control file
# **************************

mazControl <- MAZ_BG00
mazControl$HH <- mazData$Total[match(mazControl$MAZ_ORIGINAL, mazData$MAZ)]

# HH Size
#--------
mazControl <- mazControl %>% 
  group_by(BKGPIDFP00) %>%
  mutate(BKGP_HH = sum(HH)) %>%
  mutate(MAZ_Percent = ifelse(BKGP_HH>0,HH/BKGP_HH,0)) %>%  # compute proportions by hh across MAZ within ech block group
  left_join(hhsize_BG, by = "BKGPIDFP00") %>% 
  mutate(TOT_HH   = MAZ_Percent*H013001) %>%                 # allocate to each MAZ from block group based on proportion
  mutate(HHSize1  = MAZ_Percent*H013002) %>% 
  mutate(HHSize2  = MAZ_Percent*H013003) %>% 
  mutate(HHSize3  = MAZ_Percent*H013004) %>% 
  mutate(HHSize4p = MAZ_Percent*(H013005 + H013006 + H013007 + H013008)) %>%
  ungroup()

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
  select(-H013001, -H013002, -H013003, -H013004, -H013005, -H013006, -H013007, -H013008)

# Persons by Gender and Age 
#--------------------------
mazControl <- mazControl %>%
  left_join(pop_BG, by = "BKGPIDFP00") %>%
  mutate(Tot_Pop   = MAZ_Percent * P012001) %>%
  mutate(Male      = MAZ_Percent * P012002) %>%
  mutate(Female    = (Tot_Pop - Male)) %>%
  mutate(Age_00_05 = MAZ_Percent * (P012003 + P012027)) %>%
  mutate(Age_05_09 = MAZ_Percent * (P012004 + P012028)) %>%	
  mutate(Age_10_14 = MAZ_Percent * (P012005 + P012029)) %>%	
  mutate(Age_15_17 = MAZ_Percent * (P012006 + P012030)) %>%	
  mutate(Age_18_19 = MAZ_Percent * (P012007 + P012031)) %>%	
  mutate(Age_20_20 = MAZ_Percent * (P012008 + P012032)) %>%	
  mutate(Age_21_21 = MAZ_Percent * (P012009 + P012033)) %>%	
  mutate(Age_22_24 = MAZ_Percent * (P012010 + P012034)) %>%	
  mutate(Age_25_29 = MAZ_Percent * (P012011 + P012035)) %>%	
  mutate(Age_30_34 = MAZ_Percent * (P012012 + P012036)) %>%	
  mutate(Age_35_39 = MAZ_Percent * (P012013 + P012037)) %>%	
  mutate(Age_40_44 = MAZ_Percent * (P012014 + P012038)) %>%	
  mutate(Age_45_49 = MAZ_Percent * (P012015 + P012039)) %>%	
  mutate(Age_50_54 = MAZ_Percent * (P012016 + P012040)) %>%	
  mutate(Age_55_59 = MAZ_Percent * (P012017 + P012041)) %>%	
  mutate(Age_60_61 = MAZ_Percent * (P012018 + P012042)) %>%	
  mutate(Age_62_64 = MAZ_Percent * (P012019 + P012043)) %>%	
  mutate(Age_65_66 = MAZ_Percent * (P012020 + P012044)) %>%	
  mutate(Age_67_69 = MAZ_Percent * (P012021 + P012045)) %>%	
  mutate(Age_70_74 = MAZ_Percent * (P012022 + P012046)) %>%	
  mutate(Age_75_79 = MAZ_Percent * (P012023 + P012047)) %>%	
  mutate(Age_80_84 = MAZ_Percent * (P012024 + P012048)) %>%	
  mutate(Age_85_up = MAZ_Percent * (P012025 + P012049)) %>%	
  mutate(Age_00_19 = Age_00_05 + Age_05_09 + Age_10_14 + Age_15_17 + Age_18_19) %>%	
  mutate(Age_20_34 = Age_20_20 + Age_21_21 + Age_22_24 + Age_25_29 + Age_30_34) %>%	
  mutate(Age_35_64 = Age_35_39 + Age_40_44 + Age_45_49 + Age_50_54 + Age_55_59 + Age_60_61 + Age_62_64) %>%	
  mutate(Age_65_up = Age_65_66 + Age_67_69 + Age_70_74 + Age_75_79 + Age_80_84 + Age_85_up)

  #mutate(GQ_00_17  = MAZ_Percent * (P0430003 + P0430034)) %>%	
  #mutate(GQ_18_64  = MAZ_Percent * (P0430013 + P0430044)) %>%	
  #mutate(GQ_65_up  = MAZ_Percent * (P0430023 + P0430054)) %>%	
  

# rescale to match total pop in Census
totPop_temp <- sum(mazControl$Tot_Pop)
totPop_temp1 <- sum(mazControl$Age_00_19 + mazControl$Age_20_34 + mazControl$Age_35_64 + mazControl$Age_65_up) # to fix rounding errors
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
  mutate(Age_00_19 = Age_00_19 * totPop/totPop_temp1) %>%	
  mutate(Age_20_34 = Age_20_34 * totPop/totPop_temp1) %>%	
  mutate(Age_35_64 = Age_35_64 * totPop/totPop_temp1) %>%	
  mutate(Age_65_up = Age_65_up * totPop/totPop_temp1)

# select variables to be included in final TAZ control file
mazControl <- mazControl %>%
  select(MAZSEQ, MAZ_ORIGINAL, HH, BKGPIDFP00, MAZ_Percent, TOT_HH, HHSize1, HHSize2, HHSize3, HHSize4p, 
         Tot_Pop, Male, Female, Age_00_05, Age_05_09, Age_10_14, 
         Age_15_17, Age_18_19, Age_20_20, Age_21_21, Age_22_24, Age_25_29, Age_30_34, Age_35_39, Age_40_44, 
         Age_45_49, Age_50_54, Age_55_59, Age_60_61, Age_62_64, Age_65_66, Age_67_69, Age_70_74, 
         Age_75_79, Age_80_84, Age_85_up, Age_00_19, Age_20_34, Age_35_64, Age_65_up)

# write outputs
write.csv(mazControl, paste(outputDir, "mazAllocatedCensus.csv", sep = "\\"), row.names = F)


# **************************
# Create TAZ control file
# **************************

#tazControl <- TAZ_CT10
tazControl <- mazControl[,c("MAZSEQ", "MAZ_ORIGINAL", "HH", "HHSize1", "HHSize2", "HHSize3", "HHSize4p", "Tot_Pop", 
                            "Age_00_19", "Age_20_34", "Age_35_64", "Age_65_up")] %>%
  left_join(popsyn_xwalk[,c("MAZ_ORIGINAL", "TAZ", "TAZ_ORIGINAL", "PUMA5CE00")], by = "MAZ_ORIGINAL") %>%
  mutate(PUMA = PUMA5CE00) %>%
  select(-PUMA5CE00) %>%
  left_join(PUMA_Dist[,c("PUMA", "avg_size_4p")], by = "PUMA") %>%
  group_by(TAZ) %>%
  summarise(TAZSEQ = max(TAZ), TAZ_ORIGINAL = max(TAZ_ORIGINAL), TAZ_HH = sum(HH), HH1 = sum(HHSize1), HH2 = sum(HHSize2)
            , HH3=sum(HHSize3), HH4=sum(HHSize4p), TAZ_Pop=sum(Tot_Pop), avg_size_4p = max(avg_size_4p)
            , Age_00_19 = sum(Age_00_19), Age_20_34 = sum(Age_20_34), Age_35_64 = sum(Age_35_64), Age_65_up = sum(Age_65_up)) %>%
  select(-TAZ) %>%
  mutate(TOT_GQ = pmax(TAZ_Pop - (HH1 + 2*HH2 + 3*HH3 + avg_size_4p*HH4), 0)) %>%
  select(-HH1, -HH2, -HH3, -HH4) %>%
  left_join(TAZ_CT00, by = "TAZSEQ") %>%
  group_by(CTIDFP00) %>%
  mutate(CT00_HH = sum(TAZ_HH)) %>%
  mutate(CT00_GQ = sum(TOT_GQ)) %>%
  mutate(CT00_Age_00_19 = sum(Age_00_19)) %>%
  mutate(CT00_Age_20_34 = sum(Age_20_34)) %>%
  mutate(CT00_Age_35_64 = sum(Age_35_64)) %>%
  mutate(CT00_Age_65_up = sum(Age_65_up)) %>%
  mutate(TAZ_Percent00 = ifelse(CT00_HH>0,TAZ_HH/CT00_HH,0)) %>%
  mutate(TAZGQ_Percent00 = ifelse(CT00_GQ>0,TOT_GQ/CT00_GQ,0)) %>%
  ungroup() 

# Presence of Children
#---------------------
tazControl <- tazControl %>%
  left_join(hhpkid_CT, by = "CTIDFP00") %>%
  mutate(TOT_HH  = TAZ_Percent00 * (P019002 + P019011)) %>%
  mutate(WO_KIDS = TAZ_Percent00 * (P019011)) %>%
  mutate(WI_KIDS = TAZ_Percent00 * (P019002))

# rescale to match total HHs in Census
totHHs_temp <- sum(tazControl$TOT_HH)
tazControl <- tazControl %>% 
  mutate(TOT_HH  = TOT_HH  * totHHs/totHHs_temp) %>%
  mutate(WI_KIDS = WI_KIDS * totHHs/totHHs_temp) %>%
  mutate(WO_KIDS = WO_KIDS * totHHs/totHHs_temp)

# Number of Workers [CTPP2000]
#---------------------
tazControl <- tazControl %>%
  left_join(hhworker_CT, by = "CTIDFP00") %>%
  mutate(WORKERS0 = TAZ_Percent00 * (hh_workers_0)) %>%
  mutate(WORKERS1 = TAZ_Percent00 * (hh_workers_1)) %>%
  mutate(WORKERS2 = TAZ_Percent00 * (hh_workers_2)) %>%
  mutate(WORKERS3 = TAZ_Percent00 * (hh_workers_3_plus))


# rescale to match total HHs in Census
totHHs_temp <- sum(tazControl$TOT_HH)
tazControl <- tazControl %>% 
  mutate(WORKERS0 = WORKERS0 * totHHs/totHHs_temp) %>%
  mutate(WORKERS1 = WORKERS1 * totHHs/totHHs_temp) %>%
  mutate(WORKERS2 = WORKERS2 * totHHs/totHHs_temp) %>%
  mutate(WORKERS3 = WORKERS3 * totHHs/totHHs_temp) 
  
# Household Income [Existing TAZ data]
#---------------------
tazControl <- tazControl %>% 
  left_join(tazData, by = "TAZ_ORIGINAL")

# GQ Population by age 
#---------------------
totGQ <- sum(gqPop_CT$PCT017003+gqPop_CT$PCT017015+gqPop_CT$PCT017027+gqPop_CT$PCT017040+gqPop_CT$PCT017052+gqPop_CT$PCT017064)
tazControl <- tazControl %>%
  left_join(gqPop_CT[,c("PCT017003", "PCT017015", "PCT017027", "PCT017040", "PCT017052", "PCT017064", "CTIDFP00")], by = "CTIDFP00") %>%
  mutate(CT_GQ_00_17  = (PCT017003 + PCT017040)) %>%
  mutate(CT_GQ_18_64  = (PCT017015 + PCT017052)) %>%
  mutate(CT_GQ_65_up  = (PCT017027 + PCT017064)) %>% 
  mutate(GQ_00_17  = TAZGQ_Percent00 * (PCT017003 + PCT017040)) %>%
  mutate(GQ_18_64  = TAZGQ_Percent00 * (PCT017015 + PCT017052)) %>%
  mutate(GQ_65_up  = TAZGQ_Percent00 * (PCT017027 + PCT017064)) 

# rescale to match total GQs in Census
totGQ_temp <- sum(tazControl$GQ_00_17+tazControl$GQ_18_64+tazControl$GQ_65_up)
tazControl <- tazControl %>% 
  mutate(GQ_00_17  = GQ_00_17  * totGQ/totGQ_temp) %>%
  mutate(GQ_18_64  = GQ_18_64  * totGQ/totGQ_temp) %>%
  mutate(GQ_65_up  = GQ_65_up  * totGQ/totGQ_temp)  

# select variables to be included in final TAZ control file
tazControl <- tazControl %>%
  select(TAZSEQ, TAZ_ORIGINAL, TAZ_HH, CTIDFP00, TAZ_Percent00, TOT_HH, WI_KIDS, WO_KIDS,
         WORKERS0, WORKERS1, WORKERS2, WORKERS3,
         GQ_00_17, GQ_18_64, GQ_65_up, TOT_GQ, CT00_GQ,CT_GQ_00_17, CT_GQ_18_64, CT_GQ_65_up, 
         CT00_Age_00_19, CT00_Age_20_34, CT00_Age_35_64, CT00_Age_65_up, 
         INC00_30 = inc_00_30, INC30_60 = inc_30_60, INC60_100 = inc_60_100, INC100p = inc_100p)

# set NAs to zero
tazControl[is.na(tazControl)] <- 0

# write outputs
write.csv(tazControl, paste(outputDir, "tazAllocatedCensus.csv", sep = "\\"), row.names = F)

# **************************
# Create County control file
# **************************

countyControl <- persons_occupation_df 

#--------------------------------------------------------------------------------------------------------------
# *********************************
# Prepare control files for PopSyn
# *********************************

# Create MAZ level control file
mazControlFile <- select(mazControl, MAZSEQ, MAZ_ORIGINAL, HH, HHSize1, HHSize2, HHSize3, HHSize4p, 
                         Male, Female, Age_00_19, Age_20_34, Age_35_64, Age_65_up) %>%
  mutate(Pop = Male+Female)


totControlHHs <- sum(mazControlFile$HH)
totControlPop <- sum(mazControlFile$Male+mazControlFile$Female)


# Scale controls to match total HH & population
mazControlFile <- mazControlFile %>%
  mutate(tempSum   = pmax(HHSize1+HHSize2+HHSize3+HHSize4p, 0.001)) %>%
  mutate(HHSize1   = HHSize1   * HH/tempSum) %>%
  mutate(HHSize2   = HHSize2   * HH/tempSum) %>%
  mutate(HHSize3   = HHSize3   * HH/tempSum) %>%
  mutate(HHSize4p  = HHSize4p  * HH/tempSum) %>%
  mutate(tempSum   = pmax(Male+Female, 0.001)) %>%
  mutate(Male      = Male      * Pop/tempSum) %>%
  mutate(Female    = Female    * Pop/tempSum) %>%
  mutate(tempSum   = pmax(Age_00_19+Age_20_34+Age_35_64+Age_65_up, 0.001)) %>%
  mutate(Age_00_19 = Age_00_19 * Pop/tempSum) %>%
  mutate(Age_20_34 = Age_20_34 * Pop/tempSum) %>%
  mutate(Age_35_64 = Age_35_64 * Pop/tempSum) %>%
  mutate(Age_65_up = Age_65_up * Pop/tempSum) %>%
  mutate(Pop       = Male + Female)
  
# round all controls to integers
mazControlFile <- round(mazControlFile)

# Adjust each control to match total HH/population for each MAZ
mazControlFile <- mazControlFile %>%
  mutate(HHSize_Diff = HH - HHSize1-HHSize2-HHSize3-HHSize4p) %>%
  mutate(HHSize_Max  = pmax(HHSize1,HHSize2,HHSize3,HHSize4p)) %>%
  mutate(HHSize1     = ifelse(HHSize_Diff!=0 & HHSize1 ==HHSize_Max, HHSize1 +HHSize_Diff, HHSize1 )) %>%
  mutate(HHSize_Diff = HH - HHSize1-HHSize2-HHSize3-HHSize4p) %>%
  mutate(HHSize2     = ifelse(HHSize_Diff!=0 & HHSize2 ==HHSize_Max, HHSize2 +HHSize_Diff, HHSize2 )) %>%
  mutate(HHSize_Diff = HH - HHSize1-HHSize2-HHSize3-HHSize4p) %>%
  mutate(HHSize3     = ifelse(HHSize_Diff!=0 & HHSize3 ==HHSize_Max, HHSize3 +HHSize_Diff, HHSize3 )) %>%
  mutate(HHSize_Diff = HH - HHSize1-HHSize2-HHSize3-HHSize4p) %>%
  mutate(HHSize4p    = ifelse(HHSize_Diff!=0 & HHSize4p==HHSize_Max, HHSize4p+HHSize_Diff, HHSize4p)) %>%
  mutate(Gender_Diff = Pop - Male-Female) %>%
  mutate(Gender_Max  = pmax(Male,Female)) %>%
  mutate(Male        = ifelse(Gender_Diff!=0 & Male ==Gender_Max, Male +Gender_Diff, Male)) %>%
  mutate(Gender_Diff = Pop - Male-Female) %>%
  mutate(Female      = ifelse(Gender_Diff!=0 & Female ==Gender_Max, Female +Gender_Diff, Female)) %>%
  mutate(Age2_Diff   = Pop - Age_00_19-Age_20_34-Age_35_64-Age_65_up) %>%
  mutate(Age2_Max    = pmax(Age_00_19,Age_20_34,Age_35_64,Age_65_up)) %>%
  mutate(Age_00_19   = ifelse(Age2_Diff!=0 & Age_00_19 == Age2_Max, Age_00_19 +Age2_Diff, Age_00_19)) %>%
  mutate(Age2_Diff   = Pop - Age_00_19-Age_20_34-Age_35_64-Age_65_up) %>%
  mutate(Age_20_34   = ifelse(Age2_Diff!=0 & Age_20_34 == Age2_Max, Age_20_34 +Age2_Diff, Age_20_34)) %>%
  mutate(Age2_Diff   = Pop - Age_00_19-Age_20_34-Age_35_64-Age_65_up) %>%
  mutate(Age_35_64   = ifelse(Age2_Diff!=0 & Age_35_64 == Age2_Max, Age_35_64 +Age2_Diff, Age_35_64)) %>%
  mutate(Age2_Diff   = Pop - Age_00_19-Age_20_34-Age_35_64-Age_65_up) %>%
  mutate(Age_65_up   = ifelse(Age2_Diff!=0 & Age_65_up == Age2_Max, Age_65_up +Age2_Diff, Age_65_up))

# TAZ Control File
#----------------

# Aggregate maz controls file to get HHs and population at TAZ level
tazMazAgg <- mazControlFile %>% 
  left_join(popsyn_xwalk[,c("MAZ_ORIGINAL", "TAZ", "TAZ_ORIGINAL")], by = "MAZ_ORIGINAL") %>%
  group_by(TAZ) %>%
  summarise(TAZSEQ = max(TAZ), TAZ_ORIGINAL = max(TAZ_ORIGINAL), TAZ_HH = sum(HH), TAZ_POP = sum(Pop), 
            Age_00_19 = sum(Age_00_19),Age_20_34 = sum(Age_20_34),Age_35_64 = sum(Age_35_64),Age_65_up = sum(Age_65_up)) %>%
  ungroup()

tazControlFile <- select(tazControl, TAZSEQ, TAZ_ORIGINAL, WI_KIDS, WO_KIDS, 
                         GQ_00_17, GQ_18_64, 
                         GQ_65_up, TOT_GQ, CT00_GQ, CT_GQ_00_17, CT_GQ_18_64, CT_GQ_65_up, 
                         CT00_Age_00_19, CT00_Age_20_34, CT00_Age_35_64, CT00_Age_65_up,
                         WORKERS0, WORKERS1, WORKERS2, WORKERS3,
                         INC00_30, INC30_60, INC60_100, INC100p) %>%
  mutate(TAZ_GQ = GQ_00_17+GQ_18_64+GQ_65_up)

# Scale controls to match total HH & population
tazControlFile <- tazControlFile %>%
  left_join(tazMazAgg[,c("TAZSEQ", "TAZ_HH", "TAZ_POP", "Age_00_19", "Age_20_34", "Age_35_64", "Age_65_up")], by = "TAZSEQ") %>%
  mutate(tempSum   = pmax(WI_KIDS+WO_KIDS, 0.001)) %>%
  mutate(WI_KIDS   = WI_KIDS   * TAZ_HH/tempSum) %>%
  mutate(WO_KIDS   = WO_KIDS   * TAZ_HH/tempSum) %>%
  mutate(tempSum   = pmax(WORKERS0+WORKERS1+WORKERS2+WORKERS3, 0.001)) %>%
  mutate(WORKERS0  = WORKERS0  * TAZ_HH/tempSum) %>%
  mutate(WORKERS1  = WORKERS1  * TAZ_HH/tempSum) %>%
  mutate(WORKERS2  = WORKERS2  * TAZ_HH/tempSum) %>%
  mutate(WORKERS3  = WORKERS3  * TAZ_HH/tempSum) %>%
  mutate(tempSum   = pmax(INC00_30+INC30_60+INC60_100+INC100p, 0.001)) %>%
  mutate(INC00_30  = INC00_30  * TAZ_HH/tempSum) %>%
  mutate(INC30_60  = INC30_60  * TAZ_HH/tempSum) %>%
  mutate(INC60_100 = INC60_100  * TAZ_HH/tempSum) %>%
  mutate(INC100p   = INC100p  * TAZ_HH/tempSum) %>%
  mutate(tempSum   = pmax(GQ_00_17+GQ_18_64+GQ_65_up, 0.001)) %>%
  mutate(GQ_00_17  = GQ_00_17  * TAZ_GQ/tempSum) %>%
  mutate(GQ_18_64  = GQ_18_64  * TAZ_GQ/tempSum) %>%
  mutate(GQ_65_up  = GQ_65_up  * TAZ_GQ/tempSum) %>%
  mutate(TAZ_GQ    = GQ_00_17+GQ_18_64+GQ_65_up)

# Descale TAZ level age controls to reflect on residential population
taz_puma_xwalk <- popsyn_xwalk[,c("TAZ_ORIGINAL", "PUMA5CE00")]
colnames(taz_puma_xwalk) <- c("taz_original", "PUMA")
taz_puma_xwalk  <- taz_puma_xwalk %>%
  group_by(taz_original) %>%
  summarise(PUMA = max(PUMA))

tazControlFile <- tazControlFile %>%
  left_join(taz_puma_xwalk, by = c("TAZ_ORIGINAL"="taz_original")) %>%
  left_join(PUMA_Dist[, c("PUMA","percent_18_19","percent_20_34","percent_35_64")], by = "PUMA") %>%
  mutate(Age_00_19 = round(Age_00_19 * (CT00_Age_00_19 - CT_GQ_00_17 + round(CT_GQ_18_64 * percent_18_19))/CT00_Age_00_19)) %>%
  mutate(Age_20_34 = round(Age_20_34 * (CT00_Age_20_34 - round(CT_GQ_18_64 * percent_20_34))/CT00_Age_20_34)) %>%
  mutate(Age_35_64 = round(Age_35_64 * (CT00_Age_35_64 - round(CT_GQ_18_64 * percent_35_64))/CT00_Age_35_64)) %>%
  mutate(Age_65_up = round(Age_65_up * (CT00_Age_65_up - CT_GQ_65_up)/CT00_Age_65_up)) %>%
  mutate(Age_00_19 = ifelse(is.na(Age_00_19) | (Age_00_19<0), 0, Age_00_19)) %>%
  mutate(Age_20_34 = ifelse(is.na(Age_20_34) | (Age_20_34<0), 0, Age_20_34)) %>%
  mutate(Age_35_64 = ifelse(is.na(Age_35_64) | (Age_35_64<0), 0, Age_35_64)) %>%
  mutate(Age_65_up = ifelse(is.na(Age_65_up) | (Age_65_up<0), 0, Age_65_up))

  
# round all controls to integers
tazControlFile <- round(tazControlFile)

# Adjust each control to match total HH/population for each MAZ
tazControlFile <- tazControlFile %>%
  mutate(Kids_Diff   = TAZ_HH - WI_KIDS-WO_KIDS) %>%
  mutate(Kids_Max    = pmax(WI_KIDS,WO_KIDS)) %>%
  mutate(WI_KIDS     = ifelse(Kids_Diff!=0 & WI_KIDS ==Kids_Max, WI_KIDS +Kids_Diff, WI_KIDS )) %>%
  mutate(Kids_Diff   = TAZ_HH - WI_KIDS-WO_KIDS) %>%
  mutate(WO_KIDS     = ifelse(Kids_Diff!=0 & WO_KIDS ==Kids_Max, WO_KIDS +Kids_Diff, WO_KIDS )) %>%
  mutate(Workers_Diff= TAZ_HH - WORKERS0-WORKERS1-WORKERS2-WORKERS3) %>%
  mutate(Workers_Max = pmax(WORKERS0,WORKERS1,WORKERS2,WORKERS3)) %>%
  mutate(WORKERS0    = ifelse(Workers_Diff!=0 & WORKERS0 ==Workers_Max, WORKERS0 +Workers_Diff, WORKERS0)) %>%
  mutate(Workers_Diff= TAZ_HH - WORKERS0-WORKERS1-WORKERS2-WORKERS3) %>%
  mutate(WORKERS1    = ifelse(Workers_Diff!=0 & WORKERS1 ==Workers_Max, WORKERS1 +Workers_Diff, WORKERS1)) %>%
  mutate(Workers_Diff= TAZ_HH - WORKERS0-WORKERS1-WORKERS2-WORKERS3) %>%
  mutate(WORKERS2    = ifelse(Workers_Diff!=0 & WORKERS2 ==Workers_Max, WORKERS2 +Workers_Diff, WORKERS2)) %>%
  mutate(Workers_Diff= TAZ_HH - WORKERS0-WORKERS1-WORKERS2-WORKERS3) %>%
  mutate(WORKERS3    = ifelse(Workers_Diff!=0 & WORKERS3 ==Workers_Max, WORKERS3 +Workers_Diff, WORKERS3)) %>%
  mutate(GQ_Diff     = TAZ_GQ - GQ_00_17-GQ_18_64-GQ_65_up) %>%
  mutate(GQ_Max      = pmax(GQ_00_17,GQ_18_64,GQ_65_up)) %>%
  mutate(GQ_00_17    = ifelse(GQ_Diff!=0 & GQ_00_17 ==GQ_Max, GQ_00_17 + GQ_Diff, GQ_00_17)) %>%
  mutate(GQ_Diff     = TAZ_GQ - GQ_00_17-GQ_18_64-GQ_65_up) %>%
  mutate(GQ_18_64    = ifelse(GQ_Diff!=0 & GQ_18_64 ==GQ_Max, GQ_18_64 + GQ_Diff, GQ_18_64)) %>%
  mutate(GQ_Diff     = TAZ_GQ - GQ_00_17-GQ_18_64-GQ_65_up) %>%
  mutate(GQ_65_up    = ifelse(GQ_Diff!=0 & GQ_65_up ==GQ_Max, GQ_65_up + GQ_Diff, GQ_65_up))

# County Control File
#----------------
countyControlFile <- countyControl 

#-------------------------------------------------------------------------------------------------------------- 
# Write control files
#--------------------
dir.create(file.path(INTERMEDIATE_DIR, YEAR), showWarnings=FALSE)

# -HH, -Pop,
mazControlFile <- select(mazControlFile, -MAZSEQ, -HHSize_Diff, -HHSize_Max, 
                         -Gender_Diff, -Gender_Max, -Age2_Diff, -Age2_Max, -tempSum)
colnames(mazControlFile)[colnames(mazControlFile) == 'MAZ_ORIGINAL'] <- 'maz_original'
mazControlFile[is.na(mazControlFile)] <-0
write.csv(mazControlFile, file.path(INTERMEDIATE_DIR, YEAR, "mazControlFile.csv"), row.names = F)

# -TAZ_HH, -TAZ_POP,
tazControlFile <- select(tazControlFile, -TAZSEQ, -Kids_Diff, -Kids_Max,  -Workers_Diff, -Workers_Max,
                         -GQ_Diff, -GQ_Max, -TAZ_HH, -TAZ_POP, -GQ_00_17, -GQ_18_64, -GQ_65_up, 
                         -TOT_GQ,	-CT00_GQ,	-CT_GQ_00_17,	-CT_GQ_18_64,	-CT_GQ_65_up,	-CT00_Age_00_19,	
                         -CT00_Age_20_34,	-CT00_Age_35_64,	-CT00_Age_65_up,
                         -percent_18_19,-percent_20_34,-percent_35_64, -PUMA, -tempSum)
colnames(tazControlFile)[colnames(tazControlFile) == 'TAZ_ORIGINAL'] <- 'taz_original'
tazControlFile[is.na(tazControlFile)] <-0
write.csv(tazControlFile, file.path(INTERMEDIATE_DIR, YEAR, "tazControlFile.csv"), row.names = F)

countyControlFile <- select(countyControlFile, mtc_county_id, county_name, occupation_management, occupation_manual, 
                            occupation_military, occupation_professional,occupation_retail, 
                            occupation_services)
countyControlFile[is.na(countyControlFile)] <-0
write.csv(countyControlFile, file.path(INTERMEDIATE_DIR, YEAR, "countyControlFile.csv"), row.names = F)
