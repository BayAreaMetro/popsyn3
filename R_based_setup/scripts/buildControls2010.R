############################################################################################################
# Script to download BKGP and Tract level Decennial Census and ACS data and build controls for year 2010
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
message("=== Running buildControls2010.R")

# LIBRARIES
############
source(paste(WORKING_DIR, "scripts\\downloadCensusData.R", sep = "\\"))
suppressMessages(library(dplyr))
suppressMessages(library(RMySQL))	
suppressMessages(library(stringr)	)
suppressMessages(library(reshape2))

# INPUTS
#########

outputDir <- paste(WORKING_DIR, "data\\CensusData2010", sep = "\\")
censusDownloadDir <- paste(outputDir, "Downloads", sep = "\\")
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
mazData    <- read.csv(paste(WORKING_DIR, "data\\2010\\mazData.csv", sep = "\\"))
tazData    <- read.csv(paste(WORKING_DIR, "data\\2010\\tazData.csv", sep = "\\"))
colnames(tazData) <- c("TAZ_ORIGINAL", "inc_00_30", "inc_30_60", "inc_60_100", "inc_100p")
countyData <- read.csv(paste(WORKING_DIR, "data\\2010\\countyData.csv", sep = "\\"))

# Read PopSyn XWalk
popsyn_xwalk <- read.csv(paste(WORKING_DIR, "data\\geographicCWalk.csv", sep = "\\"))
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

# read 2007-11 PUMS person and HH data
mysql_connection <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)

pums_pop_10 <- dbReadTable(conn = mysql_connection, name = paste('person_table', "2007_2011", sep = '_'))	
pums_pop_10 <- pums_pop_10[pums_pop_10$PUMA %in% relevant_pumas,]
pums_pop <- pums_pop_10

pums_hh_10 <- dbReadTable(conn = mysql_connection, name = paste('household_table', "2007_2011", sep = '_'))	
pums_hh_10 <- pums_hh_10[pums_hh_10$PUMA %in% relevant_pumas,]
pums_hh <- pums_hh_10

dbDisconnect(mysql_connection)
	
#createGQDist <- F
#if(createGQDist){
#  # Create distribution of GQ population by age for each PUMA [run only once]
#  # Non-Institutional GQ population
#  pums_pop <- pums_pop %>%
#    left_join(pums_hh[,c("SERIALNO", "WGTP", "TYPE")],  by = c("SERIALNO")) %>%
#    filter(TYPE >= 2) %>%  # GQ population
#    mutate(age_group3 = ifelse(AGEP>=35 & AGEP<=64, 3, 0)) %>%
#    mutate(age_group2 = ifelse(AGEP>=20 & AGEP<=34, 2, 0)) %>%
#    mutate(age_group1 = ifelse(AGEP>=18 & AGEP<=19, 1, 0)) %>%
#    group_by(PUMA)  %>%
#    summarise(age_group1 = sum(age_group1 * PWGTP), age_group2 = sum(age_group2 * PWGTP), age_group3 = sum(age_group3 * PWGTP)) %>%
#    mutate(percent_18_19 = age_group1/(age_group1+age_group2+age_group3)) %>%
#    mutate(percent_20_34 = age_group2/(age_group1+age_group2+age_group3)) %>%
#    mutate(percent_35_64 = age_group3/(age_group1+age_group2+age_group3)) %>%
#    ungroup()
#  pums_hh <- pums_hh %>%
#    filter(TYPE == 1) %>%
#    mutate(hhsize_4p = ifelse(NP>=4, 1, 0)) %>%
#    group_by(PUMA)  %>%
#    summarise(avg_size_4p = sum(hhsize_4p*WGTP*NP)/sum(hhsize_4p*WGTP)) %>%
#    ungroup()
#   
#    pums_pop <- pums_pop %>%
#      left_join(pums_hh, by = "PUMA")
#    
#  write.csv(pums_pop, paste(outputDir, "PUMA_Distributions.csv", sep = "\\"), row.names = F)
#}

PUMA_Dist <- read.csv(paste(outputDir, "PUMA_Distributions.csv", sep = "\\"))

# Read MAZ-BLK_GRP crosswalk
MAZ_BG10 <- read.csv(paste(DATA_DIR, "GeographicXWalk\\GeogXWalk2010_MAZ_BG.csv", sep = "\\"))

# Read TAZ-CT XWalk
TAZ_CT10 <- read.csv(paste(DATA_DIR, "GeographicXWalk\\GeogXWalk2010_TAZ_CT_PUMA_CY.csv", sep = "\\"))
TAZ_CT00 <- read.csv(paste(DATA_DIR, "GeographicXWalk\\GeogXWalk2000_TAZ_CT_PUMA_CY.csv", sep = "\\"))


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
                             vars=c("PCT0160001","PCT0160004", "PCT0160010", "PCT0160016", "PCT0160022"), 	
                             region = paste("tract:", "*", sep = ""),	
                             regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "sf1", 	
                               vintage = 2010, 	
                               key = api_key$key, 	
                               vars=c("PCT0160001","PCT0160004", "PCT0160010", "PCT0160016", "PCT0160022"), 	
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
                                 "P0120041", "P0120042", "P0120043", "P0120044", "P0120045", "P0120046", "P0120047", "P0120048",
                                 "P0120049"), 	
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
                                      "P0120041", "P0120042", "P0120043", "P0120044", "P0120045", "P0120046", "P0120047", "P0120048",
                                      "P0120049"), 	
                               region = paste("block+group:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    pop_BG <- rbind(pop_BG, temp_data)
  }
  # Create blcok group ID
  pop_BG$BKGPIDFP10 <- pop_BG$state * 10000000000 + pop_BG$county * 10000000 + pop_BG$tract * 10 + pop_BG$`block group`
  write.csv(pop_BG, paste(censusDownloadDir, "pop_BG_2010_sf1.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "GQPop_CT_2010_sf1.csv", sep = "\\"))){
  # GQ Population by Gender and Age at Census Tract Level [2010 decennial, SF1	]
  #----------------------------------------------------------
  gqPop_CT <- getCensusData(name = "sf1", 	
                             vintage = 2010, 	
                             key = api_key$key, 	
                             vars=c("P0430003", "P0430013", "P0430023", "P0430034", "P0430044", "P0430054"), 	
                             region = paste("tract:", "*", sep = ""),	
                             regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "sf1", 	
                               vintage = 2010, 	
                               key = api_key$key, 	
                               vars=c("P0430003", "P0430013", "P0430023", "P0430034", "P0430044", "P0430054"), 	
                               region = paste("tract:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    gqPop_CT <- rbind(gqPop_CT, temp_data)
  }
  # Create census tract ID
  gqPop_CT$CTIDFP10 <- gqPop_CT$state * 1000000000 + gqPop_CT$county * 1000000 + gqPop_CT$tract
  write.csv(gqPop_CT, paste(censusDownloadDir, "GQPop_CT_2010_sf1.csv", sep = "\\"), row.names = F)
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

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "hhincome_CT_2011_acs5.csv", sep = "\\"))){
  # HHs by income at Census Tract Level [2007-11 5 year ACS, http://api.census.gov/data/2011/acs5/variables.html]
  #----------------------------------------------------------
  hhincome_CT <- getCensusData(name = "acs5", 	
                               vintage = 2011, 	
                               key = api_key$key, 	
                               vars=c("B19001_001E", "B19001_002E", "B19001_003E", "B19001_004E", "B19001_005E", "B19001_006E", 
                                      "B19001_007E", "B19001_008E", "B19001_009E", "B19001_010E", "B19001_011E", 
                                      "B19001_012E", "B19001_013E", "B19001_014E", "B19001_015E", "B19001_016E", "B19001_017E"), 	
                               region = paste("tract:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))	
  
  for (cen in COUNTY_FIPS_STRING[-1]){
    temp_data <- getCensusData(name = "acs5", 	
                               vintage = 2011, 	
                               key = api_key$key, 	
                               vars=c("B19001_001E", "B19001_002E", "B19001_003E", "B19001_004E", "B19001_005E", "B19001_006E", 
                                      "B19001_007E", "B19001_008E", "B19001_009E", "B19001_010E", "B19001_011E", 
                                      "B19001_012E", "B19001_013E", "B19001_014E", "B19001_015E", "B19001_016E", "B19001_017E"), 	
                               region = paste("tract:", "*", sep = ""),	
                               regionin = paste("state:", STATE_FIPS, "+county:", cen, sep = ""))
    
    hhincome_CT <- rbind(hhincome_CT, temp_data)
  }
  # Create census tract ID
  hhincome_CT$CTIDFP10 <- hhincome_CT$state * 1000000000 + hhincome_CT$county * 1000000 + hhincome_CT$tract
  write.csv(hhincome_CT, paste(censusDownloadDir, "hhincome_CT_2011_acs5.csv", sep = "\\"), row.names = F)
}

if(downloadCensus==TRUE | !file.exists(paste(censusDownloadDir, "occupation_county_2010.csv", sep = "\\"))){
  # Persons by occupation type at County Level [PUMS data]
  #----------------------------------------------------------
  # TODO fix the NA removal -- why are weights NA?	
  working <- pums_pop_10 %>%	
    select(PUMA, occupation, WGTP) %>%	
    filter(!(is.na(WGTP))) %>%	
    group_by(PUMA, occupation) %>%	
    summarise(persons = sum(WGTP)) %>%	
    ungroup() %>%	
    filter(!(is.na(occupation)))	
  
  sum(working$NP)	
  
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
  write.csv(persons_occupation_df, paste(censusDownloadDir, "occupation_county_2010.csv", sep = "\\"), row.names = F)
}

# PROCESS DATA
###############

# read Census data from Census download directory
hhsize_BG   <- read.csv(paste(censusDownloadDir, "hhsize_BG_2010_sf1.csv", sep = "\\"))
hhtype_BG   <- read.csv(paste(censusDownloadDir, "hhtype_BG_2010_sf1.csv", sep = "\\"))
hhpkid_CT   <- read.csv(paste(censusDownloadDir, "hhpkid_CT_2010_sf1.csv", sep = "\\"))
pop_BG      <- read.csv(paste(censusDownloadDir, "pop_BG_2010_sf1.csv", sep = "\\"))
hhworker_CT <- read.csv(paste(censusDownloadDir, "hhworker_CT_2011_acs5.csv", sep = "\\"))
hhincome_CT <- read.csv(paste(censusDownloadDir, "hhincome_CT_2011_acs5.csv", sep = "\\"))
gqPop_CT    <- read.csv(paste(censusDownloadDir, "GQPop_CT_2010_sf1.csv", sep = "\\"))
persons_occupation_df <- read.csv(paste(censusDownloadDir, "occupation_county_2010.csv", sep = "\\"))

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
  mutate(HHSize4p = MAZ_Percent*(H0130005 + H0130006 + H0130007 + H0130008)) %>%
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
  select(MAZSEQ, MAZ_ORIGINAL, HH, BKGPIDFP10, MAZ_Percent, TOT_HH, HHSize1, HHSize2, HHSize3, HHSize4p, 
         HHType1, HHType2, HHType3, HHType4, HHType5, Tot_Pop, Male, Female, Age_00_05, Age_05_09, Age_10_14, 
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
  left_join(TAZ_CT10, by = "TAZSEQ") %>%
  group_by(CTIDFP10) %>%
  mutate(CT10_HH = sum(TAZ_HH)) %>%
  mutate(CT10_GQ = sum(TOT_GQ)) %>%
  mutate(CT10_Age_00_19 = sum(Age_00_19)) %>%
  mutate(CT10_Age_20_34 = sum(Age_20_34)) %>%
  mutate(CT10_Age_35_64 = sum(Age_35_64)) %>%
  mutate(CT10_Age_65_up = sum(Age_65_up)) %>%
  mutate(TAZ_Percent10 = ifelse(CT10_HH>0,TAZ_HH/CT10_HH,0)) %>%
  mutate(TAZGQ_Percent10 = ifelse(CT10_GQ>0,TOT_GQ/CT10_GQ,0)) %>%
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
  mutate(WO_KIDS = TAZ_Percent10 * (PCT0160004 + PCT0160010 + PCT0160016 + PCT0160022)) %>%
  mutate(WI_KIDS = TOT_HH - WO_KIDS)

# rescale to match total HHs in Census
totHHs_temp <- sum(tazControl$TOT_HH)
tazControl <- tazControl %>% 
  mutate(TOT_HH  = TOT_HH  * totHHs/totHHs_temp) %>%
  mutate(WI_KIDS = WI_KIDS * totHHs/totHHs_temp) %>%
  mutate(WO_KIDS = WO_KIDS * totHHs/totHHs_temp)
  
# Number of Workers [ACS 2007-11, Census Tracts in Census 2010 geography]
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

# Household Income [ACS 2007-11, Census Tracts in Census 2010 geography]
#---------------------
tazControl <- tazControl %>%
  left_join(hhincome_CT, by = "CTIDFP10") %>%
  mutate(TOT_HH    = TAZ_Percent10 * (B19001_001E)) %>%
  mutate(INC00_30  = TAZ_Percent10 * (B19001_002E + B19001_003E + B19001_004E + B19001_005E + B19001_006E)) %>%
  mutate(INC30_60  = TAZ_Percent10 * (B19001_007E + B19001_008E + B19001_009E + B19001_010E + B19001_011E)) %>%
  mutate(INC60_100 = TAZ_Percent10 * (B19001_012E + B19001_013E)) %>%
  mutate(INC100p   = TAZ_Percent10 * (B19001_014E + B19001_015E + B19001_016E + B19001_017E))

# rescale to match total HHs in Census
totHHs_temp <- sum(tazControl$TOT_HH)
tazControl <- tazControl %>% 
  mutate(TOT_HH    = TOT_HH    * totHHs/totHHs_temp) %>%
  mutate(INC00_30  = INC00_30  * totHHs/totHHs_temp) %>%
  mutate(INC30_60  = INC30_60  * totHHs/totHHs_temp) %>%
  mutate(INC60_100 = INC60_100 * totHHs/totHHs_temp) %>%
  mutate(INC100p   = INC100p   * totHHs/totHHs_temp) 

# GQ Population by age 
#---------------------
totGQ <- sum(gqPop_CT$P0430003+gqPop_CT$P0430013+gqPop_CT$P0430023+gqPop_CT$P0430034+gqPop_CT$P0430044+gqPop_CT$P0430054)
tazControl <- tazControl %>%
  left_join(gqPop_CT[,c("P0430003", "P0430013", "P0430023", "P0430034", "P0430044", "P0430054", "CTIDFP10")], by = "CTIDFP10") %>%
  mutate(CT_GQ_00_17  = (P0430003 + P0430034)) %>%
  mutate(CT_GQ_18_64  = (P0430013 + P0430044)) %>%
  mutate(CT_GQ_65_up  = (P0430023 + P0430054)) %>% 
  mutate(GQ_00_17  = TAZGQ_Percent10 * (P0430003 + P0430034)) %>%
  mutate(GQ_18_64  = TAZGQ_Percent10 * (P0430013 + P0430044)) %>%
  mutate(GQ_65_up  = TAZGQ_Percent10 * (P0430023 + P0430054)) 

# rescale to match total GQs in Census
totGQ_temp <- sum(tazControl$GQ_00_17+tazControl$GQ_18_64+tazControl$GQ_65_up)
tazControl <- tazControl %>% 
  mutate(GQ_00_17  = GQ_00_17  * totGQ/totGQ_temp) %>%
  mutate(GQ_18_64  = GQ_18_64  * totGQ/totGQ_temp) %>%
  mutate(GQ_65_up  = GQ_65_up  * totGQ/totGQ_temp)  

# select variables to be included in final TAZ control file
tazControl <- tazControl %>%
  select(TAZSEQ, TAZ_ORIGINAL, TAZ_HH, CTIDFP10, TAZ_Percent10, TOT_HH, WI_KIDS, WO_KIDS, 
         WORKERS0, WORKERS1, WORKERS2, WORKERS3, INC00_30, INC30_60, INC60_100, INC100p, 
         GQ_00_17, GQ_18_64, GQ_65_up, TOT_GQ, CT10_GQ,CT_GQ_00_17, CT_GQ_18_64, CT_GQ_65_up, 
         CT10_Age_00_19, CT10_Age_20_34, CT10_Age_35_64, CT10_Age_65_up)

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
mazControlFile <- select(mazControl, MAZSEQ, MAZ_ORIGINAL, HH, HHSize1, HHSize2, HHSize3, HHSize4p, HHType1, HHType2, 
                         HHType3, HHType4, HHType5, Male, Female, Age_00_19, Age_20_34, Age_35_64, Age_65_up) %>%
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
  mutate(HHType1   = HHType1   * HH/tempSum) %>%
  mutate(HHType2   = HHType2   * HH/tempSum) %>%
  mutate(HHType3   = HHType3   * HH/tempSum) %>%
  mutate(HHType4   = HHType4   * HH/tempSum) %>%
  mutate(HHType5   = HHType5   * HH/tempSum) %>%
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
  mutate(HHType_Diff = HH - HHType1-HHType2-HHType3-HHType4-HHType5) %>%
  mutate(HHType_Max  = pmax(HHType1,HHType2,HHType3,HHType4,HHType5)) %>%
  mutate(HHType1     = ifelse(HHType_Diff!=0 & HHType1 ==HHType_Max, HHType1 +HHType_Diff, HHType1)) %>%
  mutate(HHType_Diff = HH - HHType1-HHType2-HHType3-HHType4-HHType5) %>%
  mutate(HHType2     = ifelse(HHType_Diff!=0 & HHType2 ==HHType_Max, HHType2 +HHType_Diff, HHType2)) %>%
  mutate(HHType_Diff = HH - HHType1-HHType2-HHType3-HHType4-HHType5) %>%
  mutate(HHType3     = ifelse(HHType_Diff!=0 & HHType3 ==HHType_Max, HHType3 +HHType_Diff, HHType3)) %>%
  mutate(HHType_Diff = HH - HHType1-HHType2-HHType3-HHType4-HHType5) %>%
  mutate(HHType4     = ifelse(HHType_Diff!=0 & HHType4 ==HHType_Max, HHType4 +HHType_Diff, HHType4)) %>%
  mutate(HHType_Diff = HH - HHType1-HHType2-HHType3-HHType4-HHType5) %>%
  mutate(HHType5     = ifelse(HHType_Diff!=0 & HHType5 ==HHType_Max, HHType5 +HHType_Diff, HHType5)) %>%
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
                         WORKERS0,WORKERS1,WORKERS2,WORKERS3, INC00_30, INC30_60, INC60_100, INC100p, GQ_00_17, GQ_18_64, 
                         GQ_65_up, TOT_GQ, CT10_GQ, CT_GQ_00_17, CT_GQ_18_64, CT_GQ_65_up, 
                         CT10_Age_00_19, CT10_Age_20_34, CT10_Age_35_64, CT10_Age_65_up) %>%
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
  mutate(INC60_100 = INC60_100 * TAZ_HH/tempSum) %>%
  mutate(INC100p   = INC100p   * TAZ_HH/tempSum) %>%
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
  mutate(Age_00_19 = round(Age_00_19 * (CT10_Age_00_19 - CT_GQ_00_17 + round(CT_GQ_18_64 * percent_18_19))/CT10_Age_00_19)) %>%
  mutate(Age_20_34 = round(Age_20_34 * (CT10_Age_20_34 - round(CT_GQ_18_64 * percent_20_34))/CT10_Age_20_34)) %>%
  mutate(Age_35_64 = round(Age_35_64 * (CT10_Age_35_64 - round(CT_GQ_18_64 * percent_35_64))/CT10_Age_35_64)) %>%
  mutate(Age_65_up = round(Age_65_up * (CT10_Age_65_up - CT_GQ_65_up)/CT10_Age_65_up)) %>%
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
  mutate(income_Diff = TAZ_HH - INC00_30-INC30_60-INC60_100-INC100p) %>%
  mutate(income_Max  = pmax(INC00_30,INC30_60,INC60_100,INC100p)) %>%
  mutate(INC00_30    = ifelse(income_Diff!=0 & INC00_30 ==income_Max, INC00_30 + income_Diff, INC00_30)) %>%
  mutate(income_Diff = TAZ_HH - INC00_30-INC30_60-INC60_100-INC100p) %>%
  mutate(INC30_60    = ifelse(income_Diff!=0 & INC30_60 ==income_Max, INC30_60 + income_Diff, INC30_60)) %>%
  mutate(income_Diff = TAZ_HH - INC00_30-INC30_60-INC60_100-INC100p) %>%
  mutate(INC60_100   = ifelse(income_Diff!=0 & INC60_100 ==income_Max, INC60_100 + income_Diff, INC60_100)) %>%
  mutate(income_Diff = TAZ_HH - INC00_30-INC30_60-INC60_100-INC100p) %>%
  mutate(INC100p     = ifelse(income_Diff!=0 & INC100p ==income_Max, INC100p + income_Diff, INC100p)) %>%
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
# -HH, -Pop,
mazControlFile <- select(mazControlFile, -MAZSEQ, -HHSize_Diff, -HHSize_Max, -HHType_Diff,  
                         -HHType_Max, -Gender_Diff, -Gender_Max, -Age2_Diff, -Age2_Max, 
                         -Male, -Female, -Age_00_19, -Age_20_34, -Age_35_64, -Age_65_up, -Pop, -tempSum)
colnames(mazControlFile)[colnames(mazControlFile) == 'MAZ_ORIGINAL'] <- 'maz_original'
write.csv(mazControlFile, paste(DATA_DIR, YEAR, "mazControlFile.csv", sep = "/"), row.names = F)

# -TAZ_HH, -TAZ_POP,
tazControlFile <- select(tazControlFile, -TAZSEQ, -Kids_Diff, -Kids_Max, -Workers_Diff, -Workers_Max, 
                         -income_Diff, -income_Max, -GQ_Diff, -GQ_Max, -TAZ_HH, -TAZ_POP, 
                         -percent_18_19,-percent_20_34,-percent_35_64, -PUMA, -tempSum,
                         -GQ_00_17,	-GQ_18_64,	-GQ_65_up,	-TOT_GQ, -CT10_GQ, -CT_GQ_00_17, -CT_GQ_18_64, 	
                         -CT_GQ_65_up, -CT10_Age_00_19, -CT10_Age_20_34, -CT10_Age_35_64, -CT10_Age_65_up, -TAZ_GQ)
colnames(tazControlFile)[colnames(tazControlFile) == 'TAZ_ORIGINAL'] <- 'taz_original'
write.csv(tazControlFile, paste(DATA_DIR, YEAR, "tazControlFile.csv", sep = "/"), row.names = F)

countyControlFile <- select(countyControlFile, mtc_county_id, county_name, occupation_management, occupation_manual, 
                            occupation_military, occupation_professional,occupation_retail, 
                            occupation_services)
countyControlFile[is.na(countyControlFile)] <-0
write.csv(countyControlFile, paste(DATA_DIR, YEAR, "countyControlFile.csv", sep = "/"), row.names = F)

  
  

  
  

