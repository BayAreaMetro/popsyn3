#' This script builds controls
#' author: Dave Ory, MTC
#' 	##############################################################


#devtools::install_github("hrecht/censusapi")	
suppressMessages(library(censusapi))	
suppressMessages(library(RMySQL))	
library(stringr)	
library(reshape2)	
suppressMessages(library(dplyr))	

#### Paramaters	
	
STATE_FIPS = "06"	
COUNTY_FIPS_STRING = "01,13,41,55,75,81,85,95,97"	
	
county_codes <- data.frame(county_name = c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma"),	
                           county = c("001","013","041","055","075","081","085","095","097"), 	
                           mtc_county_id = c(4,5,9,7,1,2,3,6,8),	
                           stringsAsFactors = FALSE)	
	
occupation_df = data.frame(occupation = c(1,2,3,4,5,6), 	
                           occupation_category = c("Management", "Professional", "Services", "Retail", "Manual", "Military"),	
                           stringsAsFactors = FALSE)	
	
MYSQL_SERVER    = "localhost"	
MYSQL_DATABASE  = "mtc_popsyn"	
MYSQL_USER_NAME = "root" 	

### Remote file locations	
	
CENSUS_API_KEY_FILE <- "E:/Projects/Clients/mtc/TO2_Task2/census_api_key.csv"	
	
MYSQL_PASSWORD_FILE <- "E:/Projects/Clients/mtc/TO2_Task2/mysql.csv"	
	
CONTROL_DIR <- "E:/Projects/Clients/mtc/TO2_Task2/MTCPopSynIII/data"	
	
GEOG_CONTROL_FILE <-   paste(CONTROL_DIR, "geographicCWalk.csv", sep = "/")	
	
### Get the Census API key	
	
api_key <- read.csv(CENSUS_API_KEY_FILE, header = TRUE)	
api_key <- api_key %>%	
  mutate(key = paste(key))	

### Build the PUMA to County cross-walk	
	
input_geog <- read.csv(GEOG_CONTROL_FILE, header = TRUE, stringsAsFactors = FALSE)

input_geog <- input_geog %>%	
  rename(county_name = COUNTYNAME)
	
puma_to_county <- input_geog %>%	
  select(PUMA = PUMA5CE00, county_name) %>%	
  group_by(PUMA, county_name) %>%	
  summarise(count = n()) %>%	
  ungroup() %>%	
  select(-count)

remove(input_geog)	

# Data reads	
	
# get password	
mysql_passes <- read.csv(MYSQL_PASSWORD_FILE, header = TRUE)	
	
mysql_passes <- mysql_passes %>%	
  filter(user == MYSQL_USER_NAME) %>%	
  mutate(pwd = paste(pwd))	
	
# connection	
mysql_connection <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)	
	
# read the synthetic population tables	
pums_household_0711 <- dbReadTable(conn = mysql_connection, name = paste('household_table', "2007_2011", sep = '_'))	
pums_household_00   <- dbReadTable(conn = mysql_connection, name = paste('household_table', "2000", sep = '_'))	
	
pums_person_0711 <- dbReadTable(conn = mysql_connection, name = paste('person_table', "2007_2011", sep = '_'))	
pums_person_00   <- dbReadTable(conn = mysql_connection, name = paste('person_table', "2000", sep = '_'))	
	
dbDisconnect(mysql_connection)	
	
#### County Control: Households by size	
 	
# 2000: use decennial, SF1	
raw_census <- getCensus(name = "sf1", 	
                        vintage = 2000, 	
                        key = api_key$key, 	
                        vars=c("H013001", "H013002", "H013003", "H013004", "H013005", "H013006", "H013007", "H013008"), 	
                        region = paste("county:", COUNTY_FIPS_STRING, sep = ""),	
                        regionin = paste("state:", STATE_FIPS, sep = ""))	
	
working <- left_join(raw_census, county_codes, by = c("county"))	
	
household_size_df <- working %>%	
  rename(hh_size_1 = H013002, hh_size_2 = H013003, hh_size_3 = H013004) %>%	
  mutate(hh_size_4_plus = H013005 + H013006 + H013007 + H013008) %>%	
  mutate(year = 2000) %>%	
  select(year, county_name, mtc_county_id, hh_size_1, hh_size_2, hh_size_3, hh_size_4_plus)	
	
# 2005	
# the 2005 to 2009 estimate is availabe via API, the 2005 is not, but it is available via American Fact Finder: http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_05_EST_B11016&prodType=table	
# for now, enter manually	
county_name = c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma")	
hh_size_1      = c(138023,  86591, 30247, 13619, 135391, 64287, 138519, 27870, 48213)	
hh_size_2      = c(158682, 117955, 37125, 16347, 102717, 80142, 167536, 40804, 59038)	
hh_size_3      = c( 84858,  62441, 14124,  6699,  37105, 43252, 101650, 25748, 28744)	
hh_size_4_plus = c(139817,  95115, 20897, 11537,  47186, 66546, 172425, 40202, 41217)	
	
working <- data.frame(county_name, hh_size_1, hh_size_2, hh_size_3, hh_size_4_plus, stringsAsFactors = FALSE)	
	
working <- left_join(working, county_codes, by = c("county_name"))	
	
working <- working %>%	
  mutate(year = 2005) %>%	
  select(-county)	
	
household_size_df <- rbind(household_size_df, working)	
	
	
# 2010: use decennial, SF1 (note that variable names are different than for 2000)	
raw_census <- getCensus(name = "sf1", 	
                        vintage = 2010, 	
                        key = api_key$key,	
                        vars=c("H0130001", "H0130002", "H0130003", "H0130004", "H0130005", "H0130006", "H0130007", "H0130008"), 	
                        region = paste("county:", COUNTY_FIPS_STRING, sep = ""),	
                        regionin = paste("state:", STATE_FIPS, sep = ""))	
	
working <- left_join(raw_census, county_codes, by = c("county"))	
	
working <- working %>%	
  rename(hh_size_1 = H0130002, hh_size_2 = H0130003, hh_size_3 = H0130004) %>%	
  mutate(hh_size_4_plus = H0130005 + H0130006 + H0130007 + H0130008) %>%	
  mutate(year = 2010) %>%	
  select(year, county_name, mtc_county_id, hh_size_1, hh_size_2, hh_size_3, hh_size_4_plus)	
	
household_size_df <- rbind(household_size_df, working)	
	
remove(working, raw_census)	
 	
#### County Control: Household and Group Quarters Population	
	
# 2000: use decennial, SF1	
raw_census <- getCensus(name = "sf1", 	
                        vintage = 2000, 	
                        key = api_key$key, 	
                        vars=c("H010001", "P037006"), 	
                        region = paste("county:", COUNTY_FIPS_STRING, sep = ""),	
                        regionin = paste("state:", STATE_FIPS, sep = ""))	
	
working <- left_join(raw_census, county_codes, by = c("county"))	
	
hh_gq_pop_df <- working %>%	
  rename(hh_population = H010001, non_inst_gq_population = P037006) %>%	
  mutate(year = 2000) %>%	
  select(year, county_name, mtc_county_id, hh_population, non_inst_gq_population)	
	
# 2010: use decennial, SF1 (note that variable names are different than for 2000)	
raw_census <- getCensus(name = "sf1", 	
                        vintage = 2010, 	
                        key = api_key$key, 	
                        vars=c("H0100001", "P0420007"), 	
                        region = paste("county:", COUNTY_FIPS_STRING, sep = ""),	
                        regionin = paste("state:", STATE_FIPS, sep = ""))	
	
working <- left_join(raw_census, county_codes, by = c("county"))	
	
working <- working %>%	
  rename(hh_population = H0100001, non_inst_gq_population = P0420007) %>%	
  mutate(year = 2010) %>%	
  select(year, county_name, mtc_county_id, hh_population, non_inst_gq_population)	
	
hh_gq_pop_df <- rbind(hh_gq_pop_df, working)	
	
# 2005: use ACS, which is not available via API.	
# http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_05_EST_B25008&prodType=table	
# enter hh population manually and use the average of 2000 and 2010 for group quarters	
county_name = c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma")	
hh_population = c(1421308, 1006486, 235609, 127445, 719077, 689271, 1669890, 395426, 453850)	
working <- data.frame(county_name, hh_population, stringsAsFactors = FALSE)	
	
gq_2000_df <- hh_gq_pop_df %>%	
  filter(year == 2000) %>%	
  select(county_name, gq_2000 = non_inst_gq_population)	
	
gq_2010_df <- hh_gq_pop_df %>%	
  filter(year == 2010) %>%	
  select(county_name, gq_2010 = non_inst_gq_population)	
	
gq_2005_df <- left_join(gq_2000_df, gq_2010_df, by = c("county_name"))	
	
gq_2005_df <- gq_2005_df %>%	
  mutate(non_inst_gq_population = round((gq_2000 + gq_2010)/2.0)) %>%	
  select(-gq_2000, -gq_2010)	
	
working <- left_join(working, gq_2005_df, by = c("county_name"))	
working <- left_join(working, county_codes, by = c("county_name"))	
	
working <- working %>%	
  mutate(year = 2005) %>%	
  select(-county)	
	
hh_gq_pop_df <- rbind(hh_gq_pop_df, working)	
	
remove(gq_2000_df, gq_2005_df, gq_2010_df, working, raw_census)	
	
 	
#### County Control: Occupation	
 	
# 2000: use PUMS	
relevant_pumas <- unique(puma_to_county$PUMA)	
	
pums_per <- pums_person_00[pums_person_00$PUMA %in% relevant_pumas,]	
	
# TODO fix the NA removal -- why are weights NA?	
working <- pums_per %>%	
  select(PUMA, occupation, wgtp) %>%	
  filter(!(is.na(wgtp))) %>%	
  group_by(PUMA, occupation) %>%	
  summarise(persons = sum(wgtp)) %>%	
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
	
persons_occupation_df <- persons_occupation_df %>%	
  mutate(year = 2000)	
	
# 2010: use 2007 to 2011 PUMS	
pums_per <- pums_person_0711[pums_person_0711$PUMA %in% relevant_pumas,]	
	
sum(pums_per$PWGTP)	
	
working <- pums_per %>%	
  select(PUMA, occupation, PWGTP) %>%	
  group_by(PUMA, occupation) %>%	
  summarise(persons = sum(PWGTP)) %>%	
  ungroup() %>%	
  filter(!(is.na(occupation) | occupation==0))	
	
sum(working$persons)	
	
working <- left_join(working, occupation_df, by = c("occupation"))	
working <- left_join(working, puma_to_county, by = c("PUMA"))	
	
working_sum <- working %>%	
  mutate(occupation_category = paste("occupation", str_to_lower(occupation_category), sep = "_")) %>%	
  group_by(county_name, occupation_category) %>%	
  summarise(persons = sum(persons))	
	
working_reshape <- dcast(working_sum, county_name ~ occupation_category, sum, value.var = c("persons"))	
	
working_reshape <- working_reshape %>%	
  mutate(year = 2010)	
	
persons_occupation_df <- rbind(persons_occupation_df, working_reshape)	
	
# 2005: use the average of the two PUMS estimates	
occ_2000 <- persons_occupation_df %>%	
  filter(year == 2000) %>%	
  select(-year)	
occ_2000 <- melt(occ_2000, id = c("county_name"), variable.name = "occupation_category", value.name = "persons_2000", factorsAsStrings = FALSE)	
	
occ_2010 <- persons_occupation_df %>%	
  filter(year == 2010) %>%	
  select(-year)	
occ_2010 <- melt(occ_2010, id = c("county_name"), variable.name = "occupation_category", value.name = "persons_2010", factorsAsStrings = FALSE)	
	
occ_2005 <- left_join(occ_2000, occ_2010, by = c("county_name", "occupation_category"))	
	
occ_2005 <- occ_2005 %>%	
  mutate(persons = round((persons_2000 + persons_2010) / 2.0)) %>%	
  select(-persons_2000, -persons_2010)	
	
working_reshape <- dcast(occ_2005, county_name ~ occupation_category, sum, value.var = c("persons"))	
	
working_reshape <- working_reshape %>%	
  mutate(year = 2005)	
	
persons_occupation_df <- rbind(persons_occupation_df, working_reshape)	
	
remove(working, working_sum, working_reshape, occ_2000, occ_2005, occ_2010)	
	
#### County Control: Age	
 	
# 2000: use decennial, SF1	
raw_census <- getCensus(name = "sf1", 	
                        vintage = 2000, 	
                        key = api_key$key, 	
                        vars=c("P012003", "P012004", "P012005", "P012006", "P012007", "P012008", "P012009", "P012010",	
                               "P012011", "P012012", "P012013", "P012014", "P012015", "P012016", "P012017", "P012018",	
                               "P012019", "P012020", "P012021", "P012022", "P012023", "P012024", "P012025",	
                               "P012027", "P012028", "P012029", "P012030", "P012031", "P012032", "P012033", "P012034", 	
                               "P012035", "P012036", "P012037", "P012038", "P012039", "P012040", "P012041", "P012042", 	
                               "P012043", "P012044", "P012045", "P012046", "P012047", "P012048", "P012049"), 	
                        region = paste("county:", COUNTY_FIPS_STRING, sep = ""),	
                        regionin = paste("state:", STATE_FIPS, sep = ""))	
	
working <- left_join(raw_census, county_codes, by = c("county"))	
	
age_df <- working %>%	
  mutate(age_00_05 = P012003 + P012027) %>%	
  mutate(age_05_09 = P012004 + P012028) %>%	
  mutate(age_10_14 = P012005 + P012029) %>%	
  mutate(age_15_17 = P012006 + P012030) %>%	
  mutate(age_18_19 = P012007 + P012031) %>%	
  mutate(age_20_20 = P012008 + P012032) %>%	
  mutate(age_21_21 = P012009 + P012033) %>%	
  mutate(age_22_24 = P012010 + P012034) %>%	
  mutate(age_25_29 = P012011 + P012035) %>%	
  mutate(age_30_34 = P012012 + P012036) %>%	
  mutate(age_35_39 = P012013 + P012037) %>%	
  mutate(age_40_44 = P012014 + P012038) %>%	
  mutate(age_45_49 = P012015 + P012039) %>%	
  mutate(age_50_54 = P012016 + P012040) %>%	
  mutate(age_55_59 = P012017 + P012041) %>%	
  mutate(age_60_61 = P012018 + P012042) %>%	
  mutate(age_62_64 = P012019 + P012043) %>%	
  mutate(age_65_66 = P012020 + P012044) %>%	
  mutate(age_67_69 = P012021 + P012045) %>%	
  mutate(age_70_74 = P012022 + P012046) %>%	
  mutate(age_75_79 = P012023 + P012047) %>%	
  mutate(age_80_84 = P012024 + P012048) %>%	
  mutate(age_85_up = P012025 + P012049) %>%	
  mutate(age_00_18 = age_00_05 + age_05_09 + age_10_14 + age_15_17 + round(0.5 * age_18_19)) %>%	
  mutate(age_19_64 = round(0.5 * age_18_19) + age_20_20 + age_21_21 + age_22_24 + age_25_29 + age_30_34 + age_35_39 + age_40_44 + age_45_49 +	
           age_50_54 + age_55_59 + age_60_61 + age_62_64) %>%	
  mutate(age_65_up = age_65_66 + age_67_69 + age_70_74 + age_75_79 + age_80_84 + age_85_up) %>%	
  mutate(age_00_14 = age_00_05 + age_05_09 + age_10_14) %>%	
  mutate(age_15_19 = age_15_17 + age_18_19) %>%	
  mutate(age_20_44 = age_20_20 + age_21_21 + age_22_24 + age_25_29 + age_30_34 + age_35_39 + age_40_44) %>%	
  mutate(age_45_64 = age_45_49 + age_50_54 + age_55_59 + age_60_61 + age_62_64) %>%	
  mutate(year = 2000) %>%	
  select(year, county_name, mtc_county_id, age_00_18, age_19_64, age_65_up, age_00_14, age_15_19, age_20_44, age_45_64)	
	
# 2010: use decennial, SF1	
raw_census <- getCensus(name = "sf1", 	
                        vintage = 2010, 	
                        key = api_key$key, 	
                        vars=c("P0120003", "P0120004", "P0120005", "P0120006", "P0120007", "P0120008", "P0120009", "P0120010",	
                               "P0120011", "P0120012", "P0120013", "P0120014", "P0120015", "P0120016", "P0120017", "P0120018",	
                               "P0120019", "P0120020", "P0120021", "P0120022", "P0120023", "P0120024", "P0120025",	
                               "P0120027", "P0120028", "P0120029", "P0120030", "P0120031", "P0120032", "P0120033", "P0120034", 	
                               "P0120035", "P0120036", "P0120037", "P0120038", "P0120039", "P0120040", "P0120041", "P0120042", 	
                               "P0120043", "P0120044", "P0120045", "P0120046", "P0120047", "P0120048", "P0120049"), 	
                        region = paste("county:", COUNTY_FIPS_STRING, sep = ""),	
                        regionin = paste("state:", STATE_FIPS, sep = ""))	
	
working <- left_join(raw_census, county_codes, by = c("county"))	
	
working <- working %>%	
  mutate(age_00_05 = P0120003 + P0120027) %>%	
  mutate(age_05_09 = P0120004 + P0120028) %>%	
  mutate(age_10_14 = P0120005 + P0120029) %>%	
  mutate(age_15_17 = P0120006 + P0120030) %>%	
  mutate(age_18_19 = P0120007 + P0120031) %>%	
  mutate(age_20_20 = P0120008 + P0120032) %>%	
  mutate(age_21_21 = P0120009 + P0120033) %>%	
  mutate(age_22_24 = P0120010 + P0120034) %>%	
  mutate(age_25_29 = P0120011 + P0120035) %>%	
  mutate(age_30_34 = P0120012 + P0120036) %>%	
  mutate(age_35_39 = P0120013 + P0120037) %>%	
  mutate(age_40_44 = P0120014 + P0120038) %>%	
  mutate(age_45_49 = P0120015 + P0120039) %>%	
  mutate(age_50_54 = P0120016 + P0120040) %>%	
  mutate(age_55_59 = P0120017 + P0120041) %>%	
  mutate(age_60_61 = P0120018 + P0120042) %>%	
  mutate(age_62_64 = P0120019 + P0120043) %>%	
  mutate(age_65_66 = P0120020 + P0120044) %>%	
  mutate(age_67_69 = P0120021 + P0120045) %>%	
  mutate(age_70_74 = P0120022 + P0120046) %>%	
  mutate(age_75_79 = P0120023 + P0120047) %>%	
  mutate(age_80_84 = P0120024 + P0120048) %>%	
  mutate(age_85_up = P0120025 + P0120049) %>%	
  mutate(age_00_18 = age_00_05 + age_05_09 + age_10_14 + age_15_17 + round(0.5 * age_18_19)) %>%	
  mutate(age_19_64 = round(0.5 * age_18_19) + age_20_20 + age_21_21 + age_22_24 + age_25_29 + age_30_34 + age_35_39 + age_40_44 + age_45_49 +	
           age_50_54 + age_55_59 + age_60_61 + age_62_64) %>%	
  mutate(age_65_up = age_65_66 + age_67_69 + age_70_74 + age_75_79 + age_80_84 + age_85_up) %>%	
  mutate(age_00_14 = age_00_05 + age_05_09 + age_10_14) %>%	
  mutate(age_15_19 = age_15_17 + age_18_19) %>%	
  mutate(age_20_44 = age_20_20 + age_21_21 + age_22_24 + age_25_29 + age_30_34 + age_35_39 + age_40_44) %>%	
  mutate(age_45_64 = age_45_49 + age_50_54 + age_55_59 + age_60_61 + age_62_64) %>%	
  mutate(year = 2010) %>%	
  select(year, county_name, mtc_county_id, age_00_18, age_19_64, age_65_up, age_00_14, age_15_19, age_20_44, age_45_64)	
	
age_df <- rbind(age_df, working)	
	
# 2005: use ACS, which is not available via API.	
# http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_05_EST_B01001&prodType=table	
# enter manually	
county_name = c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma")	
age_00_05 = c(106908,68260,13955,8100,39718,49797,131918,29678,28865)	
age_05_09 = c(104189,74666,12746,9037,23755,46707,112739,29571,25054)	
age_10_14 = c(93476,73569,14895,8596,29156,41749,117183,33619,35134)	
age_15_17 = c(57096,46312,8341,5282,16041,25568,64735,20278,20116)	
age_18_19 = c(32022,24508,3919,2994,9715,14697,35339,10688,10638)	
age_20_20 = c(15717,11824,2164,1042,6599,5810,19549,4840,6580)	
age_21_21 = c(20097,14319,1453,1379,6564,7616,14483,4789,6480)	
age_22_24 = c(49944,34136,6218,6030,25761,21761,61025,15397,17358)	
age_25_29 = c(92506,59040,9620,7029,54777,35869,103938,24888,27369)	
age_30_34 = c(108497,64526,12649,7905,78554,48873,139552,24985,25526)	
age_35_39 = c(113056,76663,18873,10175,75578,54828,143690,28250,29610)	
age_40_44 = c(120890,78074,17216,8711,65197,59092,145507,29022,35987)	
age_45_49 = c(113387,82380,20440,9926,54768,56481,132298,31840,37518)	
age_50_54 = c(100395,73702,22379,8804,51128,52187,111624,28666,36346)	
age_55_59 = c(88979,65471,21429,8444,43051,49321,98489,24059,33605)	
age_60_61 = c(24567,21145,7511,2866,14063,14471,30259,5899,10048)	
age_62_64 = c(35327,27245,8324,4438,19476,17813,38122,8777,12229)	
age_65_66 = c(18301,14408,4870,1875,13318,11565,21265,4274,6716)	
age_67_69 = c(23417,18036,5605,2172,16990,13380,27223,7038,7973)	
age_70_74 = c(34544,26253,7314,4383,20620,19542,44820,9708,12220)	
age_75_79 = c(28792,22246,6663,3005,21310,16403,33600,8335,10527)	
age_80_84 = c(22579,15664,5675,2528,17425,13691,22209,5333,9642)	
age_85_up = c(16622,14039,3350,2724,15513,12050,20323,5492,8309)	
	
working <- data.frame(county_name, 	
                      age_00_05, age_05_09, age_10_14, age_15_17, age_18_19, age_20_20, age_21_21, age_22_24,	
                      age_25_29, age_30_34, age_35_39, age_40_44, age_45_49, age_50_54, age_55_59, age_60_61, 	
                      age_62_64, age_65_66, age_67_69, age_70_74, age_75_79, age_80_84, age_85_up, 	
                      stringsAsFactors = FALSE)	
	
working <- left_join(raw_census, county_codes, by = c("county"))	
	
working <- working %>%	
  mutate(age_00_18 = age_00_05 + age_05_09 + age_10_14 + age_15_17 + round(0.5 * age_18_19)) %>%	
  mutate(age_19_64 = round(0.5 * age_18_19) + age_20_20 + age_21_21 + age_22_24 + age_25_29 + age_30_34 + age_35_39 + age_40_44 + age_45_49 +	
           age_50_54 + age_55_59 + age_60_61 + age_62_64) %>%	
  mutate(age_65_up = age_65_66 + age_67_69 + age_70_74 + age_75_79 + age_80_84 + age_85_up) %>%	
  mutate(age_00_14 = age_00_05 + age_05_09 + age_10_14) %>%	
  mutate(age_15_19 = age_15_17 + age_18_19) %>%	
  mutate(age_20_44 = age_20_20 + age_21_21 + age_22_24 + age_25_29 + age_30_34 + age_35_39 + age_40_44) %>%	
  mutate(age_45_64 = age_45_49 + age_50_54 + age_55_59 + age_60_61 + age_62_64) %>%	
  mutate(year = 2005) %>%	
  select(year, county_name, mtc_county_id, age_00_18, age_19_64, age_65_up, age_00_14, age_15_19, age_20_44, age_45_64)	
	
age_df <- rbind(age_df, working)	
	
remove(age_00_05, age_05_09, age_10_14, age_15_17, age_18_19, age_20_20, age_21_21, age_22_24,	
       age_25_29, age_30_34, age_35_39, age_40_44, age_45_49, age_50_54, age_55_59, age_60_61, 	
       age_62_64, age_65_66, age_67_69, age_70_74, age_75_79, age_80_84, age_85_up,	
       working, raw_census)	
	
#### County Control: Workers per Household	
 	
# 2000: use PUMS	
working <- pums_household_00 %>%	
  select(PUMA, hh_workers_from_esr, hweight) %>%	
  group_by(PUMA, hh_workers_from_esr) %>%	
  summarise(households = sum(hweight)) %>%	
  ungroup()	
	
working <- left_join(working, puma_to_county, by = c("PUMA"))	
	
working <- working %>%	
  group_by(county_name, hh_workers_from_esr) %>%	
  summarise(households = sum(households)) %>%	
  ungroup()	
	
working <- working %>%	
  mutate(variable = "ERROR") %>%	
  mutate(variable = ifelse(hh_workers_from_esr == 0, "hh_workers_0", variable)) %>%	
  mutate(variable = ifelse(hh_workers_from_esr == 1, "hh_workers_1", variable)) %>%	
  mutate(variable = ifelse(hh_workers_from_esr == 2, "hh_workers_2", variable)) %>%	
  mutate(variable = ifelse(hh_workers_from_esr > 2,  "hh_workers_3_plus", variable))	
	
working_reshape <- dcast(working, county_name ~ variable, sum, value.var = c("households"))	
	
working_reshape <- left_join(working_reshape, county_codes, by = c("county_name"))	
	
hh_workers_df <- working_reshape %>%	
  mutate(year = 2000) %>%	
  select(-county)	
	
# 2010: use PUMS	
working <- pums_household_0711 %>%	
  select(PUMA, hh_workers_from_esr, WGTP) %>%	
  group_by(PUMA, hh_workers_from_esr) %>%	
  summarise(households = sum(WGTP)) %>%	
  ungroup()	
	
working <- left_join(working, puma_to_county, by = c("PUMA"))	
	
working <- working %>%	
  group_by(county_name, hh_workers_from_esr) %>%	
  summarise(households = sum(households)) %>%	
  ungroup()	
	
working <- working %>%	
  mutate(variable = "ERROR") %>%	
  mutate(variable = ifelse(hh_workers_from_esr == 0, "hh_workers_0", variable)) %>%	
  mutate(variable = ifelse(hh_workers_from_esr == 1, "hh_workers_1", variable)) %>%	
  mutate(variable = ifelse(hh_workers_from_esr == 2, "hh_workers_2", variable)) %>%	
  mutate(variable = ifelse(hh_workers_from_esr > 2,  "hh_workers_3_plus", variable))	
	
working_reshape <- dcast(working, county_name ~ variable, sum, value.var = c("households"))	
	
working_reshape <- left_join(working_reshape, county_codes, by = c("county_name"))	
	
working_reshape <- working_reshape %>%	
  mutate(year = 2010) %>%	
  select(-county)	
	
hh_workers_df <- rbind(hh_workers_df, working_reshape)	
	
# START HERE	
# 2005: start with http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_05_EST_B08202&prodType=table	
# use B08202 as a proxy	
# TODO: factor up for non-commuters?	
county_name = c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma")	
hh_workers_0      = c(124157,  87323, 26147, 11714,  88249, 62455, 121032, 32441, 45808)	
hh_workers_1      = c(216343, 145254, 43234, 18142, 133830, 99921, 243797, 49443, 66796)	
hh_workers_2      = c(145179, 106185, 29565, 14308,  79223, 70778, 170640, 43740, 52430)	
hh_workers_3_plus = c(35701,   23340,  3447,  4038,  21097, 21073,  44661,  9000, 12178)	
	
working <- data.frame(county_name, hh_workers_0, hh_workers_1, hh_workers_2, hh_workers_3_plus, stringsAsFactors = FALSE)	
	
working <- left_join(working, county_codes, by = c("county_name"))	
	
working <- working %>%	
  mutate(year = 2005) %>%	
  select(-county)	
	
hh_workers_df <- rbind(hh_workers_df, working)	
	
remove(working, working_reshape, hh_workers_0, hh_workers_1, hh_workers_2, hh_workers_3_plus)	
	
#### Write to Disk	
 	
# bind data	
county_controls_df <- left_join(age_df, hh_gq_pop_df, by = c("year", "county_name", "mtc_county_id"))	
county_controls_df <- left_join(county_controls_df, hh_workers_df, by = c("year", "county_name", "mtc_county_id"))	
county_controls_df <- left_join(county_controls_df, household_size_df, by = c("year", "county_name", "mtc_county_id"))	
county_controls_df <- left_join(county_controls_df, persons_occupation_df, by = c("year", "county_name"))	
	
# 2000 write	
YEAR = 2000	
write <- county_controls_df %>%	
  filter(year == YEAR) %>%	
  select(-year)	
write.csv(write, file = paste(CONTROL_DIR, "/year_", YEAR, "/county_controls.csv", sep = ""), row.names = FALSE, quote = F)	
	
# 2005 write	
YEAR = 2005	
write <- county_controls_df %>%	
  filter(year == YEAR) %>%	
  select(-year)	
write.csv(write, file = paste(CONTROL_DIR, "/year_", YEAR, "/county_controls.csv", sep = ""), row.names = FALSE, quote = F)	
	
# 2010 write	
YEAR = 2010	
write <- county_controls_df %>%	
  filter(year == YEAR) %>%	
  select(-year)	
write.csv(write, file = paste(CONTROL_DIR, "/year_", YEAR, "/county_controls.csv", sep = ""), row.names = FALSE, quote = F)	
