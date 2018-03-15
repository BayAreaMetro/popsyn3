# ---	
# title: "Step 03 Controls to Database"	
# author: "David Ory"	

suppressMessages(library(dplyr))	
library(stringr)	
suppressMessages(library(RMySQL))	

#DATA_DIR <- "E:/Projects/Clients/mtc/TO2_Task2/MTCPopSynIII/data"
#YEAR <- "year_2010"

#####
MAZ_CONTROL_FILE        <- paste(DATA_DIR, YEAR, "mazControlFile.csv", sep = "/")	
TAZ_CONTROL_FILE        <- paste(DATA_DIR, YEAR, "tazControlFile.csv", sep = "/")	
COUNTY_CONTROL_FILE     <- paste(DATA_DIR, YEAR, "countyControlFile.csv", sep = "/")	
MAZ_CONTROL_FILE_GQ     <- paste(DATA_DIR, YEAR, "gq00051015_maz.csv", sep = "/")
GQ_COUNTY_CONTROL_FILE  <- paste(DATA_DIR, YEAR, "meta_controls_gq.csv", sep = "/")
GEOG_CONTROL_FILE       <- paste(DATA_DIR, "geographicCWalk.csv", sep = "/")

#### Data reads	
 	
input_maz     <- read.csv(MAZ_CONTROL_FILE, header = TRUE)
input_maz_gq  <- read.csv(MAZ_CONTROL_FILE_GQ, header = TRUE)
input_taz     <- read.csv(TAZ_CONTROL_FILE, header = TRUE)	
input_county  <- read.csv(COUNTY_CONTROL_FILE, header = TRUE, stringsAsFactors = FALSE)	
input_meta_gq <- read.csv(GQ_COUNTY_CONTROL_FILE, header = TRUE, stringsAsFactors = FALSE)	
input_geog    <- read.csv(GEOG_CONTROL_FILE, header = TRUE)


#### Prepare MAZ controls	
 	
control_totals_maz <- input_maz	
	
# the geographies file reminds us we are using the 2000 Census PUMAs	
input_geog <- input_geog %>%	
  rename(PUMA = PUMA5CE00, maz_original = MAZ_ORIGINAL, taz_original = TAZ_ORIGINAL, 
         maz = MAZ, taz = TAZ, mtc_county_id = MTCCountyID, county_name = COUNTYNAME)	

control_totals_maz <- control_totals_maz %>% 
  rename(households = HH)

control_totals_maz <- left_join(control_totals_maz, input_geog, by = c("maz_original"))	

#### Prepare TAZ controls	
 	
#control_totals_taz <- control_totals_maz %>%	
#  group_by(taz_original) %>%	
#  summarise(households = sum(households))	
	
# create TAZ crosswalk, checking for PUMA and COUNTY overlaps	
taz_geog <- input_geog %>%	
  group_by(taz, taz_original, PUMA, COUNTYFP, mtc_county_id, county_name) %>%	
  summarise(count = n()) %>%	
  select(-count)	
	
check_taz_geog <- taz_geog %>%	
  group_by(taz) %>%	
  summarise(count = n()) %>%	
  filter(count > 1)	
	
#control_totals_taz <- left_join(control_totals_taz, taz_geog, by = c("taz_original"))	
#	
## bring in income	
#input_taz <- input_taz %>%
#  rename(taz_original = TAZ, income_2010_0_30 = Less.than..30.000, income_2010_30_60 = X.30.000.to..59.999, 
#         income_2010_60_100 = X.60.000.to..99.999, income_2010_100_up = X.100.000.or.more)
#
#to_join_taz <- input_taz %>%	
#  select(taz_original, hh_income_quartile_1 = income_2010_0_30, hh_income_quartile_2 = income_2010_30_60, 	
#         hh_income_quartile_3 = income_2010_60_100, hh_income_quartile_4 = income_2010_100_up)	
	
control_totals_taz <- left_join(input_taz, taz_geog, by = c("taz_original"))	
	
#remove(taz_geog, to_join_taz)	
	
#### Prepare County/Meta controls	
 	
# bring in hh size, hh workers, and occupation	
control_totals_meta <- input_county	
	
# create county crosswalk	
county_names <- input_county %>%	
  select(county_name_factor = county_name, mtc_county_id) %>%	
  mutate(county_name = paste(county_name_factor)) %>%	
  select(-county_name_factor)	
	
control_totals_meta <- left_join(control_totals_meta, county_names, by = c("county_name", "mtc_county_id"))	
	
remove(county_names)	

#### Prepare GQ controls
control_totals_maz_gq <- control_totals_maz
control_totals_maz_gq$PUMA_GQ <- control_totals_maz_gq$mtc_county_id
control_totals_maz_gq$region <- 1

control_totals_maz_gq <- select(control_totals_maz_gq, -PUMA)
names(control_totals_maz_gq)[names(control_totals_maz_gq) == 'PUMA_GQ'] <- 'PUMA'
if(YEAR == "year_2000"){
  control_totals_maz_gq <- control_totals_maz_gq %>%
    left_join(input_maz_gq[,c("MAZ", "univ00", "mil00", "othnon00")], by = c("maz_original" = "MAZ")) %>%
    mutate(popgq = univ00+mil00+othnon00) %>%
    rename(univ = univ00, mil = mil00, othnon = othnon00) 
}

if(YEAR == "year_2005"){
  control_totals_maz_gq <- control_totals_maz_gq %>%
    left_join(input_maz_gq[,c("MAZ", "univ05", "mil05", "othnon05")], by = c("maz_original" = "MAZ")) %>%
    mutate(popgq = univ05+mil05+othnon05) %>%
    rename(univ = univ05, mil = mil05, othnon = othnon05) 
}

if(YEAR == "year_2010"){
  control_totals_maz_gq <- control_totals_maz_gq %>%
    left_join(input_maz_gq[,c("MAZ", "univ10", "mil10", "othnon10")], by = c("maz_original" = "MAZ")) %>%
    mutate(popgq = univ10+mil10+othnon10) %>%
    rename(univ = univ10, mil = mil10, othnon = othnon10)
}

if(YEAR == "year_2015"){
  control_totals_maz_gq <- control_totals_maz_gq %>%
    left_join(input_maz_gq[,c("MAZ", "univ15", "mil15", "othnon15")], by = c("maz_original" = "MAZ")) %>%
    mutate(popgq = univ15+mil15+othnon15) %>%
    rename(univ = univ15, mil = mil15, othnon = othnon15)
}

	
#### Put data into MySQL database	
 	
# get password	
mysql_passes <- read.csv(MYSQL_PASSWORD_FILE, header = TRUE)	
	
mysql_passes <- mysql_passes %>%	
  filter(user == MYSQL_USER_NAME) %>%	
  mutate(pwd = paste(pwd))	
	
# connection	
mysql_connection <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)	
	
# write the control tables	
dbWriteTable(conn = mysql_connection, name = paste('control_totals_maz',  YEAR, sep = '_'), value = as.data.frame(control_totals_maz),  overwrite = TRUE)	
dbWriteTable(conn = mysql_connection, name = paste('control_totals_maz_gq',  YEAR, sep = '_'), value = as.data.frame(control_totals_maz_gq),  overwrite = TRUE)	
dbWriteTable(conn = mysql_connection, name = paste('control_totals_taz',  YEAR, sep = '_'), value = as.data.frame(control_totals_taz),  overwrite = TRUE)	
dbWriteTable(conn = mysql_connection, name = paste('control_totals_meta', YEAR, sep = '_'), value = as.data.frame(control_totals_meta), overwrite = TRUE)	
dbWriteTable(conn = mysql_connection, name = paste('control_totals_meta_gq', YEAR, sep = '_'), value = as.data.frame(input_meta_gq), overwrite = TRUE)	

dbDisconnect(mysql_connection)	
 	
