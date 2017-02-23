# ---	
# title: "Step 03 Controls to Database"	
# author: "David Ory"	

suppressMessages(library(dplyr))	
library(stringr)	
suppressMessages(library(RMySQL))	

YEAR <- "year_2010"

WORKING_DIR <- "E:/Projects/Clients/mtc/TO2_Task2/MTCPopSynIII"

MYSQL_SERVER    = "localhost"	
MYSQL_DATABASE  = "mtc_popsyn"	
MYSQL_USER_NAME = "root" 
MYSQL_PASSWORD_FILE <- "E:/Projects/Clients/mtc/TO2_Task2/mysql.csv"	

CONTROL_DIR <- paste(WORKING_DIR, "data", sep="/")

PUMS_0711_HH_FILE    <- paste(CONTROL_DIR, "ss11hca.csv", sep = "/")	
PUMS_0711_PER_FILE   <- paste(CONTROL_DIR, "ss11pca.csv", sep = "/")
PUMS_00_HH_FILE      <- paste(CONTROL_DIR, "hbayarea5_2000.csv", sep = "/")	
PUMS_00_PER_FILE     <- paste(CONTROL_DIR, "pbayarea5_2000.csv", sep = "/")	

MAZ_CONTROL_FILE     <- paste(CONTROL_DIR, YEAR, "mazData.csv", sep = "/")	
TAZ_CONTROL_FILE     <- paste(CONTROL_DIR, YEAR, "tazData.csv", sep = "/")	
COUNTY_CONTROL_FILE  <- paste(CONTROL_DIR, YEAR, "county_controls.csv", sep = "/")	
GEOG_CONTROL_FILE    <- paste(CONTROL_DIR, "geographies.csv", sep = "/")



#### Data reads	
 	
input_maz <- read.csv(MAZ_CONTROL_FILE, header = TRUE)	
input_taz <- read.csv(TAZ_CONTROL_FILE, header = TRUE)	
input_county <- read.csv(COUNTY_CONTROL_FILE, header = TRUE, stringsAsFactors = FALSE)	
input_geog <- read.csv(GEOG_CONTROL_FILE, header = TRUE)	
	
#### Prepare MAZ controls	
 	
control_totals_maz <- input_maz	
	
# the geographies file reminds us we are using the 2000 Census PUMAs	
input_geog <- input_geog %>%	
  rename(PUMA = PUMA5CE00, maz_original = MAZ_ORIGINAL, taz_original = TAZ_ORIGINAL, 
         maz = MAZ, taz = TAZ, mtc_county_id = MTCCountyID, county_name = COUNTYNAME)	

control_totals_maz <- control_totals_maz %>% 
  rename(maz_original = MAZ, households = X2010.HH)
	
control_totals_maz <- left_join(control_totals_maz, input_geog, by = c("maz_original"))	

#### Prepare TAZ controls	
 	
control_totals_taz <- control_totals_maz %>%	
  group_by(taz_original) %>%	
  summarise(households = sum(households))	
	
# create TAZ crosswalk, checking for PUMA and COUNTY overlaps	
taz_geog <- input_geog %>%	
  group_by(taz, taz_original, PUMA, COUNTYFP, mtc_county_id, county_name) %>%	
  summarise(count = n()) %>%	
  select(-count)	
	
check_taz_geog <- taz_geog %>%	
  group_by(taz) %>%	
  summarise(count = n()) %>%	
  filter(count > 1)	
	
control_totals_taz <- left_join(control_totals_taz, taz_geog, by = c("taz_original"))	
	
# bring in income	
input_taz <- input_taz %>%
  rename(taz_original = TAZ, income_2010_0_30 = Less.than..30.000, income_2010_30_60 = X.30.000.to..59.999, 
         income_2010_60_100 = X.60.000.to..99.999, income_2010_100_up = X.100.000.or.more)

to_join_taz <- input_taz %>%	
  select(taz_original, hh_income_quartile_1 = income_2010_0_30, hh_income_quartile_2 = income_2010_30_60, 	
         hh_income_quartile_3 = income_2010_60_100, hh_income_quartile_4 = income_2010_100_up)	
	
control_totals_taz <- left_join(control_totals_taz, to_join_taz, by = c("taz_original"))	
	
remove(taz_geog, to_join_taz)	
	
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
dbWriteTable(conn = mysql_connection, name = paste('control_totals_taz',  YEAR, sep = '_'), value = as.data.frame(control_totals_taz),  overwrite = TRUE)	
dbWriteTable(conn = mysql_connection, name = paste('control_totals_meta', YEAR, sep = '_'), value = as.data.frame(control_totals_meta), overwrite = TRUE)	
	
dbDisconnect(mysql_connection)	
 	
