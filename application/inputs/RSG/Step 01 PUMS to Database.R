#' Prepares PUMS database inputs for the Population Synthesizer
#' author: Dave Ory, MTC
#' 	##############################################################	
 	
suppressMessages(library(dplyr))	
library(stringr)	
suppressMessages(library(RMySQL))	
 	
#### Paramaters	
 	
MYSQL_SERVER    = "localhost"	
MYSQL_DATABASE  = "mtc_popsyn"	
MYSQL_USER_NAME = "root" 	
	
# build occupation code data frame	
socp10_first_two = c(11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55)	
occupation_code  = c( 1,  2,  2,  2,  2,  3,  2,  2,  3,  2,  3,  3,  4,  5,  3,  4,  3,  5,  5,  5,  5,  5,  6)	
	
socp10_occupation_df = data.frame(soc = socp10_first_two, occupation = occupation_code)	
	
occupation_code = c(1,2,3,4,5,6)	
occupation_category = c("Management", "Professional", "Services", "Retail", "Manual", "Military")	
	
occupation_df = data.frame(occupation = occupation_code, occupation_category)	
	
socp10_occupation_df <- left_join(socp10_occupation_df, occupation_df, by = c("occupation"))	
	
remove(occupation_df, occupation_code, occupation_category, socp10_first_two)	
 	
#### Remote file locations	
 	
MYSQL_PASSWORD_FILE <- "E:/Projects/Clients/mtc/TO2_Task2/mysql.csv"	
	
PUMS_0711_DIR <- "E:/Projects/Clients/mtc/TO2_Task2/MTCPopSynIII/data"	
PUMS_0711_HH_FILE <- paste(PUMS_0711_DIR, "ss11hca.csv", sep = "/")	
PUMS_0711_PER_FILE <- paste(PUMS_0711_DIR, "ss11pca.csv", sep = "/")	
	
PUMS_00_DIR <- "E:/Projects/Clients/mtc/TO2_Task2/PUMS/2000/FromMTC"	
PUMS_00_HH_FILE <- paste(PUMS_00_DIR, "hbayarea5_2000.csv", sep = "/")	
PUMS_00_PER_FILE <- paste(PUMS_00_DIR, "pbayarea5_2000.csv", sep = "/")	
	
CONTROL_DIR <- "E:/Projects/Clients/mtc/TO2_Task2/MTCPopSynIII/data"	
GEOG_CONTROL_FILE <-   paste(CONTROL_DIR, "geographicCWalk.csv", sep = "/")	
	
	
#### Data reads	
 	
input_geog <- read.csv(GEOG_CONTROL_FILE, header = TRUE)	
	
input_pums_0711_hh <- read.csv(PUMS_0711_HH_FILE, header = TRUE)	
input_pums_0711_per <- read.csv(PUMS_0711_PER_FILE, header = TRUE)	
	
input_pums_00_hh <- read.csv(PUMS_00_HH_FILE, header = TRUE)	
input_pums_00_per <- read.csv(PUMS_00_PER_FILE, header = TRUE)	
	
# rename geography	
input_pums_00_hh <- input_pums_00_hh %>%	
  rename(PUMA = puma5)	
	
input_pums_00_per <- input_pums_00_per %>%	
  rename(PUMA = puma5)	
 	
#### Prepare 2007-2011 PUMS data	
 	
# the geographies file reminds us we are using the 2000 Census PUMAs	
input_geog <- input_geog %>%	
  rename(PUMA = PUMA5CE00)	
	
# get list of relevant pums	
relevant_pumas <- unique(input_geog$PUMA)	
	
pums_hh  <- input_pums_0711_hh[input_pums_0711_hh$PUMA %in% relevant_pumas,]	
pums_per <- input_pums_0711_per[input_pums_0711_per$PUMA %in% relevant_pumas,]	
	
# remove vacant units and group quarters households	
pums_hh <- pums_hh %>%	
  filter(!(NP ==0)) %>%	
  filter(TYPE <= 1)	
	
# compute number of workers in the household	
num_workers <- pums_per %>%
  mutate(ESR = ifelse(is.na(ESR), 0, ESR)) %>%
  mutate(workers = ifelse(ESR == 1 | ESR == 2 | ESR == 4 | ESR == 5, 1, 0)) %>%	
  group_by(SERIALNO) %>%	
  summarise(hh_workers_from_esr = sum(workers))	
	
pums_hh <- left_join(pums_hh, num_workers, by = c("SERIALNO"))	
	
pums_hh <- pums_hh %>%	
  mutate(hh_workers_from_esr = ifelse(is.na(hh_workers_from_esr), 0, hh_workers_from_esr))	
	
# use ESR to set employment dummy	
pums_per <- pums_per %>%	
  mutate(employed = ifelse(ESR == 1 | ESR == 2 | ESR == 4 | ESR == 5, 1, 0L))	
  
# set null to zero
pums_per$employed[is.na(pums_per$employed)] <- 0
	
# put income in constant year dollars (SQL says reported income * rolling reference factor * inflation adjustment)	
	
# from PUMS Data Dictionary	
# Adjustment factor for income and earnings dollar amounts (6 implied decimal places) 	
# 1102938 .2007 factor (1.016787 * 1.08472906) 	
# 1063801 .2008 factor (1.018389 * 1.04459203)  	
# 1048026 .2009 factor (0.999480 * 1.04857143)	
# 1039407 .2010 factor (1.007624 * 1.03154279) 	
# 1018237 .2011 factor (1.018237 * 1.00000000) 	
# Note: The values of ADJINC inflation-adjusts reported income to 2011 dollars.  	
# ADJINC incorporates an adjustment that annualizes the different rolling reference periods for reported income (as done in the single-	
# year data using the variable ADJUST) and an adjustment to inflation-	
# adjust the annualized income to 2011 dollars.  ADJINC applies to variables FINCP and HINCP in the housing record, and 	
# variables INTP, OIP, PAP, PERNP, PINCP, RETP, SEMP, SSIP, SSP, and WAGP in the person record. 	
	
pums_hh <- pums_hh %>%	
  mutate(hh_income_2010 = 999) %>%	
  mutate(hh_income_2010 = ifelse(ADJINC == 1102938, HINCP/1.0 * 1.016787 * 1.08472906/1.03154279, hh_income_2010)) %>%	
  mutate(hh_income_2010 = ifelse(ADJINC == 1063801, HINCP/1.0 * 1.018389 * 1.04459203/1.03154279, hh_income_2010)) %>%	
  mutate(hh_income_2010 = ifelse(ADJINC == 1048026, HINCP/1.0 * 0.999480 * 1.04857143/1.03154279, hh_income_2010)) %>%	
  mutate(hh_income_2010 = ifelse(ADJINC == 1039407, HINCP/1.0 * 1.007624 * 1.03154279/1.03154279, hh_income_2010)) %>%	
  mutate(hh_income_2010 = ifelse(ADJINC == 1018237, HINCP/1.0 * 1.018237 * 1.00000000/1.03154279, hh_income_2010))	
	
# give each hh a more manageable unique id	
unique_hh_id <- 1:nrow(pums_hh)	
pums_hh <- cbind(pums_hh, unique_hh_id)	
rownames(pums_hh) <- 1:nrow(pums_hh)	
	
# set the initial person weight as the hh weight and give the person table the hhnum id	
hh_weights <- pums_hh %>%	
  select(SERIALNO, wgtp = WGTP, unique_hh_id)	
	
pums_per <- left_join(pums_per, hh_weights, by = c("SERIALNO"))	
	
# extract the occupation code	
pums_per <- pums_per %>%	
  mutate(soc = str_trim(socp00, side = c("both"))) %>%	
  mutate(soc = ifelse(soc == "N.A.//", str_trim(socp10, side = c("both")), soc)) %>%	
  mutate(soc = as.numeric(str_sub(soc, start = 1, end = 2)))	
	
pums_per <- left_join(pums_per, socp10_occupation_df, by = c("soc"))	
	
table(pums_per$occupation)	
table(pums_per$occupation_category)	
	
pums_per <- pums_per %>%	
  select(-occupation_category)	
  
# set occupation to  zero where null and employed=0
pums_per$occupation[is.na(pums_per$occupation) | pums_per$employed==0] <- 0
	
remove(hh_weights, num_workers, relevant_pumas)	
	
	
	
#### Put 2007 to 2011 data into MySQL database	
 	
# year of the PUMS data	
PUMS_YEAR = "2007_2011"	
	
# get password	
mysql_passes <- read.csv(MYSQL_PASSWORD_FILE, header = TRUE)	
	
mysql_passes <- mysql_passes %>%	
  filter(user == MYSQL_USER_NAME) %>%	
  mutate(pwd = paste(pwd))	
	
# connection	
mysql_connection <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)	
	
# write the household and person tables	
dbWriteTable(conn = mysql_connection, name = paste('household_table', PUMS_YEAR, sep = '_'), value = as.data.frame(pums_hh),  overwrite = TRUE)	
dbWriteTable(conn = mysql_connection, name = paste('person_table', PUMS_YEAR, sep = '_'),    value = as.data.frame(pums_per), overwrite = TRUE)	
	
dbDisconnect(mysql_connection)	
	
#### Prepare 2000 PUMS Data	
 	
# get list of relevant pums	
relevant_pumas <- unique(input_geog$PUMA)	
	
pums_hh  <- input_pums_00_hh[input_pums_00_hh$PUMA %in% relevant_pumas,]	
pums_per <- input_pums_00_per[input_pums_00_per$PUMA %in% relevant_pumas,]	
	
# remove vacant units and group quarters households	
pums_hh <- pums_hh %>%	
  filter(!(hht == 0))	
	
# compute number of workers in the household	
num_workers <- pums_per %>%	
  mutate(workers = ifelse(esr == 1 | esr == 2 | esr == 4 | esr == 5, 1, 0)) %>%	
  group_by(serialno) %>%	
  summarise(hh_workers_from_esr = sum(workers))	


pums_hh <- left_join(pums_hh, num_workers, by = c("serialno"))	

pums_hh <- pums_hh %>%	
  mutate(hh_workers_from_esr = ifelse(is.na(hh_workers_from_esr), 0, hh_workers_from_esr))	
	
# use ESR to set employment dummy	
pums_per <- pums_per %>%	
  mutate(employed = ifelse(esr == 1 | esr == 2 | esr == 4 | esr == 5, 1, 0L))	

# household income	
# inflation assumptions reference: http://analytics.mtc.ca.gov/foswiki/Main/inflationassumptions	
pums_hh <- pums_hh %>%	
  mutate(hh_income_2010 = hinc * 227.47 / 172.50)	

# give each hh a more manageable unique id	
unique_hh_id <- 1:nrow(pums_hh)	
	
pums_hh <- cbind(pums_hh, unique_hh_id)	
rownames(pums_hh) <- 1:nrow(pums_hh)	
	
# set the initial person weight as the hh weight and give the person table the hhnum id, remove NAs (group quarters)	
hh_weights <- pums_hh %>%	
  select(serialno, wgtp = hweight, unique_hh_id)	
	
pums_per <- left_join(pums_per, hh_weights, by = c("serialno"))	
	
pums_per <- pums_per %>%	
  filter(!is.na(wgtp))	
	
# extract the occupation code	
# *appears* to be consistent with 2010 (http://www.census.gov/people/io/files/occ2000t.pdf)	
pums_per <- pums_per %>%	
  mutate(soc = as.numeric(str_sub(occsoc1, start = 1L, end = 2L)))	
	
pums_per <- left_join(pums_per, socp10_occupation_df, by = c("soc"))	
	
table(pums_per$occupation_category)	
	
pums_per <- pums_per %>%	
  select(-occupation_category)	
	
remove(hh_weights, num_workers, relevant_pumas)	
	
 	
#### Put 2000 data into MySQL database	
 	
# year of the PUMS data	
PUMS_YEAR = "2000"	
	
# connection	
mysql_connection <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)	
	
# write the household and person tables	
dbWriteTable(conn = mysql_connection, name = paste('household_table', PUMS_YEAR, sep = '_'), value = as.data.frame(pums_hh),  overwrite = TRUE)	
dbWriteTable(conn = mysql_connection, name = paste('person_table', PUMS_YEAR, sep = '_'),    value = as.data.frame(pums_per), overwrite = TRUE)	
	
dbDisconnect(mysql_connection)	
	
