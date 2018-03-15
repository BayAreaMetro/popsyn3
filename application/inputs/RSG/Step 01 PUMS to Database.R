#' Prepares PUMS database inputs for the Population Synthesizer
#' author: Dave Ory, MTC
#' 	##############################################################	
 	
suppressMessages(library(dplyr))	
suppressMessages(library(stringr)	)
suppressMessages(library(RMySQL))	
 	
#### Paramaters	
 	
#MYSQL_SERVER    = "localhost"	
#MYSQL_DATABASE  = "mtc_popsyn"	
#MYSQL_USER_NAME = "root" 	
#DATA_DIR <- "E:/Projects/Clients/mtc/TO2_Task2/MTCPopSynIII/data"
#YEAR <- "year_2010"

	
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
GEOG_CONTROL_FILE <-   paste(DATA_DIR, "geographicCWalk.csv", sep = "/")	
	
	
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

# create PUMA ID for GQ-PopSyn run [single PUMA ID for each county]
input_geog$PUMA_GQ <- input_geog$MTCCountyID

# create PUMA-PUMA_GQ crosswalk
puma_GQ_Xwalk <- input_geog %>%
  group_by(PUMA) %>%
  summarise(PUMA_GQ = max(PUMA_GQ))
	
# get list of relevant puma	
relevant_pumas <- unique(input_geog$PUMA)	
	
pums_hh  <- input_pums_0711_hh[input_pums_0711_hh$PUMA %in% relevant_pumas,]	
pums_per <- input_pums_0711_per[input_pums_0711_per$PUMA %in% relevant_pumas,]	


	
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
  
# set occupation to  zero where null
pums_per$occupation[is.na(pums_per$occupation)] <- 0

# add dummy GQ fields to hh table
pums_hh <- pums_hh %>%
  mutate(GQFlag = 0) %>%
  mutate(GQWGTP = 0) %>%
  mutate(GQType = 0)
	
remove(num_workers, relevant_pumas)	

## separate GQ and HH records
# institutional GQs
pums_hh_gq <- pums_hh %>%
  filter(TYPE >=3)
pums_per_gq <- pums_per %>%
  left_join(pums_hh_gq[,c("SERIALNO", "WGTP")],  by = c("SERIALNO")) %>%
  filter(!is.na(WGTP)) %>%
  select(-WGTP)

# remove vacant units and  group quarters households	
pums_hh <- pums_hh %>%	
  filter(!(NP ==0)) %>%	
  filter(TYPE <= 1)	

pums_per <- pums_per %>%
  left_join(pums_hh[,c("SERIALNO", "WGTP")],  by = c("SERIALNO"))

#remove persons belongign to vacant units and  GQ households
pums_per <- pums_per %>%
  filter(!is.na(WGTP)) %>%
  select(-WGTP)

## Code GQ fields
pums_hh_gq <- pums_hh_gq %>%
  left_join(pums_per_gq[,c("SERIALNO", "PWGTP", "SCHG", "MIL")],  by = c("SERIALNO")) %>%
  mutate(GQFlag = 1) %>%
  mutate(GQWGTP = PWGTP) %>%
  mutate(SCHG   = ifelse(is.na(SCHG), 0, SCHG)) %>%
  mutate(MIL    = ifelse(is.na(MIL), 0, MIL)) %>%
  mutate(GQType = ifelse(SCHG==6 | SCHG==7, 1, ifelse(MIL==1, 2, 3))) %>%
  select(-PWGTP, -SCHG, -MIL)
	
#pums_hh  <- rbind(pums_hh, pums_hh_gq)	
#pums_per <- rbind(pums_per, pums_per_gq)


# give each hh a more manageable unique id	
unique_hh_id <- 1:nrow(pums_hh)	
pums_hh <- cbind(pums_hh, unique_hh_id)	
rownames(pums_hh) <- 1:nrow(pums_hh)

unique_hh_id <- 1:nrow(pums_hh_gq)	
pums_hh_gq <- cbind(pums_hh_gq, unique_hh_id)	
rownames(pums_hh_gq) <- 1:nrow(pums_hh_gq)


# set the initial person weight as the hh weight and give the person table the hhnum id	
hh_weights <- pums_hh %>%	
  select(SERIALNO, WGTP, unique_hh_id)	
pums_per <- left_join(pums_per, hh_weights, by = c("SERIALNO"))	

hh_weights_gq <- pums_hh_gq %>%	
  select(SERIALNO, WGTP, unique_hh_id)	
pums_per_gq <- left_join(pums_per_gq, hh_weights_gq, by = c("SERIALNO"))

remove(hh_weights, hh_weights_gq, input_pums_0711_hh, input_pums_0711_per)	

# add GQ_PUMA IDs to GQ HH and person table
pums_hh_gq <- pums_hh_gq %>%
  left_join(puma_GQ_Xwalk, by = "PUMA")%>%
  select(-PUMA)
names(pums_hh_gq)[names(pums_hh_gq) == 'PUMA_GQ'] <- 'PUMA'

pums_per_gq <- pums_per_gq %>%
  left_join(puma_GQ_Xwalk, by = "PUMA") %>%
  select(-PUMA)
names(pums_per_gq)[names(pums_per_gq) == 'PUMA_GQ'] <- 'PUMA'

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

# write the household and person tables for GQ records	
dbWriteTable(conn = mysql_connection, name = paste('gqhousehold_table', PUMS_YEAR, sep = '_'), value = as.data.frame(pums_hh_gq),  overwrite = TRUE)	
dbWriteTable(conn = mysql_connection, name = paste('gqperson_table', PUMS_YEAR, sep = '_'),    value = as.data.frame(pums_per_gq), overwrite = TRUE)	

	
dbDisconnect(mysql_connection)	
	
#### Prepare 2000 PUMS Data	
 	
# get list of relevant pums	
relevant_pumas <- unique(input_geog$PUMA)	
	
pums_hh  <- input_pums_00_hh[input_pums_00_hh$PUMA %in% relevant_pumas,]	
pums_per <- input_pums_00_per[input_pums_00_per$PUMA %in% relevant_pumas,]	


	
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

# presence of people under 18
pums_hh <- pums_hh %>%	
  mutate(p18 = ifelse(is.na(p18), 0, p18)) %>%
  mutate(pres_child = ifelse(p18>0, 1, 0))

# extract the occupation code	
# *appears* to be consistent with 2010 (http://www.census.gov/people/io/files/occ2000t.pdf)	
pums_per <- pums_per %>%	
  mutate(soc = as.numeric(str_sub(occsoc1, start = 1L, end = 2L)))	
	
pums_per <- left_join(pums_per, socp10_occupation_df, by = c("soc"))	
	
table(pums_per$occupation_category)	
	
pums_per <- pums_per %>%	
  select(-occupation_category)	

# add dummy GQ fields to hh table
pums_hh <- pums_hh %>%
  mutate(GQFlag = 0) %>%
  mutate(GQWGTP = 0) %>%
  mutate(GQType = 0)
	
remove(num_workers, relevant_pumas)	

## Separate HH and GQ records
# institutional GQs
pums_hh_gq <- pums_hh %>%
  filter(hht == 0) %>%
  rename(WGTP = hweight)
pums_per_gq <- pums_per %>%
  left_join(pums_hh_gq[,c("serialno", "WGTP")],  by = c("serialno")) %>%
  filter(!is.na(WGTP)) %>%
  select(-WGTP)

# remove vacant units and group quarters households	
pums_hh <- pums_hh %>%	
  filter(!(hht == 0))	 %>%
  rename(WGTP = hweight)
pums_hh_gq <- pums_hh_gq %>%
  left_join(pums_per_gq[,c("serialno", "pweight", "grade", "miltary")],  by = c("serialno")) %>%
  mutate(GQFlag = 1) %>%
  mutate(GQWGTP = pweight) %>%
  mutate(grade   = ifelse(is.na(grade), 0, grade)) %>%
  mutate(miltary    = ifelse(is.na(miltary), 0, miltary)) %>%
  mutate(GQType = ifelse(grade==6 | grade==7, 1, ifelse(miltary==1, 2, 3))) %>%
  select(-pweight, -grade, -miltary)

#pums_hh  <- rbind(pums_hh, pums_hh_gq)	
#pums_per <- rbind(pums_per, pums_per_gq)


# give each hh a more manageable unique id	
unique_hh_id <- 1:nrow(pums_hh)	
pums_hh <- cbind(pums_hh, unique_hh_id)	
rownames(pums_hh) <- 1:nrow(pums_hh)

unique_hh_id <- 1:nrow(pums_hh_gq)	
pums_hh_gq <- cbind(pums_hh_gq, unique_hh_id)	
rownames(pums_hh_gq) <- 1:nrow(pums_hh_gq)

# set the initial person weight as the hh weight and give the person table the hhnum id	
hh_weights <- pums_hh %>%	
  select(serialno, WGTP, unique_hh_id)	
pums_per <- left_join(pums_per, hh_weights, by = c("serialno"))	

hh_weights_gq <- pums_hh_gq %>%	
  select(serialno, WGTP, unique_hh_id)	
pums_per_gq <- left_join(pums_per_gq, hh_weights_gq, by = c("serialno"))

remove(hh_weights, hh_weights_gq, input_pums_00_hh, input_pums_00_per)	

# add GQ_PUMA IDs to GQ HH and person table
pums_hh_gq <- pums_hh_gq %>%
  left_join(puma_GQ_Xwalk, by = "PUMA")%>%
  select(-PUMA)
names(pums_hh_gq)[names(pums_hh_gq) == 'PUMA_GQ'] <- 'PUMA'

pums_per_gq <- pums_per_gq %>%
  left_join(puma_GQ_Xwalk, by = "PUMA") %>%
  select(-PUMA)
names(pums_per_gq)[names(pums_per_gq) == 'PUMA_GQ'] <- 'PUMA'
	
 	
#### Put 2000 data into MySQL database	
 	
# year of the PUMS data	
PUMS_YEAR = "2000"	
	
# connection	
mysql_connection <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)	
	
# write the household and person tables	
dbWriteTable(conn = mysql_connection, name = paste('household_table', PUMS_YEAR, sep = '_'), value = as.data.frame(pums_hh),  overwrite = TRUE)	
dbWriteTable(conn = mysql_connection, name = paste('person_table', PUMS_YEAR, sep = '_'),    value = as.data.frame(pums_per), overwrite = TRUE)	

# write the household and person tables for GQ records	
dbWriteTable(conn = mysql_connection, name = paste('gqhousehold_table', PUMS_YEAR, sep = '_'), value = as.data.frame(pums_hh_gq),  overwrite = TRUE)	
dbWriteTable(conn = mysql_connection, name = paste('gqperson_table', PUMS_YEAR, sep = '_'),    value = as.data.frame(pums_per_gq), overwrite = TRUE)
	
dbDisconnect(mysql_connection)	
	
