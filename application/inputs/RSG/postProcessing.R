#############################################################################
#
# Script to export final PopSYn outputs from MySQL database
# outputs are expanded and a unique ID is assigned to both HH and person file
# files are exported in CSV formatted, separate file for HH and GQ populations
#
#############################################################################

## Read Command Line Arguments
args            <- commandArgs(trailingOnly = TRUE)
Parameters_File <- args[1]

suppressMessages(library(RMySQL))	
suppressMessages(library(dplyr))

#### INPUTS ####
## Read parameters from Parameters_File
parameters <- read.csv(Parameters_File, header = TRUE)

WORKING_DIR          <- trimws(paste(parameters$Value[parameters$Key=="WORKING_DIR"]))	
MYSQL_SERVER         <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_SERVER"]))
MYSQL_DATABASE       <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_DATABASE"]))
MYSQL_USER_NAME      <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_USER_NAME"]))
MYSQL_PASSWORD_FILE  <- trimws(paste(parameters$Value[parameters$Key=="MYSQL_PASSWORD_FILE"]))
YEAR                 <- trimws(paste(parameters$Value[parameters$Key=="PopSyn_YEAR"]))
Run_HH_PopSyn        <- trimws(paste(parameters$Value[parameters$Key=="Run_HH_PopSyn"]))
Run_GQ_PopSyn        <- trimws(paste(parameters$Value[parameters$Key=="Run_GQ_PopSyn"]))

setwd(paste(WORKING_DIR, "scripts", sep="/"))

DATA_DIR <- paste(WORKING_DIR, "data", sep="/")

GEOG_CONTROL_FILE <-   paste(DATA_DIR, "geographicCWalk.csv", sep = "/")	
input_geog <- read.csv(GEOG_CONTROL_FILE, header = TRUE)	

# get password	
mysql_passes <- read.csv(MYSQL_PASSWORD_FILE, header = TRUE)	

mysql_passes <- mysql_passes %>%	
  filter(user == MYSQL_USER_NAME) %>%	
  mutate(pwd = paste(pwd))	

# connection	
mysql_connection <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)	


### HH Population
if(Run_HH_PopSyn=="YES"){
  # read the synthetic population tables	
  year_num <- substr(YEAR, nchar(YEAR)-3, nchar(YEAR))
  synpop_hh  <- dbReadTable(conn = mysql_connection, name = paste('synpop_hh', year_num, sep = '_'))	
  synpop_hh <- synpop_hh[order(synpop_hh$tempId), ]
  synpop_per <- dbReadTable(conn = mysql_connection, name = paste('synpop_person', year_num, sep = '_'))
  if(year_num<2010){
    synpop_per <- synpop_per[order(synpop_per$tempId, synpop_per$pnum), ]
  }
  if(year_num>=2010){
    synpop_per <- synpop_per[order(synpop_per$tempId, synpop_per$sporder), ]
  }
  
  
  # Expand HH table
  hh.expanded <- synpop_hh[rep(row.names(synpop_hh), synpop_hh$finalweight), ]
  hh.expanded <- hh.expanded[order(hh.expanded$tempId), ]
  hh.expanded$hhid <- seq.int(nrow(hh.expanded))
  if(year_num>=2010){
    df <- hh.expanded[, c("hhid", "tempId", "np")]
  } else {
    df <- hh.expanded[, c("hhid", "tempId", "persons")]
  }
  colnames(df) <- c("hhid", "tempId", "np")
  
  #create HH IDs for expanded person file
  perHHID <- by(df, df$tempId, function(x) {rep(x$hhid, min(x$np))})
  perHHID <- unlist(perHHID)
  
  # Expand person table
  per.expanded <- synpop_per[rep(row.names(synpop_per), synpop_per$finalweight), ]
  if(year_num<2010){
    per.expanded <- per.expanded[order(per.expanded$tempId, per.expanded$pnum), ]
  }
  if(year_num>=2010){
    per.expanded <- per.expanded[order(per.expanded$tempId, per.expanded$sporder), ]
  }
  
  per.expanded$hhid <- perHHID
  
  # Select final set of variables to output
  hh.expanded <- select(hh.expanded, -tempId, -mtc_county_id, -PUMA, -taz)
  hh.expanded <- hh.expanded %>%
    left_join(input_geog[, c("MAZ","TAZ","PUMA5CE00","MTCCountyID")], by = c("maz"="MAZ"))
  
  per.expanded <- select(per.expanded, -tempId, -mtc_county_id, -PUMA, -taz)
  per.expanded <- per.expanded %>%
    left_join(input_geog[, c("MAZ","TAZ","PUMA5CE00","MTCCountyID")], by = c("maz"="MAZ"))
}



### GQ Population
if(Run_GQ_PopSyn=="YES"){
  # read the synthetic population tables	
  year_num <- substr(YEAR, nchar(YEAR)-3, nchar(YEAR))
  synpop_hh_gq  <- dbReadTable(conn = mysql_connection, name = paste('synpop_hh_gq', year_num, sep = '_'))	
  synpop_hh_gq <- synpop_hh_gq[order(synpop_hh_gq$tempId), ]
  synpop_per_gq <- dbReadTable(conn = mysql_connection, name = paste('synpop_person_gq', year_num, sep = '_'))
  if(year_num<2010){
    synpop_per_gq <- synpop_per_gq[order(synpop_per_gq$tempId, synpop_per_gq$pnum), ]
  }
  if(year_num>=2010){
    synpop_per_gq <- synpop_per_gq[order(synpop_per_gq$tempId, synpop_per_gq$sporder), ]
  }
  
  
  # Expand HH table
  hh_gq.expanded <- synpop_hh_gq[rep(row.names(synpop_hh_gq), synpop_hh_gq$finalweight), ]
  hh_gq.expanded <- hh_gq.expanded[order(hh_gq.expanded$tempId), ]
  hh_gq.expanded$hhid <- seq.int(nrow(hh_gq.expanded))
  if(year_num>=2010){
    df <- hh_gq.expanded[, c("hhid", "tempId", "np")]
  } else {
    df <- hh_gq.expanded[, c("hhid", "tempId", "persons")]
  }
  colnames(df) <- c("hhid", "tempId", "np")
  
  #create HH IDs for expanded person file
  perHHID <- by(df, df$tempId, function(x) {rep(x$hhid, min(x$np))})
  perHHID <- unlist(perHHID)
  
  # Expand person table
  per_gq.expanded <- synpop_per_gq[rep(row.names(synpop_per_gq), synpop_per_gq$finalweight), ]
  if(year_num<2010){
    per_gq.expanded <- per_gq.expanded[order(per_gq.expanded$tempId, per_gq.expanded$pnum), ]
  }
  if(year_num>=2010){
    per_gq.expanded <- per_gq.expanded[order(per_gq.expanded$tempId, per_gq.expanded$sporder), ]
  }
  
  per_gq.expanded$hhid <- perHHID
  
  # bring the datasets to standard format
  hh_gq.expanded <- select(hh_gq.expanded, -tempId, -region, -PUMA, -taz)
  names(hh_gq.expanded)[names(hh_gq.expanded) == 'GQWGTP'] <- 'WGTP'
  hh_gq.expanded <- hh_gq.expanded %>%
    left_join(input_geog[, c("MAZ","TAZ","PUMA5CE00","MTCCountyID")], by = c("maz"="MAZ"))
  
  per_gq.expanded <- select(per_gq.expanded, -tempId, -region, -PUMA, -taz)
  names(per_gq.expanded)[names(per_gq.expanded) == 'GQWGTP'] <- 'WGTP'
  per_gq.expanded <- per_gq.expanded %>%
    left_join(input_geog[, c("MAZ","TAZ","PUMA5CE00","MTCCountyID")], by = c("maz"="MAZ"))
}



### Combine HH and GQ populations if both were run
if(Run_HH_PopSyn=="YES" & Run_GQ_PopSyn=='YES'){
  households <- rbind(hh.expanded, hh_gq.expanded)
  persons <- rbind(per.expanded, per_gq.expanded)
}

if(Run_HH_PopSyn=="YES" & Run_GQ_PopSyn=='NO'){
  households <- hh.expanded
  persons <- per.expanded
}

if(Run_HH_PopSyn=="NO" & Run_GQ_PopSyn=='YES'){
  households <- hh_gq.expanded
  persons <- per_gq.expanded
}


# Write outputs
hh_filename <- paste(WORKING_DIR, "/outputs/households_",YEAR, ".csv", sep="")
per_filename <- paste(WORKING_DIR, "/outputs/persons_",YEAR, ".csv", sep="")
write.csv(households, hh_filename, row.names = F)
write.csv(persons, per_filename, row.names = F)

dbDisconnect(mysql_connection)




