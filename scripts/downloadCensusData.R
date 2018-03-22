#################################################################################################################################
# Script to download Block Group Level Census Data [returns data frame]
# requires Census API key
# Example call:
# hhsize_BG <- getCensusData(name = "sf1", 	
#                            vintage = 2010, 	
#                            key = api_key, 	
#                            vars=c("H0130001", "H0130002", "H0130003", "H0130004", "H0130005", "H0130006", "H0130007", "H0130008"), 	
#                            region = paste("block+group:", "*", sep = ""),	
#                            regionin = paste("state:", STATE_FIPS, "+county:01", sep = ""))
# region and regionin can be modified to access data at different geographic level
# Census API reference:
# http://api.census.gov/data/2010/sf1/variables.html
# http://api.census.gov/data/2010/sf1/examples.html
# http://api.census.gov/data.html
# https://github.com/hrecht/censusapi [censusapi package, works for County level data]
################################################################################################################################
message("=== Running downloadCensusData.R")

# LIBRARIES
############
if (!"httr" %in% installed.packages()) install.packages("httr", repos='http://cran.us.r-project.org')
library(httr)

# FUNCTIONS
############
getCensusData <- function(name, vintage, key, vars, region, regionin){
  
  url <- paste("https://api.census.gov/data", vintage, name, sep = "/")
  url <- paste(url, "?get=", sep = "")
  var_list <- vars[1]
  if(length(vars)>1){
    for(i in 2:length(vars)){
      var_list <- paste(var_list, vars[i], sep = ",")
    }
  }
  
  url <- paste(url, var_list,sep = "")
  url <- paste(url, "&for=", sep = "")
  url <- paste(url, region, sep = "")
  url <- paste(url, "&in=", sep = "")
  url <- paste(url, regionin, sep = "")
  url <- paste(url, "&key=", sep = "")
  url <- paste(url, key, sep = "")
  #cat(url)
  get_data <- GET(url)
  raw_data <- content(get_data, as="parsed")
  temp_data <- as.data.frame(t(as.data.frame(lapply(raw_data, unlist), stringsAsFactors = F)),stringsAsFactors = F)
  
  row.names(temp_data) <- NULL
  colnames(temp_data) <- temp_data[1,]
  temp_data <- temp_data[-1,]
  temp_data <- apply(temp_data, 2, function(x) as.numeric(x))
  temp_data <- as.data.frame(temp_data)
  
  return(temp_data)
  
}

