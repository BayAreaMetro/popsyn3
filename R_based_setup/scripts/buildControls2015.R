############################################################################################################
# Script to build 2015 controls using 2010 controls
#
# All controls are scaled in proportion to rate at which households change between 2010 and 2015
# Number of HHs are available at MAZ level for both 2010 and 2015
#
# Author: binny.mathewpaul@rsginc.com Mar 2017
###########################################################################################################

# LIBRARIES
############
suppressMessages(library(dplyr))

# INPUTS
#########

#DATA_DIR <- paste(WORKING_DIR, paste("data"), sep = "\\")
controls2010Dir <- paste(WORKING_DIR, paste("data\\year_2010"), sep = "\\")
controls2015Dir <- paste(WORKING_DIR, paste("data\\year_2015"), sep = "\\")

# read 2010 controls
maz2010 <- read.csv(paste(controls2010Dir, "mazControlFile.csv", sep = "\\"))
taz2010 <- read.csv(paste(controls2010Dir, "tazControlFile.csv", sep = "\\"))
county2010 <- read.csv(paste(controls2010Dir, "countyControlFile.csv", sep = "\\"))

geogXWalk <- read.csv(paste(DATA_DIR, "geographicCWalk.csv", sep = "\\"))
tazCounty <- geogXWalk[,c("TAZ_ORIGINAL", "MTCCountyID")] %>%
  group_by(TAZ_ORIGINAL) %>%
  summarise(MTCCountyID = max(MTCCountyID))

# read 2015 data
maz2015Data <- read.csv(paste(controls2015Dir, "hh15_maz.csv", sep = "\\"))

# PROCESS DATA
##############

# get HHs at all geographic levels for 2010
maz_hh_10 <- maz2010[, c("maz_original","HH")]
colnames(maz_hh_10) <- c("MAZ_ORIGINAL", "HH00")

taz_hh_10 <- maz_hh_10 %>%
  left_join(geogXWalk[,c("MAZ_ORIGINAL", "TAZ_ORIGINAL")], by = "MAZ_ORIGINAL") %>%
  group_by(TAZ_ORIGINAL) %>%
  summarise(HH00 = sum(HH00)) %>%
  ungroup()

county_hh_10 <- maz_hh_10 %>%
  left_join(geogXWalk[,c("MAZ_ORIGINAL", "MTCCountyID")], by = "MAZ_ORIGINAL") %>%
  group_by(MTCCountyID) %>%
  summarise(HH00 = sum(HH00)) %>%
  ungroup()

# get HHs at all geographic levels for 2015
maz_hh_15 <- maz2015Data %>%
  mutate(HH = HHq1+HHq2+HHq3+HHq4) %>%
  select(MAZ, HH)
colnames(maz_hh_15) <- c("MAZ_ORIGINAL", "HH05")

taz_hh_15 <- maz_hh_15 %>%
  left_join(geogXWalk[,c("MAZ_ORIGINAL", "TAZ_ORIGINAL")], by = "MAZ_ORIGINAL") %>%
  group_by(TAZ_ORIGINAL) %>%
  summarise(HH05 = sum(HH05)) %>%
  ungroup()

county_hh_15 <- maz_hh_15 %>%
  left_join(geogXWalk[,c("MAZ_ORIGINAL", "MTCCountyID")], by = "MAZ_ORIGINAL") %>%
  group_by(MTCCountyID) %>%
  summarise(HH05 = sum(HH05)) %>%
  ungroup()

# compute factors for each geogrpahic level
maz_factor <- maz_hh_10 %>%
  left_join(maz_hh_15, by = "MAZ_ORIGINAL") %>%
  mutate(factor = ifelse(HH00>0, HH05/HH00, 0)) %>%
  mutate(case_zero = ifelse(HH00==0 & HH05>0, 1, 0))

taz_factor <- taz_hh_10 %>%
  left_join(taz_hh_15, by = "TAZ_ORIGINAL") %>%
  mutate(factor = ifelse(HH00>0, HH05/HH00, 0)) %>%
  mutate(case_zero = ifelse(HH00==0 & HH05>0, 1, 0))

county_factor <- county_hh_10 %>%
  left_join(county_hh_15, by = "MTCCountyID") %>%
  mutate(factor = ifelse(HH00>0, HH05/HH00, 0))

# compute county level control totals of MAZ and TAZ controls
mazCountyControls <- maz2010 %>%
  left_join(geogXWalk[,c("MAZ_ORIGINAL", "MTCCountyID")], by = c("maz_original"="MAZ_ORIGINAL")) %>%
  group_by(MTCCountyID) %>%
  summarise(HH = sum(HH), HHSize1 = sum(HHSize1), HHSize2 = sum(HHSize2), HHSize3 = sum(HHSize3), HHSize4p = sum(HHSize4p)) %>%
  ungroup() %>%
  mutate(psize1 = ifelse(HH>0, HHSize1/HH, 0)) %>%
  mutate(psize2 = ifelse(HH>0, HHSize2/HH, 0)) %>%
  mutate(psize3 = ifelse(HH>0, HHSize3/HH, 0)) %>%
  mutate(psize4 = ifelse(HH>0, HHSize4p/HH, 0)) %>%
  select(MTCCountyID, psize1, psize2, psize3, psize4)

              
tazCountyControls <- taz2010 %>%
  left_join(tazCounty[,c("TAZ_ORIGINAL", "MTCCountyID")], by = c("taz_original"="TAZ_ORIGINAL")) %>%
  group_by(MTCCountyID) %>%
  summarise(WI_KIDS = sum(WI_KIDS), WO_KIDS = sum(WO_KIDS), WORKERS0 = sum(WORKERS0), WORKERS1 = sum(WORKERS1), 
            WORKERS2 = sum(WORKERS2), WORKERS3 = sum(WORKERS3), INC00_30 = sum(INC00_30), INC30_60 = sum(INC30_60), 
            INC60_100 = sum(INC60_100), INC100p = sum(INC100p), Age_00_19 = sum(Age_00_19), Age_20_34 = sum(Age_20_34), 
            Age_35_64 = sum(Age_35_64), Age_65_up = sum(Age_65_up)) %>%
  ungroup() %>%
  mutate(HH = WI_KIDS+WO_KIDS) %>%
  mutate(pkid1 = ifelse(HH>0, WI_KIDS/HH, 0)) %>%
  mutate(pkid2 = ifelse(HH>0, WO_KIDS/HH, 0)) %>%
  mutate(pwork0 = ifelse(HH>0, WORKERS0/HH, 0)) %>%
  mutate(pwork1 = ifelse(HH>0, WORKERS1/HH, 0)) %>%
  mutate(pwork2 = ifelse(HH>0, WORKERS2/HH, 0)) %>%
  mutate(pwork3 = ifelse(HH>0, WORKERS3/HH, 0)) %>%
  mutate(pinc1 = ifelse(HH>0, INC00_30/HH, 0)) %>%
  mutate(pinc2 = ifelse(HH>0, INC30_60/HH, 0)) %>%
  mutate(pinc3 = ifelse(HH>0, INC60_100/HH, 0)) %>%
  mutate(pinc4 = ifelse(HH>0, INC100p/HH, 0)) %>%
  mutate(page1 = ifelse(HH>0, Age_00_19/HH, 0)) %>%
  mutate(page2 = ifelse(HH>0, Age_20_34/HH, 0)) %>%
  mutate(page3 = ifelse(HH>0, Age_35_64/HH, 0)) %>%
  mutate(page4 = ifelse(HH>0, Age_65_up/HH, 0)) %>%
  select(MTCCountyID, pkid1, pkid2, pwork0, pwork1, pwork2, pwork3, pinc1, pinc2, pinc3, pinc4, page1, page2, page3, page4)


### CREATE 2015 CONTROL FILES
tempMAZ05 <- maz2010 %>%
  left_join(maz_factor, by = c("maz_original"="MAZ_ORIGINAL")) %>%
  left_join(geogXWalk[,c("MAZ_ORIGINAL", "MTCCountyID")], by = c("maz_original"="MAZ_ORIGINAL")) %>%
  left_join(mazCountyControls, by = "MTCCountyID") %>%
  mutate(HH = HH05) %>%
  mutate(HHSize1 = round(HHSize1*factor)) %>%
  mutate(HHSize2 = round(HHSize2*factor)) %>%
  mutate(HHSize3 = round(HHSize3*factor)) %>%
  mutate(HHSize4p = round(HHSize4p*factor)) %>%
  mutate(HHSize1 = round(ifelse(case_zero==1, HH*psize1, HHSize1))) %>%
  mutate(HHSize2 = round(ifelse(case_zero==1, HH*psize2, HHSize2))) %>%
  mutate(HHSize3 = round(ifelse(case_zero==1, HH*psize3, HHSize3))) %>%
  mutate(HHSize4p = round(ifelse(case_zero==1, HH*psize4, HHSize4p))) %>%
  select(maz_original, HH, HHSize1, HHSize2, HHSize3, HHSize4p)

tempTAZ05 <- taz2010 %>%
  left_join(taz_factor, by = c("taz_original"="TAZ_ORIGINAL")) %>%
  left_join(tazCounty[,c("TAZ_ORIGINAL", "MTCCountyID")], by = c("taz_original"="TAZ_ORIGINAL")) %>%
  left_join(tazCountyControls, by = "MTCCountyID") %>%
  mutate(HH = HH05) %>%
  mutate(WI_KIDS = round(WI_KIDS*factor)) %>%
  mutate(WO_KIDS = round(WO_KIDS*factor)) %>%
  mutate(INC00_30 = round(INC00_30*factor)) %>%
  mutate(INC30_60 = round(INC30_60*factor)) %>%
  mutate(INC60_100 = round(INC60_100*factor)) %>%
  mutate(INC100p = round(INC100p*factor)) %>%
  mutate(WORKERS0 = round(WORKERS0*factor)) %>%
  mutate(WORKERS1 = round(WORKERS1*factor)) %>%
  mutate(WORKERS2 = round(WORKERS2*factor)) %>%
  mutate(WORKERS3 = round(WORKERS3*factor)) %>%
  mutate(Age_00_19 = round(Age_00_19*factor)) %>%
  mutate(Age_20_34 = round(Age_20_34*factor)) %>%
  mutate(Age_35_64 = round(Age_35_64*factor)) %>%
  mutate(Age_65_up = round(Age_65_up*factor)) %>%
  mutate(WI_KIDS = round(ifelse(case_zero==1, HH*pkid1, WI_KIDS))) %>%
  mutate(WO_KIDS = round(ifelse(case_zero==1, HH*pkid2, WO_KIDS))) %>%
  mutate(INC00_30 = round(ifelse(case_zero==1, HH*pinc1, INC00_30))) %>%
  mutate(INC30_60 = round(ifelse(case_zero==1, HH*pinc2, INC30_60))) %>%
  mutate(INC60_100 = round(ifelse(case_zero==1, HH*pinc3, INC60_100))) %>%
  mutate(INC100p = round(ifelse(case_zero==1, HH*pinc4, INC100p))) %>%
  mutate(WORKERS0 = round(ifelse(case_zero==1, HH*pwork0, WORKERS0))) %>%
  mutate(WORKERS1 = round(ifelse(case_zero==1, HH*pwork1, WORKERS1))) %>%
  mutate(WORKERS2 = round(ifelse(case_zero==1, HH*pwork2, WORKERS2))) %>%
  mutate(WORKERS3 = round(ifelse(case_zero==1, HH*pwork3, WORKERS3))) %>%
  mutate(Age_00_19 = round(ifelse(case_zero==1, HH*page1, Age_00_19))) %>%
  mutate(Age_20_34 = round(ifelse(case_zero==1, HH*page2, Age_20_34))) %>%
  mutate(Age_35_64 = round(ifelse(case_zero==1, HH*page3, Age_35_64))) %>%
  mutate(Age_65_up = round(ifelse(case_zero==1, HH*page4, Age_65_up))) %>%
  select(taz_original, HH, WI_KIDS, WO_KIDS, INC00_30, INC30_60, INC60_100, INC100p, 
         WORKERS0, WORKERS1, WORKERS2, WORKERS3, Age_00_19, Age_20_34, Age_35_64, Age_65_up)

tempCounty05 <- county2010 %>%
  left_join(county_factor, by = c("mtc_county_id"="MTCCountyID")) %>%
  mutate(HH = HH05) %>%
  mutate(occupation_management = round(occupation_management*factor)) %>%
  mutate(occupation_manual = round(occupation_manual*factor)) %>%
  mutate(occupation_military = round(occupation_military*factor)) %>%
  mutate(occupation_professional = round(occupation_professional*factor)) %>%
  mutate(occupation_retail = round(occupation_retail*factor)) %>%
  mutate(occupation_services = round(occupation_services*factor))

# round and fix inconsistencies
mazControlFile <- tempMAZ05 %>%
  mutate(HHSize_Diff = HH - HHSize1-HHSize2-HHSize3-HHSize4p) %>%
  mutate(HHSize_Max  = pmax(HHSize1,HHSize2,HHSize3,HHSize4p)) %>%
  mutate(HHSize1     = ifelse(HHSize_Diff!=0 & HHSize1 ==HHSize_Max, pmax(HHSize1 +HHSize_Diff, 0), HHSize1 )) %>%
  mutate(HHSize_Diff = HH - HHSize1-HHSize2-HHSize3-HHSize4p) %>%
  mutate(HHSize2     = ifelse(HHSize_Diff!=0 & HHSize2 ==HHSize_Max, pmax(HHSize2 +HHSize_Diff, 0), HHSize2 )) %>%
  mutate(HHSize_Diff = HH - HHSize1-HHSize2-HHSize3-HHSize4p) %>%
  mutate(HHSize3     = ifelse(HHSize_Diff!=0 & HHSize3 ==HHSize_Max, pmax(HHSize3 +HHSize_Diff, 0), HHSize3 )) %>%
  mutate(HHSize_Diff = HH - HHSize1-HHSize2-HHSize3-HHSize4p) %>%
  mutate(HHSize4p    = ifelse(HHSize_Diff!=0 & HHSize4p==HHSize_Max, pmax(HHSize4p+HHSize_Diff, 0), HHSize4p)) %>%
  select(-HHSize_Diff, -HHSize_Max)

tazControlFile <- tempTAZ05 %>%
  mutate(Kids_Diff   = HH - WI_KIDS-WO_KIDS) %>%
  mutate(Kids_Max    = pmax(WI_KIDS,WO_KIDS)) %>%
  mutate(WI_KIDS     = ifelse(Kids_Diff!=0 & WI_KIDS ==Kids_Max, pmax(WI_KIDS +Kids_Diff, 0), WI_KIDS )) %>%
  mutate(Kids_Diff   = HH - WI_KIDS-WO_KIDS) %>%
  mutate(WO_KIDS     = ifelse(Kids_Diff!=0 & WO_KIDS ==Kids_Max, pmax(WO_KIDS +Kids_Diff, 0), WO_KIDS )) %>%
  mutate(Workers_Diff= HH - WORKERS0-WORKERS1-WORKERS2-WORKERS3) %>%
  mutate(Workers_Max = pmax(WORKERS0,WORKERS1,WORKERS2,WORKERS3)) %>%
  mutate(WORKERS0    = ifelse(Workers_Diff!=0 & WORKERS0 ==Workers_Max, pmax(WORKERS0 +Workers_Diff, 0), WORKERS0)) %>%
  mutate(Workers_Diff= HH - WORKERS0-WORKERS1-WORKERS2-WORKERS3) %>%
  mutate(WORKERS1    = ifelse(Workers_Diff!=0 & WORKERS1 ==Workers_Max, pmax(WORKERS1 +Workers_Diff, 0), WORKERS1)) %>%
  mutate(Workers_Diff= HH - WORKERS0-WORKERS1-WORKERS2-WORKERS3) %>%
  mutate(WORKERS2    = ifelse(Workers_Diff!=0 & WORKERS2 ==Workers_Max, pmax(WORKERS2 +Workers_Diff, 0), WORKERS2)) %>%
  mutate(Workers_Diff= HH - WORKERS0-WORKERS1-WORKERS2-WORKERS3) %>%
  mutate(WORKERS3    = ifelse(Workers_Diff!=0 & WORKERS3 ==Workers_Max, pmax(WORKERS3 +Workers_Diff, 0), WORKERS3)) %>%
  mutate(inc_Diff    = HH - INC00_30-INC30_60-INC60_100-INC100p) %>%
  mutate(inc_Max     = pmax(INC00_30,INC30_60,INC60_100,INC100p)) %>%
  mutate(INC00_30    = ifelse(inc_Diff!=0 & INC00_30 ==inc_Max, pmax(INC00_30 +inc_Diff, 0), INC00_30)) %>%
  mutate(inc_Diff    = HH - INC00_30-INC30_60-INC60_100-INC100p) %>%
  mutate(INC30_60    = ifelse(inc_Diff!=0 & INC30_60 ==inc_Max, pmax(INC30_60 +inc_Diff, 0), INC30_60)) %>%
  mutate(inc_Diff    = HH - INC00_30-INC30_60-INC60_100-INC100p) %>%
  mutate(INC60_100   = ifelse(inc_Diff!=0 & INC60_100 ==inc_Max, pmax(INC60_100 +inc_Diff, 0), INC60_100)) %>%
  mutate(inc_Diff    = HH - INC00_30-INC30_60-INC60_100-INC100p) %>%
  mutate(INC100p     = ifelse(inc_Diff!=0 & INC100p ==inc_Max, pmax(INC100p +inc_Diff, 0), INC100p)) %>%
  select(-Kids_Diff, -Kids_Max, -Workers_Diff, -Workers_Max, -inc_Diff, -inc_Max)
  
  
# WRITE OUTPUTS
###############
write.csv(mazControlFile, paste(controls2015Dir, "mazControlFile.csv", sep = "\\"), row.names = F)
write.csv(tazControlFile, paste(controls2015Dir, "tazControlFile.csv", sep = "\\"), row.names = F)
write.csv(tempCounty05, paste(controls2015Dir, "countyControlFile.csv", sep = "\\"), row.names = F)









