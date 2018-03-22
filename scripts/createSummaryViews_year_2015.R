####################################################################
#
# Script to create summaries from PopSyn output for validation
# NON GQ 2015
#
####################################################################

# MySQL connection	
channel <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)

dbSendQuery(channel, "drop view if exists mazSummaryVIEW, tazSummaryVIEW, metaSummaryVIEW, uniformity")

mazQuery <- "CREATE VIEW mazSummaryVIEW 
            AS 
            SELECT maz
                ,SUM(finalweight) AS HH
                ,SUM(CASE WHEN np=1 THEN finalweight ELSE 0 END) AS HHSIZE1
                ,SUM(CASE WHEN np=2 THEN finalweight ELSE 0 END) AS HHSIZE2
                ,SUM(CASE WHEN np=3 THEN finalweight ELSE 0 END) AS HHSIZE3
                ,SUM(CASE WHEN np>=4 THEN finalweight ELSE 0 END) AS HHSIZE4PLUS
                FROM synpop_hh_2015
                GROUP BY maz"

tazQuery <- "CREATE VIEW tazSummaryVIEW
             AS 
             SELECT t1.*
               ,t2.AGE0019 ,t2.AGE2034, t2.AGE3564 ,t2.AGE65PLUS
             FROM (SELECT taz
                   ,SUM(CASE WHEN ((hh_income_2010 >= -99999999) AND (hh_income_2010 < 30000)) THEN finalweight ELSE 0 END) AS HHINC1
                   ,SUM(CASE WHEN ((hh_income_2010 >= 30000) AND (hh_income_2010 < 60000)) THEN finalweight ELSE 0 END) AS HHINC2
                   ,SUM(CASE WHEN ((hh_income_2010 >= 60000) AND (hh_income_2010 < 100000)) THEN finalweight ELSE 0 END) AS HHINC3
                   ,SUM(CASE WHEN (hh_income_2010 >= 100000) THEN finalweight ELSE 0 END) AS HHINC4
                   ,SUM(CASE WHEN hh_workers_from_esr=0 THEN finalweight ELSE 0 END) AS HHWORK0
                   ,SUM(CASE WHEN hh_workers_from_esr=1 THEN finalweight ELSE 0 END) AS HHWORK1
                   ,SUM(CASE WHEN hh_workers_from_esr=2 THEN finalweight ELSE 0 END) AS HHWORK2
                   ,SUM(CASE WHEN hh_workers_from_esr>=3 THEN finalweight ELSE 0 END) AS HHWORK3PLUS
                   ,SUM(CASE WHEN ((hupac >= 1)	AND (hupac <= 3)) THEN finalweight ELSE 0 END) AS WIKIDS
                   ,SUM(CASE WHEN ((hupac >= 4)) THEN finalweight ELSE 0 END) AS WOKIDS
                   FROM synpop_hh_2015
                   GROUP BY taz) t1,
             (SELECT taz
              ,SUM(CASE WHEN ((agep >= 0)	AND (agep <= 19)) THEN finalweight ELSE 0 END) AS AGE0019
              ,SUM(CASE WHEN ((agep >= 20)	AND (agep <= 34)) THEN finalweight ELSE 0 END) AS AGE2034
              ,SUM(CASE WHEN ((agep >= 35)	AND (agep <= 64)) THEN finalweight ELSE 0 END) AS AGE3564
              ,SUM(CASE WHEN ((agep >= 65)) THEN finalweight ELSE 0 END) AS AGE65PLUS
              FROM synpop_person_2015
              GROUP BY taz) AS t2 
             WHERE t1.taz = t2.taz"

metaQuery <- "CREATE VIEW metaSummaryVIEW
             AS 
             SELECT t1.*
               ,t2.AGE0019 ,t2.AGE2064 ,t2.AGE65PLUS
             ,t2.OCCP_Management ,t2.OCCP_Professional ,t2.OCCP_Services ,t2.OCCP_Retail ,t2.OCCP_Manual ,t2.OCCP_Military
             FROM (SELECT mtc_county_id
                   ,SUM(CASE WHEN np=1 THEN finalweight ELSE 0 END) AS HHSIZE1
                   ,SUM(CASE WHEN np=2 THEN finalweight ELSE 0 END) AS HHSIZE2
                   ,SUM(CASE WHEN np=3 THEN finalweight ELSE 0 END) AS HHSIZE3
                   ,SUM(CASE WHEN np>=4 THEN finalweight ELSE 0 END) AS HHSIZE4PLUS
                   ,SUM(CASE WHEN hh_workers_from_esr=0 THEN finalweight ELSE 0 END) AS HHWORK0
                   ,SUM(CASE WHEN hh_workers_from_esr=1 THEN finalweight ELSE 0 END) AS HHWORK1
                   ,SUM(CASE WHEN hh_workers_from_esr=2 THEN finalweight ELSE 0 END) AS HHWORK2
                   ,SUM(CASE WHEN hh_workers_from_esr>=3 THEN finalweight ELSE 0 END) AS HHWORK3PLUS
                   FROM synpop_hh_2015
                   GROUP BY mtc_county_id) t1,
             (SELECT mtc_county_id
              ,SUM(CASE WHEN ((agep >= 0)	AND (agep <= 19)) THEN finalweight ELSE 0 END) AS AGE0019
              ,SUM(CASE WHEN ((agep >= 20)	AND (agep <= 64)) THEN finalweight ELSE 0 END) AS AGE2064
              ,SUM(CASE WHEN ((agep >= 65)) THEN finalweight ELSE 0 END) AS AGE65PLUS
              ,SUM(CASE WHEN occupation=1 THEN finalweight ELSE 0 END) AS OCCP_Management
              ,SUM(CASE WHEN occupation=2 THEN finalweight ELSE 0 END) AS OCCP_Professional
              ,SUM(CASE WHEN occupation=3 THEN finalweight ELSE 0 END) AS OCCP_Services
              ,SUM(CASE WHEN occupation=4 THEN finalweight ELSE 0 END) AS OCCP_Retail
              ,SUM(CASE WHEN occupation=5 THEN finalweight ELSE 0 END) AS OCCP_Manual
              ,SUM(CASE WHEN occupation=6 THEN finalweight ELSE 0 END) AS OCCP_Military
              FROM synpop_person_2015
              GROUP BY mtc_county_id) AS t2 
             WHERE t1.mtc_county_id = t2.mtc_county_id"

uniformQuery <- "CREATE VIEW uniformity
                 AS
                 SELECT t1.*, t2.FINALWEIGHT FROM
                 (SELECT SERIALNO, PUMA, WGTP FROM household_table_2007_2011) t1,
                 (SELECT SERIALNO,
                  MAX(WGTP) AS INITIALWEIGHT, 
                  SUM(finalweight) AS FINALWEIGHT
                  FROM synpop_hh_2015 GROUP BY serialno) t2
                 WHERE t1.SERIALNO = t2.SERIALNO"

dbSendQuery(channel, mazQuery)
dbSendQuery(channel, tazQuery)
dbSendQuery(channel, metaQuery)
dbSendQuery(channel, uniformQuery)

dbClearResult(dbListResults(channel)[[1]])
dbDisconnect(channel)














