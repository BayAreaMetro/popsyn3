####################################################################
#
# Script to create summaries from PopSyn output for validation
# GQ 2005
#
####################################################################


# MySQL connection	
channel <- dbConnect(MySQL(), user = MYSQL_USER_NAME, password = mysql_passes$pwd, host = MYSQL_SERVER, dbname = MYSQL_DATABASE)

dbSendQuery(channel, "drop view if exists GQmazSummaryVIEW, GQmetaSummaryVIEW, GQuniformity")

mazQuery <- "CREATE VIEW GQmazSummaryVIEW
             AS 
             SELECT maz
             ,SUM(finalweight) AS TOTGQ
             ,SUM(CASE WHEN GQType=1 THEN finalweight ELSE 0 END) AS GQUNI
             ,SUM(CASE WHEN GQType=2 THEN finalweight ELSE 0 END) AS GQMIL
             ,SUM(CASE WHEN GQType=3 THEN finalweight ELSE 0 END) AS GQOTH
             FROM synpop_hh_gq_2005
             GROUP BY maz"


metaQuery <- "CREATE VIEW GQmetaSummaryVIEW
              AS 
              SELECT region
              	,SUM(finalweight) AS POPGQ
              	FROM synpop_hh_gq_2005
              	GROUP BY region"

uniformQuery <- "CREATE VIEW GQuniformity
                 AS
                 SELECT t1.*, t2.FINALWEIGHT FROM
                 (SELECT SERIALNO, PUMA, GQWGTP AS WGTP FROM gqhousehold_table_2000) t1,
                 (SELECT SERIALNO,
                 			MAX(GQWGTP) AS INITIALWEIGHT, 
                 			SUM(finalweight) AS FINALWEIGHT
                 FROM synpop_hh_gq_2005 GROUP BY serialno) t2
                 WHERE t1.SERIALNO = t2.SERIALNO"

dbSendQuery(channel, mazQuery)
dbSendQuery(channel, metaQuery)
dbSendQuery(channel, uniformQuery)

dbClearResult(dbListResults(channel)[[1]])
dbDisconnect(channel)














