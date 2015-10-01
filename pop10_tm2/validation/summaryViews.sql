--Metropolitan Transportation Commission - MTC PopSynIII Validation Scripts
--Creates views that summarizes the outputs - compare with control totals to validate
--Sriram Narayanamoorthy, narayanamoorthys@pbworld.com, 082014
-------------------------------------------------------------------------------------
USE [MTCPopSynIII]
GO

IF OBJECT_ID ('mazSummaryVIEW', 'V') IS NOT NULL
	DROP VIEW mazSummaryVIEW;
IF OBJECT_ID ('tazSummaryVIEW', 'V') IS NOT NULL
	DROP VIEW tazSummaryVIEW;
IF OBJECT_ID ('metaSummaryVIEW', 'V') IS NOT NULL
	DROP VIEW metaSummaryVIEW;
GO
IF OBJECT_ID ('uniformity', 'V') IS NOT NULL
	DROP VIEW uniformity;
GO

CREATE VIEW mazSummaryVIEW
AS 
SELECT MAZ
	,SUM(finalweight) AS HH
FROM dbo.synpop_hh
GROUP BY MAZ
GO

CREATE VIEW tazSummaryVIEW
AS 
SELECT TAZ
	,SUM(CASE WHEN ((hhincAdj >= -99999999) AND (hhincAdj < 30000)) THEN finalweight ELSE 0 END) AS HHINC1
	,SUM(CASE WHEN ((hhincAdj >= 30000) AND (hhincAdj < 60000)) THEN finalweight ELSE 0 END) AS HHINC2
	,SUM(CASE WHEN ((hhincAdj >= 60000) AND (hhincAdj < 100000)) THEN finalweight ELSE 0 END) AS HHINC3
	,SUM(CASE WHEN (hhincAdj >= 100000) THEN finalweight ELSE 0 END) AS HHINC4
FROM dbo.synpop_hh
GROUP BY TAZ
GO

CREATE VIEW metaSummaryVIEW
AS 
SELECT t1.*
		,t2.AGE0018 ,t2.AGE1964 ,t2.AGE65PLUS
		,t2.OCCP_Management ,t2.OCCP_Professional ,t2.OCCP_Services ,t2.OCCP_Retail ,t2.OCCP_Manual ,t2.OCCP_Military
FROM (SELECT MTCCountyID
	,SUM(CASE WHEN np=1 THEN finalweight ELSE 0 END) AS HHSIZE1
	,SUM(CASE WHEN np=2 THEN finalweight ELSE 0 END) AS HHSIZE2
	,SUM(CASE WHEN np=3 THEN finalweight ELSE 0 END) AS HHSIZE3
	,SUM(CASE WHEN np>=4 THEN finalweight ELSE 0 END) AS HHSIZE4PLUS
	,SUM(CASE WHEN nwrkrs_esr=0 THEN finalweight ELSE 0 END) AS HHWORK0
	,SUM(CASE WHEN nwrkrs_esr=1 THEN finalweight ELSE 0 END) AS HHWORK1
	,SUM(CASE WHEN nwrkrs_esr=2 THEN finalweight ELSE 0 END) AS HHWORK2
	,SUM(CASE WHEN nwrkrs_esr>=3 THEN finalweight ELSE 0 END) AS HHWORK3PLUS
FROM dbo.synpop_hh
GROUP BY MTCCountyID) t1
JOIN (SELECT MTCCountyID
		,SUM(CASE WHEN ((agep >= 0)	AND (agep <= 18)) THEN finalweight ELSE 0 END) AS AGE0018
		,SUM(CASE WHEN ((agep >= 19)	AND (agep <= 64)) THEN finalweight ELSE 0 END) AS AGE1964
		,SUM(CASE WHEN ((agep >= 65)) THEN finalweight ELSE 0 END) AS AGE65PLUS
		,SUM(CASE WHEN occp=1 THEN finalweight ELSE 0 END) AS OCCP_Management
		,SUM(CASE WHEN occp=2 THEN finalweight ELSE 0 END) AS OCCP_Professional
		,SUM(CASE WHEN occp=3 THEN finalweight ELSE 0 END) AS OCCP_Services
		,SUM(CASE WHEN occp=4 THEN finalweight ELSE 0 END) AS OCCP_Retail
		,SUM(CASE WHEN occp=5 THEN finalweight ELSE 0 END) AS OCCP_Manual
		,SUM(CASE WHEN occp=6 THEN finalweight ELSE 0 END) AS OCCP_Military
FROM dbo.synpop_person
GROUP BY MTCCountyID) AS t2 
ON t1.MTCCountyID = t2.MTCCountyID
GO

CREATE VIEW uniformity
AS
SELECT t1.*, t2.FINALWEIGHT FROM
(SELECT SERIALNO, PUMA, WGTP FROM dbo.hhtable) t1
LEFT JOIN (SELECT SERIALNO,
			MAX(WGTP) AS INITIALWEIGHT, 
			SUM(finalweight) AS FINALWEIGHT
FROM dbo.synpop_hh GROUP BY serialno) AS t2
ON t1.SERIALNO = t2.SERIALNO
GO