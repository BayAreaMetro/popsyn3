--Setting up MAZ, TAZ and META control tables for Metropolitan Transportation Commission - MTC PopSynIII
--Sriram Narayanamoorthy, narayanamoorthys@pbworld.com, 082014

--This implementation of PopSyn uses these geographies
--	MAZ  -> MTC TM2 MAZ System
--	TAZ  -> MTC TM2 TAZ System
--	META -> Bay Area Counties
--------------------------------------------------------------------------------
SET NOCOUNT ON;	

--Removing existing tables from previous runs
IF OBJECT_ID('dbo.control_totals_maz') IS NOT NULL 
	DROP TABLE control_totals_maz;
IF OBJECT_ID('dbo.control_totals_taz') IS NOT NULL 
	DROP TABLE control_totals_taz;
IF OBJECT_ID('dbo.control_totals_meta') IS NOT NULL 
	DROP TABLE control_totals_meta;

IF OBJECT_ID('tempdb..#mazData') IS NOT NULL 
	DROP TABLE #mazData;
IF OBJECT_ID('tempdb..#tazData') IS NOT NULL 
	DROP TABLE #tazData;
IF OBJECT_ID('tempdb..#countyData') IS NOT NULL 
	DROP TABLE #countyData;
IF OBJECT_ID('tempdb..#personByAge') IS NOT NULL 
	DROP TABLE #personByAge;
IF OBJECT_ID('tempdb..#geographicCWalk') IS NOT NULL 
	DROP TABLE #geographicCWalk;

/*###################################################################################################*/
--									INPUT FILE LOCATIONS
/*###################################################################################################*/
DECLARE @mazData_File VARCHAR(256);
DECLARE @tazData_File VARCHAR(256);
DECLARE @countyData_File VARCHAR(256);
DECLARE @personsByAge_File VARCHAR(256);
DECLARE @geographicCWalk_File VARCHAR(256);
DECLARE @query VARCHAR(1000);

--Input files
SET @mazData_File = (SELECT filename FROM csv_filenames WHERE dsc = 'mazData_File');
SET @tazData_File = (SELECT filename FROM csv_filenames WHERE dsc = 'tazData_File');
SET @countyData_File = (SELECT filename FROM csv_filenames WHERE dsc = 'countyData_File');
SET @personsByAge_File = (SELECT filename FROM csv_filenames WHERE dsc = 'personsByAge_File');
SET @geographicCWalk_File = (SELECT filename FROM csv_filenames WHERE dsc = 'geographicCWalk_File');

/*###################################################################################################*/
--							  SETTING UP TEMPORARY TABLES FOR RAW INPUTS
/*###################################################################################################*/
--Loading MAZ data table
CREATE TABLE #mazData ([MAZ_ORIGINAL] INT
	,[HH] INT
	CONSTRAINT [PK tempdb.mazData MAZ_ORIGINAL] PRIMARY KEY CLUSTERED (MAZ_ORIGINAL)
);
SET @query = ('BULK INSERT #mazData FROM ' + '''' + @mazData_File + '''' + ' WITH (FIELDTERMINATOR = ' + 
				''',''' + ', ROWTERMINATOR = ' + '''\n''' + ', FIRSTROW = 2, MAXERRORS = 0, TABLOCK);');
EXEC(@query);

--Loading TAZ data table
CREATE TABLE #tazData( [TAZ_ORIGINAL] INT
	, [HHINC1] INT
	, [HHINC2] INT
	, [HHINC3] INT
	, [HHINC4] INT

	CONSTRAINT [PK tempdb.tazData TAZ_ORIGINAL]
      PRIMARY KEY (TAZ_ORIGINAL)
);
SET @query = ('BULK INSERT #tazData FROM ' + '''' + @tazData_File + '''' + ' WITH (FIELDTERMINATOR = ' + 
				''',''' + ', ROWTERMINATOR = ' + '''\n''' + ', FIRSTROW = 2, MAXERRORS = 0, TABLOCK);');
EXEC(@query);

--Loading County data table
CREATE TABLE #countyData( [COUNTYNAME] VARCHAR(50)
	,[MTCCountyID] INT
	,[HHSIZE1] INT
	,[HHSIZE2] INT
	,[HHSIZE3] INT
	,[HHSIZE4PLUS] INT
	,[HHWORK0] INT
	,[HHWORK1] INT
	,[HHWORK2] INT
	,[HHWORK3PLUS] INT
	,[OCCP_Management] INT
	,[OCCP_Professional] INT
	,[OCCP_Services] INT
	,[OCCP_Retail] INT
	,[OCCP_Manual] INT
	,[OCCP_Military] INT

	CONSTRAINT [PK tempdb.countyData MTCCountyID]
      PRIMARY KEY (MTCCountyID)
);
SET @query = ('BULK INSERT #countyData FROM ' + '''' + @countyData_File + '''' + ' WITH (FIELDTERMINATOR = ' + 
				''',''' + ', ROWTERMINATOR = ' + '''\n''' + ', FIRSTROW = 2, MAXERRORS = 0, TABLOCK);');
EXEC(@query);

--Loading the County level persons by age data	
CREATE TABLE #personByAge([COUNTY] varchar(50),
		[Age_category] varchar(10),
		[numPeople] INT
);
SET @query = ('BULK INSERT #personByAge FROM ' + '''' + @personsByAge_File + '''' + ' WITH (FIELDTERMINATOR = ' + 
				''',''' + ', ROWTERMINATOR = ' + '''\n''' + ', FIRSTROW = 2, MAXERRORS = 0, TABLOCK);');
EXEC(@query);

--Loading the geographic correspondence MAZ -> TAZ -> PUMA -> COUNTY
CREATE TABLE #geographicCWalk( [MAZ] INT
	,[TAZ] INT
	,[MAZ_ORIGINAL] INT
	,[TAZ_ORIGINAL] INT
	,[PUMA] INT
	,[COUNTYFP] INT
	,[MTCCountyID] INT
	,[COUNTYNAME] VARCHAR(100)
	CONSTRAINT [PK tempdb.geographicCWalk MAZ,TAZ, PUMA, MTCCountyID] PRIMARY KEY CLUSTERED (MAZ,TAZ, PUMA, MTCCountyID)
)
SET @query = ('BULK INSERT #geographicCWalk FROM ' + '''' + @geographicCWalk_File + '''' + ' WITH (FIELDTERMINATOR = ' + 
				''',''' + ', ROWTERMINATOR = ' + '''\n''' + ', FIRSTROW = 2, MAXERRORS = 0, TABLOCK);');
EXEC(@query);

PRINT 'Created raw tables...'

/*###################################################################################################*/
--									CREATING MAZ CONTROL TABLE
/*###################################################################################################*/
--Creating MAZ Controls
SELECT MAZ_ORIGINAL, HH
INTO control_totals_maz
FROM #mazData

ALTER TABLE control_totals_maz
	ADD MAZ INT
		,TAZ INT
		,TAZ_ORIGINAL INT
		,PUMA BIGINT
		,COUNTYFP INT
		,MTCCountyID INT
		,COUNTYNAME VARCHAR(50)
GO

UPDATE control_totals_maz
	SET MAZ = t1.MAZ
		,TAZ = t1.TAZ
		,TAZ_ORIGINAL = t1.TAZ_ORIGINAL
		,PUMA = t1.PUMA
		,COUNTYFP = t1.COUNTYFP
		,MTCCountyID = t1.MTCCountyID
		,COUNTYNAME = t1.COUNTYNAME
	FROM (SELECT * FROM #geographicCWalk) AS t1, 
		control_totals_maz t2
	WHERE (t1.MAZ_ORIGINAL = t2.MAZ_ORIGINAL)


ALTER TABLE dbo.control_totals_maz 
	ALTER COLUMN MAZ INT NOT NULL
GO

ALTER TABLE dbo.control_totals_maz 
	ALTER COLUMN TAZ INT NOT NULL
GO

ALTER TABLE dbo.control_totals_maz 
	ALTER COLUMN PUMA INT NOT NULL
GO

ALTER TABLE dbo.control_totals_maz 
	ALTER COLUMN MTCCountyID INT NOT NULL
GO

ALTER TABLE dbo.control_totals_maz
	ADD CONSTRAINT [PK dbo.control_totals_maz MAZ, TAZ, PUMA, MTCCountyID] 
	PRIMARY KEY (MAZ, TAZ, PUMA, MTCCountyID)
GO
PRINT 'Created MAZ controls...'

/*###################################################################################################*/
--									CREATING TAZ CONTROL TABLE
/*###################################################################################################*/
SELECT t1.*, t2.HHINC1, t2.HHINC2, t2.HHINC3, t2.HHINC4
INTO control_totals_taz 
FROM (SELECT TAZ
		, MAX(TAZ_ORIGINAL) AS TAZ_ORIGINAL
		, MAX(PUMA) AS PUMA
		, MAX(COUNTYFP) AS COUNTYFP
		, MAX(MTCCountyID) AS MTCCountyID
		, SUM(HH) AS HH
		FROM control_totals_maz 
		GROUP BY TAZ) t1
LEFT JOIN (SELECT * FROM #tazData) t2
ON (t1.TAZ_ORIGINAL = t2.TAZ_ORIGINAL)

ALTER TABLE dbo.control_totals_taz
	ADD CONSTRAINT [PK dbo.control_totals_taz TAZ] 
	PRIMARY KEY CLUSTERED (TAZ)
GO

--Temporary fix for missing data [Delete once finalized]
UPDATE control_totals_taz
	SET HHINC1 = (CASE WHEN HHINC1 IS NULL THEN 0 ELSE HHINC1 END)
	, HHINC2 = (CASE WHEN HHINC2 IS NULL THEN 0 ELSE HHINC2 END)
	, HHINC3 = (CASE WHEN HHINC3 IS NULL THEN 0 ELSE HHINC3 END)
	, HHINC4 = (CASE WHEN HHINC4 IS NULL THEN 0 ELSE HHINC4 END)

ALTER TABLE dbo.control_totals_taz
ADD HHI INT
	,CHECKI INT
GO

UPDATE control_totals_taz
	SET HHI = HHINC1 + HHINC2 + HHINC3 + HHINC4

UPDATE dbo.control_totals_taz
	SET CHECKI	=	HHI-HH

ALTER TABLE dbo.control_totals_taz
	DROP COLUMN HHI,CHECKI
GO
PRINT 'Created TAZ controls...'

/*###################################################################################################*/
--								CREATING META CONTROL TABLE
/*###################################################################################################*/
--Processing the persons by age data file
ALTER TABLE #personByAge
ADD	Age INT
GO

UPDATE #personByAge SET COUNTY = LEFT(COUNTY, LEN(COUNTY)-7)

UPDATE #personByAge SET Age=(CASE
									WHEN RTRIM(LTRIM(RIGHT(RTRIM(LTRIM(Age_category)),2)))='0+' THEN 100
									ELSE CAST(RTRIM(LTRIM(RIGHT(RTRIM(LTRIM(Age_category)),2))) AS INT)
								END
								);	

--Creating a unified meta control table
SELECT t2.*, t1.AGE0018, t1.AGE1964, t1.AGE65PLUS
INTO control_totals_meta 
FROM (SELECT COUNTY
			,AGE0018 = SUM(CASE WHEN (AGE < 19) THEN numPeople ELSE 0 END)
			,AGE1964 = SUM(CASE WHEN (AGE >= 19 AND AGE <= 64) THEN numPeople ELSE 0 END)
			,AGE65PLUS = SUM(CASE WHEN (AGE >= 65) THEN numPeople ELSE 0 END)
			FROM #personByAge
			GROUP BY COUNTY) t1
JOIN (SELECT * FROM #countyData) t2
ON (t1.COUNTY = t2.COUNTYNAME)

ALTER TABLE control_totals_meta
	ALTER COLUMN MTCCountyID INT NOT NULL

ALTER TABLE dbo.control_totals_meta
	ADD CONSTRAINT [PK dbo.control_totals_meta MTCCountyID] 
	PRIMARY KEY (MTCCountyID)
GO
PRINT 'Created META controls...'
PRINT 'Control table creation complete!'
--SELECT * FROM control_totals_maz
--SELECT * FROM control_totals_taz
--SELECT * FROM control_totals_meta