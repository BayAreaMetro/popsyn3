--Setting up seed tables for Metropolitan Transportation Commission - MTC PopSynIII
--Sriram Narayanamoorthy, narayanamoorthys@pbworld.com, 082014
-----------------------------------------------------------------------------------

SET NOCOUNT ON;

IF OBJECT_ID('dbo.psam_h06') IS NOT NULL 
	DROP TABLE dbo.psam_h06;
IF OBJECT_ID('dbo.psam_p06') IS NOT NULL 
	DROP TABLE dbo.psam_p06;
IF OBJECT_ID('dbo.hhtable') IS NOT NULL 
	DROP TABLE dbo.hhtable;
IF OBJECT_ID('dbo.perstable') IS NOT NULL 
	DROP TABLE dbo.perstable;

IF OBJECT_ID('tempdb..#geographicCWalk') IS NOT NULL 
	DROP TABLE #geographicCWalk;
IF OBJECT_ID('tempdb..#PUMALIST') IS NOT NULL 
	DROP TABLE #PUMALIST;
IF OBJECT_ID('tempdb..#tempHH') IS NOT NULL 
	DROP TABLE #tempHH;
IF OBJECT_ID('tempdb..#tempPer') IS NOT NULL 
	DROP TABLE #tempPer;
IF OBJECT_ID('tempdb..#Numbers') IS NOT NULL 
	DROP TABLE #Numbers;
GO

/*###################################################################################################*/
--									INPUT FILE LOCATIONS
/*###################################################################################################*/

DECLARE @pumsHH_File VARCHAR(256);
DECLARE @pumsPersons_File VARCHAR(256);
DECLARE @geographicCWalk_File VARCHAR(256);
DECLARE @query VARCHAR(1000);
DECLARE @pumaList VARCHAR(1000);

--Input files
SET @pumsHH_File = (SELECT filename FROM csv_filenames WHERE dsc = 'pumsHH_File');
SET @pumsPersons_File = (SELECT filename FROM csv_filenames WHERE dsc = 'pumsPersons_File');
SET @geographicCWalk_File = (SELECT filename FROM csv_filenames WHERE dsc = 'geographicCWalk_File');

/*###################################################################################################*/
--									SETTING UP PUMS DATABASE
/*###################################################################################################*/

CREATE TABLE [dbo].[psam_h06](
	[RT] VARCHAR(5) NULL,
	[SERIALNO] BIGINT NOT NULL,
	[DIVISION] INT NULL,
	[PUMA] INT NULL,
	[REGION] INT NULL,
	[ST] INT NULL,
	[ADJHSG] INT NULL,
	[ADJINC] [int] NULL,
	[WGTP] INT NULL,
	[NP] INT NULL,
	[TYPE] INT NULL,
	[ACR] INT NULL,
	[AGS] INT NULL,
	[BDS] INT NULL,
	[BLD] INT NULL,
	[BUS] INT NULL,
	[CONP] INT NULL,
	[ELEP] INT NULL,
	[FS] INT NULL,
	[FULP] INT NULL,
	[GASP] INT NULL,
	[HFL] INT NULL,
	[INSP] INT NULL,
	[MHP] INT NULL,
	[MRGI] INT NULL,
	[MRGP] INT NULL,
	[MRGT] INT NULL,
	[MRGX] INT NULL,
	[RMS] INT NULL,
	[RNTM] INT NULL,
	[RNTP] INT NULL,
	[SMP] INT NULL,
	[TEL] INT NULL,
	[TEN] INT NULL,
	[VACS] INT NULL,
	[VAL] INT NULL,
	[VEH] INT NULL,
	[WATP] INT NULL,
	[YBL] INT NULL,
	[FES] INT NULL,
	[FINCP] INT NULL,
	[FPARC] INT NULL,
	[GRNTP] INT NULL,
	[GRPIP] INT NULL,
	[HHL] INT NULL,
	[HHT] INT NULL,
	[HINCP] INT NULL,
	[HUGCL] INT NULL,
	[HUPAC] INT NULL,
	[HUPAOC] INT NULL,
	[HUPARC] INT NULL,
	[KIT] INT NULL,
	[LNGI] INT NULL,
	[MV] INT NULL,
	[NOC] INT NULL,
	[NPF] INT NULL,
	[NPP] INT NULL,
	[NR] INT NULL,
	[NRC] INT NULL,
	[OCPIP] INT NULL,
	[PARTNER] INT NULL,
	[PLM] INT NULL,
	[PSF] INT NULL,
	[R18] INT NULL,
	[R60] INT NULL,
	[R65] INT NULL,
	[RESMODE] INT NULL,
	[SMOCP] INT NULL,
	[SMX] INT NULL,
	[SRNT] INT NULL,
	[SVAL] INT NULL,
	[TAXP] INT NULL,
	[WIF] INT NULL,
	[WKEXREL] INT NULL,
	[WORKSTAT] INT NULL,
	[FACRP] INT NULL,
	[FAGSP] INT NULL,
	[FBDSP] INT NULL,
	[FBLDP] INT NULL,
	[FBUSP] INT NULL,
	[FCONP] INT NULL,
	[FELEP] INT NULL,
	[FFSP] INT NULL,
	[FFULP] INT NULL,
	[FGASP] INT NULL,
	[FHFLP] INT NULL,
	[FINSP] INT NULL,
	[FKITP] INT NULL,
	[FMHP] INT NULL,
	[FMRGIP] INT NULL,
	[FMRGP] INT NULL,
	[FMRGTP] INT NULL,
	[FMRGXP] INT NULL,
	[FMVP] INT NULL,
	[FPLMP] INT NULL,
	[FRMSP] INT NULL,
	[FRNTMP] INT NULL,
	[FRNTP] INT NULL,
	[FSMP] INT NULL,
	[FSMXHP] INT NULL,
	[FSMXSP] INT NULL,
	[FTAXP] INT NULL,
	[FTELP] INT NULL,
	[FTENP] INT NULL,
	[FVACSP] INT NULL,
	[FVALP] INT NULL,
	[FVEHP] INT NULL,
	[FWATP] INT NULL,
	[FYBLP] INT NULL,
	[WGTP1] INT NULL,
	[WGTP2] INT NULL,
	[WGTP3] INT NULL,
	[WGTP4] INT NULL,
	[WGTP5] INT NULL,
	[WGTP6] INT NULL,
	[WGTP7] INT NULL,
	[WGTP8] INT NULL,
	[WGTP9] INT NULL,
	[WGTP10] INT NULL,
	[WGTP11] INT NULL,
	[WGTP12] INT NULL,
	[WGTP13] INT NULL,
	[WGTP14] INT NULL,
	[WGTP15] INT NULL,
	[WGTP16] INT NULL,
	[WGTP17] INT NULL,
	[WGTP18] INT NULL,
	[WGTP19] INT NULL,
	[WGTP20] INT NULL,
	[WGTP21] INT NULL,
	[WGTP22] INT NULL,
	[WGTP23] INT NULL,
	[WGTP24] INT NULL,
	[WGTP25] INT NULL,
	[WGTP26] INT NULL,
	[WGTP27] INT NULL,
	[WGTP28] INT NULL,
	[WGTP29] INT NULL,
	[WGTP30] INT NULL,
	[WGTP31] INT NULL,
	[WGTP32] INT NULL,
	[WGTP33] INT NULL,
	[WGTP34] INT NULL,
	[WGTP35] INT NULL,
	[WGTP36] INT NULL,
	[WGTP37] INT NULL,
	[WGTP38] INT NULL,
	[WGTP39] INT NULL,
	[WGTP40] INT NULL,
	[WGTP41] INT NULL,
	[WGTP42] INT NULL,
	[WGTP43] INT NULL,
	[WGTP44] INT NULL,
	[WGTP45] INT NULL,
	[WGTP46] INT NULL,
	[WGTP47] INT NULL,
	[WGTP48] INT NULL,
	[WGTP49] INT NULL,
	[WGTP50] INT NULL,
	[WGTP51] INT NULL,
	[WGTP52] INT NULL,
	[WGTP53] INT NULL,
	[WGTP54] INT NULL,
	[WGTP55] INT NULL,
	[WGTP56] INT NULL,
	[WGTP57] INT NULL,
	[WGTP58] INT NULL,
	[WGTP59] INT NULL,
	[WGTP60] INT NULL,
	[WGTP61] INT NULL,
	[WGTP62] INT NULL,
	[WGTP63] INT NULL,
	[WGTP64] INT NULL,
	[WGTP65] INT NULL,
	[WGTP66] INT NULL,
	[WGTP67] INT NULL,
	[WGTP68] INT NULL,
	[WGTP69] INT NULL,
	[WGTP70] INT NULL,
	[WGTP71] INT NULL,
	[WGTP72] INT NULL,
	[WGTP73] INT NULL,
	[WGTP74] INT NULL,
	[WGTP75] INT NULL,
	[WGTP76] INT NULL,
	[WGTP77] INT NULL,
	[WGTP78] INT NULL,
	[WGTP79] INT NULL,
	[WGTP80] INT NULL,

	CONSTRAINT [PK dbo.psam_h11 SERIALNO]
      PRIMARY KEY (SERIALNO)

 );
SET @query = ('BULK INSERT psam_h06
				FROM ' + '''' + @pumsHH_File + '''' + '
				WITH (      FIELDTERMINATOR = '','',
							FIRSTROW =2, MAXERRORS = 0, TABLOCK,
							ROWTERMINATOR = '''+CHAR(10)+''')');
EXEC(@query);

CREATE TABLE [dbo].[psam_p06](
	[RT] VARCHAR(1) NULL,
	[SERIALNO] BIGINT NOT NULL,
	[SPORDER] INT NOT NULL,
	[PUMA] INT NULL,
	[ST] INT NULL,
	[ADJINC] INT NULL,
	[PWGTP] INT NULL,
	[AGEP] INT NULL,
	[CIT] INT NULL,
	[COW] INT NULL,
	[ENG] INT NULL,
	[FER] INT NULL,
	[GCL] INT NULL,
	[GCM] INT NULL,
	[GCR] INT NULL,
	[INTP] INT NULL,
	[JWMNP] INT NULL,
	[JWRIP] INT NULL,
	[JWTR] INT NULL,
	[LANX] INT NULL,
	[MAR] INT NULL,
	[MIG] INT NULL,
	[MIL] INT NULL,
	[MLPA] INT NULL,
	[MLPB] INT NULL,
	[MLPC] INT NULL,
	[MLPD] INT NULL,
	[MLPE] INT NULL,
	[MLPF] INT NULL,
	[MLPG] INT NULL,
	[MLPH] INT NULL,
	[MLPI] INT NULL,
	[MLPJ] INT NULL,
	[MLPK] INT NULL,
	[NWAB] INT NULL,
	[NWAV] INT NULL,
	[NWLA] INT NULL,
	[NWLK] INT NULL,
	[NWRE] INT NULL,
	[OIP] INT NULL,
	[PAP] INT NULL,
	[RELP] INT NULL,
	[RETP] INT NULL,
	[SCH] INT NULL,
	[SCHG] INT NULL,
	[SCHL] INT NULL,
	[SEMP] INT NULL,
	[SEX] INT NULL,
	[SSIP] INT NULL,
	[SSP] INT NULL,
	[WAGP] INT NULL,
	[WKHP] INT NULL,
	[WKL] INT NULL,
	[WKW] INT NULL,
	[YOEP] INT NULL,
	[ANC] INT NULL,
	[ANC1P] INT NULL,
	[ANC2P] INT NULL,
	[DECADE] INT NULL,
	[DRIVESP] INT NULL,
	[ESP] INT NULL,
	[ESR] INT NULL,
	[HISP] INT NULL,
	[JWAP] INT NULL,
	[JWDP] INT NULL,
	[LANP] INT NULL,
	[MIGPUMA] INT NULL,
	[MIGSP] INT NULL,
	[MSP] INT NULL,
	[NATIVITY] INT NULL,
	[NOP] INT NULL,
	[OC] INT NULL,
	[PAOC] INT NULL,
	[PERNP] INT NULL,
	[PINCP] INT NULL,
	[POBP] INT NULL,
	[POVPIP] INT NULL,
	[POWPUMA] INT NULL,
	[POWSP] INT NULL,
	[QTRBIR] INT NULL,
	[RAC1P] INT NULL,
	[RAC2P] INT NULL,
	[RAC3P] INT NULL,
	[RACAIAN] INT NULL,
	[RACASN] INT NULL,
	[RACBLK] INT NULL,
	[RACNHPI] INT NULL,
	[RACNUM] INT NULL,
	[RACSOR] INT NULL,
	[RACWHT] INT NULL,
	[RC] INT NULL,
	[SFN] INT NULL,
	[SFR] INT NULL,
	[VPS] INT NULL,
	[WAOB] INT NULL,
	[FAGEP] INT NULL,
	[FANCP] INT NULL,
	[FCITP] INT NULL,
	[FCOWP] INT NULL,
	[FENGP] INT NULL,
	[FESRP] INT NULL,
	[FFERP] INT NULL,
	[FGCLP] INT NULL,
	[FGCMP] INT NULL,
	[FGCRP] INT NULL,
	[FHISP] INT NULL,
	[FINDP] INT NULL,
	[FINTP] INT NULL,
	[FJWDP] INT NULL,
	[FJWMNP] INT NULL,
	[FJWRIP] INT NULL,
	[FJWTRP] INT NULL,
	[FLANP] INT NULL,
	[FLANXP] INT NULL,
	[FMARP] INT NULL,
	[FMIGP] INT NULL,
	[FMIGSP] INT NULL,
	[FMILPP] INT NULL,
	[FMILSP] INT NULL,
	[FOCCP] INT NULL,
	[FOIP] INT NULL,
	[FPAP] INT NULL,
	[FPOBP] INT NULL,
	[FPOWSP] INT NULL,
	[FRACP] INT NULL,
	[FRELP] INT NULL,
	[FRETP] INT NULL,
	[FSCHGP] INT NULL,
	[FSCHLP] INT NULL,
	[FSCHP] INT NULL,
	[FSEMP] INT NULL,
	[FSEXP] INT NULL,
	[FSSIP] INT NULL,
	[FSSP] INT NULL,
	[FWAGP] INT NULL,
	[FWKHP] INT NULL,
	[FWKLP] INT NULL,
	[FWKWP] INT NULL,
	[FYOEP] INT NULL,
	[PWGTP1] INT NULL,
	[PWGTP2] INT NULL,
	[PWGTP3] INT NULL,
	[PWGTP4] INT NULL,
	[PWGTP5] INT NULL,
	[PWGTP6] INT NULL,
	[PWGTP7] INT NULL,
	[PWGTP8] INT NULL,
	[PWGTP9] INT NULL,
	[PWGTP10] INT NULL,
	[PWGTP11] INT NULL,
	[PWGTP12] INT NULL,
	[PWGTP13] INT NULL,
	[PWGTP14] INT NULL,
	[PWGTP15] INT NULL,
	[PWGTP16] INT NULL,
	[PWGTP17] INT NULL,
	[PWGTP18] INT NULL,
	[PWGTP19] INT NULL,
	[PWGTP20] INT NULL,
	[PWGTP21] INT NULL,
	[PWGTP22] INT NULL,
	[PWGTP23] INT NULL,
	[PWGTP24] INT NULL,
	[PWGTP25] INT NULL,
	[PWGTP26] INT NULL,
	[PWGTP27] INT NULL,
	[PWGTP28] INT NULL,
	[PWGTP29] INT NULL,
	[PWGTP30] INT NULL,
	[PWGTP31] INT NULL,
	[PWGTP32] INT NULL,
	[PWGTP33] INT NULL,
	[PWGTP34] INT NULL,
	[PWGTP35] INT NULL,
	[PWGTP36] INT NULL,
	[PWGTP37] INT NULL,
	[PWGTP38] INT NULL,
	[PWGTP39] INT NULL,
	[PWGTP40] INT NULL,
	[PWGTP41] INT NULL,
	[PWGTP42] INT NULL,
	[PWGTP43] INT NULL,
	[PWGTP44] INT NULL,
	[PWGTP45] INT NULL,
	[PWGTP46] INT NULL,
	[PWGTP47] INT NULL,
	[PWGTP48] INT NULL,
	[PWGTP49] INT NULL,
	[PWGTP50] INT NULL,
	[PWGTP51] INT NULL,
	[PWGTP52] INT NULL,
	[PWGTP53] INT NULL,
	[PWGTP54] INT NULL,
	[PWGTP55] INT NULL,
	[PWGTP56] INT NULL,
	[PWGTP57] INT NULL,
	[PWGTP58] INT NULL,
	[PWGTP59] INT NULL,
	[PWGTP60] INT NULL,
	[PWGTP61] INT NULL,
	[PWGTP62] INT NULL,
	[PWGTP63] INT NULL,
	[PWGTP64] INT NULL,
	[PWGTP65] INT NULL,
	[PWGTP66] INT NULL,
	[PWGTP67] INT NULL,
	[PWGTP68] INT NULL,
	[PWGTP69] INT NULL,
	[PWGTP70] INT NULL,
	[PWGTP71] INT NULL,
	[PWGTP72] INT NULL,
	[PWGTP73] INT NULL,
	[PWGTP74] INT NULL,
	[PWGTP75] INT NULL,
	[PWGTP76] INT NULL,
	[PWGTP77] INT NULL,
	[PWGTP78] INT NULL,
	[PWGTP79] INT NULL,
	[PWGTP80] INT NULL,
	[indp02] VARCHAR(10) NULL,
	[naicsp02] VARCHAR(10) NULL,
	[occp02] VARCHAR(10) NULL,
	[socp00] VARCHAR(10) NULL,
	[occp10] VARCHAR(10) NULL,
	[socp10] VARCHAR(10) NULL,
	[indp07] VARCHAR(10) NULL,
	[naicsp07] VARCHAR(10) NULL,

	CONSTRAINT [PK dbo.psam_p11 SPORDER, SERIALNO]
      PRIMARY KEY (SPORDER, SERIALNO)
 );
SET @query = ('BULK INSERT psam_p06
				FROM ' + '''' + @pumsPersons_File + '''' + '
				WITH (      FIELDTERMINATOR = '','',
							FIRSTROW =2, MAXERRORS = 0, TABLOCK,
							ROWTERMINATOR = '''+CHAR(10)+''')');
EXEC(@query);

PRINT 'Loaded raw PUMS datasets...'

--Loading the geographic correspondence to get unique PUMA list
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

/*###################################################################################################*/
--									PROCESSING GENERAL POPULATION
/*###################################################################################################*/
--Retrieve the unique list of PUMAs covering the region and convert to a comma separated text string
SELECT DISTINCT PUMA INTO #PUMALIST
FROM #geographicCWalk

SELECT @pumaList = COALESCE(@pumaList + ',', '') +  CONVERT(varchar(12),PUMA)
FROM #PUMALIST
ORDER BY PUMA

--Loading matching PUMA records into a temporary table
SET @query = 'SELECT * INTO tempHH FROM dbo.psam_h06 WHERE PUMA IN (' + @pumaList + ')';
EXEC(@query);

SET @query = 'SELECT * INTO tempPer FROM dbo.psam_p06 WHERE PUMA IN (' + @pumaList + ')';
EXEC(@query)

--#temptables created using dynamic SQL has a diferent scope and can't be referenced by the main SQL script
--HACK: Transferring data between a normal and a temporary table and dropping the normal table
--TODO: Create #temp table body and use INSERT syntax
SELECT * INTO #tempHH FROM tempHH
DROP TABLE dbo.tempHH

SELECT * INTO #tempPer FROM tempPer
DROP TABLE dbo.tempPer

DELETE FROM #tempHH
	WHERE NP=0			--Deleting records of Vacant Units
	OR
	TYPE>1;				--Deleting Group Quarters records
GO

ALTER table #tempHH
	ADD	nwrkrs_esr INT,
		hhincAdj REAL;
GO

--Setting number of workers in HH based on Employment Status Recode [ESR] attribute in PUMS Person File
UPDATE #tempHH SET nwrkrs_esr=temp.nwesr
	FROM (SELECT SERIALNO, COUNT(ESR) AS nwesr FROM #tempPer 
			WHERE ESR IN (1,2,4,5)
			GROUP BY SERIALNO) AS temp
	WHERE #tempHH.SERIALNO=temp.SERIALNO;

UPDATE #tempHH SET nwrkrs_esr = 0 WHERE nwrkrs_esr IS NULL

--Adjusting the household income to 2010 inflation adjusted dollars
-- 2010$s = (Reported income)*(Rolling reference factor for the year)*(Inflation adjustment)
UPDATE #tempHH
	SET hhincAdj = (CASE
						WHEN ADJINC=1102938 THEN ((hincp/1.0)*1.016787*1.05156)
						WHEN ADJINC=1063801 THEN ((hincp/1.0)*1.018389*1.01265)
						WHEN ADJINC=1048026 THEN ((hincp/1.0)*0.999480*1.01651)
						WHEN ADJINC=1039407 THEN ((hincp/1.0)*1.007624*1.00000)
						WHEN ADJINC=1018237 THEN ((hincp/1.0)*1.018237*0.96942)
						ELSE 999
					END)

PRINT 'Completed processing seed household table...'
		
ALTER table #tempPer
	ADD	wgtp INT,
		employed INT,
		soc VARCHAR(50),
		occp INT;
GO

--Deleting all GQ person records
DELETE b FROM #tempPer b 
  LEFT JOIN #tempHH a ON a.SERIALNO = b.SERIALNO 
      WHERE a.SERIALNO IS NULL
GO

--Setting person weight as HH weight for use in the initial iteration of balancing
UPDATE #tempPer SET wgtp = a.wgtp FROM (SELECT hh.SERIALNO, hh.WGTP
											FROM #tempPer per
											LEFT OUTER JOIN #tempHH hh
											ON per.SERIALNO = hh.SERIALNO
										) AS a WHERE a.SERIALNO=#tempPer.SERIALNO;

--Determining employment status of the person based on employment status recode
UPDATE #tempPer SET employed=(CASE
								WHEN ESR IN (1,2,4,5) THEN 1
								ELSE 0
							END)

--Filtering out the SOC major group
UPDATE #tempPer SET soc = (CASE 
								WHEN (employed = 0) THEN '0'
								ELSE LEFT(LTRIM(RTRIM(socp00)),2)
							END
							);

UPDATE #tempPer SET soc = LEFT(LTRIM(RTRIM(socp10)),2) WHERE soc='N.'

--Setting the occupation category for the person based on SOC major group
UPDATE #tempPer
SET occp = (CASE 
					WHEN soc IN (11) THEN '1'						--Management
					WHEN soc IN (13,15,17,19,23,25,29) THEN '2'		--Professional
					WHEN soc IN (21,27,31,33,39,43) THEN '3'		--Services
					WHEN soc IN (35,41) THEN '4'					--Retail
					WHEN soc IN (37,45,47,49,51,53) THEN '5'		--Manual
					WHEN soc IN (55) THEN '6'						--Military
					ELSE '999'										--NLF
			END
			);

PRINT 'Completed processing seed person table...'

------------------------------------------------------------------------------------
SELECT * INTO hhtable FROM #tempHH
SELECT * INTO perstable FROM #tempPer

--Adding primary key to the household table
ALTER TABLE hhtable
	ADD CONSTRAINT [PK dbo.hhtable SERIALNO] 
	PRIMARY KEY (SERIALNO);

--Adding primary key to the person table
ALTER TABLE perstable
	ADD CONSTRAINT [PK dbo.perstable SERIALNO,sporder] 
	PRIMARY KEY (SERIALNO,sporder)

PRINT 'Created seed tables for general population...'

/*###################################################################################################*/
--									OPTIMIZING DATABASE
/*###################################################################################################*/
--General population
--Generating household and person ID for use in PopSynIII
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE
GO
BEGIN TRANSACTION
ALTER TABLE dbo.hhtable
   ADD hhnum INT IDENTITY 
   CONSTRAINT [UQ dbo.hhtable hhnum] UNIQUE
   GO
COMMIT TRANSACTION
SET TRANSACTION ISOLATION LEVEL READ COMMITTED

ALTER TABLE dbo.perstable
   ADD hhnum INT
   GO

--Rebuilding index for optimizing database performance
ALTER INDEX ALL ON dbo.hhtable
REBUILD WITH (FILLFACTOR = 80, SORT_IN_TEMPDB = ON,
              STATISTICS_NORECOMPUTE = ON);
GO

ALTER INDEX ALL ON dbo.perstable
REBUILD WITH (FILLFACTOR = 80, SORT_IN_TEMPDB = ON,
              STATISTICS_NORECOMPUTE = ON);
GO

PRINT 'Finished rebuilding indexes...'

--Linking person to HH using the user-defined ID
UPDATE P
SET hhnum = H.hhnum
FROM dbo.hhtable AS H
JOIN dbo.perstable AS P
    ON P.SERIALNO = H.SERIALNO
OPTION (LOOP JOIN);

PRINT 'Seed table creation complete!'

--SELECT * FROM hhtable
--SELECT * FROM perstable