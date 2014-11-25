--Post-processing PopSynIII output to generate a fully expanded synthetic population
--Sriram Narayanamoorthy, narayanamoorthys@pbworld.com, 20130729
------------------------------------------------------------------------------------
SET NOCOUNT ON;

--Cleaning up objects created during previous SQL transactions
IF OBJECT_ID('dbo.persons') IS NOT NULL 
	DROP TABLE dbo.persons;
IF OBJECT_ID('dbo.households') IS NOT NULL 
	DROP TABLE dbo.households;
IF OBJECT_ID('tempdb..#Numbers') IS NOT NULL 
	DROP TABLE #Numbers;

IF EXISTS (SELECT * 
	FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_TYPE = 'PRIMARY KEY' 
	AND TABLE_NAME = 'synpop_hh'
	AND TABLE_SCHEMA ='dbo')
		ALTER TABLE [dbo].[synpop_hh] 
			DROP CONSTRAINT [PK dbo.synpop_hh tempId];

IF EXISTS (SELECT * 
	FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_TYPE = 'CHECK' 
	AND TABLE_NAME = 'synpop_hh'
	AND TABLE_SCHEMA ='dbo')
		ALTER TABLE [dbo].[synpop_hh] 
			DROP CONSTRAINT [CK dbo.synpop_hh finalweight 1-500]
	
IF EXISTS (SELECT * 
	FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_TYPE = 'PRIMARY KEY' 
	AND TABLE_NAME = 'synpop_person'
	AND TABLE_SCHEMA ='dbo')
		ALTER TABLE [dbo].[synpop_person] 
			DROP CONSTRAINT [PK dbo.synpop_person tempId,sporder]

IF EXISTS (SELECT * 
	FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_TYPE = 'CHECK' 
	AND TABLE_NAME = 'synpop_person'
	AND TABLE_SCHEMA ='dbo')
		ALTER TABLE [dbo].[synpop_person] 
			DROP CONSTRAINT [CK dbo.synpop_person finalweight 1-500]

------------------------------------------------------------------------------------
--Creating an auxiliary table of numbers for inner join
--Auxiliary table of numbers credit [http://sqlblog.com/blogs/paul_white/default.aspx]
CREATE TABLE tempdb.#Numbers
(
    n INTEGER NOT NULL,

    CONSTRAINT [PK tempdb.#Numbers n]
        PRIMARY KEY CLUSTERED (n)
);

WITH
    N1 AS (SELECT N1.n FROM (VALUES (1),(1),(1),(1),(1),(1),(1),(1),(1),(1)) AS N1 (n)),
    N2 AS (SELECT L.n FROM N1 AS L CROSS JOIN N1 AS R),
    N3 AS (SELECT L.n FROM N2 AS L CROSS JOIN N2 AS R),
    N4 AS (SELECT L.n FROM N3 AS L CROSS JOIN N3 AS R),
    N AS (SELECT ROW_NUMBER() OVER (ORDER BY @@SPID) AS n FROM N4)
INSERT tempdb.#Numbers
    (n)
SELECT TOP (1000000)
    n
FROM N
ORDER BY N.n
OPTION (MAXDOP 1);

------------------------------------------------------------------------------------
--Post-processing synthesized household records	
ALTER TABLE dbo.synpop_hh
	ADD CONSTRAINT [PK dbo.synpop_hh tempId] 
	PRIMARY KEY (tempId)

ALTER TABLE dbo.synpop_hh
	ADD CONSTRAINT [CK dbo.synpop_hh finalweight 1-500]
	CHECK (finalweight BETWEEN 1 AND 500)

SELECT * INTO dbo.households
FROM dbo.synpop_hh AS HH
JOIN tempdb.#Numbers AS N
    ON N.n BETWEEN 1 AND HH.finalweight;

--Adding primary key to the table
ALTER TABLE dbo.households
	ADD CONSTRAINT [PK dbo.households tempId,n] 
	PRIMARY KEY (tempId,n);

------------------------------------------------------------------------------------
--Post-processing synthesized person records
ALTER TABLE dbo.synpop_person
	ALTER COLUMN sporder INT NOT NULL
	GO

ALTER TABLE dbo.synpop_person
	ADD CONSTRAINT [PK dbo.synpop_person tempId,sporder] 
	PRIMARY KEY (tempId,sporder)
	GO

ALTER TABLE dbo.synpop_person
	ADD CONSTRAINT [CK dbo.synpop_person finalweight 1-500]
	CHECK (finalweight BETWEEN 1 AND 500)
	GO

SELECT * INTO dbo.persons
FROM dbo.synpop_person AS PER
JOIN tempdb.#Numbers AS N
    ON N.n BETWEEN 1 AND PER.finalweight;
    
--Adding primary key to the table
ALTER TABLE dbo.persons
	ADD CONSTRAINT [PK dbo.persons tempId,n,sporder] 
	PRIMARY KEY (tempId,n,sporder)

------------------------------------------------------------------------------------
--Generating household and person ID for use in ABM
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE
GO
BEGIN TRANSACTION
ALTER TABLE dbo.households
   ADD HHID INT IDENTITY 
   CONSTRAINT [UQ dbo.households HHID] UNIQUE
   GO
   
ALTER TABLE dbo.persons
   ADD PERID INT IDENTITY
   CONSTRAINT [UQ dbo.persons HHID] UNIQUE
   GO
COMMIT TRANSACTION
SET TRANSACTION ISOLATION LEVEL READ COMMITTED

ALTER TABLE dbo.persons
   ADD HHID INT
   GO

ALTER INDEX ALL ON dbo.households
REBUILD WITH (FILLFACTOR = 80, SORT_IN_TEMPDB = ON,
              STATISTICS_NORECOMPUTE = ON);
GO

ALTER INDEX ALL ON dbo.persons
REBUILD WITH (FILLFACTOR = 80, SORT_IN_TEMPDB = ON,
              STATISTICS_NORECOMPUTE = ON);
GO

UPDATE P
SET HHID = H.HHID
FROM dbo.households AS H
JOIN dbo.persons AS P
    ON P.tempId = H.tempId
    AND P.n = H.n
OPTION (LOOP JOIN);