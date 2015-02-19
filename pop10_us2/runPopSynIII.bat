
REM # Batch file to run Metropolitan Transportation Commission - MTC PopSynIII
REM # Sriram Narayanamoorthy, narayanamoorthys@pbworld.com, 2013-08-08
REM ###########################################################################

SET SCENARIO=TESTRUN
SET SQLSERVER=LUMODEL\LU_MSSQL
SET DATABASE=popsyn3_pop10_us2
SET MY_PATH=%CD%

SET pumsHH_File='C:\Working\popsyn3\pop10_us2\data\ss11hca.csv'
SET pumsPersons_File='C:\Working\popsyn3\pop10_us2\data\ss11pca.csv'

SET mazData_File='C:\Working\popsyn3\pop10_us2\data\mazData.csv'
SET tazData_File='C:\Working\popsyn3\pop10_us2\data\tazData.csv'
SET countyData_File='C:\Working\popsyn3\pop10_us2\data\countyData.csv'
SET personsByAge_File='C:\Working\popsyn3\pop10_us2\data\personsByAge.csv'

SET geographicCWalk_File='C:\Working\popsyn3\pop10_us2\data\geographicCwalk.csv'

SET settingsFile=settings.xml

REM ###########################################################################

@ECHO OFF
ECHO Metropolitan Transportation Commission - MTC PopSynIII

ECHO Processing input tables...
MKDIR outputs
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "IF OBJECT_ID('dbo.csv_filenames') IS NOT NULL DROP TABLE csv_filenames;" -o "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "CREATE TABLE csv_filenames(dsc varchar(100), filename varchar(256));" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "INSERT INTO csv_filenames(dsc, filename) VALUES ('pumsHH_File', %pumsHH_File%);" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "INSERT INTO csv_filenames(dsc, filename) VALUES ('pumsPersons_File', %pumsPersons_File%);" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "INSERT INTO csv_filenames(dsc, filename) VALUES ('mazData_File', %mazData_File%);" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "INSERT INTO csv_filenames(dsc, filename) VALUES ('tazData_File', %tazData_File%);" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "INSERT INTO csv_filenames(dsc, filename) VALUES ('countyData_File', %countyData_File%);" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "INSERT INTO csv_filenames(dsc, filename) VALUES ('personsByAge_File', %personsByAge_File%);" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "INSERT INTO csv_filenames(dsc, filename) VALUES ('geographicCWalk_File', %geographicCWalk_File%);" >> "%MY_PATH%\outputs\serverLog"

SQLCMD -S %SQLSERVER% -d %DATABASE% -E -i "%MY_PATH%\scripts\PUMS Table creation.sql" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -i "%MY_PATH%\scripts\importControls.sql" >> "%MY_PATH%\outputs\serverLog"

REM ###########################################################################

ECHO %startTime%%Time%
ECHO Running population synthesizer...
SET JAVA_64_PATH=C:\Progra~1\Java\jre1.8.0_25
SET CLASSPATH=runtime\config
SET CLASSPATH=%CLASSPATH%;runtime\*
SET CLASSPATH=%CLASSPATH%;runtime\lib\*
SET CLASSPATH=%CLASSPATH%;runtime\lib\JPFF-3.2.2\JPPF-3.2.2-admin-ui\lib\*
SET LIBPATH=runtime\lib

%JAVA_64_PATH%\bin\java -showversion -server -Xms8000m -Xmx12000m -cp "%CLASSPATH%" -Djppf.config=jppf-clientLocal.properties -Djava.library.path=%LIBPATH% popGenerator.PopGenerator runtime/config/%settingsFile% 
ECHO Population synthesis complete...

REM ###########################################################################

ECHO Create %SCENARIO% schema and output CSV files
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "IF OBJECT_ID('%SCENARIO%.control_totals_maz') IS NOT NULL DROP TABLE %SCENARIO%.control_totals_maz;" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "IF OBJECT_ID('%SCENARIO%.control_totals_taz') IS NOT NULL DROP TABLE %SCENARIO%.control_totals_taz;" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "IF OBJECT_ID('%SCENARIO%.control_totals_meta') IS NOT NULL DROP TABLE %SCENARIO%.control_totals_meta;" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "IF OBJECT_ID('%SCENARIO%.households') IS NOT NULL DROP TABLE %SCENARIO%.households;" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "IF OBJECT_ID('%SCENARIO%.persons') IS NOT NULL DROP TABLE %SCENARIO%.persons;" >> "%MY_PATH%\outputs\serverLog"

SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "IF EXISTS (SELECT * FROM sys.schemas WHERE name = '%SCENARIO%') DROP SCHEMA %SCENARIO%;" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "CREATE SCHEMA %SCENARIO%;" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -i "%MY_PATH%\scripts\outputs.sql" >> "%MY_PATH%\outputs\serverLog"

SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "SELECT * INTO %SCENARIO%.control_totals_maz FROM dbo.control_totals_maz;" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "SELECT * INTO %SCENARIO%.control_totals_taz FROM dbo.control_totals_taz;" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "SELECT * INTO %SCENARIO%.control_totals_meta FROM dbo.control_totals_meta;" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "SELECT * INTO %SCENARIO%.persons FROM dbo.persons;" >> "%MY_PATH%\outputs\serverLog"
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -Q "SELECT * INTO %SCENARIO%.households FROM dbo.households;" >> "%MY_PATH%\outputs\serverLog"

REM # remove row with ----- in SQL tables
SQLCMD -S %SQLSERVER% -d %DATABASE% -E -s, -W -Q "SET NOCOUNT ON; SELECT * FROM dbo.persons" >  "%MY_PATH%\outputs\persons.tmp"
TYPE %MY_PATH%\outputs\persons.tmp | findstr /r /v ^\-[,\-]*$ > %MY_PATH%\outputs\persons2.tmp 
REM # Replace NULL with -9 and N.A. with -8
@ECHO OFF
SETLOCAL
SET PATH=%CD%\%LIBPATH%;%PATH%
type %MY_PATH%\outputs\persons2.tmp |repl "NULL" "-9" |repl "N\.A\." "-8" > %MY_PATH%\outputs\persons.csv
ENDLOCAL
DEL %MY_PATH%\outputs\persons.tmp
DEL %MY_PATH%\outputs\persons2.tmp

SQLCMD -S %SQLSERVER% -d %DATABASE% -E -s, -W -Q "SET NOCOUNT ON; SELECT * FROM dbo.households" >  "%MY_PATH%\outputs\households.tmp"
TYPE %MY_PATH%\outputs\households.tmp | findstr /r /v ^\-[,\-]*$ > %MY_PATH%\outputs\households2.tmp
REM # Replace NULL with -9 and N.A. with -8
@ECHO OFF
SETLOCAL
SET PATH=%CD%\%LIBPATH%;%PATH%
type %MY_PATH%\outputs\households2.tmp |repl "NULL" "-9" |repl "N\.A\." "-8" > %MY_PATH%\outputs\households.csv
ENDLOCAL
DEL %MY_PATH%\outputs\households.tmp
DEL %MY_PATH%\outputs\households2.tmp
