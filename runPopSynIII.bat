:: ############################################################################
:: # Batch file to run MTC PopSynIII
:: # Binny M Paul, binny.mathewpaul@rsginc.com, Jan 2017
:: # "\\" required for paths to mysql pwd and census api key files
:: # these files are read from read by R shell script
:: ############################################################################

SET WORKING_DIR=%CD%
:: location of files that are not in github
SET BOX_POPSYN_APP=C:\Users\%USERNAME%\Box\Modeling and Surveys\Development\Travel Model Two Development\Population Synthesis\Application
:: robust copy over the census data; we want the local copy because we can download additional data into this
C:\windows\System32\robocopy /MIR "%BOX_POPSYN_APP%\census_data" %CD%\census_data
:: including PUMS, CTPP2000, etc
SET CENSUS_DATA_DIR=%CD%\census_data

:: runtime configuration -- definitions of controls
set RUNTIME_CONFIG_DIR=%CD%\runtime\config
:: input controls, in model year subdirs
SET INPUT_CONTROLS_DIR=%BOX_POPSYN_APP%\input_controls
:: todo: what are these
SET GEOXWALK_DIR=%BOX_POPSYN_APP%\GeographicXWalk
:: output
set OUTPUT_DIR=%CD%\outputs
:: intermediate outputs
set INTERMEDIATE_DIR=%CD%\outputs\intermediate
mkdir %INTERMEDIATE_DIR%

:: MySQL
SET MYSQLSERVER=localhost
SET DATABASE=mtc_popsyn
SET DB_USER=root
SET MYSQL_PWD_FILE=%BOX_POPSYN_APP%\runtime_config\mysql.csv

:: R
SET RSCRIPT_64_PATH=C:\Program Files\R\R-3.4.1\bin\x64
SET R_MAIN_SCRIPT=scripts\R_Shell_Script.R
SET R_POST_PROCESS=scripts\postProcessing.R
SET R_VALIDATION=scripts\MTC_Popsyn_vis.R

:: Census API
SET CENSUS_API_KEY_FILE=%BOX_POPSYN_APP%\runtime_config\census_api_key.csv

:: Parameters file [Do not change file name]
SET PARAMETERS_FILE="%RUNTIME_CONFIG_DIR%\parameters.csv"

:: SET PopSyn_YEAR as year_2000, year_2005, year_2010 or year_2015
:: ----------------------------------------------------------------
SET PopSyn_YEAR=year_2000

:: SET switch to run HH and GQ PopSyn runs [YES/NO]
:: ----------------------------------------------------------------
SET Run_HH_PopSyn=YES
SET Run_GQ_PopSyn=YES
SET Run_PostProcessing=YES
SET Run_Validation=YES

:: Switches [YES/NO] for running data processing steps
:: 1. Step 01 PUMS to Database, 2. Step 02 Build Controls, 3. Step 03 Controls to Database
:: ----------------------------------------------------------------
SET Run_Step_1=YES
SET Run_Step_2=YES
SET Run_Step_3=YES

:: Need to download Census data only once for building controls, can be read for next time [set to TRUE/FALSE]
:: 2000 controls shoudl be build before building 2005. 2010 controls should be build before 2015
SET downloadCensus=TRUE

:: PopSyn settings file
SET settingsFile="settings_%PopSyn_YEAR%.xml"
SET GQsettingsFile="settingsGQ_%PopSyn_YEAR%.xml"

:: ############################################################################
:: ### DATA PROCESSING

@ECHO OFF
ECHO MTC PopSynIII

ECHO %startTime%%Time%: Creating parameters file...
ECHO Key,Value > %PARAMETERS_FILE%
ECHO WORKING_DIR,%CD% >> %PARAMETERS_FILE%
ECHO CENSUS_DATA_DIR,%CENSUS_DATA_DIR% >> %PARAMETERS_FILE%
ECHO RUNTIME_CONFIG_DIR,%RUNTIME_CONFIG_DIR% >> %PARAMETERS_FILE%
ECHO INPUT_CONTROLS_DIR,%INPUT_CONTROLS_DIR% >> %PARAMETERS_FILE%
ECHO GEOXWALK_DIR,%GEOXWALK_DIR% >> %PARAMETERS_FILE%
ECHO INTERMEDIATE_DIR,%INTERMEDIATE_DIR% >> %PARAMETERS_FILE%
ECHO OUTPUT_DIR,%OUTPUT_DIR% >> %PARAMETERS_FILE%
ECHO MYSQL_SERVER,%MYSQLSERVER% >> %PARAMETERS_FILE%
ECHO MYSQL_DATABASE,%DATABASE% >> %PARAMETERS_FILE%
ECHO MYSQL_USER_NAME,%DB_USER% >> %PARAMETERS_FILE%
ECHO MYSQL_PASSWORD_FILE,%MYSQL_PWD_FILE% >> %PARAMETERS_FILE%
ECHO CENSUS_API_KEY_FILE,%CENSUS_API_KEY_FILE% >> %PARAMETERS_FILE%
ECHO PopSyn_YEAR,%PopSyn_YEAR% >> %PARAMETERS_FILE%
ECHO Run_HH_PopSyn,%Run_HH_PopSyn% >> %PARAMETERS_FILE%
ECHO Run_GQ_PopSyn,%Run_GQ_PopSyn% >> %PARAMETERS_FILE%
ECHO Run_Step_1,%Run_Step_1% >> %PARAMETERS_FILE%
ECHO Run_Step_2,%Run_Step_2% >> %PARAMETERS_FILE%
ECHO Run_Step_3,%Run_Step_3% >> %PARAMETERS_FILE%
ECHO downloadCensus,%downloadCensus% >> %PARAMETERS_FILE%

ECHO %startTime%%Time%: Running R scripts to create PUMS Table and controls table...
:: Call R main script to run data processing scripts
"%RSCRIPT_64_PATH%\Rscript" "%R_MAIN_SCRIPT%" "%PARAMETERS_FILE%"
IF ERRORLEVEL 1 goto error
ECHO %startTime%%Time%: PopSyn database creation complete...

:: ############################################################################
:: ### RUN POPSYN

ECHO %startTime%%Time%: Running population synthesizer...
SET JAVA_64_PATH=C:\Program Files\Java\jdk1.8.0_161\jre
SET CLASSPATH=runtime\config;runtime\*;runtime\lib\*;runtime\lib\JPFF-3.2.2\JPPF-3.2.2-admin-ui\lib\*
SET LIBPATH=runtime\lib

:hh_popsyn
IF %Run_HH_PopSyn%==YES (
  ECHO %startTime%%Time%: Starting HH PopSyn run...
  "%JAVA_64_PATH%\bin\java" -showversion -server -Xms15000m -Xmx15000m -cp "%CLASSPATH%" -Djppf.config=jppf-clientLocal.properties -Djava.library.path=%LIBPATH% popGenerator.PopGenerator "%RUNTIME_CONFIG_DIR%/%settingsFile%"
  IF ERRORLEVEL 1 goto error
  ECHO %startTime%%Time%: Population synthesis complete...
)

:gq_popsyn
IF %Run_GQ_PopSyn%==YES (
  ECHO %startTime%%Time%: Starting GQ PopSyn run...
  "%JAVA_64_PATH%\bin\java" -showversion -server -Xms15000m -Xmx15000m -cp "%CLASSPATH%" -Djppf.config=jppf-clientLocal.properties -Djava.library.path=%LIBPATH% popGenerator.PopGenerator "%RUNTIME_CONFIG_DIR%/%GQsettingsFile%"
  IF ERRORLEVEL 1 goto error
  ECHO %startTime%%Time%: Population synthesis for GQ complete...
)

:: ############################################################################
:: ### POST PROCESSING

:post_processing
IF %Run_PostProcessing%==YES (
    ECHO %startTime%%Time%: Post Processing - creating final HH and person files...
    :: Call post processing R script
    "%RSCRIPT_64_PATH%\Rscript" %R_POST_PROCESS% %PARAMETERS_FILE%
    IF ERRORLEVEL 1 goto error
    ECHO %startTime%%Time%: PopSyn database creation complete...
)


:: ############################################################################
:: ### VALIDATION REPORT

:validation
IF %Run_Validation%==YES (
    ECHO %startTime%%Time%: Generate validation plot and summaries for each PopSyn run
    :: Call validation R script
    "%RSCRIPT_64_PATH%\Rscript" %R_VALIDATION% %PARAMETERS_FILE%
    IF ERRORLEVEL 1 goto error
    ECHO %startTime%%Time%: PopSyn validation complete...
)

:error
echo Failed with error %errorlevel%
exit /b %errorlevel%
