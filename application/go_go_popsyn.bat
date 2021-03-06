:: START HERE
:: TODO:
:: 2. change maz/taz and maz_original/taz_original to maz_consecutive and maz_model? 
:: 3. experiment with JPPF settings to explore run-time improvements (note num threads *appears* to be fixed in PopGenerator class)
:: 4. any mysql optimization that can improve runtime? (lisa task?)
:: 8. explore impact of different sets of controls (2000 PUMS to 2010 forecast and 2010 PUMS to 2000 forecast)
:: 10. any benefit to combining the 2000 and 2007/11 pums into a single, large database, and then use it to build 2000 and 2010 syn pops? 
:: 11. determine if I can re-create the previous validation results for any set of weights (may be that they manually adjusted the controls so that totals within categories matched?)
:: 12. introduce a person control at the county level to avoid measure category discrepancies (will this work? if it does, remove the scaling in the validation summaries)
:: 13. introduce control on ages 16-18 (after confirming problem exists for 2000, 2005, and 2010)
:: 14. for group quarters, create another set of settings and run with only MAZ or TAZ or county level controls depending on what Mike R. can provide. Perhaps use MAZ and then scale up or down
::     using a county control. bind the two datasets to run the travel model.
:: 15. build some batch files to run with various weight differences to explore which are best  

:: ms-dos batch file to run the population synthesizer

:: step 01: prepare the inputs
:: inputs are created by inputs/Prepare Inputs.Rmd

:: step 02: build the settings.xml file
set cred_dir=C:\Users\dory\Desktop
set set_dir=.\runtime\config
copy /B %cred_dir%\mysql_credentials.xml+%set_dir%\settings_no_credentials.xml %set_dir%\settings.xml

:: step 03: execute java
echo %Time%
echo Running population synthesizer...
set JAVA_64_PATH=C:\Progra~1\Java\jre1.8.0_111
set CLASSPATH=runtime\config
set CLASSPATH=%CLASSPATH%;runtime\*
set CLASSPATH=%CLASSPATH%;runtime\lib\*
set CLASSPATH=%CLASSPATH%;runtime\lib\JPFF-3.2.2\JPPF-3.2.2-admin-ui\lib\*
set CLASSPATH=%CLASSPATH%;M:\Development\Travel Model Two\Source\cmf\common-base\release
set LIBPATH=runtime\lib

%JAVA_64_PATH%\bin\java -showversion -server -Xms8000m -Xmx12000m -cp "%CLASSPATH%" -Djppf.config=jppf-clientLocal.properties -Djava.library.path=%LIBPATH% popGenerator.PopGenerator runtime/config/settings.xml 

ECHO Population synthesis complete...
echo %Time%

del %set_dir%\settings.xml

:: step 03: consume the output
:: outputs are consumed by outputs/Consume Outputs.Rmd and presented in validation/Year XXXX Validation YYYY



