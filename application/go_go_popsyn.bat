:: START HERE
:: TODO:
:: 2. change maz/taz and maz_original/taz_original to maz_consecutive and maz_model? 
:: 3. experiment with JPPF settings to explore run-time improvements (note num threads *appears* to be fixed in PopGenerator class)
:: 4. any mysql optimization that can improve runtime? (lisa task?)
:: 5. do a few source checks on the controls, perhaps scripting the construction
:: 8. explore impact of different sets of controls (2000 PUMS to 2010 forecast and 2010 PUMS to 2000 forecast)
:: 9. using 2000 PUMS may require new geography crosswalk (i do not think so as the 2007-2011 PUMS uses the 2000 geographies; will need to make the adjustment to income variable)
:: 10. any benefit to combining the 2000 and 2007/11 pums into a single, large database, and then use it to build 2000 and 2010 syn pops? 
:: 11. determine if I can re-create the previous validation results for any set of weights
:: 12. what about a person control at the county level with a high weight?
:: 13. introduce control on ages 16-18 (after confirming problem exists for 2000, 2005, and 2010)
:: 14. figure out how group quarters are handled? add cross validation on variable TYPE, but note that PUMS does not give weights to group quarters. so right now when we create the seed list, we
::     remove vacant units and we remove group quarters. so they are not considered in the expansion. what to do about this? we could put them back in and add a county-level control. 
::     how about we run popsyn again to only expand to group quarters. so we have a separate seed list as well as separate controls? And then append the two files?

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
set JAVA_64_PATH=C:\Progra~1\Java\jre1.8.0_101
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



