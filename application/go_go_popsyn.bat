:: START HERE
:: TODO (in roughly this order):
:: 1. get the validation output up and running
:: 2. change maz/taz and maz_original/taz_original to maz_consecutive and maz_model? 
:: 3. experiment with JPPF settings to explore run-time improvements (note num threads *appears* to be fixed in PopGenerator class)
:: 4. any mysql optimization that can improve runtime? (lisa task?)
:: 5. do a few source checks on the controls, perhaps scripting the construction
:: 6. add mag thank you to readme 
:: 7. get base runs for 2000 (and 2005) 
:: 8. explore impact of different sets of controls (2000 PUMS to 2010 forecast and 2010 PUMS to 2000 forecast)
:: 9. using 2000 PUMS may require new geography crosswalk (check with shimon on this)


:: ms-dos batch file to run the population synthesizer

:: step 01: prepare the inputs
:: TODO add call to R script as needed

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



