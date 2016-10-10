:: START HERE
:: TODO (in roughly this order):
:: 3. once this is done, go ahead and check things in to GitHub
:: 3. try and track down the bug when data is written to the database
:: 4. change variable names in inputs and database, changing the settings in the pop syn along the way
:: 5. right a script to bring the output into R and to do validation summaries in R
:: 6. experiment with JPPF settings to explore run-time improvements
:: 7. so get ready to run 2000, 2005, and 2010 with Mike's best inputs 
:: 8. 


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



