
:: path details
set JAVA_PATH=C:\Program Files\Java\jdk1.7.0_71\bin
set ANT_PATH=C:\apache-ant-1.9.7\bin
set REPOSITORY_DIR=M:\Development\Travel Model Two\Source

set PATH=%JAVA_PATH%;%ANT_PATH%

:: robust copy over the source; it's not here since we cannot post it on github
C:\windows\System32\robocopy /MIR "%REPOSITORY_DIR%\popsyn3\src" src
call ant all


