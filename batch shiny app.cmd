@echo off

for %%a in ("S:\Procurement\Commun\SIOP\Central org SIOP\N. R and shiny\my_app\my_app_v16\tableauCGvendor.csv") do set date1=%%~ta
for %%a in ("tableauCGvendor.csv") do set date2=%%~ta
echo last update : %date1%
echo your update : %date2%

if "%date2%"=="%date1%" goto :prg

set /P choice=Do you want to update your file[Y/N]?
if /I "%choice%" EQU "N" goto :prg

copy /b "S:\Procurement\Commun\SIOP\Central org SIOP\N. R and shiny\my_app\my_app_v16\runapp.r" "runapp.r"
copy /b "S:\Procurement\Commun\SIOP\Central org SIOP\N. R and shiny\my_app\my_app_v16\server.R" "server.R"
copy /b "S:\Procurement\Commun\SIOP\Central org SIOP\N. R and shiny\my_app\my_app_v16\ui.R" "ui.R"
copy /b "S:\Procurement\Commun\SIOP\Central org SIOP\N. R and shiny\my_app\my_app_v16\tableauCGvendor.csv" "tableauCGvendor.csv"
copy /b "S:\Procurement\Commun\SIOP\Central org SIOP\N. R and shiny\my_app\my_app_v16\tableauCGvendorQty.csv" "tableauCGvendorQty.csv"
copy /b "S:\Procurement\Commun\SIOP\Central org SIOP\N. R and shiny\my_app\my_app_v16\tableauCGvendorSemNet.csv" "tableauCGvendorSemNet.csv"
copy /b "S:\Procurement\Commun\SIOP\Central org SIOP\N. R and shiny\my_app\my_app_v16\SupplierName.txt" "SupplierName.txt"

:prg
for %%a in ("tableauCGvendor.csv") do set date2=%%~ta
@echo shiny running on file updated on %date2%

"..\..\R-3.1.3\bin\x64\R.exe" CMD BATCH --slave "..\..\my_app\my_app_v16\runapp.r" "..\..\my_app\my_app_v16\report.txt"

pause