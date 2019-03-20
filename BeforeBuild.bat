REM -ma5 - RAR5 format
REM -m5 - compression rate
REM -s - solid archive type
REM -dh - open shared files
del sources.rar
"C:\Program Files\WinRAR\rar.exe" a -ma5 -m5 -s -dh -x*.exe -x*.dcu -x*.~* -x*.ini -x*.rar -x*.zip -xsources.res sources.rar *.* packages