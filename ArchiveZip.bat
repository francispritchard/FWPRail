@echo on
rem change to the directory the batch file is in
cd %~dp0
rem create the new batch file programmatically
"C:\Doc\Dropbox\RAD Studio\Projects\ArchiveZip\ArchiveZip" < ArchiveZip.ini > TempZip.bat
rem execute it
call TempZip.bat
rem and erase it afterwards
rem erase TempZip.bat
