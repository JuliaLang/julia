set RootPath to (path to me)
set JuliaPath to POSIX path of ((RootPath as text) & "Contents:Resources:julia:bin:julia")
set JuliaFile to POSIX file JuliaPath
do shell script "open -a Terminal '" & JuliaFile & "'"
