set RootPath to (path to me)
set JuliaPath to POSIX path of ((RootPath as text) & "Contents:Resources:julia:bin:julia")
do shell script "open -a Terminal '" & JuliaPath & "'"
