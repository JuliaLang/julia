# This file is a part of Julia. License is MIT: https://julialang.org/license

# source this file to correct perms for locally extracted pre-compiled binaries

# if extraction directory location is other than $home modify accordingly

# iwr -Uri https://raw.githubusercontent.com/JuliaLang/julia/master/contrib/perms.ps1 -OutFile $home\perms.ps1
# . $home\perms.ps1

gci -Path $home\julia*\share\julia -ex compiled | gci -fi * -r -File | % {$_.IsReadOnly=$True}
