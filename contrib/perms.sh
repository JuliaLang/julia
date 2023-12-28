#!/bin/sh

# This file is a part of Julia. License is MIT: https://julialang.org/license

# run this script to correct perms for locally extracted pre-compiled binaries

# if extraction directory location is other than $HOME modify accordingly

# wget -q https://raw.githubusercontent.com/JuliaLang/julia/master/contrib/perms.sh
# ./perms.sh

if [ -d ~/julia*/share/julia ]; then
  chmod 444 ~/julia*/share/julia/{*.cache,*.pem}
  chmod 555 ~/julia*/share/julia/julia-config.jl
  find ~/julia*/share/julia/{base,stdlib,test} -perm 644 -exec chmod 444 {} \;
fi
