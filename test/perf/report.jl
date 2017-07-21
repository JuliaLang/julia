# This file is a part of Julia. License is MIT: https://julialang.org/license

using HTTPClient.HTTPC

env_name = chomp(read(`hostname`, String))
commit = Base.GIT_VERSION_INFO.commit
flavor = ENV["JULIA_FLAVOR"]
json = "{\"env\": \"$env_name\", \"blas\":\"$flavor\", \"commit\":\"$commit\"}"
post("http://status.julialang.org/put/codespeed", json )
