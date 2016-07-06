# This file is a part of Julia. License is MIT: http://julialang.org/license

using HTTPClient.HTTPC

env_name = chomp(readstring(`hostname`))
commit = Base.GIT_VERSION_INFO.commit
flavor = ENV["JULIA_FLAVOR"]
json = "{\"env\": \"$env_name\", \"blas\":\"$flavor\", \"commit\":\"$commit\"}"
post("http://status.julialang.org/put/codespeed", json )
