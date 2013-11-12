using HTTPClient.HTTPC

env_name = chomp(readall(`hostname`))
commit = Base.BUILD_INFO.commit
flavor = ENV["JULIA_FLAVOR"]
json = "{\"env\": \"$env_name\", \"blas\":\"$flavor\", \"commit\":\"$commit\"}"
post("http://status.julialang.org/put/codespeed", json )
