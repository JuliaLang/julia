# This file is a part of Julia. License is MIT: https://julialang.org/license

# Set HOME to control where the .gitconfig file may be found.
# Note: In Cygwin environments `git` will use HOME instead of USERPROFILE.
# Setting both environment variables ensures home was overridden.
mktempdir() do dir
    dir = realpath(dir)
    withenv("HOME" => dir, "USERPROFILE" => dir) do
        include("online-tests.jl")
    end
end
