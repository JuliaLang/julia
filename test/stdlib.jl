# This file is a part of Julia. License is MIT: https://julialang.org/license

# test default packages

using Pkg

cd(joinpath(JULIA_HOME,"..","share","julia","site","v$(VERSION.major).$(VERSION.minor)")) do
    Pkg.Entry.test(convert(Vector{AbstractString}, readdir()))
end
