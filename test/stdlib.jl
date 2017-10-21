# This file is a part of Julia. License is MIT: https://julialang.org/license

# test default packages

cd(joinpath(JULIA_HOME,"..","share","julia","site","v$(VERSION.major).$(VERSION.minor)")) do
    Pkg.Entry.test(convert(Vector{AbstractString}, readdir()))
end
