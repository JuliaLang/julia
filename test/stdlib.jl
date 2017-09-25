# test default packages

cd(joinpath(JULIA_HOME,"..","share","julia","site","v$(VERSION.major).$(VERSION.minor)")) do
    Pkg.Entry.test(convert(Vector{AbstractString}, readdir()))
end
