# This file is a part of Julia. License is MIT: https://julialang.org/license

# test default packages

cd(joinpath(JULIA_HOME,"..","share","julia","site","v$(VERSION.major).$(VERSION.minor)")) do
    pkgs = readdir()
    if startswith(string(Sys.ARCH), "arm")
        # Remove profile from default tests on ARM since it currently segfaults
        # Allow explicitly adding it for testing
        warn("Skipping Profile tests")
        filter!(x -> (x != "Profile"), pkgs)
    end
    Pkg.Entry.test(convert(Vector{AbstractString}, pkgs))
end
