let
    pkgs = ["JSON"]

    JULIA_HOME = ccall(:jl_get_julia_home, Any, ())

    dir = joinpath(JULIA_HOME, "..", "..", "usr", "share", "julia", "site")

    !isdir(dir) && mkdir(dir)

    dir′ = get(ENV, "JULIA_PKGDIR", false)
    ENV["JULIA_PKGDIR"] = dir

    !isdir(joinpath(dir, "v0.4", "METADATA")) && Pkg.init()

    Pkg.update()

    for pkg in pkgs
        if Pkg.installed(pkg) == nothing
            println("Adding $pkg.jl")
            Pkg.add(pkg)
        end
        @eval using $(symbol(pkg))
    end

    dir′ == false ? delete!(ENV, "JULIA_PKGDIR") : (ENV["JULIA_PKGDIR"] = dir′)

    "JSON" in pkgs && @eval export json
end
