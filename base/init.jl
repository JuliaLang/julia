# We need to know the Pkg.jl URI and version for bootstrapping
const PKG_URI = "https://github.com/rofinn/Pkg.jl"
const PKG_VERSION = "v0.2.0"
const VERS = "v$(VERSION.major).$(VERSION.minor)"

_pkg_home() = abspath(JULIA_HOME, "..", "share", "julia", "site", VERS, "Pkg.jl")
_pkg_require() = abspath(JULIA_HOME, "..", "etc", "julia", "REQUIRE")

"""
    initialize(interactive)

Initializes a collection of core julia packages.
If `interactive=true` you will be prompted to select the packages to include by
editing the default REQUIRE file included in base julia.
"""
macro initialize(arg)
    quote
        info("Initializing installation...")
        interactive = $arg

        if !isdir(Base._pkg_home())
            info("Cloning $(Base.PKG_URI) ...")
            repo = Base.LibGit2.clone(Base.PKG_URI, Base._pkg_home())
            info("Checking out $(Base.PKG_VERSION)")
            Base.LibGit2.branch!(
                repo, Base.PKG_VERSION,
                track=Base.LibGit2.Consts.REMOTE_ORIGIN
            )
        end

        info("Loading Pkg.jl")
        Pkg = include(joinpath(Base._pkg_home(), "src", "Pkg.jl"))

        info("Initializing package directory...")
        if !isdir(Pkg.dir()) || !ispath(Pkg.dir("REQUIRE"))
            Pkg.init()
            cp(Base._pkg_require(), Pkg.dir("REQUIRE"); remove_destination=true)

            info("Installing default packages.")
            if interactive
                Pkg.edit()
            else
                Pkg.resolve()
            end

            # NOTE: might want to add `using Pkg` to the juliarc
        else
            info("Package directory $(Pkg.dir()) is already initialized")
        end
    end
end
