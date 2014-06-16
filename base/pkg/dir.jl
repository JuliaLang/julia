module Dir

import ..Pkg: DEFAULT_META, META_BRANCH
import ..Git

const DIR_NAME = ".julia"

_pkgroot() = abspath(get(ENV,"JULIA_PKGDIR",joinpath(homedir(),DIR_NAME)))
isversioned(p::String) = ((x,y) = (VERSION.major, VERSION.minor); basename(p) == "v$x.$y")

function path()
    b = _pkgroot()
    x, y = VERSION.major, VERSION.minor
    d = joinpath(b,"v$x.$y")
    if isdir(d) || !isdir(b) || !isdir(joinpath(b, "METADATA"))
        return d
    end
    return b
end
path(pkg::String...) = normpath(path(),pkg...)

function cd(f::Function, args...; kws...)
    dir = path()
    if !isdir(dir)
        !haskey(ENV,"JULIA_PKGDIR") ? init() :
            error("package directory $dir doesn't exist; run Pkg.init() to create it.")
    end
    Base.cd(()->f(args...; kws...), dir)
end

function init(meta::String=DEFAULT_META, branch::String=META_BRANCH)
    dir = path()
    info("Initializing package repository $dir")
    if isdir(joinpath(dir,"METADATA"))
        info("Package directory $dir is already initialized.")
        Git.set_remote_url(meta, dir=joinpath(dir,"METADATA"))
        return
    end
    try
        mkpath(joinpath(dir, "METADATA"))
        Base.cd(dir) do
            info("Cloning METADATA from $meta")
            info("Dir $dir, pwd $(pwd())")
            info("Clonar o repo de $(meta) em $(joinpath(dir, "METADATA"))")
            repo = Base.LibGit2.repo_clone(meta, joinpath(dir, "METADATA"))
            info("Checking out HEAD")
            Base.LibGit2.checkout_head!(repo, {:strategy => :safe_create})
            info("Setting remote")
            Git.set_remote_url(meta, dir=joinpath(dir,"METADATA"))
            info("Creating REQUIRE")
            run(`touch REQUIRE`)
        end
    catch e
        rm(dir, recursive=true)
        rethrow(e)
    end
end

end # module
