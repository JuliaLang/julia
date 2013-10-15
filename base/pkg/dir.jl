module Dir

using Base.Git

const DEFAULT_META = "git://github.com/JuliaLang/METADATA.jl"
const META_BRANCH = "devel"

const DIR_NAME = ".julia"

function path()
    b = abspath(get(ENV,"JULIA_PKGDIR",joinpath(Base.user_prefdir(),DIR_NAME)))
    x, y = VERSION.major, VERSION.minor
    d = joinpath(b,"v$x.$y")
    isdir(d) && return d
    d = joinpath(b,"v$x")
    isdir(d) && return d
    return b
end
path(pkg::String...) = normpath(path(),pkg...)

function cd(f::Function, args...; kws...)
    dir = path()
    if !isdir(dir)
        if haskey(ENV,"JULIA_PKGDIR")
            error("package directory $dir doesn't exist; run Pkg.init() to create it.")
        else
            info("Initializing package repository $dir")
            init()
        end
    end
    Base.cd(()->f(args...; kws...), dir)
end

function init(meta::String=DEFAULT_META)
    dir = path()
    if isdir(joinpath(dir,"METADATA"))
        info("Package directory $dir is already initialized.")
        Git.set_remote_url(meta, dir=joinpath(dir,"METADATA"))
        return
    end
    try
        mkpath(dir)
        Base.cd(dir) do
            info("Cloning METADATA from $meta")
            run(`git clone -q -b $META_BRANCH $meta METADATA`)
            Git.set_remote_url(meta, dir="METADATA")
            run(`touch REQUIRE`)
        end
    catch e
        run(`rm -rf $dir`)
        rethrow(e)
    end
end

end # module
