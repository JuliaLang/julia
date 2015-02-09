module Dir

import ..Pkg: DEFAULT_META, META_BRANCH
import ..Git

const DIR_NAME = ".julia"

_pkgroot() = abspath(get(ENV,"JULIA_PKGDIR",joinpath(homedir(),DIR_NAME)))
isversioned(p::AbstractString) = ((x,y) = (VERSION.major, VERSION.minor); basename(p) == "v$x.$y")

function path()
    b = _pkgroot()
    x, y = VERSION.major, VERSION.minor
    d = joinpath(b,"v$x.$y")
    if isdir(d) || !isdir(b) || !isdir(joinpath(b, "METADATA"))
        return d
    end
    return b
end
path(pkg::AbstractString...) = normpath(path(),pkg...)

function cd(f::Function, args...; kws...)
    dir = path()
    if !isdir(dir)
        !haskey(ENV,"JULIA_PKGDIR") ? init() :
            error("package directory $dir doesn't exist; run Pkg.init() to create it.")
    end
    Base.cd(()->f(args...; kws...), dir)
end

function init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH)
    if Git.version() < v"1.7.3"
        warn("Pkg only works with git versions greater than v1.7.3")
    end
    dir = path()
    info("Initializing package repository $dir")
    if isdir(joinpath(dir,"METADATA"))
        info("Package directory $dir is already initialized.")
        Git.set_remote_url(meta, dir=joinpath(dir,"METADATA"))
        return
    end
    try
        mkpath(dir)
        Base.cd(dir) do
            info("Cloning METADATA from $meta")
            run(`git clone -q -b $branch $meta METADATA`)
            Git.set_remote_url(meta, dir="METADATA")
            touch("REQUIRE")
            touch("META_BRANCH")
            open("META_BRANCH", "w") do io
                write(io, branch)
                close(io)
            end
        end
    catch e
        ispath(dir) && rm(dir, recursive=true)
        rethrow(e)
    end
end

function getmetabranch()
    try
        open(joinpath(path(),"META_BRANCH")) do io
          chomp(readuntil(io, "/n"))
        end
    catch
        META_BRANCH
    end
end

end # module
