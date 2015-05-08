module Dir

import ..Pkg: DEFAULT_META, META_BRANCH
import ..LibGit2

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
    metadata_dir = joinpath(dir, "METADATA")
    if !isdir(metadata_dir)
        !haskey(ENV,"JULIA_PKGDIR") ? init() :
            error("Package metadata directory $metadata_dir doesn't exist; run Pkg.init() to initialize it.")
    end
    Base.cd(()->f(args...; kws...), dir)
end

function init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH)
    dir = path()
    info("Initializing package repository $dir")
    metadata_dir = joinpath(dir, "METADATA")
    if isdir(metadata_dir)
        info("Package directory $dir is already initialized.")
        LibGit2.set_remote_url(metadata_dir, meta)
        return
    end
    try
        mkpath(dir)
        Base.cd(dir) do
            info("Cloning METADATA from $meta")
            metadata_repo = LibGit2.clone(meta, "METADATA", branch = branch)
            LibGit2.set_remote_url(metadata_repo, meta)
            LibGit2.free!(metadata_repo)
            touch("REQUIRE")
            touch("META_BRANCH")
            open("META_BRANCH", "w") do io
                write(io, branch)
                close(io)
            end
        end
    catch e
        ispath(metadata_dir) && rm(metadata_dir, recursive=true)
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
