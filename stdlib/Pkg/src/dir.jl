# This file is a part of Julia. License is MIT: https://julialang.org/license

module Dir

import Pkg
import ..DEFAULT_META, ..META_BRANCH, ..PkgError
import LibGit2, LibGit2.with
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
            throw(PkgError("Package metadata directory $metadata_dir doesn't exist; run Pkg.init() to initialize it."))
    end
    if haskey(ENV,"JULIA_PKGDIR")
        withenv("JULIA_PKGDIR" => abspath(ENV["JULIA_PKGDIR"])) do
            Base.cd(()->f(args...; kws...), dir)
        end
    else
        Base.cd(()->f(args...; kws...), dir)
    end
end

function init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH)
    dir = path()
    @info "Initializing package repository $dir"
    metadata_dir = joinpath(dir, "METADATA")
    if isdir(metadata_dir)
        @info "Package directory $dir is already initialized"
        LibGit2.set_remote_url(metadata_dir, "origin", meta)
        return
    end
    local temp_dir = ""
    try
        mkpath(dir)
        temp_dir = mktempdir(dir)
        Base.cd(temp_dir) do
            @info "Cloning METADATA from $meta"
            with(LibGit2.clone(meta, "METADATA", branch = branch)) do metadata_repo
                LibGit2.set_remote_url(metadata_repo, "origin", meta)
            end
            touch("REQUIRE")
            touch("META_BRANCH")
            write("META_BRANCH", branch)
        end
        #Move TEMP to METADATA
        Base.mv(joinpath(temp_dir,"METADATA"), metadata_dir)
        Base.mv(joinpath(temp_dir,"REQUIRE"), joinpath(dir,"REQUIRE"))
        Base.mv(joinpath(temp_dir,"META_BRANCH"), joinpath(dir,"META_BRANCH"))
        rm(temp_dir, recursive=true)
    catch err
        ispath(metadata_dir) && rm(metadata_dir, recursive=true)
        ispath(temp_dir) && rm(temp_dir, recursive=true)
        rethrow(err)
    end
end

function getmetabranch()
    try
        readline(joinpath(path(),"META_BRANCH"))
    catch err
        META_BRANCH
    end
end

end # module
