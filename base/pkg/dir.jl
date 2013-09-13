module Dir

using Base.Git

const DEFAULT_META = "git://github.com/JuliaLang/METADATA.jl"

@unix_only const DIR_NAME = ".julia"
@windows_only const DIR_NAME = "packages"

function path()
    b = abspath(get(ENV,"JULIA_PKGDIR",
        joinpath((@windows?abspath(ENV["AppData"],"julia"):ENV["HOME"]),DIR_NAME)))
    x, y = VERSION.major, VERSION.minor
    d = joinpath(b,"v$x.$y")
    isdir(d) && return d
    d = joinpath(b,"v$x")
    isdir(d) && return d
    return b
end
path(pkg::String...) = joinpath(path(),pkg...)

function cd(f::Function, d::String=path())
    if !isdir(d)
        if haskey(ENV,"JULIA_PKGDIR")
            error("Package directory $d doesn't exist; run Pkg.init() to create it.")
        else
            info("Initializing package repository $d.")
            init()
        end
    end
    Base.cd(f,d)
end

function init(meta::String=DEFAULT_META)
    d = path()
    if isdir(joinpath(d,"METADATA"))
        info("Package directory $d is already initialized.")
        Git.set_remote_url(meta, dir=joinpath(d,"METADATA"))
        return
    end
    try
        run(`mkdir -p $d`)
        cd(d) do
            info("Cloning METADATA from $meta")
            run(`git clone -q -b devel $meta METADATA`)
            Git.set_remote_url(meta, dir="METADATA")
            run(`touch REQUIRE`)
        end
    catch e
        run(`rm -rf $d`)
        rethrow(e)
    end
end

end # module
