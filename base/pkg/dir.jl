module Dir

import ..Pkg: DEFAULT_META, META_BRANCH
import ..Git

const DIR_NAME = ".julia"
const LOCK_NAME = "pkg.lck"

pkgroot() = abspath(get(ENV,"JULIA_PKGDIR",joinpath(homedir(),DIR_NAME)))

function transact(f::Function)
    lockpath = joinpath(pkgroot(), LOCK_NAME)
    if isfile(lockpath)
        error("Could not lock package directory;
        If you are sure another instance of Pkg is not
        being used, run Pkg.rmlock()")
    end
    try
        open(lockpath, "w") do fh
            write(fh, "")
        end
        return f()
    finally
        if isfile(lockpath)
            rm(lockpath)
        end
    end
end

function rmlock()
    lockpath = joinpath(pkgroot(), LOCK_NAME)
    if isfile(lockpath)
        rm(lockpath)
    end
end

function path()
    b = pkgroot()
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
    transact() do
        Base.cd(()->f(args...; kws...), dir)
    end
end

function init(meta::String=DEFAULT_META, branch::String=META_BRANCH)
    dir = path()
    info("Initializing package repository $dir")
    if isdir(joinpath(dir,"METADATA"))
        info("Package directory $dir is already initialized.")
        transact() do 
            Git.set_remote_url(meta, dir=joinpath(dir,"METADATA"))
        end
        return
    end
    try
        mkpath(dir)
        transact() do
            Base.cd(dir) do
                info("Cloning METADATA from $meta")
                run(`git clone -q -b $branch $meta METADATA`)
                Git.set_remote_url(meta, dir="METADATA")
                run(`touch REQUIRE`)
            end
        end
    catch e
        run(`rm -rf $dir`)
        rethrow(e)
    end
end

end # module
