module Dir

using Base.Git

const DEFAULT_META = "git://github.com/JuliaLang/METADATA.jl.git"

@unix_only const DIR_NAME = ".julia"
@windows_only const DIR_NAME = "packages"

function path()
    b = abspath(get(ENV,"JULIA_PKGDIR",joinpath(ENV["HOME"],DIR_NAME)))
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
            info("Auto-initializing default package repository $d.")
            init()
        end
    end
    Base.cd(f,d)
end

function init(meta::String=DEFAULT_META)
    d = path()
    isdir(joinpath(d,"METADATA")) && error("Package directory $d is already initialized.")
    try
        run(`mkdir -p $d`)
        cd() do
            # create & configure
            run(`git init`)
            run(`git commit --allow-empty -m "Initial empty commit"`)
            run(`git remote add origin .`)
            if success(`git config --global github.user`)
                base = basename(d)
                user = readchomp(`git config --global github.user`)
                run(`git config remote.origin.url git@github.com:$user/$base`)
            else
                run(`git config --unset remote.origin.url`)
            end
            run(`git config branch.master.remote origin`)
            run(`git config branch.master.merge refs/heads/master`)
            # initial content
            run(`touch REQUIRE`)
            run(`git add REQUIRE`)
            run(`git submodule add -b devel $meta METADATA`)
            run(`git commit -m "Empty package repo"`)
            cd(Git.autoconfig_pushurl,"METADATA")
            Metadata.gen_hashes()
        end
    catch e
        run(`rm -rf $d`)
        rethrow(e)
    end
end

end # module
