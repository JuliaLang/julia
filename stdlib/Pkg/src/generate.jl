# This file is a part of Julia. License is MIT: https://julialang.org/license

generate(path::String; kwargs...) = generate(Context(), path; kwargs...)
function generate(ctx::Context, path::String; kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    dir, pkg = dirname(path), basename(path)
    isdir(path) && pkgerror("$(abspath(path)) already exists")
    printstyled("Generating"; color=:green, bold=true)
    print(" project $pkg:\n")
    project(pkg, dir; preview=ctx.preview)
    entrypoint(pkg, dir; preview=ctx.preview)
    gitignore(pkg, dir; preview=ctx.preview)
    ctx.preview && preview_info()
    return
end

function genfile(f::Function, pkg::String, dir::String, file::String; preview::Bool)
    path = joinpath(dir, pkg, file)
    println(stdout, "    $path")
    preview && return
    mkpath(dirname(path))
    open(f, path, "w")
    return
end

function project(pkg::String, dir::String; preview::Bool)
    name = email = nothing
    gitname = LibGit2.getconfig("user.name", "")
    isempty(gitname) || (name = gitname)
    gitmail = LibGit2.getconfig("user.email", "")
    isempty(gitmail) || (email = gitmail)

    if name == nothing
        for env in ["GIT_AUTHOR_NAME", "GIT_COMMITTER_NAME", "USER", "USERNAME", "NAME"]
            name = get(ENV, env, nothing)
            name != nothing && break
        end
    end

    name == nothing && (name = "Unknown")

    if email == nothing
        for env in ["GIT_AUTHOR_EMAIL", "GIT_COMMITTER_EMAIL", "EMAIL"];
            email = get(ENV, env, nothing)
            email != nothing && break
        end
    end

    authorstr = "[\"$name " * (email == nothing ? "" : "<$email>") * "\"]"

    genfile(pkg, dir, "Project.toml"; preview=preview) do io
        print(io,
            """
            authors = $authorstr
            name = "$pkg"
            uuid = "$(UUIDs.uuid1())"
            version = "0.1.0"

            [deps]
            """
        )
    end
end

function entrypoint(pkg::String, dir::String; preview::Bool)
    genfile(pkg, dir, "src/$pkg.jl"; preview=preview) do io
        print(io,
           """
            module $pkg

            greet() = print("Hello World!")

            end # module
            """
        )
    end
end

function gitignore(pkg::String, dir::String; preview::Bool)
    genfile(pkg, dir, ".gitignore", preview) do io
        print(io, """
        *.jl.cov
        *.jl.*.cov
        *.jl.mem
        """)
    end
end

function tests(pkg::String, dir::Stirng; preview::Bool)
    genfile(pkg, dir, "test/runtests.jl", preview) do io
        print(io, """
        using $pkg
        @static if VERSION < v"0.7.0-DEV.2005"
            using Base.Test
        else
            using Test
        end
        # write your own tests here
        @test 1 == 2
        """)
    end
end
