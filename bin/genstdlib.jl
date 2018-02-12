#!/usr/bin/env julia

import Pkg3: TOML

prefix = joinpath(homedir(), ".julia", "registries", "Stdlib")

# TODO: use Sys.STDLIBDIR instead once implemented
let vers = "v$(VERSION.major).$(VERSION.minor)"
global stdlibdir = realpath(abspath(Sys.BINDIR, "..", "share", "julia", "site", vers))
isdir(stdlibdir) || error("stdlib directory does not exist: $stdlibdir")
end
juliadir = dirname(stdlibdir)

stdlib_uuids = Dict{String,String}()
stdlib_trees = Dict{String,String}()
stdlib_deps = Dict{String,Vector{String}}()

for pkg in readdir(stdlibdir)
    project_file = joinpath(stdlibdir, pkg, "Project.toml")
    isfile(project_file) || continue
    project = TOML.parsefile(project_file)
    stdlib_uuids[pkg] = project["uuid"]
    stdlib_trees[pkg] = split(readchomp(`git -C $juliadir ls-tree HEAD -- stdlib/$pkg`))[3]
    stdlib_deps[pkg] = String[]
    haskey(project, "deps") || continue
    append!(stdlib_deps[pkg], sort!(collect(keys(project["deps"]))))
end

#=
write_toml(prefix, "registry") do io
    repo = "https://github.com/JuliaRegistries/Stdlib.git"
    uuid = string(uuid5(uuid_registry, repo))
    println(io, "name = ", repr("Stdlib"))
    println(io, "uuid = ", repr(uuid))
    println(io, "repo = ", repr(repo))
    println(io, "\ndescription = \"\"\"")
    print(io, """
        Official Julia Standard Library registry where versions
        of packages that ship with Julia itself are registered.
        """)
    println(io, "\"\"\"")
    println(io, "\n[packages]")
    for pkg in sort!(collect(keys(stdlib_deps)), by=pkg->stdlib_uuids[pkg])
        println(io, stdlib_uuids[pkg], " = { name = ", repr(pkg), ", path = ", repr(pkg), " }")
    end
end

for (pkg, uuid) in stdlib_uuids
    url = "https://github.com/JuliaLang/julia.git"
    tree = stdlib_trees[pkg]
    deps = stdlib_deps[pkg]

    # package.toml
    write_toml(prefix, pkg, "package") do io
        println(io, "name = ", repr(pkg))
        println(io, "uuid = ", repr(uuid))
        println(io, "repo = ", repr(url))
    end

    # versions.toml
    write_toml(prefix, pkg, "versions") do io
        println(io, "[", toml_key("0.7.0-DEV+r$(tree[1:8])"), "]")
        println(io, "git-tree-sha1 = ", repr(tree))
    end

    # dependencies.toml
    if isempty(deps)
        rm(joinpath(prefix, pkg, "dependencies.toml"), force=true)
    else
        write_toml(prefix, pkg, "dependencies") do io
            println(io, "[", toml_key("0.7"), "]")
            for dep in sort!(deps, by=lowercase)
                println(io, "$dep = ", repr(stdlib_uuids[dep]))
            end
        end
    end

    # compatibility.toml
    write_toml(prefix, pkg, "compatibility") do io
        println(io, "[", toml_key("0.7"), "]")
        println(io, "julia = \"0.7\"")
    end
end
=#
