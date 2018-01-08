#!/usr/bin/env julia

function write_toml(f::Function, names::String...)
    path = joinpath(names...) * ".toml"
    mkpath(dirname(path))
    open(path, "w") do io
        f(io)
    end
end

toml_key(str::String) = contains(str, r"[^\w-]") ? repr(str) : str
toml_key(strs::String...) = join(map(toml_key, [strs...]), '.')

prefix = joinpath(homedir(), ".julia", "registries", "Uncurated")

write_toml(prefix, "registry") do io
    repo = "https://github.com/JuliaRegistries/Uncurated.git"
    uuid = string(uuid5(uuid_registry, repo))
    println(io, "name = ", repr("Uncurated"))
    println(io, "uuid = ", repr(uuid))
    println(io, "repo = ", repr(repo))
    println(io, "\ndescription = \"\"\"")
    print(io, """
        Official uncurated Julia package registry where people can
        register any package they want without too much debate about
        naming and without enforced standards on documentation or
        testing. We nevertheless encourage documentation, testing and
        some amount of consideration when choosing package names.
        """)
    println(io, "\"\"\"")
    println(io, "\n[packages]")
    for (pkg, p) in sort!(collect(pkgs), by=(p->p.uuid.value)∘last)
        bucket = string(uppercase(first(pkg)))
        path = joinpath(bucket, pkg)
        println(io, p.uuid, " = { name = ", repr(pkg), ", path = ", repr(path), " }")
    end
end

buckets = Dict()
for (pkg, p) in pkgs
    bucket = string(uppercase(first(pkg)))
    push!(get!(buckets, bucket, []), (pkg, p))
end

include("utils.jl")
include("sha1map.jl")
const trees = sha1map(pkgs)

for (bucket, b_pkgs) in buckets, (pkg, p) in b_pkgs
    url = p.url
    uuid = string(p.uuid)
    startswith(url, "git://github.com") && (url = "https"*url[4:end])

    # package.toml
    write_toml(prefix, bucket, pkg, "package") do io
        println(io, "name = ", repr(pkg))
        println(io, "uuid = ", repr(uuid))
        println(io, "repo = ", repr(url))
    end

    # versions.toml
    write_toml(prefix, bucket, pkg, "versions") do io
        for (i, (ver, v)) in enumerate(sort!(collect(p.versions), by=first))
            i > 1 && println(io)
            println(io, "[", toml_key(string(ver)), "]")
            println(io, "git-tree-sha1 = ", repr(trees[uuid][v.sha1]))
        end
    end
    versions = sort!(collect(keys(p.versions)))

    function write_versions_data(f::Function, name::String; lt::Function=isless)
        data = Dict{VersionNumber,Dict{String,String}}()
        for (ver, v) in p.versions, (dep, d) in v.requires
            val = f(dep, d)
            val == nothing && continue
            haskey(data, ver) || (data[ver] = Dict{String,String}())
            data[ver][dep] = val
        end
        compressed = compress_versions_data(data, versions)
        !isempty(compressed) && write_toml(prefix, bucket, pkg, name) do io
            vers = unique(getindex.(compressed, 1))
            keys = sort!(unique(getindex.(compressed, 2)), lt=lt)
            what = (vers, keys)
            ord = (1, 2)
            for (i, x) in enumerate(what[ord[1]])
                i > 1 && println(io)
                println(io, "[", toml_key(x), "]")
                for y in what[ord[2]]
                    for t in compressed
                        t[ord[1]] == x && t[ord[2]] == y || continue
                        println(io, toml_key(y), " = ", t[3])
                    end
                end
            end
        end
    end

    # dependencies.toml
    write_versions_data("dependencies") do dep, d
        dep != "julia" ? repr(string(pkgs[dep].uuid)) : nothing
    end

    # compatibility.toml
    write_versions_data("compatibility", lt=packagelt) do dep, d
        versions_repr(compress_versions(
            d.versions, collect(keys(pkgs[dep].versions))
        ))
    end
end
