# This file is a part of Julia. License is MIT: https://julialang.org/license

# SPDX-License-Identifier: MIT
# Run this script with each new Julia release to update "../julia.spdx.json"

using UUIDs
using Dates
using JSON
using TimeZones
using DataStructures
using TOML

rootdir = joinpath(@__DIR__, "..")
spdxDocument= joinpath(rootdir, "julia.spdx.json")
spdxData= JSON.parsefile(spdxDocument; dicttype=OrderedDict{String, Any})

stdlibs = readdir(joinpath(rootdir, "stdlib"), join=true)
filter!(isdir, stdlibs)

# At the moment we can only update a few items automatically with each release.
# These are the crucial elements to make a new version of the SPDX file.
# Any other changes (ex. Adding or removing of external dependencies, updating copyright text, etc.) must be performed manually
spdxData["documentNamespace"]= "https://julialang.org/spdxdocs/julia-spdx-" * string(uuid4())
spdxData["creationInfo"]["created"]=  Dates.format(now(tz"UTC"), "yyyy-mm-ddTHH:MM:SS") * "Z"

function find_stdlib(name)
    for stdlib in stdlibs
        if occursin(name, basename(stdlib))
            return stdlib
        end
    end
end

function parse_stdlib_version(name)
    stdlib = find_stdlib(name)
    isnothing(stdlib) && return
    filepath = joinpath(stdlib, "Project.toml")
    isfile(filepath) || return
    toml = TOML.parsefile(filepath)
    return get(toml, "version", nothing)
end

function parse_dep_version(name)
    filepath = joinpath(rootdir, "deps", "$(lowercase(name)).version")
    isfile(filepath) || return
    for line in readlines(filepath)
        if startswith(line, "$(uppercase(name))_VER")
            return strip(last(split(line, '=')))
        end
    end
end

for pkg in spdxData["packages"]
    if pkg["SPDXID"] == "SPDXRef-JuliaMain"
        pkg["versionInfo"]= readline(joinpath(rootdir, "VERSION"))
        pkg["downloadLocation"]= "git+https://github.com/JuliaLang/julia.git@v" * pkg["versionInfo"]
        continue
    else
        name = chopprefix(pkg["SPDXID"], "SPDXRef-")
        if startswith(name, "Julia")
            name = chopprefix(name, "Julia")
            version = parse_stdlib_version(name)
            isnothing(version) && continue
        else
            version = parse_dep_version(name)
            isnothing(version) && continue
        end
        pkg["versionInfo"] = version
    end
end

open(spdxDocument, "w") do f
    JSON.print(f, spdxData, 4)
end
