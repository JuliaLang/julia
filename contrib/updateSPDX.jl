# This file is a part of Julia. License is MIT: https://julialang.org/license

# SPDX-License-Identifier: MIT
# Run this script with each new Julia release to update "../julia.spdx.json"

using UUIDs
using Dates
using JSON
using TimeZones
using DataStructures

spdxDocument= "../julia.spdx.json"
spdxData= JSON.parsefile(spdxDocument; dicttype=OrderedDict{String, Any})

# At the moment we can only update a few items automatically with each release.
# These are the crucial elements to make a new version of the SPDX file.
# Any other changes (ex. Adding or removing of external dependencies, updating copyright text, etc.) must be performed manually
spdxData["documentNamespace"]= "https://julialang.org/spdxdocs/julia-spdx-" * string(uuid4())
spdxData["creationInfo"]["created"]=  Dates.format(now(tz"UTC"), "yyyy-mm-ddTHH:MM:SS") * "Z"

for pkg in spdxData["packages"]
    if pkg["SPDXID"] == "SPDXRef-JuliaMain"
        pkg["versionInfo"]= readline("../VERSION")
        pkg["downloadLocation"]= "git+https://github.com/JuliaLang/julia.git@v" * pkg["versionInfo"]
        break
    end
end

open(spdxDocument, "w") do f
    JSON.print(f, spdxData, 4)
end
