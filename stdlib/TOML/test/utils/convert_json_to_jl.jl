# This converts the ground-truth JSON files to the Julia repr format so
# we can use that without requiring a JSON parser during testing.

using JSON

const testfiles =  joinpath(@__DIR__, "..", "testfiles")

function convert_json_files()
    for (root, dirs, files) in walkdir(testfiles)
        for f in files
            file = joinpath(root, f)
            endswith(file, ".json") || continue
            d_json = open(JSON.parse, file)
            d_jl = repr(d_json)
            write(splitext(file)[1] * ".jl", d_jl)
            rm(file)
        end
    end
end

convert_json_files()
