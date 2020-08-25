# This converts the ground-truth JSON files to the Julia repr format so
# we can use that without requiring a JSON parser during testing.

using JSON

const testfiles =  joinpath(@__DIR__, "..", "testfiles")

function convert_json_files()
    for folder in ("invalid", "valid")
        for file in readdir(joinpath(testfiles, folder); join=true)
            endswith(file, ".json") || continue
            d_json = open(JSON.parse, file)
            d_jl = repr(d_json)
            write(splitext(file)[1] * ".jl", d_jl)
        end
    end
end