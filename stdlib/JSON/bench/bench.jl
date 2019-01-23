#!/usr/bin/julia --color=yes

using ArgParse
using JSON


function bench(f, simulate=false)
    fp = joinpath(JSON_DATA_DIR, string(f, ".json"))
    if !isfile(fp)
        println("Downloading benchmark file...")
        download(DATA_SOURCES[f], fp)
    end
    GC.gc()  # run gc so it doesn't affect benchmarks
    t = if args["parse"]["parse-file"]
        @elapsed JSON.parsefile(fp)
    else
        data = read(fp, String)
        @elapsed JSON.Parser.parse(data)
    end

    if !simulate
        printstyled(" [Bench$FLAGS] "; color=:yellow)
        println(f, " ", t, " seconds")
    end
    t
end


const JSON_DATA_DIR = joinpath(dirname(dirname(@__FILE__)), "data")
const s = ArgParseSettings(description="Benchmark JSON.jl")

const DATA_SOURCES = Dict(
    "canada" => "https://raw.githubusercontent.com/miloyip/nativejson-benchmark/v1.0.0/data/canada.json",
    "citm_catalog" => "https://raw.githubusercontent.com/miloyip/nativejson-benchmark/v1.0.0/data/citm_catalog.json",
    "citylots" => "https://raw.githubusercontent.com/zemirco/sf-city-lots-json/master/citylots.json",
    "twitter" => "https://raw.githubusercontent.com/miloyip/nativejson-benchmark/v1.0.0/data/twitter.json")

@add_arg_table s begin
    "parse"
        action = :command
        help = "Run a JSON parser benchmark"
    "list"
        action = :command
        help = "List available JSON files for use"
end

@add_arg_table s["parse"] begin
    "--include-compile", "-c"
        help = "If set, include the compile time in measurements"
        action = :store_true
    "--parse-file", "-f"
        help = "If set, measure JSON.parsefile, hence including IO time"
        action = :store_true
    "file"
        help = "The JSON file to benchmark (leave out to benchmark all)"
        required = false
end

const args = parse_args(ARGS, s)

if args["%COMMAND%"] == "parse"
    const FLAGS = string(
            args["parse"]["include-compile"] ? "C" : "",
            args["parse"]["parse-file"] ? "F" : "")

    if args["parse"]["file"] ≠ nothing
        const file = args["parse"]["file"]

        if !args["parse"]["include-compile"]
            bench(file, true)
        end
        bench(file)
    else
        times = 1.0
        if args["parse"]["include-compile"]
            error("Option --include-compile can only be used for single file.")
        end
        for k in sort(collect(keys(DATA_SOURCES)))
            bench(k, true)  # warm up compiler
        end
        for k in sort(collect(keys(DATA_SOURCES)))
            times *= bench(k)  # do benchmark
        end
        print_with_color(:yellow, " [Bench$FLAGS] ")
        println("Total (G.M.) ", times^(1/length(DATA_SOURCES)), " seconds")
    end
elseif args["%COMMAND%"] == "list"
    println("Available benchmarks are:")
    for k in sort(collect(keys(DATA_SOURCES)))
        println(" • $k")
    end
end
