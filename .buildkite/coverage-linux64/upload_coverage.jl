empty!(Base.DEPOT_PATH)
push!(Base.DEPOT_PATH, mktempdir(; cleanup = true))

import Pkg
import Logging
import TOML

Pkg.add(; name = "Coverage", uuid = "a2441757-f6aa-5fb2-8edb-039e3f45d037", version = "1")
Pkg.precompile()

import Coverage

function get_external_stdlib_names(stdlib_dir::AbstractString)
    filename_list = filter(x -> isfile(joinpath(stdlib_dir, x)), readdir(stdlib_dir))
    # find all of the files like `Pkg.version`, `Statistics.version`, etc.
    regex_matches_or_nothing = match.(Ref(r"^([\w].*?)\.version$"), filename_list)
    regex_matches = filter(x -> x !== nothing, regex_matches_or_nothing)
    # get the names of the external stdlibs, like `Pkg`, `Statistics`, etc.
    external_stdlib_names = only.(regex_matches)
    unique!(external_stdlib_names)
    sort!(external_stdlib_names)
    @info "# Begin list of external stdlibs"
    for (i, x) in enumerate(external_stdlib_names)
        @info "$(i). $(x)"
    end
    @info "# End list of external stdlibs"
    return external_stdlib_names
end

function get_external_stdlib_prefixes(stdlib_dir::AbstractString)
    external_stdlib_names = get_external_stdlib_names(stdlib_dir)
    prefixes_1 = joinpath.(Ref(stdlib_dir), external_stdlib_names, Ref(""))
    prefixes_2 = joinpath.(Ref(stdlib_dir), string.(external_stdlib_names, Ref("-")))
    prefixes = vcat(prefixes_1, prefixes_2)
    unique!(prefixes)
    sort!(prefixes)
    # example of what `prefixes` might look like:
    # 4-element Vector{String}:
    # "stdlib/Pkg-"
    # "stdlib/Pkg/"
    # "stdlib/Statistics-"
    # "stdlib/Statistics/"
    return prefixes
end

function print_coverage_summary(fc::Coverage.FileCoverage)
    cov_lines, tot_lines = Coverage.get_summary(fc)
    if cov_lines == tot_lines == 0
        cov_pct = 0
    else
        cov_pct = floor(Int, cov_lines/tot_lines * 100)
    end
    pad_1 = 71
    pad_2 = 15
    pad_3 = 15
    col_1 = rpad(fc.filename, pad_1)
    col_2 = rpad(string(cov_pct, " %"), pad_2)
    col_3 = string(
        rpad(string(cov_lines), pad_3),
        string(tot_lines),
    )
    @info "$(col_1) $(col_2) $(col_3)"
    return nothing
end

function print_coverage_summary(
        fcs::Vector{Coverage.FileCoverage}, description::AbstractString,
    )
    cov_lines, tot_lines = Coverage.get_summary(fcs)
    if cov_lines == tot_lines == 0
        cov_pct = 0
    else
        cov_pct = floor(Int, cov_lines/tot_lines * 100)
    end
    @info "$(description): $(cov_pct)% ($(cov_lines)/$(tot_lines))"
    return nothing
end

# `Coverage.process_folder` will have a LOT of `@info` statements that will make the log
# way too long. So before we run `Coverage.process_folder`, we disable logging for `@info`
# statements. After we run `Coverage.process_folder`, we re-enable logging for `@info`
# statements.
Logging.disable_logging(Logging.Info)
const fcs = Coverage.merge_coverage_counts(
    Coverage.process_folder("base"),
    Coverage.process_folder("stdlib"),
);
Logging.disable_logging(Logging.Debug)

# Only include source code files. Exclude test files, benchmarking files, etc.
filter!(fcs) do fc
    occursin(r"^base\/", fc.filename) || occursin("/src/", fc.filename)
end;

# Exclude all external stdlibs (stdlibs that live in external repos).
const external_stdlib_prefixes = get_external_stdlib_prefixes("stdlib")
filter!(fcs) do fc
    all(x -> !startswith(fc.filename, x), external_stdlib_prefixes)
end;

# Exclude all stdlib JLLs (stdlibs of the form `stdlib/*_jll/`).
filter!(fcs) do fc
    !occursin(r"^stdlib\/[A-Za-z0-9]*?_jll\/", fc.filename)
end

sort!(fcs; by = fc -> fc.filename)

print_coverage_summary.(fcs);
print_coverage_summary(fcs, "Total")

# In order to upload to Codecov, you need to have the `CODECOV_TOKEN` environment variable defined.
Coverage.Codecov.submit_local(fcs)

# In order to upload to Coveralls, you need to have the `COVERALLS_TOKEN` environment variable defined.
Coverage.Coveralls.submit_local(fcs)
