empty!(Base.DEPOT_PATH)
push!(Base.DEPOT_PATH, mktempdir(; cleanup = true))

import Pkg
import Logging
import TOML

Pkg.add(; name = "Coverage", uuid = "a2441757-f6aa-5fb2-8edb-039e3f45d037", version = "1")
Pkg.precompile()

import Coverage

function process_folders()
    # `Coverage.process_folder` will have a LOT of `@info` statements that will make the log
    # way too long. So before we run `Coverage.process_folder`, we disable logging for `@info`
    # statements. After we run `Coverage.process_folder`, we re-enable logging for `@info`
    # statements.
    Logging.disable_logging(Logging.Info)
    fcs_base   = Coverage.process_folder("base");
    fcs_stdlib = Coverage.process_folder("stdlib");
    Logging.disable_logging(Logging.Debug)

    fcs = Coverage.merge_coverage_counts(
        fcs_base,
        fcs_stdlib,
    );

    return fcs
end

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

function buildkite_env(name::String)
    value = String(strip(ENV[name]))
    if isempty(value)
        throw(ErrorException("environment variable $(name) is empty"))
    end
    return value
end

function buildkite_env(name_1::String, name_2::String, default::String)
    value_1 = String(strip(ENV[name_1]))
    value_2 = String(strip(ENV[name_2]))
    !isempty(value_1) && return value_1
    !isempty(value_2) && return value_2
    return default
end

function buildkite_branch_and_commit()
    branch = buildkite_env("BUILDKITE_BRANCH")
    commit = buildkite_env("BUILDKITE_COMMIT")
    head_rev_parse = String(strip(read(`git rev-parse HEAD`, String)))
    if strip(commit) == "HEAD"
        commit = head_rev_parse
    end
    if commit !== head_rev_parse
        msg = "mismatch"
        @error msg commit head_rev_parse
        throw(ErrorException(msg))
    end
    if !occursin(r"^[a-f0-9]{40}$", commit)
        msg = "BUILDKITE_COMMIT does not look like a long commit SHA"
        @error msg commit
        throw(ErrorException(msg))
    end
    return (; branch, commit)
end

function codecov_buildkite_add_local_to_kwargs()
    branch, commit = buildkite_branch_and_commit()
    kwargs = Coverage.Codecov.set_defaults(
        Dict();
        branch,
        commit,
    )
    return kwargs
end

function coveralls_buildkite_query_git_info()
    branch, commit = buildkite_branch_and_commit()
    remote_name  = "origin"
    remote       = buildkite_env("BUILDKITE_REPO")
    message      = buildkite_env("BUILDKITE_MESSAGE")
    author_name  = buildkite_env(
        "BUILDKITE_BUILD_AUTHOR",
        "BUILDKITE_BUILD_CREATOR",
        "",
    )
    author_email = buildkite_env(
        "BUILDKITE_BUILD_AUTHOR_EMAIL",
        "BUILDKITE_BUILD_CREATOR_EMAIL",
        "",
    )
    remotes = [
        Dict(
            "name"  => remote_name,
            "url"   => remote,
        )
    ]
    head = Dict(
        "id"                => commit,
        "author_name"       => author_name,
        "author_email"      => author_email,
        "committer_name"    => author_name,
        "committer_email"   => author_email,
        "message"           => message,
    )
    git_info = Dict(
        "branch"  => branch,
        "remotes" => remotes,
        "head"    => head,
    )
    return git_info
end

const fcs = process_folders()

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
end;

sort!(fcs; by = fc -> fc.filename);

print_coverage_summary.(fcs);
print_coverage_summary(fcs, "Total")

let
    git_info = coveralls_buildkite_query_git_info()
    @info "" git_info
    @info "" git_info["branch"]
    @info "" git_info["head"]

    # In order to upload to Coveralls, you need to have the `COVERALLS_TOKEN` environment variable defined.
    Coverage.Coveralls.submit_local(fcs, git_info)
end

let
    kwargs = codecov_buildkite_add_local_to_kwargs()
    @info "" kwargs

    # In order to upload to Codecov, you need to have the `CODECOV_TOKEN` environment variable defined.
    Coverage.Codecov.submit_generic(fcs, kwargs)
end
