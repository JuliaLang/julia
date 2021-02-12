#!/usr/bin/env julia

function get_makefile_variables(path)
    contents = read(open(path), String)
    lines = split(contents, "\n")
    lines = filter(line -> length(line) > 0, lines)
    parsed = map(line -> map(strip, split(line, "=")), lines)
    vars = first.(parsed)
    vals = last.(parsed)
    (;vars, vals)
end

is_versioned(path) = success(`git ls-files --error-unmatch $(path)`)
clean_stage() = success(`git diff --cached --exit-code`)

function write_makefile_variables(env, path)
    is_versioned(path) || error("File $(path) is not versioned.")
    open(path, "w") do f
        for (var, val) in  zip(env.vars, env.vals)
            println(f, "$(var) = $(val)")
        end
    end
end

function _get_hash_env(env)
    i = findlast(==("$(uppercase(stdlib_name))_SHA1"), env.vars)
    isnothing(i) || return i
    error("hash variable not found among $(env.vars)")
end

function get_hash_val(env)
    env.vals[_get_hash_env(env)]
end

function replace_hash(env, path, new_hash)
    env.vals[_get_hash_env(env)] = new_hash
    write_makefile_variables(env, path)
end

hash_dir(stdlib_name, stdlib_commit) = "deps/checksums/$(stdlib_name)-$(stdlib_commit).tar.gz"


clean_stage() || error("run this script with no changes to be committed")
stdlib_name = ARGS[1] # e.g. "Pkg"
stdlib_new_commit = ARGS[2] # e.g. "47105aaefb7eb8bda5c002315ccf1a28be8fdabb"

@show stdlib_name
@show stdlib_new_commit

version_file_path = "stdlib/$(stdlib_name).version"

hash_files = ["md5", "sha512"] # just to avoid a dependency on `Glob.jl`

length(stdlib_new_commit) == 40 || error("Commit hash expected to be 40 hex digits")

env = get_makefile_variables(version_file_path)
stdlib_old_commit = get_hash_val(env)
replace_hash(env, version_file_path, stdlib_new_commit)
run(`make -C stdlib`)

for f in hash_files
    run(`git rm $(joinpath(hash_dir(stdlib_name, stdlib_old_commit), f))`)
    run(`git add $(joinpath(hash_dir(stdlib_name, stdlib_new_commit), f))`)
end
