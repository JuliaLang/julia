function needs_USE_GPL_LIBS(s::String)
    occursin("CHOLMOD", s) && return true
    return false
end

const HEADER = """
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Steps to regenerate this file:
# 1. Remove all `precompile` calls
# 2. Rebuild system image
# 3. Enable TRACE_COMPILE in options.h and rebuild
# 4. Run `./julia 2> precompiles.txt` and do various things.
# 5. Run `./julia contrib/fixup_precompile.jl precompiles.txt to overwrite `precompile.jl`
#    or ./julia contrib/fixup_precompile.jl --merge precompiles.txt to merge into existing
#    `precompile.jl`
"""

function fixup_precompile(new_precompile_file; merge=false)
    old_precompile_file = joinpath(Sys.BINDIR, "..", "..", "base", "precompile.jl")
    precompile_statements = Set{String}()

    for file in [new_precompile_file; merge ? old_precompile_file : []]
        for line in eachline(file)
            line = strip(line)
            # filter out closures, which might have different generated names in different environments
            occursin(r"#[0-9]", line) && continue
            # Other stuff than precompile statements might have been written to STDERR
            startswith(line, "precompile(Tuple{") || continue
            # Ok, add the line
            push!(precompile_statements, line)
        end
    end

    open(old_precompile_file, "w") do f
        println(f, HEADER)
        println(f, """
        let
        PrecompileStagingArea = Module()
        for (_pkgid, _mod) in Base.loaded_modules
            if !(_pkgid.name in ("Main", "Core", "Base"))
                @eval PrecompileStagingArea \$(Symbol(_mod)) = \$_mod
            end
        end
        @eval PrecompileStagingArea begin""")
        for statement in sort(collect(precompile_statements))
            isgpl = needs_USE_GPL_LIBS(statement)
            isgpl && print(f, "if Base.USE_GPL_LIBS\n    ")
            println(f, statement)
            isgpl && println(f, "end")
        end
        println(f, "end\nend")
    end
    if merge
        "Merged $new_precompile_file into $old_precompile_file"
    else
        "Overwrite $old_precompile_file with $new_precompile_file"
    end
end

if length(ARGS) == 1
    fixup_precompile(joinpath(pwd(), ARGS[1]))
elseif length(ARGS) == 2
    @assert ARGS[1] == "--merge"
    fixup_precompile(joinpath(pwd(), ARGS[2]); merge = true)
else
    error("invalid arguments")
end
