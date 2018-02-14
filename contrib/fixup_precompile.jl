const HEADER = """
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Steps to regenerate this file:
# 1. Remove all `precompile` calls
# 2. Rebuild system image
# 3. Enable TRACE_COMPILE in options.h and rebuild
# 4. Run `./julia 2> precompiles.txt` and do various things.
# 5. Run `./julia contrib/fixup_precompile.jl precompiles.txt
"""

function fixup_precompile(precompile_file)
    precompile_file = joinpath(pwd(), ARGS[1])
    precompile_statements = Set{String}()
    for line in eachline(precompile_file)
        # filter out closures, which might have different generated names in different environments)
        # contains(line, r"#[0-9]") && continue
        ismatch(r"#[0-9]", line) && continue
        # Other stuff than precompile statements might have been written to STDERR
        startswith(line, "precompile(Tuple{") || continue
        # Ok, add the line
        push!(precompile_statements, line)
    end

    precompilefile_path = joinpath(Sys.BINDIR, "..", "..", "base", "precompile.jl")
    open(precompilefile_path, "w") do f
        println(f, HEADER)
        println(f, "module __precompile_area__")
        println(f, """
            if !(pkgid.name in ("Main", "Core", "Base"))
                @eval $(Symbol(mod)) = $mod
            end""")
        for statement in precompile_statements
            println(f, statement)
        end
        println(f, "end # module")
    end
    println("Wrote a new precompile file to $(abspath(precompilefile_path))")
end

@assert length(ARGS) == 1
fixup_precompile(ARGS[1])