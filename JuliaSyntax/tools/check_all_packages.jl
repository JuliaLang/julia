# hacky script to parse all Julia files in all packages in General
# to Exprs and report errors
#
# Run this after registry_download.jl (so the pkgs directory is populated).

using JuliaSyntax, Logging, TerminalLoggers, ProgressLogging, Serialization

include("../test/test_utils.jl")
include("../test/fuzz_test.jl")

pkgspath = joinpath(@__DIR__, "pkgs")
source_paths = find_source_in_path(pkgspath)
file_count = length(source_paths)

exception_count = 0
mismatch_count = 0
t0 = time()
exceptions = []

function parsers_disagree(text::AbstractString)
    fl_ex = fl_parseall(text, filename="none")
    if Meta.isexpr(fl_ex, (:error,:incomplete)) ||
            (Meta.isexpr(fl_ex, :toplevel) && length(fl_ex.args) >= 1 &&
             Meta.isexpr(fl_ex.args[end], (:error,:incomplete)))
        return false
    end
    try
        ex = parseall(Expr, text, filename="none", ignore_errors=true)
        return !exprs_roughly_equal(fl_ex, ex)
    catch
        @error "Reduction failed" text
        return false
    end
end

all_reduced_failures = String[]

Logging.with_logger(TerminalLogger()) do
    global exception_count, mismatch_count, t0
    @withprogress for (ifile, fpath) in enumerate(source_paths)
        @logprogress ifile/file_count time_ms=round((time() - t0)/ifile*1000, digits = 2)
        text = read(fpath, String)
        expr_cache = fpath*".Expr"
        #e2 = JuliaSyntax.fl_parseall(text)
        e2 = open(deserialize, fpath*".Expr")
        @assert Meta.isexpr(e2, :toplevel)
        try
            e1 = JuliaSyntax.parseall(Expr, text, filename=fpath, ignore_warnings=true)
            if !exprs_roughly_equal(e2, e1)
                mismatch_count += 1
                failing_source = sprint(context=:color=>true) do io
                    for c in reduce_test(text)
                        JuliaSyntax.highlight(io, c.source, range(c), context_lines_inner=5)
                        println(io, "\n")
                    end
                end
                reduced_failures = rand_reduce.(sourcetext.(reduce_test(text)),
                                                   parsers_disagree)
                append!(all_reduced_failures, reduced_failures)
                @error("Parsers succeed but disagree",
                       fpath,
                       failing_source=Text(failing_source),
                       reduced_failures,
                       )
            end
        catch err
            err isa InterruptException && rethrow()
            ex = (err, catch_backtrace())
            push!(exceptions, ex)
            ref_parse = "success"
            if length(e2.args) >= 1 && Meta.isexpr(last(e2.args), (:error, :incomplete))
                ref_parse = "fail"
                if err isa JuliaSyntax.ParseError
                    # Both parsers agree that there's an error, and
                    # JuliaSyntax didn't have an internal error.
                    continue
                end
            end

            exception_count += 1
            parse_to_syntax = "success"
            try
                JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, code)
            catch err2
                parse_to_syntax = "fail"
            end
            @error "Parse failed" fpath exception=ex parse_to_syntax
        end
    end
end

t_avg = round((time() - t0)/file_count*1000, digits = 2)

println()
@info """
    Finished parsing $file_count files.
        $(exception_count) failures compared to reference parser
        $(mismatch_count) Expr mismatches
        $(t_avg)ms per file"""

open(joinpath(@__DIR__, "reduced_failures.jl"), write=true) do io
    for str in all_reduced_failures
        println(io, repr(str))
    end
    for str in all_reduced_failures
        println(io, "#------------------------------")
        println(io, str)
        println(io)
    end
end
