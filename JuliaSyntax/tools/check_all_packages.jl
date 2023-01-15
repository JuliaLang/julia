# hacky script to parse all Julia files in all packages in General
# to Exprs and report errors
#
# Run this after registry_download.jl (so the pkgs directory is populated).

using JuliaSyntax, Logging, Serialization

include("../test/test_utils.jl")

logio = open(joinpath(@__DIR__, "logs.txt"), "w")
logger = Logging.ConsoleLogger(logio)

pkgspath = joinpath(@__DIR__, "pkgs")

exception_count = 0
mismatch_count = 0
file_count = 0
t0 = time()
exceptions = []

Logging.with_logger(logger) do
    global exception_count, mismatch_count, file_count, t0
    for (r, _, files) in walkdir(pkgspath)
        for f in files
            endswith(f, ".jl") || continue
            fpath = joinpath(r, f)
            isfile(fpath) || continue

            code = read(fpath, String)
            expr_cache = fpath*".Expr"
            #e2 = JuliaSyntax.fl_parseall(code)
            e2 = open(deserialize, fpath*".Expr")
            @assert Meta.isexpr(e2, :toplevel)
            try
                e1 = JuliaSyntax.parseall(Expr, code, filename=fpath, ignore_warnings=true)
                if !exprs_roughly_equal(e2, e1)
                    mismatch_count += 1
                    @error("Parsers succeed but disagree",
                           fpath,
                           diff=Text(sprint(show_expr_text_diff, show, e1, e2)),
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

            file_count += 1
            if file_count % 100 == 0
                t_avg = round((time() - t0)/file_count*1000, digits = 2)
                print(stderr, "\r$file_count files parsed, $t_avg ms per file")
            end
        end
    end
end
close(logio)

t_avg = round((time() - t0)/file_count*1000, digits = 2)

println()
@info """
    Finished parsing $file_count files.
        $(exception_count) failures compared to reference parser
        $(mismatch_count) Expr mismatches
        $(t_avg)ms per file"""
