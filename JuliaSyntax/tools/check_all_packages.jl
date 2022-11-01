# hacky script to parse all Julia files in all packages in General
# to Exprs and report errors
#
# Run this after registry_download.jl (so the pkgs directory is populated).

using JuliaSyntax, Logging

logio = open(joinpath(@__DIR__, "logs.txt"), "w")
logger = Logging.ConsoleLogger(logio)

pkgspath = joinpath(@__DIR__, "pkgs")

exceptions = []
Logging.with_logger(logger) do
    t = time()
    i = 0
    iob = IOBuffer()
    ex_count = 0
    for (r, _, files) in walkdir(pkgspath)
        for f in files
            endswith(f, ".jl") || continue
            fpath = joinpath(r, f)
            if isfile(fpath)
                file = read(fpath, String)
                try
                    e1 = JuliaSyntax.parseall(Expr, file)
                catch err
                    err isa InterruptException && rethrow()
                    ex = (err, catch_backtrace())
                    push!(exceptions, ex)
                    ref_parse = "success"
                    e2 = JuliaSyntax.fl_parseall(file)
                    @assert Meta.isexpr(e2, :toplevel)
                    if length(e2.args) >= 1 && Meta.isexpr(last(e2.args), (:error, :incomplete))
                        ref_parse = "fail"
                        if err isa JuliaSyntax.ParseError
                            # Both parsers agree that there's an error, and
                            # JuliaSyntax didn't have an internal error.
                            continue
                        end
                    end

                    ex_count += 1
                    parse_to_syntax = "success"
                    try
                        JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, file)
                    catch err2
                        parse_to_syntax = "fail"
                    end
                    @error "Parse failed" fpath exception=ex parse_to_syntax
                end
            end
            i += 1
            if i % 100 == 0
                runtime = time() - t
                avg = round(runtime/i*1000, digits = 2)
                print(iob, "\e[2J\e[0;0H")
                println(iob, "$i files parsed")
                println(iob, "> $(ex_count) failures compared to reference parser")
                println(iob, "> $(length(exceptions)) errors in total")
                println(iob, "> $(avg)ms per file, $(round(Int, runtime))s in total")
                println(stderr, String(take!(iob)))
            end
        end
    end
end
close(logio)
