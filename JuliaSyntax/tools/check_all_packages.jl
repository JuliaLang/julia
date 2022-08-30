# hacky script to parse all Julia files in all packages in General
# to Exprs and report errors
#
# Run this after registry_download.jl (so the pkgs directory is populated).

using JuliaSyntax, Logging

# like Meta.parseall, but throws
function parseall(str)
    pos = firstindex(str)
    exs = []
    while pos <= lastindex(str)
        ex, pos = Meta.parse(str, pos)
        push!(exs, ex)
    end
    if length(exs) == 0
        throw(Meta.ParseError("end of input"))
    elseif length(exs) == 1
        return exs[1]
    else
        return Expr(:toplevel, exs...)
    end
end

logio = open(joinpath(@__DIR__, "logs.txt"), "w")
logger = Logging.ConsoleLogger(logio)

pkgspath = joinpath(@__DIR__, "pkgs")

parallel = 50
exceptions = []
Logging.with_logger(logger) do
    for tars in Iterators.partition(readdir(pkgspath), parallel)
        @sync for tar in tars
            endswith(tar, ".tgz") || continue
            @async begin
                dir = joinpath(@__DIR__, "pkgs", replace(tar, r"\.tgz$" => ""))
                if !isdir(dir) || !isdir(joinpath(dir, "src"))
                    rm(dir; recursive=true, force=true)
                    mkpath(dir)
                    tar_path = joinpath(@__DIR__, "pkgs", tar)
                    try
                        run(`tar -xf $tar_path -C $dir`)
                    catch err
                        @error "could not untar $tar_path"
                    end
                end
            end
        end
    end

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
                    e1 = JuliaSyntax.parse(Expr, file)
                catch err
                    err isa InterruptException && rethrow()
                    ex_count += 1
                    ex = (err, catch_backtrace())
                    push!(exceptions, ex)
                    meta_parse = "success"
                    try
                        parseall(file)
                    catch err2
                        meta_parse = "fail"
                        ex_count -= 1
                    end
                    parse_to_syntax = "success"
                    try
                        JuliaSyntax.parse(JuliaSyntax.SyntaxNode, file)
                    catch err2
                        parse_to_syntax = "fail"
                    end
                    severity = parse_to_syntax == "fail" ? "error" :
                        meta_parse == "fail" ? "warn" : "error"
                    println(logio, """
                    [$(severity)] $(fpath)
                      parse-to-expr: fail
                      parse-to-syntaxtree: $(parse_to_syntax)
                      reference: $(meta_parse)
                    """)
                    @error "" exception = ex
                    flush(logio)
                end
            end
            i += 1
            if i % 100 == 0
                runtime = time() - t
                avg = round(runtime/i*1000, digits = 2)
                print(iob, "\e[2J\e[0;0H")
                println(iob, "$i files parsed")
                println(iob, "> $(ex_count) failures compared to Meta.parse")
                println(iob, "> $(length(exceptions)) errors in total")
                println(iob, "> $(avg)ms per file, $(round(Int, runtime))s in total")
                println(stderr, String(take!(iob)))
            end
        end
    end
end
close(logio)
