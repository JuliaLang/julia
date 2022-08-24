# hacky script to parse all Julia files in all packages in General
# to Exprs and report errors
#
# Run this after registry_download.jl (so the pkgs directory is populated).

using JuliaSyntax, Logging

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
    for (r, _, files) in walkdir(pkgspath)
        for f in files
            endswith(f, ".jl") || continue
            fpath = joinpath(r, f)
            try
                JuliaSyntax.parse(Expr, read(fpath, String))
            catch err
                err isa InterruptException && rethrow()
                ex = (err, catch_backtrace())
                push!(exceptions, ex)
                @error "parsing failed for $(fpath)" ex
                flush(logio)
            end
            i += 1
            if i % 100 == 0
                runtime = time() - t
                avg = round(runtime/i*1000, digits = 2)
                print(iob, "\e[2J\e[0;0H")
                println(iob, "$i files parsed")
                println(iob, "  $(length(exceptions)) failures")
                println(iob, "  $(avg)ms per file, $(round(Int, runtime))s in total")
                println(stderr, String(take!(iob)))
            end
        end
    end
end
close(logio)
