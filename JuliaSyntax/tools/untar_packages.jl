using Serialization
using JuliaSyntax

pkgspath = joinpath(@__DIR__, "pkgs")

for tars in Iterators.partition(readdir(pkgspath), 50)
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

@info "Parsing files with reference parser"

let i = 0
    for (r, _, files) in walkdir(pkgspath)
        for f in files
            endswith(f, ".jl") || continue
            fpath = joinpath(r, f)
            outpath = joinpath(r, f*".Expr")
            if isfile(fpath)
                code = read(fpath, String)
                fl_ex = JuliaSyntax.fl_parseall(code, filename=fpath)
                i += 1
                if i % 100 == 0
                    @info "$i files parsed"
                end
                open(outpath, "w") do io
                    serialize(io, fl_ex)
                end
            end
        end
    end
end
