using Serialization
using JuliaSyntax

pkgspath = joinpath(@__DIR__, "pkgs")
tarspath = joinpath(@__DIR__, "pkg_tars")

mkpath(pkgspath)
mkpath(tarspath)

tar_info = [(m = match(r"(.*)_(\d+\.\d+\.\d+.*)\.tgz$", f); (f, m[1], VersionNumber(m[2])))
            for f in readdir(tarspath) if endswith(f, ".tgz")]

tar_maxver = Dict{String,VersionNumber}()
for (_,name,ver) in tar_info
    v = get(tar_maxver, name, v"0.0.0")
    if v < ver
        tar_maxver[name] = ver
    end
end

@info "# Untarring packages"

for tinfos in Iterators.partition(tar_info, 50)
    @sync for (tarname, pkgname, pkgver) in tinfos
        @async begin
            dir = joinpath(pkgspath, "$(pkgname)_$(pkgver)")
            if pkgver != tar_maxver[pkgname]
                if isdir(dir)
                    # Clean up old packages
                    rm(dir; recursive=true, force=true)
                end
            elseif !isdir(dir) || !isdir(joinpath(dir, "src"))
                rm(dir; recursive=true, force=true)
                mkpath(dir)
                tar_path = joinpath(tarspath, tarname)
                try
                    run(`tar -xf $tar_path -C $dir`)
                catch err
                    @error "could not untar $tar_path"
                end
            end
        end
    end
end

@info "# Parsing files with reference parser"

let i = 0, tot_files = 0
    for (r, _, files) in walkdir(pkgspath)
        for f in files
            tot_files += 1
            endswith(f, ".jl") || continue
            fpath = joinpath(r, f)
            outpath = joinpath(r, f*".Expr")
            if !islink(fpath) && isfile(fpath) && !isfile(outpath)
                code = read(fpath, String)
                fl_ex = JuliaSyntax.fl_parseall(code, filename=fpath)
                i += 1
                if i % 100 == 0
                    @info "$i/$tot_files files parsed"
                end
                open(outpath, "w") do io
                    serialize(io, fl_ex)
                end
            end
        end
    end
end
