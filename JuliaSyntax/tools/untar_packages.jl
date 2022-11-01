
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

