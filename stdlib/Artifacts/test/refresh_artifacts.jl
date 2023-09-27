# This file is a part of Julia. License is MIT: https://julialang.org/license

using Artifacts: with_artifacts_directory
using Pkg.Artifacts: load_artifacts_toml, ensure_artifact_installed
let
    toml = joinpath(@__DIR__, "Artifacts.toml")
    unused = Base.BinaryPlatforms.Platform(string(Sys.ARCH), "linux")
    with_artifacts_directory(ARGS[1]) do
        # ensure_all_artifacts_installed(toml; include_lazy=false)
        dict = load_artifacts_toml(toml)
        for (name, meta) in dict
            if meta isa Array
                for meta in meta
                    get(meta, "lazy", false) && continue
                    ensure_artifact_installed(name, meta, toml; platform=unused, io = devnull)
                end
            else; meta::Dict
                get(meta, "lazy", false) && continue
                ensure_artifact_installed(name, meta, toml; platform=unused, io = devnull)
            end
        end
    end
end
