#!/usr/bin/env julia
using Sandbox, Pkg.Artifacts

if length(ARGS) < 1 || length(ARGS) > 2
    println("Usage: test_rootfs.jl <url> [gitsha]")
    exit(1)
end

url = ARGS[1]
hash = Base.SHA1(get(ARGS, 2, nothing))

if hash === nothing
    @warn("hash not provided; this will download the tarball, then fail, so you can see the true hash")
    hash = Base.SHA1("0000000000000000000000000000000000000000")
end

# If the artifact is not locally existent, download it
if !artifact_exists(hash)
    @info("Artifact did not exist, downloading")
    download_artifact(hash, url; verbose=true)
end

config = SandboxConfig(
    Dict("/" => artifact_path(hash)),
    Dict{String,String}(),
    Dict(
        "PATH" => "/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin",
        "HOME" => "/home/juliaci",
        "USER" => "juliaci",
    );
    stdin,
    stdout,
    stderr,
    uid=Sandbox.getuid(),
    gid=Sandbox.getgid(),
)
with_executor() do exe
    run(exe, config, `/bin/bash`)
end
