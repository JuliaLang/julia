#!/usr/bin/env julia

import Pkg3.TOML
import LibGit2

const STDLIBS = [
    "Base64"
    "CRC32c"
    "Dates"
    "DelimitedFiles"
    "Distributed"
    "FileWatching"
    "Future"
    "InteractiveUtils"
    "IterativeEigensolvers"
    "LibGit2"
    "Libdl"
    "LinearAlgebra"
    "Logging"
    "Markdown"
    "Mmap"
    "Pkg"
    "Printf"
    "Profile"
    "REPL"
    "Random"
    "Serialization"
    "SHA"
    "SharedArrays"
    "SparseArrays"
    "SuiteSparse"
    "Test"
    "UUIDs"
    "Unicode"
]

function uses(repo::String, tree::String, lib::String)
    pattern = string(raw"\b(import|using)\s+((\w|\.)+\s*,\s*)*", lib, raw"\b")
    success(`git -C $repo grep -Eq $pattern $tree`)
end

function gitmeta(pkgs::Dict{String,Package})
    @assert length(STDLIBS) ≤ 64 # use 64 bits to encode usage
    fd = joinpath(@__DIR__, "sha1map.toml")
    fs = joinpath(@__DIR__, "stdlib.toml")
    d = ispath(fd) ? TOML.parsefile(fd) : Dict()
    s = Dict()
    if ispath(fs)
        s = TOML.parsefile(fs)
        get(s, "STDLIBS", nothing) == STDLIBS || empty!(s)
        mv(fs, "$fs.old", remove_destination=true)
    end
    s["STDLIBS"] = STDLIBS
    io = open(fs, "w")
    println(io, "STDLIBS = [")
    for lib in STDLIBS
        println(io, "    ", repr(lib), ",")
    end
    println(io, "]")
    println(io)
    for (pkg, p) in sort!(collect(pkgs), by=first)
        (pkg == "julia" || isempty(p.versions)) && continue
        uuid = string(p.uuid)
        @info "Package [$uuid] $pkg"
        haskey(d, uuid) || (d[uuid] = Dict())
        haskey(s, uuid) || (s[uuid] = Dict())
        updated = false
        repo_path = joinpath(homedir(), ".julia", "upstream", uuid)
        repo = nothing
        for (ver, v) in p.versions
            haskey(d[uuid], v.sha1) &&
            (v"0.7" ∉ v.requires["julia"].versions ||
            haskey(s[uuid], v.sha1)) && continue
            if repo == nothing
                repo = ispath(repo_path) ? LibGit2.GitRepo(repo_path) : begin
                    updated = true
                    @info "Cloning [$uuid] $pkg"
                    LibGit2.clone(p.url, repo_path, isbare=true)
                end
            end
            git_commit_hash = LibGit2.GitHash(v.sha1)
            if !updated
                try LibGit2.GitObject(repo, git_commit_hash)
                catch err
                    err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
                    @info "Updating $pkg from $(p.url)"
                    LibGit2.fetch(repo, remoteurl=p.url, refspecs=["+refs/*:refs/remotes/cache/*"])
                end
            end
            git_commit = try LibGit2.GitObject(repo, git_commit_hash)
            catch err
                err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
                error("$pkg: git object $(v.sha1) could not be found")
            end
            git_commit isa LibGit2.GitCommit || git_commit isa LibGit2.GitTag ||
                error("$pkg: git object $(v.sha1) not a commit – $(typeof(git_commit))")
            git_tree = LibGit2.peel(LibGit2.GitTree, git_commit)
            @assert git_tree isa LibGit2.GitTree
            git_tree_hash = string(LibGit2.GitHash(git_tree))
            d[uuid][v.sha1] = git_tree_hash
            # scan for stdlib dependencies
            v"0.7" in v.requires["julia"].versions || continue
            haskey(s[uuid], v.sha1) && continue
            libs = [uses(repo_path, git_tree_hash, lib) for lib in STDLIBS]
            s[uuid][v.sha1] = sum(d*2^(i-1) for (i,d) in enumerate(libs))
        end
        isempty(s[uuid]) && continue
        println(io, "[$uuid]")
        for (sha1, n) in sort!(collect(s[uuid]), by=string∘first)
            println(io, "$sha1 = $n")
        end
        println(io)
        flush(io)
    end
    open(fd, "w") do io
        TOML.print(io, d, sorted=true)
    end
    close(io)
    rm("$fs.old", force=true)
    return d, s
end
