#!/usr/bin/env julia

using Pkg3.TOML
using Base: LibGit2

function sha1map(pkgs::Dict{String,Package})
    f = joinpath(@__DIR__, "sha1map.toml")
    d = ispath(f) ? TOML.parsefile(f) : Dict()
    for (pkg, p) in pkgs
        isempty(p.versions) && continue
        uuid = string(p.uuid)
        haskey(d, uuid) || (d[uuid] = Dict())
        updated = false
        repo = nothing
        for (ver, v) in p.versions
            haskey(d[uuid], v.sha1) && continue
            git_commit_hash = LibGit2.GitHash(v.sha1)
            if repo == nothing
                repo_path = joinpath(homedir(), ".julia", "upstream", uuid)
                repo = ispath(repo_path) ? LibGit2.GitRepo(repo_path) : begin
                    updated = true
                    info("Cloning [$uuid] $pkg")
                    LibGit2.clone(p.url, repo_path, isbare=true)
                end
            end
            if !updated
                try LibGit2.GitObject(repo, git_commit_hash)
                catch err
                    err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
                    info("Updating $pkg from $(p.url)")
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
        end
    end
    open(f, "w") do io
        TOML.print(io, d, sorted=true)
    end
    return d
end
