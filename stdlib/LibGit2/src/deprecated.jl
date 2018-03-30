# This file is a part of Julia. License is MIT: https://julialang.org/license

# BEGIN 0.7 deprecations

# PR #22062
function set_remote_url(repo::LibGit2.GitRepo, url::AbstractString; remote::AbstractString="origin")
    Base.depwarn(string(
        "`LibGit2.set_remote_url(repo, url; remote=remote)` is deprecated, use ",
        "`LibGit2.set_remote_url(repo, remote, url)` instead."), :set_remote_url)
    set_remote_url(repo, remote, url)
end

function set_remote_url(path::AbstractString, url::AbstractString; remote::AbstractString="origin")
    Base.depwarn(string(
        "`LibGit2.set_remote_url(path, url; remote=remote)` is deprecated, use ",
        "`LibGit2.set_remote_url(path, remote, url)` instead."), :set_remote_url)
    set_remote_url(path, remote, url)
end

function prompt(msg::AbstractString; default::AbstractString="", password::Bool=false)
    Base.depwarn(string(
        "`LibGit2.prompt(msg::AbstractString; default::AbstractString=\"\", password::Bool=false)` is deprecated, use ",
        "`result = Base.prompt(msg, default=default, password=password); result === nothing ? \"\" : result` instead."), :prompt)
    coalesce(Base.prompt(msg, default=default, password=password), "")
end

# PR #26437
# when this deprecation is deleted, remove all calls to it, and remove the keyword of:
# `payload` from "src/LibGit2.jl"
function deprecate_payload_keyword(f, sig, payload)
    if payload !== nothing
        Base.depwarn(string(
            "`LibGit2.$f($sig; payload=cred)` is deprecated, use ",
            "`LibGit2.$f($sig; credentials=cred)` instead."), f)
    end
end

@deprecate get_creds!(cache::CachedCredentials, credid, default) get!(cache, credid, default)

@eval Base @deprecate merge!(repo::$(GitRepo), args...; kwargs...) $(LibGit2.merge!)(repo, args...; kwargs...)
@eval Base @deprecate push!(w::$(GitRevWalker), arg) $(LibGit2.push!)(w, arg)

# PR #24594
@deprecate AbstractCredentials AbstractCredential false
@deprecate UserPasswordCredentials UserPasswordCredential false
@deprecate SSHCredentials SSHCredential false

@deprecate hex(id::LibGit2.GitHash)      string(id)
@deprecate hex(id::LibGit2.GitShortHash) string(id)
