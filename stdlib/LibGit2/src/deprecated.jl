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

# PR #23640
# when this deprecation is deleted, remove all calls to it, and replace all keywords of:
# `payload::Union{CredentialPayload, AbstractCredential, CachedCredentials, Nothing}`
#  with `payload::CredentialPayload` from base/libgit2/libgit2.jl
function deprecate_nullable_creds(f, sig, payload)
    if isa(payload, Union{AbstractCredential, CachedCredentials, Nothing})
        # Note: Be careful not to show the contents of the credentials as it could reveal a
        # password.
        if payload === nothing
            msg = "`LibGit2.$f($sig; payload=nothing)` is deprecated, use "
            msg *= "`LibGit2.$f($sig; payload=LibGit2.CredentialPayload())` instead."
            p = CredentialPayload()
        else
            cred = payload
            C = typeof(cred)
            msg = "`LibGit2.$f($sig; payload=$C(...))` is deprecated, use "
            msg *= "`LibGit2.$f($sig; payload=LibGit2.CredentialPayload($C(...)))` instead."
            p = CredentialPayload(cred)
        end
        Base.depwarn(msg, f)
    else
        p = payload::CredentialPayload
    end
    return p
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
