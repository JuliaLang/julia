# This file is a part of Julia. License is MIT: https://julialang.org/license

module Display

using UUIDs
import LibGit2

using ..Types

const colors = Dict(
    ' ' => :white,
    '+' => :light_green,
    '-' => :light_red,
    '↑' => :light_yellow,
    '~' => :light_yellow,
    '↓' => :light_magenta,
    '?' => :red,
)
const color_dark = :light_black

function git_file_stream(repo::LibGit2.GitRepo, spec::String; fakeit::Bool=false)::IO
    blob = try LibGit2.GitBlob(repo, spec)
    catch err
        err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
        fakeit && return devnull
    end
    iob = IOBuffer(LibGit2.content(blob))
    close(blob)
    return iob
end

function status(ctx::Context, mode::PackageMode, use_as_api=false)
    env = ctx.env
    project₀ = project₁ = env.project
    manifest₀ = manifest₁ = env.manifest
    diff = nothing
    if !use_as_api
        pkg = ctx.env.pkg
        if pkg !== nothing
           printstyled("Project "; color=Base.info_color(), bold=true)
           println(pkg.name, " v", pkg.version)
        end
    end
    if env.git != nothing
        LibGit2.with(LibGit2.GitRepo(env.git)) do repo
            git_path = LibGit2.path(repo)
            project_path = relpath(env.project_file, git_path)
            manifest_path = relpath(env.manifest_file, git_path)
            project₀ = read_project(git_file_stream(repo, "HEAD:$project_path", fakeit=true))
            manifest₀ = read_manifest(git_file_stream(repo, "HEAD:$manifest_path", fakeit=true))
        end
    end
    if mode == PKGMODE_PROJECT || mode == PKGMODE_COMBINED
        # TODO: handle project deps missing from manifest
        m₀ = filter_manifest(in_project(project₀["deps"]), manifest₀)
        m₁ = filter_manifest(in_project(project₁["deps"]), manifest₁)
        diff = manifest_diff(ctx, m₀, m₁)
        if !use_as_api
            printpkgstyle(ctx, :Status, pathrepr(env.project_file), #=ignore_indent=# true)
            print_diff(ctx, diff, #=status=# true)
        end
    end
    if mode == PKGMODE_MANIFEST
        diff = manifest_diff(ctx, manifest₀, manifest₁)
        if !use_as_api
            printpkgstyle(ctx, :Status, pathrepr(env.manifest_file), #=ignore_indent=# true)
            print_diff(ctx, diff, #=status=# true)
        end
    elseif mode == PKGMODE_COMBINED
        p = not_in_project(merge(project₀["deps"], project₁["deps"]))
        m₀ = filter_manifest(p, manifest₀)
        m₁ = filter_manifest(p, manifest₁)
        c_diff = filter!(x->x.old != x.new, manifest_diff(ctx, m₀, m₁))
        if !isempty(c_diff)
            if !use_as_api
                printpkgstyle(ctx, :Status, pathrepr(env.manifest_file), #=ignore_indent=# true)
                print_diff(ctx, c_diff, #=status=# true)
            end
            diff = Base.vcat(c_diff, diff)
        end
    end
    return diff
end

function print_project_diff(ctx::Context, env₀::EnvCache, env₁::EnvCache)
    pm₀ = filter_manifest(in_project(env₀.project["deps"]), env₀.manifest)
    pm₁ = filter_manifest(in_project(env₁.project["deps"]), env₁.manifest)
    diff = filter!(x->x.old != x.new, manifest_diff(ctx, pm₀, pm₁))
    if isempty(diff)
        printstyled(color = color_dark, " [no changes]\n")
    else
        print_diff(ctx, diff)
    end
end

function print_manifest_diff(ctx::Context, env₀::EnvCache, env₁::EnvCache)
    diff = manifest_diff(ctx, env₀.manifest, env₁.manifest)
    diff = filter!(x->x.old != x.new, diff)
    if isempty(diff)
        printstyled(color = color_dark, " [no changes]\n")
    else
        print_diff(ctx, diff)
    end
end

struct VerInfo
    hash::Union{SHA1,Nothing}
    path::Union{String,Nothing}
    ver::Union{VersionNumber,Nothing}
    pinned::Bool
    repo::Union{Types.GitRepo, Nothing}
end

revstring(str::String) = occursin(r"\b([a-f0-9]{40})\b", str) ? str[1:7] : str

vstring(ctx::Context, a::VerInfo) =
    string((a.ver == nothing && a.hash != nothing) ? "[$(string(a.hash)[1:16])]" : "",
           a.ver != nothing ? "v$(a.ver)" : "",
           a.path != nothing ? " [$(pathrepr(a.path))]" : "",
           a.repo != nothing ? " #$(revstring(a.repo.rev)) ($(a.repo.url))" : "",
           a.pinned == true ? " ⚲" : "",
           )

Base.:(==)(a::VerInfo, b::VerInfo) =
    a.hash == b.hash && a.ver == b.ver && a.pinned == b.pinned && a.repo == b.repo

≈(a::VerInfo, b::VerInfo) = a.hash == b.hash &&
    (a.ver == nothing || b.ver == nothing || a.ver == b.ver) &&
    (a.pinned == b.pinned) &&
    (a.repo == nothing || b.repo == nothing || a.repo == b.repo)

struct DiffEntry
    uuid::UUID
    name::String
    old::Union{VerInfo,Nothing}
    new::Union{VerInfo,Nothing}
end

function print_diff(io::IO, ctx::Context, diff::Vector{DiffEntry}, status=false)
    same = all(x.old == x.new for x in diff)
    some_packages_not_downloaded = false
    for x in diff
        pkgid = Base.PkgId(x.uuid, x.name)
        package_downloaded = pkgid in keys(Base.loaded_modules) ||
                             Base.locate_package(pkgid) !== nothing
        # Package download detection doesnt work properly when runn running targets
        ctx.currently_running_target && (package_downloaded = true)
        if x.old != nothing && x.new != nothing
            if x.old ≈ x.new
                verb = ' '
                vstr = vstring(ctx, x.new)
            else
                if x.old.hash != x.new.hash && x.old.ver != x.new.ver
                    verb = x.old.ver == nothing || x.new.ver == nothing ||
                           x.old.ver == x.new.ver ? '~' :
                           x.old.ver < x.new.ver  ? '↑' : '↓'
                elseif x.old.ver == x.new.ver && x.old.pinned != x.new.pinned ||
                        x.old.repo != nothing || x.new.repo != nothing
                    verb = '~'
                else
                    verb = '?'
                end
                vstr = (x.old.ver == x.new.ver && x.old.pinned == x.new.pinned && x.old.repo == x.new.repo) ?
                      vstring(ctx, x.new) :
                      vstring(ctx, x.old) * " ⇒ " * vstring(ctx, x.new)
            end
        elseif x.new != nothing
            verb = '+'
            vstr = vstring(ctx, x.new)
        elseif x.old != nothing
            verb = '-'
            vstr = vstring(ctx, x.old)
        else
            verb = '?'
            vstr = "[unknown]"
        end
        v = same ? "" : " $verb"
        if verb != '-' && status && !package_downloaded
            printstyled(io, "→", color=:red)
        else
            print(io, " ")
        end
        if verb != '-'
            package_downloaded || (some_packages_not_downloaded = true)
        end
        printstyled(io, " [$(string(x.uuid)[1:8])]"; color = color_dark)
        printstyled(io, "$v $(x.name) $vstr\n"; color = colors[verb])
    end
    if status && some_packages_not_downloaded
        @warn "Some packages (indicated with a red arrow) are not downloaded, use `instantiate` to instantiate the current environment"
    end
end
# TODO: Use the Context stream
print_diff(ctx::Context, diff::Vector{DiffEntry}, status=false) = print_diff(stdout, ctx, diff, status)

function manifest_by_uuid(manifest::Dict)
    entries = Dict{UUID,Dict}()
    for (name, infos) in manifest, info in infos
        uuid = UUID(info["uuid"])
        haskey(entries, uuid) && @warn("Duplicate UUID in manifest: $uuid")
        entries[uuid] = merge(info, Dict("name" => name))
    end
    return entries
end

function name_ver_info(info::Dict)
    name = info["name"]
    hash = haskey(info, "git-tree-sha1") ? SHA1(info["git-tree-sha1"])    : nothing
    ver  = haskey(info, "version")       ? VersionNumber(info["version"]) : nothing
    path =  get(info, "path", nothing)
    pin  =  get(info, "pinned", false)
    if haskey(info, "repo-url")
        repo = Types.GitRepo(info["repo-url"], info["repo-rev"])
    else
        repo = nothing
    end
    name, VerInfo(hash, path, ver, pin, repo)
end

function manifest_diff(ctx::Context, manifest₀::Dict, manifest₁::Dict)
    diff = DiffEntry[]
    entries₀ = manifest_by_uuid(manifest₀)
    entries₁ = manifest_by_uuid(manifest₁)
    for uuid in union(keys(entries₀), keys(entries₁))
        name₀ = name₁ = v₀ = v₁ = nothing
        haskey(entries₀, uuid) && ((name₀, v₀) = name_ver_info(entries₀[uuid]))
        haskey(entries₁, uuid) && ((name₁, v₁) = name_ver_info(entries₁[uuid]))
        name₀ == nothing && (name₀ = name₁)
        name₁ == nothing && (name₁ = name₀)
        if name₀ == name₁
            push!(diff, DiffEntry(uuid, name₀, v₀, v₁))
        else
            push!(diff, DiffEntry(uuid, name₀, v₀, nothing))
            push!(diff, DiffEntry(uuid, name₁, nothing, v₁))
        end
    end
    sort!(diff, by=x->(x.uuid in keys(ctx.stdlibs), x.name, x.uuid))
end

function filter_manifest!(predicate, manifest::Dict)
    empty = String[]
    for (name, infos) in manifest
        filter!(infos) do info
            predicate(name, info)
        end
        isempty(infos) && push!(empty, name)
    end
    for name in empty
        pop!(manifest, name)
    end
    return manifest
end
filter_manifest(predicate, manifest::Dict) =
    filter_manifest!(predicate, deepcopy(manifest))

# This is precompilable, an anonymous function is not.
struct InProject{D <: Dict}
    deps::D
    neg::Bool
end
function (ip::InProject)(name::String, info::Dict)
    v = haskey(ip.deps, name) && haskey(info, "uuid") && ip.deps[name] == info["uuid"]
    return ip.neg ? !v : v
end
in_project(deps::Dict) = InProject(deps, false)
not_in_project(deps::Dict) = InProject(deps, true)


end # module
