module Display

using UUIDs
import LibGit2

using Pkg3.Types
import Pkg3: @info, Nothing


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
        fakeit && return DevNull
    end
    return IOBuffer(LibGit2.rawcontent(blob))
end

function status(ctx::Context, mode::PackageMode, use_as_api=false)
    env = ctx.env
    project₀ = project₁ = env.project
    manifest₀ = manifest₁ = env.manifest
    diff = nothing

    if env.git != nothing
        git_path = LibGit2.path(env.git)
        project_path = relpath(env.project_file, git_path)
        manifest_path = relpath(env.manifest_file, git_path)
        project₀ = read_project(git_file_stream(env.git, "HEAD:$project_path", fakeit=true))
        manifest₀ = read_manifest(git_file_stream(env.git, "HEAD:$manifest_path", fakeit=true))
    end
    if mode == PKGMODE_PROJECT || mode == PKGMODE_COMBINED
        # TODO: handle project deps missing from manifest
        m₀ = filter_manifest(in_project(project₀["deps"]), manifest₀)
        m₁ = filter_manifest(in_project(project₁["deps"]), manifest₁)
        use_as_api || @info("Status $(pathrepr(env, env.project_file))")
        diff = manifest_diff(m₀, m₁)
        use_as_api || print_diff(diff)
    end
    if mode == PKGMODE_MANIFEST
        use_as_api || @info("Status $(pathrepr(env, env.manifest_file))")
        diff = manifest_diff(manifest₀, manifest₁)
        use_as_api || print_diff(diff)
    elseif mode == PKGMODE_COMBINED
        p = not_in_project(merge(project₀["deps"], project₁["deps"]))
        m₀ = filter_manifest(p, manifest₀)
        m₁ = filter_manifest(p, manifest₁)
        c_diff = filter!(x->x.old != x.new, manifest_diff(m₀, m₁))
        if !isempty(c_diff)
            use_as_api || @info("Status $(pathrepr(env, env.manifest_file))")
            use_as_api || print_diff(c_diff)
            diff = Base.vcat(c_diff, diff)
        end
    end
    return diff
end

function print_project_diff(env₀::EnvCache, env₁::EnvCache)
    pm₀ = filter_manifest(in_project(env₀.project["deps"]), env₀.manifest)
    pm₁ = filter_manifest(in_project(env₁.project["deps"]), env₁.manifest)
    diff = filter!(x->x.old != x.new, manifest_diff(pm₀, pm₁))
    if isempty(diff)
        printstyled(color = color_dark, " [no changes]\n")
    else
        print_diff(diff)
    end
end

function print_manifest_diff(env₀::EnvCache, env₁::EnvCache)
    diff = manifest_diff(env₀.manifest, env₁.manifest)
    diff = filter!(x->x.old != x.new, diff)
    if isempty(diff)
        printstyled(color = color_dark, " [no changes]\n")
    else
        print_diff(diff)
    end
end

struct VerInfo
    hash::Union{SHA1,Nothing}
    path::Union{String,Nothing}
    ver::Union{VersionNumber,Nothing}
    pinned::Bool
end

vstring(a::VerInfo) =
    string(a.ver == nothing ? "[$(string(a.hash)[1:16])]" : "v$(a.ver)",
           a.pinned == true ? "⚲" : "",
           a.path != nothing ? " [$(a.path)]" : "")

Base.:(==)(a::VerInfo, b::VerInfo) =
    a.hash == b.hash && a.ver == b.ver && a.pinned == b.pinned

≈(a::VerInfo, b::VerInfo) = a.hash == b.hash &&
    (a.ver == nothing || b.ver == nothing || a.ver == b.ver) &&
    (a.pinned == b.pinned)

struct DiffEntry
    uuid::UUID
    name::String
    old::Union{VerInfo,Nothing}
    new::Union{VerInfo,Nothing}
end

function print_diff(io::IO, diff::Vector{DiffEntry})
    same = all(x.old == x.new for x in diff)
    for x in diff
        warnings = String[]
        if x.old != nothing && x.new != nothing
            if x.old ≈ x.new
                verb = ' '
                vstr = vstring(x.new)
            else
                if x.old.hash != x.new.hash && x.old.ver != x.new.ver
                    verb = x.old.ver == nothing || x.new.ver == nothing ||
                           x.old.ver == x.new.ver ? '~' :
                           x.old.ver < x.new.ver  ? '↑' : '↓'
                elseif x.old.ver == x.new.ver && x.old.pinned != x.new.pinned
                    verb = '~'
                else
                    verb = '?'
                    msg = x.old.hash == x.new.hash ?
                        "hashes match but versions don't: $(x.old.ver) ≠ $(x.new.ver)" :
                        "versions match but hashes don't: $(x.old.hash) ≠ $(x.new.hash)"
                    push!(warnings, msg)
                end
                vstr = (x.old.ver == x.new.ver && x.old.pinned == x.new.pinned) ?
                       vstring(x.new) :
                       vstring(x.old) * " ⇒ " * vstring(x.new)
            end
        elseif x.new != nothing
            verb = '+'
            vstr = vstring(x.new)
        elseif x.old != nothing
            verb = '-'
            vstr = vstring(x.old)
        else
            verb = '?'
            vstr = "[unknown]"
        end
        v = same ? "" : " $verb"
        printstyled(color = color_dark, " [$(string(x.uuid)[1:8])]")
        printstyled(color = colors[verb], "$v $(x.name) $vstr\n")
    end
end
print_diff(diff::Vector{DiffEntry}) = print_diff(STDOUT, diff)

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
    name, VerInfo(hash, path, ver, pin)
end

function manifest_diff(manifest₀::Dict, manifest₁::Dict)
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
    sort!(diff, by=x->(x.name, x.uuid))
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
