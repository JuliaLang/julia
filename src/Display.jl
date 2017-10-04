module Display

using Base.Random: UUID
using Pkg3.Types

export print_project_diff, print_manifest_diff

const colors = Dict(
    ' ' => :white,
    '+' => :light_green,
    '-' => :light_red,
    '↑' => :light_yellow,
    '~' => :light_yellow,
    '↓' => :light_magenta,
)
const color_dark = :light_black

function status(env::EnvCache, mode::Symbol)
    project = env.project
    manifest = env.manifest
    if env.git != nothing
        git_path = LibGit2.path(env.git)
        project_path = relpath(env.project_file, git_path)
        manifest_path = relpath(env.manifest_file, git_path)
        project = read_project(git_file_stream(env.git, "HEAD:$project_path", fakeit=true))
        manifest = read_manifest(git_file_stream(env.git, "HEAD:$manifest_path", fakeit=true))
    end
    diff = manifest_diff(manifest, env.manifest, true)
    if mode == :project
        info("Status ", pathrepr(env, env.project_file))
        print_project_diff(project["deps"], env.project["deps"], true)
    elseif mode == :manifest
        info("Status ", pathrepr(env, env.manifest_file))
        print_manifest_diff(diff)
    else
        error("unexpected mode: $mode")
    end
end

function emit_project(x::Char, name::String, uuid::String)
    print_with_color(color_dark, " [$(uuid[1:8])]")
    print_with_color(colors[x], " $x $name\n")
end

function print_project_diff(deps₀::Dict, deps₁::Dict, all::Bool=false)
    clean = !all
    for name in sort!(union(keys(deps₀), keys(deps₁)), by=lowercase)
        uuid₀, uuid₁ = get(deps₀, name, ""), get(deps₁, name, "")
        if uuid₀ == uuid₁
            all && emit_project(' ', name, uuid₁)
        else
            isempty(uuid₀) || emit_project('-', name, uuid₀)
            isempty(uuid₁) || emit_project('+', name, uuid₁)
            clean = false
        end
    end
    clean && print_with_color(color_dark, " [no changes]\n")
    return nothing
end
print_project_diff(env₀::EnvCache, env₁::EnvCache) =
    print_project_diff(env₀.project["deps"], env₁.project["deps"])

struct ManifestEntry
    name::String
    uuid::UUID
    hash::SHA1
    version::Union{VersionNumber,Void}
end

function manifest_entries(manifest::Dict)
    entries = Dict{UUID,ManifestEntry}()
    for (name, infos) in manifest, info in infos
        uuid = UUID(info["uuid"])
        hash = SHA1(info["hash-sha1"])
        ver = get(info, "version", nothing)
        version = ver != nothing ? VersionNumber(ver) : nothing
        entries[uuid] = ManifestEntry(name, uuid, hash, version)
    end
    return entries
end
manifest_entries(env::EnvCache) = manifest_entries(env.manifest)

const ManifestDiff = Vector{NTuple{2,Union{ManifestEntry,Void}}}

function manifest_diff(
    infos₀::Dict{UUID,ManifestEntry},
    infos₁::Dict{UUID,ManifestEntry},
    all::Bool = false
)::ManifestDiff
    uuids = sort!(union(keys(infos₀), keys(infos₁)), by=uuid->uuid.value)
    diff = eltype(ManifestDiff)[
        (get(infos₀, uuid, nothing), get(infos₁, uuid, nothing))
        for uuid in uuids]
    all || filter!(diff) do infos
        info₀, info₁ = infos
        info₀ == nothing || info₁ == nothing || info₀.hash != info₁.hash
    end
    sort!(diff, by=pair->lowercase(pair[pair[2]!=nothing ? 2 : 1].name))
end
manifest_diff(infos₀::Dict, infos₁::Dict, all::Bool=false) =
    manifest_diff(manifest_entries(infos₀), manifest_entries(infos₁), all)

v_str(x::ManifestEntry) =
    x.version == nothing ? "[$(string(x.hash)[1:16])]" : "v$(x.version)"

function print_manifest_diff(diff::ManifestDiff)
    if isempty(diff)
        print_with_color(color_dark, " [no changes]\n")
        return
    end
    for (info₀, info₁) in diff
        uuid = info₁ != nothing ? info₁.uuid : info₀.uuid
        name = info₁ != nothing ? info₁.name : info₀.name
        if info₀ != nothing && info₁ != nothing
            v₀, v₁ = v_str(info₀), v_str(info₁)
            x = info₀.version == nothing || info₁.version == nothing ? '~' :
                info₀.version < info₁.version ? '↑' :
                info₀.version > info₁.version ? '↓' : ' '
            v = x == ' ' ? v₀ : "$v₀ ⇒ $v₁"
        elseif info₀ != nothing
            x, v = '-', v_str(info₀)
        elseif info₁ != nothing
            x, v = '+', v_str(info₁)
        else
            error("this should not happen")
        end
        print_with_color(color_dark, " [$(string(uuid)[1:8])] ")
        print_with_color(colors[x], "$x $name $v\n")
    end
end
print_manifest_diff(env₀::EnvCache, env₁::EnvCache) =
    print_manifest_diff(manifest_diff(env₀.manifest, env₁.manifest))

end # module
