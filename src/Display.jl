module Display

using Base.Random: UUID
using Pkg3.Types

export print_project_diff, print_manifest_diff

function emit_project(x::Char, name::String, uuid::String)
    color = x == '+' ? :light_green : :light_red
    print_with_color(:light_black, " [$(uuid[1:8])]")
    print_with_color(color, " $x $name\n")
end

function print_project_diff(deps₀::Dict, deps₁::Dict)
    clean = true
    for name in sort!(union(keys(deps₀), keys(deps₁)), by=lowercase)
        uuid₀, uuid₁ = get(deps₀, name, ""), get(deps₁, name, "")
        uuid₀ == uuid₁ && continue
        isempty(uuid₀) || emit_project('-', name, uuid₀)
        isempty(uuid₁) || emit_project('+', name, uuid₁)
        clean = false
    end
    clean && print_with_color(:light_black, " [no changes]\n")
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
)::ManifestDiff
    uuids = sort!(union(keys(infos₀), keys(infos₁)), by=uuid->uuid.value)
    diff = eltype(ManifestDiff)[
        (get(infos₀, uuid, nothing), get(infos₁, uuid, nothing))
        for uuid in uuids]
    filter!(diff) do infos
        info₀, info₁ = infos
        info₀ == nothing || info₁ == nothing || info₀.hash != info₁.hash
    end
    sort!(diff, by=pair->lowercase(pair[pair[2]!=nothing ? 2 : 1].name))
end

v_str(x::ManifestEntry) =
    x.version == nothing ? "[$(string(x.hash)[1:16])]" : "v$(x.version)"

function emit_manifest_diff(emit::Function, diff::ManifestDiff)
    if isempty(diff)
        print_with_color(:light_black, " [no changes]\n")
        return
    end
    for (info₀, info₁) in diff
        uuid = info₁ != nothing ? info₁.uuid : info₀.uuid
        name = info₁ != nothing ? info₁.name : info₀.name
        u = string(uuid)[1:8]
        if info₀ != nothing && info₁ != nothing
            v₀, v₁ = v_str(info₀), v_str(info₁)
            x = info₀.version == nothing || info₁.version == nothing ? '~' :
                info₀.version < info₁.version ? '↑' : '↓'
            emit(uuid, name, x, "$v₀ ⇒ $v₁")
        elseif info₀ != nothing
            emit(uuid, name, '-', v_str(info₀))
        elseif info₁ != nothing
            emit(uuid, name, '+', v_str(info₁))
        else
            error("this should not happen")
        end
    end
end

function print_manifest_diff(
    infos₀::Dict{UUID,ManifestEntry},
    infos₁::Dict{UUID,ManifestEntry},
)::Void
    emit_manifest_diff(manifest_diff(infos₀, infos₁)) do uuid, name, x, vers
        color = x == '+' ? :light_green   :
                x == '-' ? :light_red     :
                x == '↑' ? :light_yellow  :
                           :light_magenta
        print_with_color(:light_black, " [$(string(uuid)[1:8])] ")
        print_with_color(color, "$x $name $vers\n")
    end
end
print_manifest_diff(infos₀::Dict, infos₁::Dict) =
    print_manifest_diff(manifest_entries(infos₀), manifest_entries(infos₁))
print_manifest_diff(env₀::EnvCache, env₁::EnvCache) =
    print_manifest_diff(env₀.manifest, env₁.manifest)

function print_package_tree(
    io::IO,
    env::EnvCache,
    deps::Dict = env.project["deps"],
    seen::Dict{UUID,Bool} = Dict{UUID,Bool}(),
    depth::Int = 0,
)::Void
    for (name::String, uuid::UUID) in sort!(collect(deps), by=lowercase∘first)
        print(io, "  "^depth, name, " [", string(uuid)[1:8], "]")
        if haskey(seen, uuid)
            seen[uuid] && print(io, " ⋯")
            println(io)
        else
            println(io)
            seen[uuid] = false # no deps
            for (name′, infos) in env.manifest, info in infos
                uuid == UUID(info["uuid"]) || continue
                haskey(info, "deps") && !isempty(info["deps"]) || continue
                print_package_tree(io, env, info["deps"], seen, depth+1)
                seen[uuid] = true # has deps
                break # stop searching manifest
            end
        end
    end
end
print_package_tree(env::EnvCache = EnvCache()) =
    print_package_tree(STDOUT, env)

end
