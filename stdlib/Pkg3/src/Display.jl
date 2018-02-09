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

function status(env::EnvCache, mode::Symbol, use_as_api=false)
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
    if mode == :project || mode == :combined
        # TODO: handle project deps missing from manifest
        m₀ = filter_manifest(in_project(project₀["deps"]), manifest₀)
        m₁ = filter_manifest(in_project(project₁["deps"]), manifest₁)
        @info "Status $(pathrepr(env, env.project_file))"
        diff = manifest_diff(m₀, m₁)
        use_as_api || print_diff(diff)
    end
    if mode == :manifest
        @info "Status $(pathrepr(env, env.manifest_file))"
        diff = manifest_diff(manifest₀, manifest₁)
        use_as_api || print_diff(diff)
    elseif mode == :combined
        p = !in_project(merge(project₀["deps"], project₁["deps"]))
        m₀ = filter_manifest(p, manifest₀)
        m₁ = filter_manifest(p, manifest₁)
        c_diff = filter!(x->x.old != x.new, manifest_diff(m₀, m₁))
        if !isempty(c_diff)
            @info "Status $(pathrepr(env, env.manifest_file))"
            use_as_api || print_diff(c_diff)
            diff =  Base.vcat(c_diff, diff)
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
    hash::SHA1
    ver::Union{VersionNumber,Nothing}
end

vstring(a::VerInfo) =
    a.ver == nothing ? "[$(string(a.hash)[1:16])]" : "v$(a.ver)"

Base.:(==)(a::VerInfo, b::VerInfo) =
    a.hash == b.hash && a.ver == b.ver

≈(a::VerInfo, b::VerInfo) = a.hash == b.hash &&
    (a.ver == nothing || b.ver == nothing || a.ver == b.ver)

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
                else
                    verb = '?'
                    msg = x.old.hash == x.new.hash ?
                        "hashes match but versions don't: $(x.old.ver) ≠ $(x.new.ver)" :
                        "versions match but hashes don't: $(x.old.hash) ≠ $(x.new.hash)"
                    push!(warnings, msg)
                end
                vstr = x.old.ver == x.new.ver ? vstring(x.new) :
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
    hash = haskey(info, "git-tree-sha1") ? SHA1(info["git-tree-sha1"]) : nothing
    ver = haskey(info, "version") ? VersionNumber(info["version"]) : nothing
    name, VerInfo(hash, ver)
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

function filter_manifest!(predicate::Function, manifest::Dict)
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
filter_manifest(predicate::Function, manifest::Dict) =
    filter_manifest!(predicate, deepcopy(manifest))

in_project(deps::Dict) = (name::String, info::Dict) ->
    haskey(deps, name) && haskey(info, "uuid") && deps[name] == info["uuid"]

end # module
