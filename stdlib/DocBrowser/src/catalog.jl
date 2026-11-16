# This file is a part of Julia. License is MIT: https://julialang.org/license

# What is loaded: packages, the names they define, and which of those are documented.

"""
    PkgEntry

A loaded top-level module, with its place in the environment.

`role` is one of `:project` (the active project itself), `:direct` (a dependency of the
active project, or bound in `Main`), `:julia` (`Core`, `Base` and `Main`), `:indirect`
(loaded for another package, or by Julia itself) or `:extension`. `version` is `nothing` for extensions and for standard
libraries shipped with Julia, whose versions do not track Julia's. `via` is the chain of
packages that pulled an indirect package in, starting from a direct dependency.
"""
struct PkgEntry
    mod::Module
    id::PkgId
    version::Union{Nothing,VersionNumber}
    role::Symbol
    via::Vector{String}
    parent::Union{Nothing,String}
end

in_stdlib_dir(dir) = dir !== nothing && startswith(normpath(dir), normpath(Sys.STDLIB))
is_bundled(m::Module) = in_stdlib_dir(pkgdir(m))

"""
    NameEntry

One name listed in a module: `kind` is `:module`, `:type`, `:function`, `:macro`,
`:constant` or `:global`, and `vis` is `:exported`, `:public` or `:internal`.
`owner` is the module the binding comes from, which differs from `mod` for re-exports.
"""
struct NameEntry
    mod::Module
    name::Symbol
    kind::Symbol
    vis::Symbol
    documented::Bool
    owner::Module
    deprecated::Bool
end
NameEntry(mod, name, kind, vis, documented, owner) = NameEntry(mod, name, kind, vis, documented, owner, false)

const KINDS = (:module, :type, :function, :macro, :constant, :global)
const KIND_TITLES = Dict(:module => "Modules", :type => "Types", :function => "Functions",
                         :macro => "Macros", :constant => "Constants", :global => "Globals")
const KIND_GLYPHS = Dict(:module => "M", :type => "T", :function => "ƒ", :macro => "@",
                         :constant => "c", :global => "g")

kind_title(k::Symbol) = KIND_TITLES[k]
kind_glyph(k::Symbol) = KIND_GLYPHS[k]
kind_face(k::Symbol) = Symbol(:REPL_DocBrowser_kind_, k)

"""
    ManualPage

A page of the manual: its file, its title, and the section it belongs to, which is `:manual`,
`:basedocs`, `:stdlibdocs`, `:pkgdocs` or `:devdocs`. `group` is the part of the developer documentation
that it is in.
"""
struct ManualPage
    path::String
    title::String
    role::Symbol
    group::String
end

"""
    Heading

A heading of a manual page: its level, its text, and the `@id` that Documenter links to it
by, if it has one.
"""
struct Heading
    level::Int
    text::String
    id::String
    line::Int # in the file
end

"""
    ManualHit

A page of the manual, or one of its headings by its text, that matches a search.
`heading` is empty for the page itself.
"""
struct ManualHit
    path::String
    heading::String
    score::Int
end

mutable struct Catalog
    pkgs::Vector{PkgEntry}
    bymod::IdDict{Module,PkgEntry}
    documented::Set{Binding}
    entries::IdDict{Module,Vector{NameEntry}}
    index::Union{Nothing,Vector{NameEntry}}
    unloaded::Vector{PkgId} # direct dependencies of the active project that are not loaded
    texts::Union{Nothing,Vector{Pair{Binding,String}}} # lowercased docstring text, built on first use
    manual::Vector{ManualPage}
    anchors::Union{Nothing,Dict{String,String}} # the page of each heading, built on first use
    headings::Dict{String,Vector{Heading}} # the headings each page shows, by page file
    index_headings::Dict{String,Vector{Heading}} # the headings found by scanning each page
    sections::Union{Nothing,Vector{Tuple{String,String,String}}} # lowercased text of each section, built on first use
end

function parse_uuids(deps)
    uuids = UUID[]
    deps isa AbstractDict || return uuids
    for v in values(deps)
        v isa String || continue
        u = tryparse(UUID, v)
        u === nothing || push!(uuids, u)
    end
    return uuids
end

function read_project(path)
    path isa String && isfile(path) || return nothing
    d = try
        Base.parsed_toml(path)
    catch
        return nothing
    end
    return d isa AbstractDict ? d : nothing
end

function project_info()
    d = read_project(Base.active_project())
    d === nothing && return nothing, Set{UUID}()
    self = get(d, "uuid", nothing)
    self = self isa String ? tryparse(UUID, self) : nothing
    return self, Set{UUID}(parse_uuids(get(d, "deps", nothing)))
end

"""
    project_label() -> Union{String, Nothing}

The active project's name as the `pkg>` prompt shows it, such as `@v1.12` or `MyPackage`.
"""
function project_label()
    proj = Base.active_project()
    proj === nothing && return nothing
    name = try
        REPL.projname(REPL.find_root_base_project(proj))
    catch
        nothing
    end
    name === nothing && return nothing
    return textwidth(name) > 30 ? first(name, 27) * "..." : name
end

function unloaded_deps()
    d = read_project(Base.active_project())
    deps = d === nothing ? nothing : get(d, "deps", nothing)
    deps isa AbstractDict || return PkgId[]
    ids = PkgId[]
    for (name, v) in deps
        uuid = v isa String ? tryparse(UUID, v) : nothing
        uuid === nothing && continue
        id = PkgId(uuid, String(name))
        Base.root_module_exists(id) || push!(ids, id)
    end
    return sort_by!(ids, id -> lowercase(id.name))
end

function package_deps(m::Module)
    # An extension shares its parent's directory, but depends on its parent and triggers.
    ext = @lock Base.require_lock get(Base.EXT_PRIMED, PkgId(m), nothing)
    ext === nothing || return UUID[id.uuid for id in ext if id.uuid !== nothing]
    dir = pkgdir(m)
    dir === nothing && return UUID[]
    d = read_project(Base.env_project_file(dir))
    d === nothing && return UUID[]
    return parse_uuids(get(d, "deps", nothing))
end

function extension_parent(id::PkgId)
    parents = @lock Base.require_lock get(Base.EXT_PRIMED, id, nothing)
    return parents === nothing || isempty(parents) ? nothing : first(parents).name
end

bound_in_main(m::Module) =
    isdefined(Main, nameof(m)) && getglobal(Main, nameof(m)) === m

function role_of(m::Module, id::PkgId, self, direct)
    (m === Core || m === Base || m === Main) && return :julia
    uuid = id.uuid
    uuid !== nothing && uuid == self && return :project
    uuid !== nothing && uuid in direct && return :direct
    extension_parent(id) === nothing || return :extension
    bound_in_main(m) && return :direct
    return :indirect
end

isjll(name::AbstractString) = endswith(name, "_jll")

"""
    sort_by!(v, key) -> v

Sort `v` stably by `key`. All the browser's sorts go through one sort of `Vector{Any}`, so
they share one compiled copy of the sorting code. The lists are small, so the dynamic
comparisons cost little.
"""
function sort_by!(v::Vector, @nospecialize(key))
    keys = Vector{Any}(undef, length(v))
    for i in eachindex(v)
        keys[i] = key(v[i])
    end
    perm = sortperm(keys)
    old = copy(v)
    for i in eachindex(perm)
        v[i] = old[perm[i]]
    end
    return v
end

function Catalog()
    self, direct = project_info()
    mods = Module[Core, Base, Main]
    for m in Base.loaded_modules_array()
        m in mods || push!(mods, m)
    end
    roles = IdDict{Module,Symbol}(m => role_of(m, PkgId(m), self, direct) for m in mods)
    # Find how each indirect package was pulled in, by a breadth-first walk of the
    # dependency graph that starts from the direct dependencies.
    byuuid = Dict{UUID,Module}()
    for m in mods
        uuid = PkgId(m).uuid
        uuid === nothing || (byuuid[uuid] = m)
    end
    via = IdDict{Module,Vector{String}}()
    queue = Module[m for m in mods if roles[m] in (:project, :direct)]
    for m in queue
        via[m] = String[]
    end
    while !isempty(queue)
        m = popfirst!(queue)
        for uuid in package_deps(m)
            dep = get(byuuid, uuid, nothing)
            (dep === nothing || haskey(via, dep)) && continue
            via[dep] = [via[m]; String(nameof(m))]
            push!(queue, dep)
        end
    end
    pkgs = PkgEntry[]
    for m in mods
        id = PkgId(m)
        version = roles[m] === :julia ? (m === Main ? nothing : VERSION) :
            roles[m] === :extension || is_bundled(m) ? nothing : pkgversion(m)
        push!(pkgs, PkgEntry(m, id, version, roles[m], get(via, m, String[]), extension_parent(id)))
    end
    rank(p) = (p.role === :julia ? findfirst(==(p.mod), (Base, Core, Main)) : 0,
               isjll(p.id.name), lowercase(p.id.name))
    sort_by!(pkgs, rank)
    bymod = IdDict{Module,PkgEntry}(p.mod => p for p in pkgs)
    return Catalog(pkgs, bymod, documented_bindings(), IdDict{Module,Vector{NameEntry}}(), nothing,
                   unloaded_deps(), nothing, manual_pages(), nothing, Dict{String,Vector{Heading}}(), Dict{String,Vector{Heading}}(), nothing)
end

function documented_bindings()
    set = Set{Binding}()
    for m in Base.Docs.modules
        d = Base.Docs.meta(m; autoinit=false)
        d === nothing && continue
        for b in keys(d)
            b isa Binding && push!(set, b)
        end
    end
    return set
end

"""
    package_of(cat, m) -> Union{PkgEntry, Nothing}

The loaded package that `m` belongs to, walking up from submodules.
"""
function package_of(cat::Catalog, m::Module)
    while true
        p = get(cat.bymod, m, nothing)
        p === nothing || return p
        parent = parentmodule(m)
        parent === m && return nothing
        m = parent
    end
end

is_submodule(m::Module, val) = val isa Module && val !== m && parentmodule(val) === m

function classify(cat::Catalog, m::Module, s::Symbol)
    s === nameof(m) && return nothing
    str = String(s)
    occursin('#', str) && return nothing
    deprecated = Base.isdeprecated(m, s)
    # Reading a deprecated binding would warn, or throw with `--depwarn=error`.
    deprecated && Base.JLOptions().depwarn != 0 && return nothing
    isdefined(m, s) || return nothing
    vis = Base.isexported(m, s) ? :exported : Base.ispublic(m, s) ? :public : :internal
    vis === :internal && s in (:eval, :include) && return nothing
    owner = try
        which(m, s)
    catch
        m
    end
    # Internal names that were only imported belong to another module's listing.
    vis === :internal && owner !== m && return nothing
    val = getglobal(m, s)
    kind = if val isa Module
        vis === :internal && !is_submodule(m, val) && return nothing
        :module
    elseif startswith(str, '@')
        :macro
    elseif val isa Type
        :type
    elseif val isa Function
        :function
    elseif isconst(m, s)
        :constant
    else
        :global
    end
    return NameEntry(m, s, kind, vis, Binding(m, s) in cat.documented, owner, deprecated)
end

sortkey(s::Symbol) = (str = String(s); (!Base.isidentifier(lstrip(str, '@')), lowercase(str), str))

"""
    module_entries(cat, m) -> Vector{NameEntry}

Every listable name in `m`, grouped by kind and sorted by name. Cached per module.
"""
function module_entries(cat::Catalog, m::Module)
    get!(cat.entries, m) do
        syms = unique!(Symbol[names(m; all=true, imported=true); names(m)])
        out = NameEntry[]
        for s in syms
            e = try
                classify(cat, m, s)
            catch
                nothing
            end
            e === nothing || push!(out, e)
        end
        sort_by!(out, e -> (findfirst(==(e.kind), KINDS), sortkey(e.name)))
    end
end

function count_visibility(entries::Vector{NameEntry})
    counts = Dict(:exported => 0, :public => 0, :internal => 0)
    for e in entries
        counts[e.vis] += 1
    end
    return counts
end

entry_value(e::NameEntry) = getglobal(e.mod, e.name)

# An entry for a whole module, so that search finds packages and submodules too.
module_entry(cat::Catalog, m::Module) =
    NameEntry(m, nameof(m), :module, :exported, module_binding(m) in cat.documented, m)

is_module_entry(e::NameEntry) = e.kind === :module && e.name === nameof(e.mod) && entry_value(e) === e.mod

module_binding(m::Module) = (p = parentmodule(m); Binding(p === m ? m : p, nameof(m)))

"""
    search_index(cat) -> Vector{NameEntry}

Every name in every loaded module and its submodules, built on first use.
"""
function search_index(cat::Catalog)
    idx = cat.index
    idx === nothing || return idx
    idx = NameEntry[]
    seen = IdSet{Module}()
    for p in cat.pkgs
        index_module!(idx, seen, cat, p.mod)
    end
    cat.index = idx
    return idx
end

function index_module!(idx::Vector{NameEntry}, seen::IdSet{Module}, cat::Catalog, m::Module)
    m in seen && return
    push!(seen, m)
    push!(idx, module_entry(cat, m))
    for e in module_entries(cat, m)
        if e.kind === :module
            val = entry_value(e)
            is_submodule(e.mod, val) && index_module!(idx, seen, cat, val)
        else
            push!(idx, e)
        end
    end
end

function name_score(q::AbstractString, name::AbstractString)
    q == name && return 1000
    startswith(name, q) && return 700 - min(length(name) - length(q), 200)
    r = findfirst(q, name)
    r === nothing || return 400 - min(first(r), 200)
    # A subsequence match, scored down by how spread out it is.
    i = firstindex(name)
    first_hit = 0
    for c in q
        j = findnext(==(c), name, i)
        j === nothing && return 0
        first_hit == 0 && (first_hit = j)
        i = nextind(name, j)
    end
    return max(1, 150 - (i - first_hit - ncodeunits(q)))
end

const ROLE_BOOST = (project = 60, direct = 60, julia = 50, stdlib = 20, indirect = 0, extension = 0)
const VIS_BOOST = (exported = 40, public = 30, internal = 0)

"""
    search(cat, query; internal=false) -> Vector{NameEntry}

Fuzzy search over all names. A dotted query such as `Plots.pl` restricts the search to
modules whose full name contains the part before the last dot.
"""
function search(cat::Catalog, query::AbstractString; internal::Bool=false, limit::Int=500)
    query = strip(query)
    isempty(query) && return NameEntry[]
    modq, exact = "", String(query)
    dot = findlast('.', query)
    if dot !== nothing && dot > firstindex(query) && dot < lastindex(query)
        modq, exact = lowercase(query[1:prevind(query, dot)]), String(query[nextind(query, dot):end])
    end
    nameq = lowercase(exact)
    best = Dict{Tuple{Module,Symbol},Tuple{Int,NameEntry}}()
    for e in search_index(cat)
        !internal && (e.vis === :internal || e.deprecated) && continue
        name = String(e.name)
        sc = name_score(nameq, lowercase(name))
        sc > 0 || continue
        sc += 100 * (name == exact)
        if !isempty(modq)
            occursin(modq, lowercase(join(fullname(e.mod), '.'))) || continue
        end
        p = package_of(cat, e.mod)
        sc += p === nothing ? 0 : getfield(ROLE_BOOST, p.role)
        sc += getfield(VIS_BOOST, e.vis) + 15 * e.documented
        # The same binding is often listed in several modules: prefer the shallowest.
        sc -= 5 * length(fullname(e.mod))
        key = (e.owner, e.name)
        old = get(best, key, nothing)
        (old === nothing || old[1] < sc) && (best[key] = (sc, e))
    end
    hits = collect(values(best))
    sort_by!(hits, h -> (-h[1], length(String(h[2].name)), String(h[2].name)))
    return NameEntry[h[2] for h in Iterators.take(hits, limit)]
end

function doc_results(b::Binding)
    results = DocStr[]
    for m in Base.Docs.modules
        d = Base.Docs.meta(m; autoinit=false)
        d === nothing && continue
        multi = get(d, b, nothing)
        multi isa MultiDoc || continue
        for sig in multi.order
            push!(results, multi.docs[sig])
        end
    end
    if isempty(results)
        alias = Base.Docs.aliasof(b)
        alias == b || return doc_results(alias)
    end
    return results
end

"""
    qualified_name(e) -> String

How to refer to `e` from `Main`: the bare name when `Main` already sees the same
binding, the qualified name otherwise.
"""
function qualified_name(e::NameEntry)
    is_module_entry(e) && bound_in_main(e.mod) && return String(e.name)
    if !is_module_entry(e) && isdefined(Main, e.name) && !Base.isdeprecated(Main, e.name) &&
            getglobal(Main, e.name) === entry_value(e)
        return String(e.name)
    end
    return string(is_module_entry(e) ? module_binding(e.mod) : Binding(e.mod, e.name))
end

## Searching docstring text ##

function doc_texts(cat::Catalog)
    texts = cat.texts
    texts === nothing || return texts
    texts = Pair{Binding,String}[]
    for m in Base.Docs.modules
        d = Base.Docs.meta(m; autoinit=false)
        d === nothing && continue
        for (b, multi) in d
            b isa Binding && multi isa MultiDoc || continue
            io = IOBuffer()
            for sig in multi.order, part in multi.docs[sig].text
                part isa AbstractString && print(io, part, ' ')
            end
            push!(texts, b => lowercase(String(take!(io))))
        end
    end
    cat.texts = texts
    return texts
end

"""
    search_text(cat, query; internal=false) -> Vector{NameEntry}

Names whose docstrings contain every word of `query`, best matches first.
"""
function search_text(cat::Catalog, query::AbstractString; internal::Bool=false, limit::Int=300)
    words = split(lowercase(query))
    isempty(words) && return NameEntry[]
    hits = Tuple{Int,NameEntry}[]
    for (b, text) in doc_texts(cat)
        all(w -> occursin(w, text), words) || continue
        e = try
            classify(cat, b.mod, b.var)
        catch
            nothing
        end
        e === nothing && continue
        !internal && (e.vis === :internal || e.deprecated) && continue
        # The whole phrase beats scattered words, and a word in the name beats a mention.
        sc = min(sum(w -> count(w, text), words), 50)
        length(words) > 1 && occursin(join(words, ' '), text) && (sc += 500)
        name = lowercase(String(b.var))
        sc += 100 * count(w -> occursin(w, name), words)
        p = package_of(cat, e.mod)
        sc += p === nothing ? 0 : getfield(ROLE_BOOST, p.role)
        push!(hits, (sc, e))
    end
    sort_by!(hits, h -> -h[1])
    return NameEntry[h[2] for h in Iterators.take(hits, limit)]
end
