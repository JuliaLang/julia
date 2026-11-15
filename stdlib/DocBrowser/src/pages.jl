# This file is a part of Julia. License is MIT: https://julialang.org/license

# Pages: rendered documentation, as lines of styled text plus the links on them.

struct ModuleTarget
    mod::Module
end
struct NameTarget
    mod::Module
    name::Symbol
end
struct URLTarget
    url::String
end
struct SourceTarget
    file::String
    line::Int
end
struct SectionTarget
    role::Symbol
end
struct LoadTarget
    id::PkgId
end
struct AnchorTarget
    heading::Int # counts the page's headings from the top
end
struct ToggleTarget
    flag::Symbol # a section of the page to show or hide: :extended or :methodswith
end
struct CodeTarget
    code::String
end

const Target = Union{ModuleTarget,NameTarget,URLTarget,SourceTarget,SectionTarget,LoadTarget,
                     AnchorTarget,ToggleTarget,CodeTarget}

"""
    Spot

A clickable stretch of a page: display columns `cols` of line `line`, leading to `target`.
"""
struct Spot
    line::Int
    cols::UnitRange{Int}
    target::Target
end

struct Page
    lines::Vector{AnnotatedString{String}}
    spots::Vector{Spot}
    source::Union{Nothing,SourceTarget}
    headings::Vector{Int}
end

struct PageBuilder
    lines::Vector{AnnotatedString{String}}
    spots::Vector{Spot}
    width::Int
end
PageBuilder(width::Int) = PageBuilder(AnnotatedString{String}[], Spot[], width)

Page(pb::PageBuilder, source=nothing) =
    Page(pb.lines, sort!(pb.spots; by = s -> (s.line, first(s.cols))), source, heading_lines(pb.lines))

is_heading_face(v) = v isa Symbol && startswith(String(v), "markdown_h") && v !== :markdown_hrule

# The lines that hold Markdown headings, leaving out the lines that underline them.
function heading_lines(lines)
    out = Int[]
    for (i, l) in enumerate(lines)
        any(a -> a.label === :face && is_heading_face(a.value), annotations(l)) || continue
        text = strip(String(l))
        # Underlines are drawn with a different character at each level, but never letters.
        any(c -> isletter(c) || isdigit(c), text) && push!(out, i)
    end
    return out
end

# A tab has no width of its own, so it would put clicks and selections in the wrong column.
# A space is one byte too, so the annotations still line up.
untab(s::AnnotatedString{String}) =
    '\t' in String(s) ? AnnotatedString(replace(String(s), '\t' => ' '), annotations(s)) : s

function styled(text, face)
    s = untab(AnnotatedString(text isa AnnotatedString ? text : String(text)))
    isempty(s) || face === nothing || face!(s, face)
    return s
end

# A segment is plain text, or `(text, face)`, or `(text, face, target)` for a link.
segment(x::AbstractString) = (x, nothing, nothing)
segment(x::Tuple) = (x[1], x[2], length(x) > 2 ? x[3] : nothing)

function addline!(pb::PageBuilder, segs...)
    parts = AnnotatedString{String}[]
    col = 1
    row = length(pb.lines) + 1
    for seg in segs
        text, face, target = segment(seg)
        s = styled(text, target === nothing ? face : something(face, :REPL_DocBrowser_link))
        w = textwidth(String(s))
        w > 0 && target !== nothing && push!(pb.spots, Spot(row, col:col+w-1, target))
        col += w
        push!(parts, s)
    end
    push!(pb.lines, isempty(parts) ? AnnotatedString("") : annotatedstring(parts...))
    return pb
end

"""
    addflow!(pb, indent, segs)

Add `segs` as a paragraph, breaking lines between segments to fit the page width.
A vector of segments stays together on one line.
"""
function addflow!(pb::PageBuilder, indent::String, segs::Vector)
    line = Any[indent]
    used = textwidth(indent)
    for seg in segs
        group = seg isa Vector ? seg : Any[seg]
        w = sum(s -> textwidth(String(segment(s)[1])), group; init=0)
        if used + w > pb.width && length(line) > 1
            addline!(pb, line...)
            line = Any[indent]
            used = textwidth(indent)
        end
        append!(line, group)
        used += w
    end
    length(line) > 1 && addline!(pb, line...)
    return pb
end

rule!(pb::PageBuilder) = addline!(pb, ("  " * "─"^max(0, pb.width - 4), :REPL_DocBrowser_border))

function heading!(pb::PageBuilder, title::String)
    addline!(pb)
    addline!(pb, "  ", (title, :REPL_DocBrowser_section))
end

## Markdown ##

function render_markdown(md, width::Int)
    # Wrapping the buffer in an `IOContext` would lose the annotations.
    buf = AnnotatedIOBuffer()
    if md isa Markdown.MD
        Markdown.term(buf, md, width)
    else
        show(buf, MIME"text/plain"(), md)
    end
    str = read(seekstart(buf), AnnotatedString)
    return AnnotatedString{String}[AnnotatedString(l) for l in split(str, '\n')]
end

function walk_md(f, x)
    f(x)
    if x isa AbstractVector
        foreach(y -> walk_md(f, y), x)
    elseif x isa Markdown.MD
        walk_md(f, x.content)
    elseif parentmodule(typeof(x)) === Markdown
        for field in fieldnames(typeof(x))
            isdefined(x, field) || continue
            v = getfield(x, field)
            (v isa AbstractVector || parentmodule(typeof(v)) === Markdown) && walk_md(f, v)
        end
    end
end

md_text(x::AbstractString) = x
md_text(x::Markdown.Code) = x.code
md_text(x::AbstractVector) = join(map(md_text, x))
md_text(x) = hasfield(typeof(x), :text) ? md_text(getfield(x, :text)) : ""

"""
    ref_targets(md) -> Dict{String,String}

Map the text of each `[text](@ref target)` link in `md` to the name it refers to.
"""
function ref_targets(md)
    refs = Dict{String,String}()
    walk_md(md) do x
        x isa Markdown.Link || return
        startswith(x.url, "@ref") || return
        text = strip(replace(md_text(x.text), r"\s+" => ' '))
        target = strip(chopprefix(x.url, "@ref"), [' ', '`'])
        refs[text] = isempty(target) ? text : target
    end
    return refs
end

valid_name_part(p::AbstractString) = Base.isidentifier(p) || Base.isoperator(Symbol(p)) ||
    (startswith(p, '@') && Base.isidentifier(p[2:end]))

"""
    resolve_ref(text, ctx, cat) -> Union{Target, Nothing}

Find what a code span such as `sort!`, `Base.Iterators` or `sum(f, A)` refers to, looking
in `ctx`, then `Main`, then `Base`. Only documented or public names count, so that code
spans which merely mention a variable do not become links.
"""
function resolve_ref(text::AbstractString, ctx::Module, cat::Catalog)
    s = strip(text)
    i = findfirst(in("({[ "), s)
    i === nothing || (s = s[1:prevind(s, i)])
    isempty(s) && return nothing
    parts = split(s, '.')
    all(p -> !isempty(p) && Base.isidentifier(p), parts[1:end-1]) || return nothing
    valid_name_part(parts[end]) || return nothing
    for root in unique!(Module[ctx, Main, Base])
        t = resolve_path(root, parts, cat)
        t === nothing || return t
    end
    return nothing
end

visible_binding(m::Module, s::Symbol) = isdefined(m, s) && !Base.isdeprecated(m, s)

function resolve_path(root::Module, parts, cat::Catalog)
    m = root
    for p in parts[1:end-1]
        sym = Symbol(p)
        visible_binding(m, sym) || return nothing
        v = getglobal(m, sym)
        v isa Module || return nothing
        m = v
    end
    sym = Symbol(parts[end])
    visible_binding(m, sym) || return nothing
    v = getglobal(m, sym)
    v isa Module && return ModuleTarget(v)
    b = Binding(m, sym)
    public = Base.ispublic(m, sym)
    (public || b in cat.documented) || return nothing
    return NameTarget(public ? m : b.mod, sym)
end

function find_spots!(spots::Vector{Spot}, row::Int, line::AnnotatedString, ctx::Module,
                     refs::Dict{String,String}, cat::Catalog)
    str = String(line)
    anns = sort(annotations(line); by = a -> a.label !== :link)
    taken = UnitRange{Int}[]
    for ann in anns
        r = first(ann.region):thisind(str, last(ann.region))
        isempty(r) && continue
        target = if ann.label === :link && ann.value isa AbstractString
            URLTarget(String(ann.value))
        elseif ann.label === :face && ann.value in (:markdown_inlinecode, :markdown_link)
            text = strip(str[r])
            resolve_ref(get(refs, text, text), ctx, cat)
        end
        target === nothing && continue
        col = textwidth(SubString(str, 1, prevind(str, first(r)))) + 1
        cols = col:col+textwidth(str[r])-1
        any(c -> !isempty(intersect(c, cols)), taken) && continue
        push!(taken, cols)
        push!(spots, Spot(row, cols, target))
    end
    # Mentions of packages, such as `Revise.jl`.
    for m in eachmatch(r"(?<![\w.])([A-Za-z_][A-Za-z0-9_]*)\.jl\b", str)
        r = m.offset:(m.offset + ncodeunits(m.match) - 1)
        col = textwidth(SubString(str, 1, prevind(str, first(r)))) + 1
        cols = col:col+textwidth(m.match)-1
        any(c -> !isempty(intersect(c, cols)), taken) && continue
        push!(taken, cols)
        push!(spots, Spot(row, cols, mention_target(cat, m[1])))
    end
end

const DEFAULT_SEARCH_URL = "https://duckduckgo.com/?q=%s"

# Percent-encode everything but the characters that URLs leave unreserved.
escape_query(q::AbstractString) =
    join(c in 'A':'Z' || c in 'a':'z' || c in '0':'9' || c in "-._~" ? string(c) :
         join(("%" * uppercase(string(b; base=16, pad=2)) for b in codeunits(string(c)))) for c in q)

"""
    web_search(name) -> URLTarget

A web search for package `name`. The search engine comes from the
`JULIA_DOCBROWSER_SEARCH_URL` environment variable, a URL with `%s` where the query goes.
"""
function web_search(name::AbstractString)
    template = get(ENV, "JULIA_DOCBROWSER_SEARCH_URL", "")
    occursin("%s", template) || (template = DEFAULT_SEARCH_URL)
    return URLTarget(replace(template, "%s" => escape_query(name * ".jl")))
end

# Where a mention of package `name` leads: its docs when loaded, its load page when it is a
# project dependency, and otherwise a web search.
function mention_target(cat::Catalog, name::AbstractString)
    t = package_target(cat, name)
    t === nothing || return t
    for id in cat.unloaded
        id.name == name && return LoadTarget(id)
    end
    return web_search(name)
end

"""
    add_markdown!(pb, md, ctx, cat; contents=false)

Render `md`, making links of the names it mentions, which are looked up from module `ctx`.
Examples get a link that pastes them at the prompt. With `contents`, a page with several
headings starts with links to them.
"""
function add_markdown!(pb::PageBuilder, md, ctx::Module, cat::Catalog; contents::Bool=false)
    refs = ref_targets(md)
    lines = render_markdown(md, pb.width)
    if contents
        titles = [strip(String(lines[i])) for i in heading_lines(lines)]
        if length(titles) >= 3
            segs = Any[("Contents ", :REPL_DocBrowser_dim)]
            for (k, t) in enumerate(titles)
                k > 1 && push!(segs, (" · ", :REPL_DocBrowser_dim))
                push!(segs, (t, nothing, AnchorTarget(k)))
            end
            addflow!(pb, "  ", segs)
            addline!(pb)
        end
    end
    block = String[]
    for line in lines
        code = is_code_line(line)
        if !code && !isempty(block)
            add_paste_link!(pb, block)
            empty!(block)
        end
        push!(pb.lines, untab(line))
        find_spots!(pb.spots, length(pb.lines), pb.lines[end], ctx, refs, cat)
        code && push!(block, String(line))
    end
    isempty(block) || add_paste_link!(pb, block)
end

is_code_line(line::AnnotatedString) =
    any(a -> a.label === :face && a.value in (:markdown_code, :markdown_julia_prompt), annotations(line))

"""
    paste_text(lines) -> Union{String, Nothing}

What to paste for a rendered code block: the input of each `julia>` prompt in an example,
or the whole block if it is long enough to be worth it.
"""
function paste_text(lines::Vector{String})
    text = [String(chopprefix(l, "  ")) for l in lines]
    if any(startswith("julia> "), text)
        inputs = String[]
        continuing = false
        for l in text
            if startswith(l, "julia> ")
                push!(inputs, l[8:end])
                continuing = true
            elseif continuing && startswith(l, ' '^7)
                inputs[end] *= "\n" * l[8:end]
            else
                continuing = false
            end
        end
        return join(inputs, '\n')
    end
    return length(text) >= 3 ? join(rstrip.(text), '\n') : nothing
end

function add_paste_link!(pb::PageBuilder, block::Vector{String})
    code = paste_text(block)
    code === nothing && return
    addline!(pb, "  ", ("⧉ paste at the prompt", nothing, CodeTarget(code)))
end

## Source locations ##

function source_target(d::DocStr)
    path = get(d.data, :path, nothing)
    line = get(d.data, :linenumber, 0)
    path isa AbstractString && !isempty(path) || return nothing
    return SourceTarget(source_path(path), line isa Integer ? Int(line) : 0)
end

function source_path(path::AbstractString)
    path = Base.fixup_stdlib_path(String(path))
    if !isfile(path)
        found = Base.find_source_file(path)
        found === nothing || (path = found)
    end
    return path
end

function method_target(m::Method)
    file, line = Base.updated_methodloc(m)
    return SourceTarget(source_path(string(file)), Int(line))
end

const MAX_LISTED = 100

"""
    add_methods!(pb, title, ms)

List methods `ms` by signature. Each links to its definition.
"""
function add_methods!(pb::PageBuilder, title::String, ms)
    isempty(ms) && return
    heading!(pb, string(title, " (", length(ms), ")"))
    for m in Iterators.take(ms, MAX_LISTED)
        sig = String(first(split(sprint(show, m; context = :compact => true), " @ "; limit=2)))
        t = method_target(m)
        addline!(pb, "    ", (sig, nothing, t), ("  " * basename(source_label(t)), :REPL_DocBrowser_dim))
    end
    length(ms) > MAX_LISTED && addline!(pb, "    ", ("… and $(length(ms) - MAX_LISTED) more", :REPL_DocBrowser_dim))
end

function source_label(t::SourceTarget)
    path = Base.contractuser(t.file)
    return t.line > 0 ? string(path, ':', t.line) : path
end

function add_sources!(pb::PageBuilder, sources::Vector{SourceTarget})
    isempty(sources) && return
    heading!(pb, length(sources) == 1 ? "Source" : "Sources")
    for t in sources
        addline!(pb, "    ", (source_label(t), :REPL_DocBrowser_link, t))
    end
end

## Pages for names ##

const VIS_WORDS = (exported = "exported", public = "public", internal = "internal")

vis_face(vis::Symbol) = Symbol(:REPL_DocBrowser_, vis)

function kind_word(e::NameEntry)
    e.kind === :type || return String(e.kind)
    T = Base.unwrap_unionall(entry_value(e))
    T isa DataType || return "type"
    isabstracttype(T) && return "abstract type"
    isprimitivetype(T) && return "primitive type"
    return ismutabletype(T) ? "mutable struct" : "struct"
end

display_name(m::Module) = join(fullname(m), '.')

function display_name(e::NameEntry)
    is_module_entry(e) && return display_name(e.mod)
    e.mod === Main && return String(e.name)
    name = String(e.name)
    Base.isoperator(e.name) && (name = ":" * name)
    return string(display_name(e.mod), '.', name)
end

function type_target(@nospecialize(T))
    T = Base.unwrap_unionall(T)
    T isa DataType || return nothing
    m, s = parentmodule(T), nameof(T)
    visible_binding(m, s) || return nothing
    return NameTarget(m, s)
end

function add_type_info!(pb::PageBuilder, @nospecialize(T), flags::Set{Symbol})
    D = Base.unwrap_unionall(T)
    # Types such as `Tuple` and `NamedTuple` have no definite number of fields.
    nfields = D isa DataType && !isabstracttype(D) ? something(Base.datatype_fieldcount(D), 0) : 0
    if nfields > 0
        heading!(pb, "Fields")
        for (name, FT) in zip(fieldnames(D), fieldtypes(D))
            addline!(pb, "    ", String(name), ("::", :REPL_DocBrowser_dim), (string(FT), nothing, type_target(FT)))
        end
    end
    add_supertypes!(pb, T)
    if D isa DataType && isabstracttype(D)
        subs = try
            InteractiveUtils.subtypes(T)
        catch
            Any[]
        end
        if !isempty(subs)
            heading!(pb, "Subtypes ($(length(subs)))")
            segs = Any[]
            for S in Iterators.take(subs, MAX_LISTED)
                isempty(segs) || push!(segs, ("  ", nothing))
                push!(segs, (string(S), nothing, type_target(S)))
            end
            addflow!(pb, "    ", segs)
        end
    end
    add_methods!(pb, "Constructors", methods(T))
    if :methodswith in flags
        ms = try
            InteractiveUtils.methodswith(T; supertypes=false)
        catch
            Method[]
        end
        add_methods!(pb, "Methods that take this type", ms)
        isempty(ms) && (heading!(pb, "Methods that take this type"); addline!(pb, "    ", ("none found", :REPL_DocBrowser_dim)))
    else
        addline!(pb)
        addline!(pb, "  ", ("▸ Find the methods that take this type", nothing, ToggleTarget(:methodswith)))
    end
end

function add_supertypes!(pb::PageBuilder, @nospecialize(T))
    T = Base.unwrap_unionall(T)
    T isa DataType && T !== Any || return
    segs = Any[]
    for (i, S) in enumerate(InteractiveUtils.supertypes(T)[2:end])
        i > 1 && push!(segs, (" <: ", :REPL_DocBrowser_dim))
        push!(segs, (string(nameof(S)), nothing, type_target(S)))
    end
    heading!(pb, "Supertypes")
    addflow!(pb, "    ", segs)
end

function entry_page(cat::Catalog, e::NameEntry, width::Int, flags::Set{Symbol}=Set{Symbol}())
    is_module_entry(e) && return module_page(cat, e.mod, width)
    pb = PageBuilder(width)
    addline!(pb)
    addline!(pb, "  ", (display_name(e), :REPL_DocBrowser_title))
    results = doc_results(Binding(e.mod, e.name))
    meta = Any["  ", (kind_word(e), kind_face(e.kind)), (" · ", :REPL_DocBrowser_dim),
               (getfield(VIS_WORDS, e.vis), vis_face(e.vis))]
    if e.owner !== e.mod
        push!(meta, (" · from ", :REPL_DocBrowser_dim), (display_name(e.owner), nothing, ModuleTarget(e.owner)))
    end
    n = length(results)
    push!(meta, (n == 0 ? " · undocumented" : n == 1 ? "" : " · $n docstrings", :REPL_DocBrowser_dim))
    e.deprecated && push!(meta, (" · deprecated", :warning))
    addline!(pb, meta...)
    if e.kind === :function
        nm = length(methods(entry_value(e)))
        addline!(pb, "  ", (string(nm, nm == 1 ? " method" : " methods"), :REPL_DocBrowser_dim))
    end
    rule!(pb)
    b = Binding(e.mod, e.name)
    md = isempty(results) ? REPL.doc(b) : Base.Docs.catdoc(map(REPL.parsedoc, results)...)
    trimmed = false
    if md isa Markdown.MD
        brief, trimmed = REPL._trimdocs(md, true)
        trimmed && !(:extended in flags) && (md = brief)
    end
    add_markdown!(pb, md, e.owner, cat; contents=true)
    if trimmed
        addline!(pb)
        label = :extended in flags ? "▾ Hide the extended help" : "▸ Show the extended help"
        addline!(pb, "  ", (label, nothing, ToggleTarget(:extended)), ("  x", :REPL_DocBrowser_dim))
    end
    val = entry_value(e)
    if e.kind === :type
        add_type_info!(pb, val, flags)
    elseif e.kind in (:function, :macro) && val isa Function
        add_methods!(pb, "Methods", methods(val))
    end
    sources = SourceTarget[t for t in map(source_target, results) if t !== nothing]
    add_sources!(pb, unique!(sources))
    addline!(pb)
    return Page(pb, isempty(sources) ? nothing : first(sources))
end

## Links to the web ##

# Turn a git remote into the web address of the repository, or `nothing` for local paths.
function web_url(url::AbstractString)
    url = strip(url)
    m = match(r"^git@([^:]+):(.+)$", url)
    m === nothing || (url = string("https://", m[1], "/", m[2]))
    url = replace(url, r"^git://" => "https://", r"^ssh://git@" => "https://")
    startswith(url, r"https?://") || return nothing
    # A remote can hold a user name and token, which must not be shown or opened.
    url = replace(url, r"^(https?://)[^/@]*@" => s"\1")
    return String(chopsuffix(chopsuffix(url, "/"), ".git"))
end

function git_remote(dir)
    dir === nothing && return nothing
    config = joinpath(dir, ".git", "config")
    isfile(config) || return nothing
    m = match(r"\[remote \"origin\"\][^\[]*?url\s*=\s*(\S+)"s, read(config, String))
    return m === nothing ? nothing : m[1]
end

function manifest_repo(id::PkgId)
    project = Base.active_project()
    project === nothing && return nothing
    manifest = Base.project_file_manifest_path(project)
    d = read_project(manifest)
    d === nothing && return nothing
    entries = get(get(d, "deps", d), id.name, nothing)
    entries isa AbstractVector || return nothing
    for e in entries
        e isa AbstractDict && get(e, "uuid", nothing) == string(id.uuid) || continue
        url = get(e, "repo-url", nothing)
        return url isa String ? url : nothing
    end
    return nothing
end

# Only registries unpacked into directories can be read here; compressed ones are skipped.
function registry_repo(id::PkgId)
    for depot in DEPOT_PATH
        regs = joinpath(depot, "registries")
        isdir(regs) || continue
        for name in readdir(regs)
            reg = joinpath(regs, name)
            d = read_project(joinpath(reg, "Registry.toml"))
            d === nothing && continue
            entry = get(get(d, "packages", Dict()), string(id.uuid), nothing)
            entry isa AbstractDict || continue
            info = read_project(joinpath(reg, get(entry, "path", ""), "Package.toml"))
            url = info === nothing ? nothing : get(info, "repo", nothing)
            url isa String && return url
        end
    end
    return nothing
end

"""
    repo_url(id, dir) -> Union{String, Nothing}

The web address of package `id`'s repository, if it can be found offline: from the git
remote of a developed package, from the manifest, or from an unpacked registry.
"""
function repo_url(id::PkgId, dir)
    id.uuid === nothing && return nothing
    for url in (git_remote(dir), manifest_repo(id), registry_repo(id))
        url === nothing && continue
        web = web_url(url)
        web === nothing || return web
    end
    return nothing
end

link_label(url::String) = replace(url, r"^https?://(www\.)?" => "")

const LICENCES = (r"MIT License|MIT \"Expat\"|Permission is hereby granted, free of charge" => "MIT",
                  r"Apache License" => "Apache-2.0", r"GNU LESSER GENERAL PUBLIC" => "LGPL",
                  r"GNU AFFERO" => "AGPL", r"GNU GENERAL PUBLIC LICENSE" => "GPL",
                  r"Mozilla Public License" => "MPL-2.0", r"Boost Software License" => "BSL-1.0",
                  r"ISC License" => "ISC", r"Unlicense|unencumbered software" => "Unlicense",
                  r"Redistribution and use in source and binary forms" => "BSD",
                  r"^COPYRIGHT AND PERMISSION NOTICE" => "curl")

"""
    licence(dir) -> Union{String, Nothing}

The names of the licences in directory `dir`, guessed from the start of each licence file.
"""
function licence(dir)
    (dir === nothing || !isdir(dir)) && return nothing
    found = String[]
    unknown = nothing
    for f in readdir(dir)
        path = joinpath(dir, f)
        isfile(path) || continue
        # BinaryBuilder names some licence files after their SPDX identifier, as in GPL-3.0+
        if occursin(r"^[A-Z][A-Za-z]*-\d[\d.]*\+?$", f)
            push!(found, f)
            continue
        end
        occursin(r"^(un)?licen[cs]e|^copying"i, f) || continue
        head = open(io -> join(Iterators.take(eachline(io), 30), ' '), path)
        spdx = match(r"SPDX-License-Identifier:\W*([A-Za-z0-9.+-]+)", head)
        name = spdx === nothing ? nothing : String(spdx[1])
        for (re, n) in LICENCES
            name === nothing || break
            occursin(re, head) && (name = n)
        end
        name === nothing ? (unknown === nothing && (unknown = f)) : push!(found, name)
    end
    isempty(found) || return join(unique!(found), ", ")
    return unknown === nothing ? nothing : "see $unknown"
end

# The licence files of the library a JLL package provides.
function jll_licence_dir(id::PkgId)
    lib = chopsuffix(id.name, "_jll")
    root = Base.root_module_exists(id) ? Base.root_module(id) : nothing
    if root !== nothing && isdefined(root, :artifact_dir)
        dir = joinpath(string(getglobal(root, :artifact_dir)), "share", "licenses", lib)
        isdir(dir) && return dir
    end
    return joinpath(dirname(Sys.BINDIR), "share", "licenses", lib)
end

"""
    julia_docs(path="") -> String

The address of `path` in the manual for Julia version `v`.
"""
function julia_docs(path::AbstractString="", v::VersionNumber=VERSION)
    # `vX.Y` is the latest release of X.Y. `vX.Y-dev` is built from master, and then from the
    # release branch, so it covers every prerelease, including tags whose docs were not deployed.
    ver = "v$(v.major).$(v.minor)" * (isempty(v.prerelease) ? "" : "-dev")
    return "https://docs.julialang.org/en/$ver/$path"
end

const BADGE_PATTERNS = (
    r"\[!\[([^\]]*)\]\(([^)\s]+)[^)]*\)\]\(([^)\s]+)\)",   # [![alt](image)](url)
    r"^\s*\[([^\]]+)\]:\s*<?([^>\s]+)>?\s*$"m,             # [label]: url
    r"<a[^>]+href=\"([^\"]+)\"[^>]*>\s*<img([^>]+)>")       # <a href="url"><img ...>

"""
    docs_url(dir) -> Union{String, Nothing}

The address of a package's full documentation, taken from the docs badge in its README,
preferring the stable version over the development one.
"""
function docs_url(dir)
    (dir === nothing || !isdir(dir)) && return nothing
    readme = findfirst(f -> occursin(r"^readme(\.md)?$"i, f), readdir(dir))
    readme === nothing && return nothing
    text = try
        read(joinpath(dir, readdir(dir)[readme]), String)
    catch
        return nothing
    end
    found = nothing
    for (i, re) in enumerate(BADGE_PATTERNS), m in eachmatch(re, text)
        label, url = i == 1 ? (m[1] * " " * m[2], m[3]) : i == 2 ? (m[1], m[2]) : (m[2], m[1])
        url = String(url)
        occursin(r"^https?://", url) || continue
        # badge images, which may have a query string or no extension
        occursin(r"\.(svg|png)(\?|$)|img\.shields\.io|badgen\.net"i, url) && continue
        # badges for CI, coverage and registry pages
        occursin(r"/actions|codecov|coveralls|travis|appveyor|juliahub\.com/ui"i, url) && continue
        occursin(r"docs?\b|documentation"i, label) || continue
        occursin(r"stable"i, label * url) && return url
        found === nothing && (found = url)
    end
    return found
end

function add_links!(pb::PageBuilder, id::PkgId, dir, role::Symbol)
    segs = Any[]
    label = "licence "
    lic = if role === :julia
        "MIT"
    elseif isjll(id.name)
        # The wrapper's own licence matters less than that of the library it provides.
        label = chopsuffix(id.name, "_jll") * " licence "
        licence(jll_licence_dir(id))
    else
        # Standard libraries in Julia's repository are covered by Julia's licence.
        something(licence(dir), in_stdlib_dir(dir) ? "MIT" : nothing, Some(nothing))
    end
    lic === nothing || push!(segs, Any[(label, :REPL_DocBrowser_dim), (lic, nothing)])
    function link!(label, url, text=link_label(url))
        isempty(segs) || push!(segs, ("   ", nothing))
        push!(segs, Any[(label * " ", :REPL_DocBrowser_dim), (text, nothing, URLTarget(url))])
    end
    if role === :julia
        link!("repo", "https://github.com/JuliaLang/julia")
        link!("docs", julia_docs())
    else
        repo = repo_url(id, dir)
        if repo === nothing && isjll(id.name)
            # Every JLL is generated into the JuliaBinaryWrappers organisation.
            repo = "https://github.com/JuliaBinaryWrappers/$(id.name).jl"
        end
        repo === nothing || link!("repo", repo)
        docs = isjll(id.name) ? nothing : docs_url(dir)
        if docs !== nothing
            link!("docs", docs)
        elseif isjll(id.name)
        elseif role === :stdlib || Base.is_stdlib(id)
            link!("docs", julia_docs("stdlib/$(id.name)/"))
        elseif repo === nothing
            link!("search the web for", web_search(id.name).url, "$(id.name).jl")
        end
    end
    isempty(segs) || addflow!(pb, "  ", segs)
end

## Pages for modules ##

function role_line(cat::Catalog, m::Module)
    p = get(cat.bymod, m, nothing)
    dim = :REPL_DocBrowser_dim
    if p === nothing
        parent = parentmodule(m)
        return Any[("submodule of ", dim), (display_name(parent), nothing, ModuleTarget(parent))]
    end
    segs = Any[]
    if p.role === :project
        push!(segs, ("the active project", :REPL_DocBrowser_exported))
    elseif p.role === :direct
        push!(segs, (bound_in_main(m) && !in_project(p) ? "loaded in Main" : "direct dependency", :REPL_DocBrowser_exported))
    elseif p.role === :julia
        push!(segs, (m === Main ? "your workspace module" : "built into Julia", :REPL_DocBrowser_public))
    elseif p.role === :stdlib
        push!(segs, ("standard library", :REPL_DocBrowser_public))
    elseif p.role === :extension
        push!(segs, ("package extension", dim))
        p.parent === nothing || push!(segs, (" of ", dim), (p.parent, nothing, package_target(cat, p.parent)))
    else
        push!(segs, ("indirect dependency", dim))
    end
    if !isempty(p.via) && p.role in (:indirect, :stdlib)
        push!(segs, (" · loaded by ", dim))
        for (i, name) in enumerate(p.via)
            i > 1 && push!(segs, (" → ", dim))
            push!(segs, (name, nothing, package_target(cat, name)))
        end
    end
    return segs
end

function in_project(p::PkgEntry)
    _, direct = project_info()
    return p.id.uuid !== nothing && p.id.uuid in direct
end

function package_target(cat::Catalog, name::AbstractString)
    for p in cat.pkgs
        p.id.name == name && return ModuleTarget(p.mod)
    end
    return nothing
end

function count_segments(counts)
    segs = Any[]
    for vis in (:exported, :public, :internal)
        isempty(segs) || push!(segs, (" · ", :REPL_DocBrowser_dim))
        push!(segs, (string(counts[vis], ' ', getfield(VIS_WORDS, vis)), vis_face(vis)))
    end
    return segs
end

function module_page(cat::Catalog, m::Module, width::Int)
    pb = PageBuilder(width)
    p = get(cat.bymod, m, nothing)
    addline!(pb)
    version = p === nothing || p.version === nothing ? "" : string("  v", p.version)
    addline!(pb, "  ", (display_name(m), :REPL_DocBrowser_title), (version, :REPL_DocBrowser_dim))
    addflow!(pb, "  ", role_line(cat, m))
    entries = module_entries(cat, m)
    addline!(pb, "  ", count_segments(count_visibility(entries))...)
    # An extension's directory, README and licence are those of the package it extends.
    ext = p !== nothing && p.role === :extension
    dir = p === nothing ? nothing : ext ? pathof(m) : pkgdir(m)
    dir === nothing || addline!(pb, "  ", (Base.contractuser(dir), :REPL_DocBrowser_dim))
    p === nothing || ext || add_links!(pb, p.id, dir, p.role)
    rule!(pb)
    results = doc_results(module_binding(m))
    if !isempty(results)
        md = Base.Docs.catdoc(map(REPL.parsedoc, results)...)
    elseif ext
        md = Markdown.parse("No docstring found for `$(display_name(m))`.")
    else
        readme = REPL.find_readme(m)
        if readme === nothing
            md = p !== nothing && isjll(p.id.name) ? jll_markdown(m, p) :
                Markdown.parse("No docstring or README found for `$(display_name(m))`.")
        else
            addline!(pb, "  ", ("No module docstring; showing $(basename(readme)).", :REPL_DocBrowser_dim))
            addline!(pb)
            md = try
                strip_badges!(Markdown.parse(read(readme, String)))
            catch
                Markdown.parse("Could not read `$readme`.")
            end
        end
    end
    add_markdown!(pb, md, m, cat; contents=true)
    submods = Any[]
    for e in entries
        e.kind === :module && is_submodule(m, entry_value(e)) || continue
        isempty(submods) || push!(submods, ("  ", nothing))
        push!(submods, (String(e.name), nothing, ModuleTarget(entry_value(e))))
    end
    if !isempty(submods)
        heading!(pb, "Submodules")
        addflow!(pb, "    ", submods)
    end
    if p !== nothing && p.role !== :julia
        add_package_links!(pb, cat, "Loaded dependencies", loaded_deps(cat, p))
        add_package_links!(pb, cat, "Used by", dependents(cat, p))
    end
    sources = SourceTarget[t for t in map(source_target, results) if t !== nothing]
    if isempty(sources)
        path = pathof(m)
        path === nothing || !isfile(path) || push!(sources, SourceTarget(path, 1))
    end
    add_sources!(pb, unique!(sources))
    addline!(pb)
    return Page(pb, isempty(sources) ? nothing : first(sources))
end

is_badge(x) = x isa Markdown.Image || x isa Markdown.LineBreak ||
    (x isa AbstractString && isempty(strip(x))) ||
    (x isa Markdown.Link && all(is_badge, x.text))

"""
    strip_badges!(md) -> md

Remove the HTML blocks and the paragraphs made only of images from a README. They are
badges and logos, which show up as clutter in a terminal.
"""
function strip_badges!(md::Markdown.MD)
    filter!(md.content) do x
        x isa Markdown.HTMLBlock && return false
        x isa Markdown.Paragraph && !isempty(x.content) && all(is_badge, x.content) && return false
        return true
    end
    return md
end

"""
    jll_markdown(m, p) -> Markdown.MD

A page for a JLL package, which has no docstring or README of its own: what it wraps,
and the libraries and executables it provides.
"""
function jll_markdown(m::Module, p::PkgEntry)
    name = p.id.name
    v = p.version
    version = v === nothing ? "" : " $(VersionNumber(v.major, v.minor, v.patch))"
    io = IOBuffer()
    println(io, "`$name` is a binary wrapper. It provides prebuilt binaries of ",
            chopsuffix(name, "_jll"), version, ", made with ",
            "[BinaryBuilder](https://github.com/JuliaPackaging/BinaryBuilder.jl), for Julia code to use.\n")
    available = !isdefined(m, :is_available) || try
        Base.invokelatest(getglobal(m, :is_available))::Bool
    catch
        true
    end
    available || println(io, "!!! warning\n    These binaries are not available on this platform.\n")
    libs, exes = String[], String[]
    for n in names(m)
        n === nameof(m) && continue
        val = getglobal(m, n)
        pathname = Symbol(n, "_path")
        path = isdefined(m, pathname) ? getglobal(m, pathname) : nothing
        where = path isa AbstractString && !isempty(path) ? ", at `$(Base.contractuser(path))`" : ""
        if val isa Function
            push!(exes, "  * `$n()`: a command that runs `$n`$where.")
        else
            push!(libs, "  * `$n`: the library$where.")
        end
    end
    isempty(libs) || println(io, "# Libraries\n\nPass these to `ccall` or `@ccall`.\n\n", join(libs, '\n'), '\n')
    isempty(exes) || println(io, "# Executables\n\nCall these to get a `Cmd` to `run`.\n\n", join(exes, '\n'), '\n')
    return Markdown.parse(String(take!(io)))
end

function loaded_deps(cat::Catalog, p::PkgEntry)
    uuids = Set(package_deps(p.mod))
    return PkgEntry[q for q in cat.pkgs if q.id.uuid !== nothing && q.id.uuid in uuids]
end

function dependents(cat::Catalog, p::PkgEntry)
    p.id.uuid === nothing && return PkgEntry[]
    return PkgEntry[q for q in cat.pkgs if q.id.uuid !== nothing && p.id.uuid in package_deps(q.mod)]
end

function add_package_links!(pb::PageBuilder, cat::Catalog, title::String, pkgs::Vector{PkgEntry})
    isempty(pkgs) && return
    heading!(pb, string(title, " (", length(pkgs), ")"))
    segs = Any[]
    for q in pkgs
        isempty(segs) || push!(segs, ("  ", nothing))
        push!(segs, (q.id.name, nothing, ModuleTarget(q.mod)))
    end
    addflow!(pb, "    ", segs)
end

## Pages for sidebar sections ##

const SECTION_TITLES = (project = "Project", direct = "Direct dependencies", julia = "Julia",
                        stdlib = "Standard library", indirect = "Indirect dependencies",
                        extension = "Extensions")

const SECTION_BLURBS = (
    project = "The package of the active project.",
    direct = "Packages that the active project depends on, and packages loaded into `Main` with `using` or `import`.",
    julia = "`Core` and `Base` are always loaded. `Main` is the module the REPL evaluates in, so it holds your own definitions.",
    stdlib = "Standard libraries that are loaded, but that you did not ask for directly. Many are loaded by the REPL itself.",
    indirect = "Packages that were loaded because one of your packages needs them.",
    extension = "Package extensions: code that a package loads once some other package is loaded too.",
)

function section_page(cat::Catalog, role::Symbol, width::Int)
    pb = PageBuilder(width)
    addline!(pb)
    addline!(pb, "  ", (getfield(SECTION_TITLES, role), :REPL_DocBrowser_title))
    rule!(pb)
    add_markdown!(pb, Markdown.parse(getfield(SECTION_BLURBS, role)), Base, cat)
    if role in (:direct, :project)
        proj = Base.active_project()
        proj === nothing || addline!(pb, "  ", ("Active project: " * Base.contractuser(proj), :REPL_DocBrowser_dim))
    end
    pkgs = PkgEntry[p for p in cat.pkgs if p.role === role]
    addline!(pb)
    for p in pkgs
        version = p.version === nothing ? "" : string("  v", p.version)
        via = isempty(p.via) ? "" : string("  via ", join(p.via, " → "))
        addline!(pb, "    ", (p.id.name, nothing, ModuleTarget(p.mod)), (version * via, :REPL_DocBrowser_dim))
    end
    if role === :direct
        for id in cat.unloaded
            addline!(pb, "    ", (id.name, nothing, LoadTarget(id)), ("  not loaded", :REPL_DocBrowser_dim))
        end
    end
    addline!(pb)
    return Page(pb)
end

function unloaded_page(id::PkgId, width::Int)
    pb = PageBuilder(width)
    addline!(pb)
    addline!(pb, "  ", (id.name, :REPL_DocBrowser_title))
    addline!(pb, "  ", ("direct dependency · not loaded", :REPL_DocBrowser_dim))
    add_links!(pb, id, nothing, :direct)
    rule!(pb)
    addflow!(pb, "  ", Any[w * " " for w in split(
        "The active project depends on $(id.name), but it has not been loaded yet, so its " *
        "documentation is not available. Loading it runs import $(id.name) in Main.")])
    addline!(pb)
    addline!(pb, "  ", (" Load $(id.name) ", :REPL_DocBrowser_link_active, LoadTarget(id)),
             ("  or press ⏎", :REPL_DocBrowser_dim))
    addline!(pb)
    return Page(pb)
end
