# This file is a part of Julia. License is MIT: https://julialang.org/license

# The browser: state, layout, drawing and event handling.

struct SideRow
    role::Symbol
    pkg::Union{Nothing,PkgEntry}
    unloaded::Union{Nothing,PkgId} # a dependency that can be loaded from the browser
    page::Union{Nothing,ManualPage}
end
SideRow(role::Symbol, pkg) = SideRow(role, pkg, nothing, nothing)
SideRow(role::Symbol, pkg, unloaded) = SideRow(role, pkg, unloaded, nothing)

is_header(r::SideRow) = r.pkg === nothing && r.unloaded === nothing && r.page === nothing

struct NameRow
    kind::Symbol # :about, :header, :entry, :heading (of a manual page) or :hit (of a search)
    entry::Union{Nothing,NameEntry}
    group::Symbol
    count::Int
    hit::Union{Nothing,ManualHit}
end
NameRow(kind::Symbol, entry, group::Symbol, count::Int) = NameRow(kind, entry, group, count, nothing)

mutable struct ListState
    cursor::Int
    scroll::Int
end

struct Hit
    row::Int
    cols::UnitRange{Int}
    action::Any
end

struct Snapshot
    mod::Module
    focus::Symbol
    source::Symbol
    # The selected rows, by what they show rather than where, as unfolding moves them.
    side::Any
    names::Any
    doc_scroll::Int
    override::Any
    query::Union{Nothing,String}
    internal::Bool
    textsearch::Bool
end

mutable struct Browser
    term::REPL.Terminals.TextTerminal
    in::IO
    out::IO
    cat::Catalog
    height::Int
    width::Int
    focus::Symbol   # the pane keys go to: :side, :names or :doc
    source::Symbol  # the list that picks what the doc pane shows: :side or :names
    side::ListState
    names::ListState
    side_rows::Vector{SideRow}
    name_rows::Vector{NameRow}
    expanded::Set{Symbol}
    collapsed::Set{Symbol}
    mod::Module
    internal::Bool
    query::Union{Nothing,String}
    typing::Bool
    results::Vector{NameEntry}
    override::Any
    doc_key::Any
    doc_scroll::Int
    link::Int
    pages::Dict{Tuple{Any,Int,Vector{Symbol}},Page}
    back::Vector{Snapshot}
    forward::Vector{Snapshot}
    hits::Vector{Hit}
    hover::Any
    status::String
    help::Bool
    quit::Bool
    result::Union{Nothing,String}
    prev::Vector{String}
    last_click::Tuple{Float64,Any}
    lock::ReentrantLock
    flags::Dict{Any,Set{Symbol}} # the parts of each page that are unfolded
    textsearch::Bool             # search docstring text instead of names
    layout::Symbol               # :auto, or a number of panes: :one, :two or :three
    color::Bool
    # Text selected in the documentation, from where the drag started to where it is now,
    # each as a page line and a column.
    sel::Union{Nothing,NTuple{4,Int}}
    dragging::Bool
    clicks::NTuple{4,Float64}    # time, row and column of the last press, and the click count
    under_help::Any              # the page, scroll and link that the help covers
    manual_hits::Vector{ManualHit} # what a search finds in the manual
    indirect::Bool               # whether a search includes indirect dependencies
    hidden::Int                  # how many names a search found in indirect dependencies, but left out
end

function Browser(term::REPL.Terminals.TextTerminal, cat::Catalog=Catalog())
    h, w = displaysize(term)
    b = Browser(term, term.in_stream, term.out_stream, cat, h, w, :side, :side,
                ListState(1, 0), ListState(1, 0), SideRow[], NameRow[],
                Set(DEFAULT_EXPANDED), Set{Symbol}(), Main, false, nothing, false,
                NameEntry[], nothing, nothing, 0, 0, Dict{Tuple{Any,Int,Vector{Symbol}},Page}(),
                Snapshot[], Snapshot[], Hit[], nothing, "", false, false, nothing, String[],
                (0.0, nothing), ReentrantLock(), Dict{Any,Set{Symbol}}(), false, LAYOUT[],
                get(term.out_stream, :color, true)::Bool || REPL.Terminals.hascolor(term),
                nothing, false, (0.0, 0.0, 0.0, 0.0), nothing, ManualHit[], SEARCH_INDIRECT[], 0)
    any(p -> p.role in (:project, :direct), cat.pkgs) || push!(b.expanded, :indirect)
    refresh_side!(b)
    first_pkg = findfirst(r -> r.pkg !== nothing, b.side_rows)
    b.side.cursor = something(first_pkg, 1)
    sync_side!(b)
    return b
end

## Rows ##

# The sidebar has two blocks: the curated documentation, and the docstrings of loaded code.
# Indirect dependencies are a section inside the dependencies.
const BLOCKS = (docs = (:manual, :basedocs, :stdlibdocs, :devdocs),
                code = (:project, :direct, :indirect, :julia, :extension))
const SECTION_ORDER = (:docs, BLOCKS.docs..., :code, BLOCKS.code...)
const MANUAL_ROLES = BLOCKS.docs
const DEFAULT_EXPANDED = (:docs, :code, :project, :direct, :julia)
const SIDEBAR_TITLES = (docs = "Documentation", code = "Docstrings", manual = "Manual", basedocs = "Base",
                        stdlibdocs = "Standard library", pkgdocs = "Pkg", devdocs = "Developer docs", project = "Project",
                        direct = "Dependencies", indirect = "Indirect & stdlib", julia = "Julia",
                        extension = "Extensions")

is_block(role::Symbol) = role === :docs || role === :code

# Whether the sidebar has anything to list in section `role`.
# Pkg's manual is one row of the sidebar, which stands for all its pages. It shows the first.
pkg_pages(b::Browser) = filter(p -> p.role === :pkgdocs, b.cat.manual)

function has_section(b::Browser, role::Symbol)
    role in MANUAL_ROLES && return any(p -> p.role === role, b.cat.manual)
    role === :direct && !isempty(b.cat.unloaded) && return true
    return any(p -> p.role === role, b.cat.pkgs)
end

function refresh_side!(b::Browser)
    selected = row_key(side_row(b))
    rows = SideRow[]
    for (block, roles) in pairs(BLOCKS)
        any(r -> has_section(b, r), roles) || continue
        push!(rows, SideRow(block, nothing))
        block in b.expanded || continue
        for role in roles
            has_section(b, role) || continue
            # Indirect dependencies are inside the dependencies, when there are any.
            role === :indirect && has_section(b, :direct) && !(:direct in b.expanded) && continue
            push!(rows, SideRow(role, nothing))
            role in b.expanded || continue
            if role in MANUAL_ROLES
                pages = filter(p -> p.role === role, b.cat.manual)
                # Pkg's manual is among the standard libraries, in its place.
                pkg = role === :stdlibdocs ? pkg_pages(b) : ManualPage[]
                if !isempty(pkg)
                    k = something(findfirst(p -> lowercase(p.title) > "pkg", pages), length(pages) + 1)
                    insert!(pages, k, pkg[1])
                end
                for p in pages
                    push!(rows, SideRow(p.role, nothing, nothing, p))
                end
                continue
            end
            for p in b.cat.pkgs
                p.role === role && push!(rows, SideRow(role, p))
            end
            if role === :direct
                for id in b.cat.unloaded
                    push!(rows, SideRow(role, nothing, id))
                end
            end
        end
    end
    b.side_rows = rows
    # Keep the same row selected. If it is gone, the names pane must follow the new one.
    i = findfirst(r -> row_key(r) == selected, rows)
    b.side.cursor = something(i, clamp(b.side.cursor, 1, max(1, length(rows))))
    i === nothing && selected !== nothing && refresh_names!(b)
end

function refresh_names!(b::Browser)
    rows = NameRow[]
    if b.query !== nothing
        # The manual comes first, unless the query is exactly a name.
        q = lowercase(last(split(b.query, '.')))
        names_first = !isempty(b.results) && lowercase(String(b.results[1].name)) == q
        names_first && add_name_hits!(rows, b)
        if !isempty(b.manual_hits)
            push!(rows, NameRow(:header, nothing, :manual_hits, length(b.manual_hits)))
            if !(:manual_hits in b.collapsed)
                for (i, h) in enumerate(b.manual_hits)
                    push!(rows, NameRow(:hit, nothing, :manual_hits, i, h))
                end
            end
        end
        names_first || add_name_hits!(rows, b)
    elseif side_row(b) !== nothing && side_row(b).role === :pkgdocs
        # Pkg's manual lists its pages, each of which unfolds to its headings.
        push!(rows, NameRow(:about, nothing, :none, 0))
        for (j, p) in enumerate(pkg_pages(b))
            at = ManualHit(p.path, "", 0)
            push!(rows, NameRow(:page, nothing, :none, j, at))
            Symbol(p.path) in b.expanded || continue
            for (k, h) in enumerate(page_headings(b.cat, p.path))
                k == 1 && h.level == 1 && continue # the title
                push!(rows, NameRow(:heading, nothing, :none, k, at))
            end
        end
    elseif side_page(b) !== nothing
        # A page of the manual lists its headings, by their place on the page.
        push!(rows, NameRow(:about, nothing, :none, 0))
        for (k, h) in enumerate(page_headings(b.cat, side_page(b).path))
            k == 1 && h.level == 1 && continue # the title
            push!(rows, NameRow(:heading, nothing, :none, k))
        end
    else
        push!(rows, NameRow(:about, nothing, :none, 0))
        entries = module_entries(b.cat, b.mod)
        for k in KINDS
            n = 0
            for e in entries
                e.kind === k && (b.internal || (e.vis !== :internal && !e.deprecated)) && (n += 1)
            end
            n == 0 && continue
            push!(rows, NameRow(:header, nothing, k, n))
            k in b.collapsed && continue
            for e in entries
                e.kind === k && (b.internal || (e.vis !== :internal && !e.deprecated)) || continue
                push!(rows, NameRow(:entry, e, k, 0))
            end
        end
    end
    b.name_rows = rows
    b.names.cursor = clamp(b.names.cursor, 1, max(1, length(rows)))
end

# The names a search finds, under a heading when the manual has results too.
function add_name_hits!(rows::Vector{NameRow}, b::Browser)
    isempty(b.manual_hits) || isempty(b.results) || push!(rows, NameRow(:header, nothing, :name_hits, length(b.results)))
    isempty(b.manual_hits) || !(:name_hits in b.collapsed) || return
    for e in b.results
        push!(rows, NameRow(:entry, e, e.kind, 0))
    end
    b.hidden > 0 && push!(rows, NameRow(:more, nothing, :none, b.hidden))
end

side_row(b::Browser) = get(b.side_rows, b.side.cursor, nothing)

# The selected package row when it is a section heading or a package that is not loaded.
# The names pane then shows nothing, rather than the names of the last module shown.
function side_placeholder(b::Browser)
    b.query === nothing && b.source === :side || return nothing
    row = side_row(b)
    return row === nothing || row.pkg !== nothing || row.page !== nothing ? nothing : row
end

# The manual page picked in the sidebar.
function side_page(b::Browser)
    row = side_row(b)
    row === nothing && return nothing
    # For Pkg's manual, the names pane picks the page.
    if row.role === :pkgdocs && b.query === nothing
        r = name_row(b)
        i = r === nothing || r.hit === nothing ? nothing : manual_page_of(b.cat, r.hit.path)
        i === nothing || return b.cat.manual[i]
    end
    return row.page
end
name_row(b::Browser) = get(b.name_rows, b.names.cursor, nothing)

# Make the names pane follow the package picked in the sidebar.
function sync_side!(b::Browser)
    row = side_row(b)
    if row !== nothing && row.pkg !== nothing && b.mod !== row.pkg.mod
        b.mod = row.pkg.mod
        b.names = ListState(1, 0)
        b.collapsed = Set{Symbol}()
    end
    row !== nothing && row.page !== nothing && (b.names = ListState(1, 0))
    refresh_names!(b)
end

# Unfold what section `role` is in, so that its heading is listed, and with `open`, the section too.
function unfold_to!(b::Browser, role::Symbol; open::Bool=false)
    is_block(role) && !open && return
    role === :pkgdocs && (role = :stdlibdocs) # Pkg's manual is a row of the standard libraries
    sections = Symbol[role in MANUAL_ROLES ? :docs : :code]
    role === :indirect && push!(sections, :direct)
    open && push!(sections, role)
    all(in(b.expanded), sections) && return
    union!(b.expanded, sections)
    refresh_side!(b)
end

function select_package!(b::Browser, m::Module)
    p = package_of(b.cat, m)
    p === nothing && return
    unfold_to!(b, p.role; open=true)
    i = findfirst(r -> r.pkg !== nothing && r.pkg.mod === p.mod, b.side_rows)
    i === nothing && return
    b.side.cursor = i
    reveal!(b.side, content_rows(b))
end

# Scroll a list so that its cursor is on screen. The first `offset` rows are not scrolled.
function reveal!(ls::ListState, n::Int, offset::Int=0)
    pos = ls.cursor - offset
    pos < 1 && return ls
    pos <= ls.scroll && (ls.scroll = pos - 1)
    pos > ls.scroll + n && (ls.scroll = pos - n)
    return ls
end

# The names pane shows its first row, the module itself, as its title, outside the list.
names_offset(b::Browser) = !isempty(b.name_rows) && b.name_rows[1].kind === :about ? 1 : 0
list_offset(b::Browser, pane::Symbol) = pane === :names ? names_offset(b) : 0

## Navigation ##

snapshot(b::Browser) = Snapshot(b.mod, b.focus, b.source, row_key(side_row(b)), row_key(name_row(b)),
                                b.doc_scroll, b.override, b.query, b.internal, b.textsearch)

row_key(::Nothing) = nothing
row_key(r::SideRow) = (r.role, r.pkg === nothing ? nothing : r.pkg.mod, r.unloaded,
                       r.page === nothing ? nothing : r.page.path)
row_key(r::NameRow) = r.entry !== nothing ? (r.entry.mod, r.entry.name) :
    r.hit !== nothing && r.kind !== :hit ? (r.kind, r.hit.path, r.count) : (r.kind, r.group, r.count)

function restore!(b::Browser, s::Snapshot)
    b.mod, b.focus, b.source, b.override, b.internal = s.mod, s.focus, s.source, s.override, s.internal
    b.query = s.query
    b.typing = false
    b.textsearch = s.textsearch
    run_search!(b)
    refresh_side!(b)
    i = findfirst(r -> row_key(r) == s.side, b.side_rows)
    if i === nothing && s.side !== nothing
        # The row was folded away since.
        unfold_to!(b, s.side[1]; open=true)
        i = findfirst(r -> row_key(r) == s.side, b.side_rows)
    end
    i === nothing || (b.side.cursor = i)
    refresh_names!(b)
    i = findfirst(r -> row_key(r) == s.names, b.name_rows)
    b.names.cursor = something(i, 1)
    reveal!(b.side, content_rows(b))
    reveal!(b.names, content_rows(b), names_offset(b))
    b.doc_key = current_key(b)
    b.doc_scroll = s.doc_scroll
    b.link = 0
end

function navigate!(f, b::Browser)
    push!(b.back, snapshot(b))
    length(b.back) > 200 && popfirst!(b.back)
    empty!(b.forward)
    f()
end

function go_back!(b::Browser)
    isempty(b.back) && return (b.status = "Nothing to go back to"; true)
    push!(b.forward, snapshot(b))
    restore!(b, pop!(b.back))
    return true
end

function go_forward!(b::Browser)
    isempty(b.forward) && return (b.status = "Nothing to go forward to"; true)
    push!(b.back, snapshot(b))
    restore!(b, pop!(b.forward))
    return true
end

function close_search!(b::Browser)
    b.query = nothing
    b.typing = false
    empty!(b.results)
    empty!(b.manual_hits)
    b.hidden = 0
    refresh_names!(b)
end

"""
    show_module!(b, m; entry=nothing)

Show module `m` in the names pane, with the row for name `entry` selected.
"""
function show_module!(b::Browser, m::Module; entry::Union{Nothing,Symbol}=nothing)
    b.query === nothing || close_search!(b)
    if b.mod !== m
        b.mod = m
        b.collapsed = Set{Symbol}()
    end
    b.override = nothing
    b.source = :names
    select_package!(b, m)
    refresh_names!(b)
    b.names = ListState(1, 0)
    entry === nothing && return
    find(b) = findfirst(r -> r.entry !== nothing && r.entry.name === entry, b.name_rows)
    # The name may be in a folded group.
    for e in module_entries(b.cat, m)
        e.name === entry && e.kind in b.collapsed || continue
        delete!(b.collapsed, e.kind)
        refresh_names!(b)
    end
    i = find(b)
    if i === nothing && !b.internal
        b.internal = true
        refresh_names!(b)
        i = find(b)
        if i === nothing
            b.internal = false # it is not listed at all, so showing internals does not help
            refresh_names!(b)
        else
            b.status = "Showing internal names"
        end
    end
    if i === nothing
        e = try
            classify(b.cat, m, entry)
        catch
            nothing
        end
        b.override = something(e, ModuleTarget(m))
    else
        b.names.cursor = i
        b.names.scroll = max(0, i - names_offset(b) - content_rows(b) ÷ 3)
    end
end

function follow!(b::Browser, t::Target)
    if t isa AnchorTarget
        p = page(b, current_key(b), doc_width(b))
        t.heading in eachindex(p.headings) || return
        b.doc_scroll = p.headings[t.heading] - 1
        b.focus = :doc
        b.link = 0
    elseif t isa ToggleTarget
        toggle_flag!(b, t.flag)
    elseif t isa CodeTarget
        b.result = t.code
        b.quit = true
    elseif t isa URLTarget
        open_url(b, t.url)
    elseif t isa LoadTarget
        load_package!(b, t.id)
    elseif t isa SourceTarget
        edit_source!(b, t)
    elseif t isa ManualTarget
        open_manual!(b, t)
    elseif t isa SectionTarget
        unfold_to!(b, t.role)
        i = findfirst(r -> is_header(r) && r.role === t.role, b.side_rows)
        i === nothing && return
        navigate!(b) do
            b.side.cursor = i
            b.source = :side
            b.focus = :side
        end
    else
        navigate!(b) do
            b.focus = :doc
            if t isa ModuleTarget
                show_module!(b, t.mod)
            else
                v = getglobal(t.mod, t.name)
                if is_submodule(t.mod, v)
                    show_module!(b, v)
                else
                    show_module!(b, t.mod; entry=t.name)
                end
            end
        end
    end
end

function open_entry!(b::Browser, e::NameEntry)
    if e.kind === :module
        val = entry_value(e)
        val isa Module || return
        navigate!(b) do
            b.focus = :names
            show_module!(b, val)
        end
    else
        navigate!(b) do
            b.focus = :doc
            show_module!(b, e.mod; entry=e.name)
        end
    end
end

function current_key(b::Browser)
    b.help && return :help
    b.override === nothing || return b.override
    if b.source === :side
        row = side_row(b)
        row === nothing && return ModuleTarget(b.mod)
        row.unloaded === nothing || return LoadTarget(row.unloaded)
        row.page === nothing || return ManualTarget(side_page(b).path, "")
        return row.pkg === nothing ? SectionTarget(row.role) : ModuleTarget(row.pkg.mod)
    end
    p = b.query === nothing ? side_page(b) : nothing
    p === nothing || return ManualTarget(p.path, "")
    row = name_row(b)
    row !== nothing && row.kind === :hit && return ManualTarget(row.hit.path, "")
    row !== nothing && row.kind === :entry && return row.entry
    return ModuleTarget(b.mod)
end

## Side effects that leave the browser ##

function web_browser(url::String)
    # On Windows, `cmd /c start` would run whatever follows an `&` in the URL as a command.
    cmd = Sys.isapple() ? `open $url` :
        Sys.iswindows() ? `rundll32 url.dll,FileProtocolHandler $url` : `xdg-open $url`
    run(pipeline(cmd; stdin=devnull, stdout=devnull, stderr=devnull); wait=false)
end

# How links leave the browser. Tests replace these so nothing is launched.
const URL_OPENER = Ref{Any}(web_browser)
const EDITOR = Ref{Any}(InteractiveUtils.edit)

function open_url(b::Browser, url::String)
    if URL_OPENER[] === web_browser && haskey(ENV, "SSH_CONNECTION")
        # A web browser would open on the remote machine, so hand the link over instead.
        how = COPY[](b, url)
        b.status = "Copied $url $how"
        return
    end
    try
        URL_OPENER[](url)
        b.status = "Opened " * url
    catch
        b.status = "Could not open " * url
    end
end

function edit_source!(b::Browser, t::SourceTarget)
    isfile(t.file) || return (b.status = "Source file not found: " * Base.contractuser(t.file); nothing)
    suspend(b) do
        EDITOR[](t.file, max(t.line, 1))
    end
end

# Run `f` on the normal screen. Returns whether it succeeded.
function suspend(f, b::Browser)
    leave_screen(b)
    try
        f()
        return true
    catch err
        b.status = "Error: " * first(split(sprint(showerror, err), '\n'))
        return false
    finally
        enter_screen(b)
    end
end

"""
    load_package!(b, id)

Load the package `id` with `import` in `Main`, then show it. Loading happens on the normal
screen, so that precompilation output is visible.
"""
function load_package!(b::Browser, id::PkgId)
    # The precompile workload must not load whatever the active project depends on.
    Base.generating_output() && return
    loaded = suspend(b) do
        # Show the import as the REPL would have, had it been typed at the prompt.
        print(IOContext(b.out, :color => b.color), styled("julia> ", :repl_prompt_julia))
        println(b.out, "import ", id.name)
        Core.eval(Main, Expr(:import, Expr(:., Symbol(id.name))))
        println(b.out)
    end
    loaded || return
    # The package's bindings are only visible in the latest world.
    Base.invokelatest(show_loaded!, b, id)
end

function show_loaded!(b::Browser, id::PkgId)
    b.cat = Catalog()
    empty!(b.pages)
    refresh_side!(b)
    m = Base.root_module(id)
    navigate!(b) do
        b.focus = :side
        show_module!(b, m)
        b.source = :side
    end
    b.status = "Loaded " * id.name
end

function page_source(b::Browser)
    key = current_key(b)
    return page(b, key, doc_width(b)).source
end

"""
    source_of(b, x) -> Union{SourceTarget, Nothing}

Where `x` is defined: a link target, a name, a module or a package that is not loaded.
For a name this is its docstring, or else its first method.
"""
function source_of(b::Browser, x)
    x isa SourceTarget && return x
    x isa NameTarget && return source_of(b, classify(b.cat, x.mod, x.name))
    x isa ModuleTarget && return page(b, x, doc_width(b)).source
    x isa PkgEntry && return source_of(b, ModuleTarget(x.mod))
    if x isa LoadTarget || x isa PkgId
        id = x isa LoadTarget ? x.id : x
        path = Base.locate_package(id)
        return path === nothing ? nothing : SourceTarget(path, 1)
    end
    x isa NameEntry || return nothing
    is_module_entry(x) && return source_of(b, ModuleTarget(x.mod))
    for d in doc_results(Binding(x.mod, x.name))
        t = source_target(d)
        t === nothing || return t
    end
    val = entry_value(x)
    val isa Module && return source_of(b, ModuleTarget(val))
    if val isa Function || val isa Type
        ms = methods(val)
        isempty(ms) || return method_target(first(ms))
    end
    return nothing
end

# The thing that a screen region shows, for finding its source.
function hit_subject(b::Browser, action)
    action isa Tuple || return nothing
    kind = first(action)
    if kind === :spot
        p = page(b, current_key(b), doc_width(b))
        k = action[2]
        return k in eachindex(p.spots) ? p.spots[k].target : nothing
    elseif kind === :side
        row = get(b.side_rows, action[2], nothing)
        row === nothing && return nothing
        row.page === nothing || return SourceTarget(row.page.path, 1)
        return row.unloaded !== nothing ? row.unloaded : row.pkg
    elseif kind === :names
        row = get(b.name_rows, action[2], nothing)
        row === nothing && return nothing
        row.kind === :entry && return row.entry
        row.kind === :hit && return SourceTarget(row.hit.path, hit_line(b.cat, row.hit))
        p = side_page(b)
        return p === nothing ? ModuleTarget(b.mod) : SourceTarget(p.path, 1)
    elseif kind === :load
        return action[2]
    elseif kind === :crumb
        return ModuleTarget(action[2])
    end
    return nothing
end

# The selected link, or else the selected row of the focused list.
function selected_subject(b::Browser)
    if b.focus === :doc
        p = page(b, current_key(b), doc_width(b))
        b.link in eachindex(p.spots) && return p.spots[b.link].target
        return nothing
    end
    return hit_subject(b, (b.focus, b.focus === :side ? b.side.cursor : b.names.cursor))
end

function edit_subject!(b::Browser, x)
    src = x === nothing ? nothing : source_of(b, x)
    src === nothing ? (b.status = "No source location for that") : edit_source!(b, src)
    return true
end

function current_name(b::Browser)
    key = current_key(b)
    key isa NameEntry && return qualified_name(key)
    key isa ModuleTarget && return qualified_name(module_entry(b.cat, key.mod))
    return nothing
end

## Layout ##

const LAYOUTS = (:auto, :three, :two, :one)

function layout(b::Browser)
    W = b.width
    mode = b.layout
    if mode === :one || W < 60
        return [(b.focus, 1, W)]
    elseif mode === :three || (mode === :auto && W >= 100)
        sw = clamp(W ÷ 5, 18, 28)
        nw = clamp(W ÷ 4, 24, 36)
        return [(:side, 1, sw), (:names, sw + 2, nw), (:doc, sw + nw + 3, W - sw - nw - 2)]
    else
        lw = clamp(W ÷ 3, 20, 32)
        left = b.focus === :side || (b.focus === :doc && b.source === :side) ? :side : :names
        return [(left, 1, lw), (:doc, lw + 2, W - lw - 1)]
    end
end

content_rows(b::Browser) = max(1, b.height - 3)

function doc_width(b::Browser)
    for (pane, _, w) in layout(b)
        pane === :doc && return max(10, w - 1)
    end
    return max(10, b.width - 1)
end

page_flags(b::Browser, key) = get(b.flags, key, Set{Symbol}())

function page(b::Browser, key, width::Int)
    flags = page_flags(b, key)
    get!(b.pages, (key, width, sort!(collect(flags)))) do
        try
            key isa NameEntry ? entry_page(b.cat, key, width, flags) :
            key isa ModuleTarget ? module_page(b.cat, key.mod, width) :
            key isa SectionTarget ? section_page(b.cat, key.role, width) :
            key isa LoadTarget ? unloaded_page(key.id, width) :
            key isa ManualTarget ? manual_page(b.cat, key.path, width) :
            key === :help ? help_page(b.cat, width) : Page(PageBuilder(width))
        catch err
            pb = PageBuilder(width)
            addline!(pb)
            addline!(pb, "  ", ("Could not show this page:", :error))
            for line in split(sprint(showerror, err), '\n')
                addline!(pb, "  ", line)
            end
            Page(pb)
        end
    end
end

## Drawing ##

function fit(s::AnnotatedString{String}, w::Int)
    w <= 0 && return AnnotatedString("")
    str = String(s)
    if any(c -> c < ' ', str)
        str = map(c -> c < ' ' ? ' ' : c, str)
        s = AnnotatedString(str, annotations(s))
    end
    total = textwidth(str)
    total <= w && return annotatedstring(s, ' '^(w - total))
    used = 0
    last = 0
    for (i, c) in pairs(str)
        cw = textwidth(c)
        used + cw > w - 1 && break
        used += cw
        last = i
    end
    head = last == 0 ? AnnotatedString("") : s[1:last]
    return annotatedstring(head, styled("…", :REPL_DocBrowser_dim), ' '^(w - 1 - used))
end
fit(s::AbstractString, w::Int) = fit(AnnotatedString(String(s)), w)

# Apply `face` to the display columns `cols` of `s`.
function face_cols!(s::AnnotatedString, cols::UnitRange{Int}, face)
    str = String(s)
    col = 1
    b1 = b2 = 0
    for (i, c) in pairs(str)
        col > last(cols) && break
        if col >= first(cols)
            b1 == 0 && (b1 = i)
            b2 = i + ncodeunits(c) - 1
        end
        col += textwidth(c)
    end
    b1 > 0 && face!(s, b1:b2, face)
    return s
end

copy_line(s::AnnotatedString{String}) = AnnotatedString(String(s), copy(annotations(s)))

"""
    RowBuilder

Builds one screen row from segments, recording where clickable segments land.
"""
mutable struct RowBuilder
    b::Browser
    row::Int
    col::Int
    parts::Vector{AnnotatedString{String}}
end
RowBuilder(b::Browser, row::Int, col::Int) = RowBuilder(b, row, col, AnnotatedString{String}[])

function put_seg!(rb::RowBuilder, text, face=nothing, action=nothing)
    s = styled(text, face)
    w = textwidth(String(s))
    if action !== nothing && w > 0
        push!(rb.b.hits, Hit(rb.row, rb.col:rb.col+w-1, action))
        rb.b.hover == action && face!(s, :REPL_DocBrowser_hover)
    end
    push!(rb.parts, s)
    rb.col += w
    return rb
end

width(rb::RowBuilder) = sum(p -> textwidth(String(p)), rb.parts; init=0)
build(rb::RowBuilder) = isempty(rb.parts) ? AnnotatedString("") : annotatedstring(rb.parts...)

function hint_segments(b::Browser)
    narrow = b.width < 100 ? [("w", "layout")] : Tuple{String,String}[]
    if b.help
        return [("esc", "close")]
    elseif b.typing
        return [("↑↓", "choose"), ("⏎", "open"), ("^T", b.textsearch ? "names" : "text"),
                ("^A", b.indirect ? "direct only" : "+indirect"), ("⇥", "results"), ("esc", "cancel")]
    elseif b.focus === :doc
        p = page(b, current_key(b), doc_width(b))
        hints = [("↑↓", "scroll"), ("⇥", "next link"), ("⏎", "follow"), ("⌫", "back")]
        isempty(p.headings) || push!(hints, ("{ }", "headings"))
        any(s -> s.target == ToggleTarget(:extended), p.spots) && push!(hints, ("x", "extended"))
        return vcat(narrow, hints, [("/", "search"), ("y", "insert"), ("e", "source")])
    else
        # On a narrow terminal the layout matters most, so it comes first.
        return vcat(narrow, [("↑↓", "move"), ("⏎", "open"), ("←→", "panes"), ("/", "search"),
                     ("i", b.internal ? "hide internal" : "internal/deprecated"), ("⌫", "back"),
                     ("y", "insert")])
    end
end

const HINT_KEYS = Dict("↑↓" => nothing, "←→" => nothing, "⏎" => KeyEvent(:enter),
                       "⇥" => KeyEvent(:tab), "⌫" => KeyEvent(:backspace),
                       "esc" => KeyEvent(:escape), "{ }" => KeyEvent('}'),
                       "^T" => KeyEvent('t', CTRL))
hint_key(k::String) = haskey(HINT_KEYS, k) ? HINT_KEYS[k] : KeyEvent(first(k))

const TIP = "Tip: click anything · drag to copy · right-click opens its source · ? lists every key"
const FIRST_OPEN = Ref(true)

"""
    describe(b, action) -> Union{String, Nothing}

What clicking a region of the screen does, for the status line.
"""
function describe(b::Browser, action)
    action isa Tuple || return nothing
    kind = first(action)
    if kind === :spot
        p = page(b, current_key(b), doc_width(b))
        k = action[2]
        k in eachindex(p.spots) || return nothing
        return describe_target(b, p, p.spots[k].target)
    elseif kind === :side
        row = get(b.side_rows, action[2], nothing)
        row === nothing && return nothing
        row.unloaded === nothing || return "↓ $(row.unloaded.name) is not loaded · ⏎ load it"
        row.role === :pkgdocs && return "Pkg's manual · ⏎ its pages"
        row.page === nothing || return "$(row.page.title) · ⏎ its sections · right-click: source"
        row.pkg === nothing && return "⏎ fold or unfold this section"
        return "$(row.pkg.id.name) · ⏎ open · right-click: source"
    elseif kind === :names
        row = get(b.name_rows, action[2], nothing)
        row === nothing && return nothing
        row.kind === :header && return "⏎ fold or unfold this group"
        row.kind === :heading && return "↓ jumps to this section of the page"
        row.kind === :page && return "this page · ⏎ show or hide its sections"
        row.kind === :hit && return "the manual: $(manual_title(b.cat, row.hit.path)) · ⏎ open"
        row.kind === :more && return "⏎ search indirect dependencies too (^A)"
        row.kind === :about && side_page(b) !== nothing && return "the top of the page"
        row.kind === :about && return "the documentation of $(display_name(b.mod))"
        e = row.entry::NameEntry
        open = e.kind === :module ? "⏎ open the module" : "⏎ read"
        return "$(display_name(e)) · $open · right-click: source"
    elseif kind === :load
        return "↓ runs import $(action[2].name), then shows its documentation"
    elseif kind === :crumb
        return "→ $(display_name(action[2]))"
    elseif kind === :back
        return "‹ go back (⌫)"
    elseif kind === :forward
        return "› go forward (])"
    elseif kind === :search
        return "/ search the $(b.textsearch ? "text of docstrings" : "names") of every loaded module"
    elseif kind === :toggle_text
        return "⇄ search $(b.textsearch ? "names" : "the text of docstrings") instead (^T)"
    elseif kind === :project
        proj = Base.active_project()
        return proj === nothing ? nothing : "the active project: " * Base.contractuser(proj)
    elseif kind === :help
        return "? every key and mouse action"
    elseif kind === :quit
        return "✕ close the browser (q)"
    elseif kind === :scrollbar
        return "click or drag to scroll"
    end
    return nothing
end

function describe_target(b::Browser, p::Page, t)
    if t isa NameTarget
        return "→ $(display_name(t.mod)).$(t.name) · ⏎ follow · right-click: source"
    elseif t isa ModuleTarget
        return "→ $(display_name(t.mod)) · ⏎ follow · right-click: source"
    elseif t isa URLTarget
        return "↗ opens $(t.url) in your web browser"
    elseif t isa SourceTarget
        return "✎ opens $(source_label(t)) in your editor"
    elseif t isa LoadTarget
        return "↓ runs import $(t.id.name), then shows its documentation"
    elseif t isa CodeTarget
        n = count(==('\n'), t.code) + 1
        return "⧉ closes the browser and puts $n line$(n == 1 ? "" : "s") of code at the prompt"
    elseif t isa ManualTarget
        i = manual_page_of(b.cat, t.path)
        title = i === nothing ? basename(dirname(dirname(dirname(t.path)))) : b.cat.manual[i].title
        return "→ the manual: $(title)$(isempty(t.anchor) ? "" : ", “" * t.anchor * "”") · ⏎ follow"
    elseif t isa AnchorTarget
        t.heading in eachindex(p.headings) || return nothing
        return "↓ jumps to “$(strip(String(p.lines[p.headings[t.heading]])))” ({ } step through headings)"
    elseif t isa ToggleTarget
        t.flag === :extended && return "shows or hides the extended help (x)"
        return "finds every method that takes this type; this can take a moment"
    elseif t isa SectionTarget
        return "→ $(getfield(SECTION_TITLES, t.role))"
    end
    return nothing
end

# The line at the left of the footer: a message, else what is under the pointer or
# selected, else the first-open tip.
function footer_message(b::Browser)
    isempty(b.status) || return b.status
    d = describe(b, b.hover)
    d === nothing || return d
    if b.focus === :doc && b.link != 0
        d = describe(b, (:spot, b.link))
        d === nothing || return d
    end
    return FIRST_OPEN[] ? TIP : ""
end

function footer_bar(b::Browser)
    rb = RowBuilder(b, b.height, 1)
    put_seg!(rb, " ")
    msg = footer_message(b)
    if !isempty(msg)
        put_seg!(rb, msg, :REPL_DocBrowser_key)
        put_seg!(rb, "   ")
    end
    # `? help` and `q quit` are always kept, at the end of the hints.
    tail = b.help ? Tuple{String,String}[] : [("?", "help"), ("q", "quit")]
    tailw = sum(h -> textwidth(h[1]) + textwidth(h[2]) + 3, tail; init=0)
    for (key, label) in hint_segments(b)
        width(rb) + textwidth(key) + textwidth(label) + 3 + tailw > b.width && break
        put_hint!(rb, key, label)
    end
    for (key, label) in tail
        put_hint!(rb, key, label)
    end
    legend = b.color ?
        Seg[Seg(" ● ", :REPL_DocBrowser_exported), Seg("exported"), Seg(" ○ ", :REPL_DocBrowser_public),
            Seg("public"), Seg(" · ", :REPL_DocBrowser_internal), Seg("internal ")] :
        Seg[Seg(" E "), Seg("exported"), Seg(" P "), Seg("public"), Seg(" - "), Seg("internal ")]
    lw = segs_width(legend)
    if !b.help && b.width - width(rb) > lw + 2
        put_seg!(rb, ' '^(b.width - width(rb) - lw))
        put_segs!(rb, legend)
    end
    return fit(build(rb), b.width)
end

function put_hint!(rb::RowBuilder, key::String, label::String)
    action = hint_key(key)
    put_seg!(rb, key, :REPL_DocBrowser_key, action === nothing ? nothing : (:key, action))
    put_seg!(rb, " " * label * "  ", nothing, action === nothing ? nothing : (:key, action))
end

# A piece of a bar, kept in one concrete type so that lists of them compile once.
struct Seg
    text::String
    face::Any
    action::Any
end
Seg(text::String, face=nothing) = Seg(text, face, nothing)

segs_width(segs::Vector{Seg}) = (w = 0; for s in segs; w += textwidth(s.text); end; w)

function put_segs!(rb::RowBuilder, segs::Vector{Seg})
    for s in segs
        put_seg!(rb, s.text, s.face, s.action)
    end
end

# The right of the header. A higher `level` leaves out more, for narrow terminals.
function header_right(b::Browser, level::Int)
    right = Seg[]
    mode = b.textsearch ? "text" : "names"
    if b.query === nothing
        push!(right, Seg(" / search all $mode ", nothing, (:search,)))
    else
        # The field being typed into stands out; once typing stops it is only shaded.
        face = b.typing ? :REPL_DocBrowser_searchbox : :REPL_DocBrowser_unfocused
        text = string(" ", b.query, b.typing ? "▏" : " ")
        pad = level == 0 ? max(0, 22 - textwidth(text)) : 0
        level < 2 && push!(right, Seg(" "), Seg(" " * mode * " ⇄ ", face, (:toggle_text,)))
        push!(right, Seg(text * ' '^pad, face, (:search,)))
        level == 0 && push!(right, Seg(string(" ", length(b.results), " found "), :REPL_DocBrowser_dim))
    end
    push!(right, Seg("  "), Seg("?", :REPL_DocBrowser_key, (:help,)),
          Seg("  "), Seg("✕", :REPL_DocBrowser_key, (:quit,)), Seg(" "))
    return right
end

function header_bar(b::Browser)
    right = header_right(b, 0)
    for level in 1:2
        segs_width(right) < b.width && break
        right = header_right(b, level)
    end
    rw = segs_width(right)
    room = max(0, b.width - rw)
    rb = RowBuilder(b, 1, 1)
    put_seg!(rb, " Julia docs ", :REPL_DocBrowser_title)
    proj = project_label()
    proj === nothing || put_seg!(rb, " ($proj)", :REPL_DocBrowser_dim, (:project,))
    put_seg!(rb, " ")
    put_seg!(rb, "‹", isempty(b.back) ? :REPL_DocBrowser_dim : :REPL_DocBrowser_key, (:back,))
    put_seg!(rb, " ")
    put_seg!(rb, "›", isempty(b.forward) ? :REPL_DocBrowser_dim : :REPL_DocBrowser_key, (:forward,))
    put_seg!(rb, "  ")
    placeholder = side_placeholder(b)
    if placeholder !== nothing
        # Not the module shown before, which is no longer what is selected.
        put_seg!(rb, placeholder.unloaded === nothing ? getfield(SECTION_TITLES, placeholder.role) :
                 placeholder.unloaded.name, :REPL_DocBrowser_pane_title)
    elseif side_page(b) !== nothing && b.override === nothing
        p = side_page(b)
        role = p.role === :pkgdocs ? :stdlibdocs : p.role
        put_seg!(rb, getfield(SECTION_TITLES, role), :REPL_DocBrowser_pane_title, (:section, role))
        put_seg!(rb, " › ", :REPL_DocBrowser_dim)
        p.role === :pkgdocs && put_seg!(rb, "Pkg › ", :REPL_DocBrowser_dim)
        put_seg!(rb, p.title, :REPL_DocBrowser_pane_title)
    else
        for (i, m) in enumerate(module_path(b.mod))
            i > 1 && put_seg!(rb, " › ", :REPL_DocBrowser_dim)
            put_seg!(rb, String(nameof(m)), :REPL_DocBrowser_pane_title, (:crumb, m))
        end
    end
    key = current_key(b)
    if placeholder === nothing && key isa NameEntry && key.mod === b.mod && !is_module_entry(key)
        put_seg!(rb, " › ", :REPL_DocBrowser_dim)
        put_seg!(rb, String(key.name))
    end
    if width(rb) > room
        # Crop the breadcrumbs, so the search box and buttons stay put.
        left = fit(build(rb), room)
        filter!(h -> h.row != 1 || last(h.cols) <= room - 1, b.hits)
        rb = RowBuilder(b, 1, room + 1, [left])
    else
        put_seg!(rb, ' '^(room - width(rb)))
    end
    put_segs!(rb, right)
    return fit(build(rb), b.width)
end

# `m` and the modules it is inside, outermost first. Following parents rather than looking
# up names keeps working when a module has been replaced or is bound under another name.
function module_path(m::Module)
    path = Module[m]
    while parentmodule(m) !== m
        m = parentmodule(m)
        pushfirst!(path, m)
    end
    return path
end

function list_line(b::Browser, pane::Symbol, i::Int, x::Int, w::Int, row::Int)
    rows = pane === :side ? b.side_rows : b.name_rows
    i > length(rows) && return fit("", w)
    rb = RowBuilder(b, row, x)
    action = (pane, i)
    first_hit = length(b.hits) + 1
    push!(b.hits, Hit(row, x:x+w-1, action))
    if pane === :side
        r = b.side_rows[i]
        if r.unloaded !== nothing
            put_seg!(rb, "   ")
            put_seg!(rb, r.unloaded.name, nothing, action)
            room = w - width(rb) - 1
            if room >= 7
                put_seg!(rb, ' '^(room - 6))
                put_seg!(rb, "↓ load", :REPL_DocBrowser_key, (:load, r.unloaded))
            end
        elseif r.page !== nothing
            put_seg!(rb, "   ")
            put_seg!(rb, r.role === :pkgdocs ? "Pkg" : r.page.title, nothing, action)
        elseif r.pkg === nothing
            role = r.role
            chevron = role in b.expanded ? "▾ " : "▸ "
            if is_block(role)
                put_seg!(rb, chevron, :REPL_DocBrowser_dim, action)
                put_seg!(rb, getfield(SIDEBAR_TITLES, role), :REPL_DocBrowser_pane_title, action)
            else
                n = count(p -> p.role === role, b.cat.pkgs) + (role === :direct ? length(b.cat.unloaded) : 0) +
                    count(p -> p.role === role, b.cat.manual) + (role === :stdlibdocs && !isempty(pkg_pages(b)))
                put_seg!(rb, role === :indirect && has_section(b, :direct) ? "   " : " ")
                put_seg!(rb, chevron, :REPL_DocBrowser_dim, action)
                put_seg!(rb, getfield(SIDEBAR_TITLES, role), :REPL_DocBrowser_section, action)
                put_seg!(rb, " $n", :REPL_DocBrowser_dim, action)
            end
        else
            p = r.pkg
            version = p.version === nothing || p.role === :julia ? "" : string(p.version)
            put_seg!(rb, p.role === :indirect && has_section(b, :direct) ? "     " : "   ")
            put_seg!(rb, p.id.name, isjll(p.id.name) ? :REPL_DocBrowser_dim : nothing, action)
            room = w - width(rb) - 1
            if !isempty(version) && textwidth(version) + 1 <= room
                put_seg!(rb, ' '^(room - textwidth(version)))
                put_seg!(rb, version, :REPL_DocBrowser_dim)
            end
        end
    else
        r = b.name_rows[i]
        if r.kind === :about
            p = b.query === nothing ? side_page(b) : nothing
            pkg = p !== nothing && p.role === :pkgdocs
            title = pkg ? "Pkg" : p === nothing ? String(nameof(b.mod)) : p.title
            n = pkg ? "$(count(r -> r.kind === :page, b.name_rows)) pages" :
                p === nothing ? "$(count(r -> r.kind === :entry, b.name_rows)) names" : "$(length(b.name_rows) - 1) sections"
            put_seg!(rb, " " * title, b.focus === :names ? :REPL_DocBrowser_title : :REPL_DocBrowser_section, action)
            put_seg!(rb, "  " * n, :REPL_DocBrowser_dim, action)
        elseif r.kind === :page
            p = b.cat.manual[manual_page_of(b.cat, r.hit.path)]
            put_seg!(rb, Symbol(p.path) in b.expanded ? " ▾ " : " ▸ ", :REPL_DocBrowser_dim, action)
            put_seg!(rb, p.title, :REPL_DocBrowser_pane_title, action)
        elseif r.kind === :heading
            path = r.hit === nothing ? (p = side_page(b); p === nothing ? nothing : p.path) : r.hit.path
            hs = path === nothing ? Heading[] : page_headings(b.cat, path)
            r.count in eachindex(hs) || return fit("", w)
            h = hs[r.count]
            put_seg!(rb, (r.hit === nothing ? "  " : "    ") * "  "^max(0, h.level - 2))
            put_seg!(rb, h.text, h.level <= 2 ? :REPL_DocBrowser_pane_title : nothing, action)
        elseif r.kind === :more
            put_seg!(rb, " + $(r.count) in indirect dependencies (^A)", :REPL_DocBrowser_dim, action)
        elseif r.kind === :hit
            # a section of the manual with its page on the right, or a page with its part of the docs
            h = r.hit::ManualHit
            put_seg!(rb, " § ", :REPL_DocBrowser_kind_module)
            put_match!(rb, hit_title(b.cat, h), nothing, action)
            i = manual_page_of(b.cat, h.path)
            where = !isempty(h.heading) ? " " * manual_title(b.cat, h.path) :
                i === nothing ? "" : " " * getfield(SIDEBAR_TITLES, b.cat.manual[i].role)
            if !isempty(where)
                room = w - width(rb) - 1
                if room > 4
                    put_seg!(rb, ' '^max(1, room - textwidth(where)))
                    put_seg!(rb, textwidth(where) > room ? first(where, room) : where, :REPL_DocBrowser_dim)
                end
            end
        elseif r.kind === :header
            title = r.group === :manual_hits ? "In the manual" : r.group === :name_hits ? "Names" : kind_title(r.group)
            put_seg!(rb, r.group in b.collapsed ? " ▸ " : " ▾ ", :REPL_DocBrowser_dim, action)
            put_seg!(rb, title, :REPL_DocBrowser_section, action)
            put_seg!(rb, " $(r.count)", :REPL_DocBrowser_dim, action)
        else
            e = r.entry::NameEntry
            dot = if b.color
                e.vis === :exported ? " ●" : e.vis === :public ? " ○" : " ·"
            else
                e.vis === :exported ? " E" : e.vis === :public ? " P" : " -"
            end
            put_seg!(rb, dot, vis_face(e.vis))
            put_seg!(rb, kind_glyph(e.kind), kind_face(e.kind))
            put_seg!(rb, " ")
            name = is_module_entry(e) ? display_name(e.mod) : String(e.name)
            face = e.vis === :internal || !e.documented ? :REPL_DocBrowser_dim : nothing
            e.deprecated && (face = [:REPL_DocBrowser_dim, :strikethrough])
            put_match!(rb, name, face, action)
            if b.query !== nothing && !is_module_entry(e)
                where = " " * display_name(e.mod)
                room = w - width(rb) - 1
                if room > 4
                    put_seg!(rb, ' '^max(1, room - textwidth(where)))
                    put_seg!(rb, textwidth(where) > room ? first(where, room) : where, :REPL_DocBrowser_dim)
                end
            end
        end
    end
    line = fit(build(rb), w)
    # A long name is cut off at the edge of the pane, and so is the area that can be clicked.
    for k in first_hit:length(b.hits)
        h = b.hits[k]
        b.hits[k] = Hit(h.row, max(first(h.cols), x):min(last(h.cols), x + w - 1), h.action)
    end
    if i == (pane === :side ? b.side.cursor : b.names.cursor)
        if b.focus === pane
            face!(line, :REPL_DocBrowser_selected)
        elseif b.source === pane || pane === :side
            face!(line, :REPL_DocBrowser_unfocused)
        end
        # Without colour the selection would not show, so mark it.
        if !b.color && startswith(String(line), ' ')
            line = annotatedstring(b.focus === pane ? "▶" : "›", line[2:end])
        end
    end
    return line
end

# Show `name`, with the part that matches the search query emphasised.
function put_match!(rb::RowBuilder, name::String, face, action)
    q = rb.b.query
    r = q === nothing || isempty(q) ? nothing : findfirst(lowercase(last(split(q, '.'))), lowercase(name))
    if r === nothing || isempty(r) || ncodeunits(lowercase(name)) != ncodeunits(name)
        put_seg!(rb, name, face, action)
        return
    end
    put_seg!(rb, name[1:prevind(name, first(r))], face, action)
    put_seg!(rb, name[first(r):last(r)], :REPL_DocBrowser_match, action)
    put_seg!(rb, name[nextind(name, last(r)):end], face, action)
end

function draw_list(b::Browser, pane::Symbol, x::Int, w::Int)
    n = content_rows(b)
    lines = AnnotatedString{String}[]
    placeholder = pane === :names ? side_placeholder(b) : nothing
    if placeholder !== nothing
        for k in 1:n
            push!(lines, fit("", w))
        end
        if placeholder.unloaded !== nothing && n >= 1
            rb = RowBuilder(b, 3, x)
            put_seg!(rb, " Press ⏎ to load it")
            lines[1] = fit(build(rb), w)
        end
        return lines
    end
    ls = pane === :side ? b.side : b.names
    offset = list_offset(b, pane)
    len = (pane === :side ? length(b.side_rows) : length(b.name_rows)) - offset
    ls.scroll = clamp(ls.scroll, 0, max(0, len - n))
    for k in 1:n
        push!(lines, list_line(b, pane, offset + ls.scroll + k, x, w, k + 2))
    end
    return lines
end

function pane_title(b::Browser, pane::Symbol, x::Int, w::Int)
    rb = RowBuilder(b, 2, x)
    face = b.focus === pane ? :REPL_DocBrowser_title : :REPL_DocBrowser_section
    if pane === :side
        put_seg!(rb, " Contents", face, (:focus, :side))
    elseif pane === :names
        placeholder = side_placeholder(b)
        if placeholder !== nothing
            name = placeholder.unloaded === nothing ?
                getfield(SECTION_TITLES, placeholder.role) : placeholder.unloaded.name
            put_seg!(rb, " " * name, face, (:focus, :names))
            placeholder.unloaded === nothing || put_seg!(rb, " not loaded")
        elseif names_offset(b) == 1
            return list_line(b, :names, 1, x, w, 2)
        else
            put_seg!(rb, " Results", face, (:focus, :names))
            put_seg!(rb, " $(length(b.results) + length(b.manual_hits))", :REPL_DocBrowser_dim)
        end
    else
        key = current_key(b)
        title = key isa NameEntry ? display_name(key) :
                key isa ModuleTarget ? display_name(key.mod) :
                key isa SectionTarget ? getfield(SECTION_TITLES, key.role) :
                key isa LoadTarget ? key.id.name :
                key isa ManualTarget ? manual_title(b.cat, key.path) :
                key === :help ? "Keys and mouse" : ""
        p = page(b, key, doc_width(b))
        n = content_rows(b)
        pos = length(p.lines) <= n ? "" :
            string(round(Int, 100 * min(b.doc_scroll + n, length(p.lines)) / length(p.lines)), "% ")
        # Say whether this is a page written as a guide, or the docstrings of some code.
        kind = key isa ManualTarget ? "manual" : key isa NameEntry || key isa ModuleTarget ? "docstrings" : ""
        isempty(kind) || (pos = kind * (isempty(pos) ? " " : " · " * pos))
        put_seg!(rb, " " * title, face, (:focus, :doc))
        room = w - width(rb)
        room > textwidth(pos) && put_seg!(rb, ' '^(room - textwidth(pos)) * pos, :REPL_DocBrowser_dim)
    end
    return fit(build(rb), w)
end

function draw_doc(b::Browser, x::Int, w::Int)
    n = content_rows(b)
    pw = max(10, w - 1)
    key = current_key(b)
    if key != b.doc_key
        # The help covers a page for a while, which should be as it was when the help closes.
        under = b.under_help
        key === :help && (b.under_help = (b.doc_key, b.doc_scroll, b.link))
        b.doc_key = key
        b.doc_scroll, b.link = under !== nothing && under[1] == key ? (under[2], under[3]) : (0, 0)
        key === :help || (b.under_help = nothing)
        # What was under the pointer, and what was selected, belonged to the old page.
        b.hover = nothing
        b.sel = nothing
        b.dragging = false
    end
    p = page(b, key, pw)
    maxscroll = max(0, length(p.lines) - n)
    b.doc_scroll = clamp(b.doc_scroll, 0, maxscroll)
    # A scroll bar in the last column, when the page does not fit.
    thumb = 0:-1
    if maxscroll > 0
        size = max(1, round(Int, n * n / length(p.lines)))
        start = round(Int, (n - size) * b.doc_scroll / maxscroll)
        thumb = (start + 1):(start + size)
    end
    lines = AnnotatedString{String}[]
    for k in 1:n
        li = b.doc_scroll + k
        line = li <= length(p.lines) ? copy_line(p.lines[li]) : AnnotatedString("")
        b.textsearch && b.query !== nothing && mark_words!(line, b.query)
        for (si, s) in enumerate(p.spots)
            s.line == li || continue
            face_cols!(line, s.cols, si == b.link ? :REPL_DocBrowser_link_active : :REPL_DocBrowser_link)
            b.hover == (:spot, si) && face_cols!(line, s.cols, :REPL_DocBrowser_link_active)
            push!(b.hits, Hit(k + 2, (x + first(s.cols) - 1):(x + last(s.cols) - 1), (:spot, si)))
        end
        cols = selected_cols(b, li)
        if cols !== nothing
            # The indentation is not copied, so do not show it as selected.
            str = String(line)
            indent = textwidth(str) - textwidth(lstrip(str))
            face_cols!(line, max(first(cols), indent + 1):last(cols), :REPL_DocBrowser_selection)
        end
        line = fit(line, pw)
        if maxscroll > 0
            bar = styled(k in thumb ? "┃" : "│", k in thumb ? :REPL_DocBrowser_key : :REPL_DocBrowser_border)
            push!(b.hits, Hit(k + 2, (x + pw):(x + pw), (:scrollbar, (k - 1) / max(1, n - 1))))
            line = annotatedstring(line, bar)
        else
            line = fit(line, w)
        end
        push!(lines, line)
    end
    return lines
end

# Emphasise the words of a text search where they appear in `line`.
function mark_words!(line::AnnotatedString, query::String)
    str = String(line)
    low = lowercase(str)
    ncodeunits(low) == ncodeunits(str) || return
    for w in split(lowercase(query))
        length(w) >= 2 || continue
        for r in findall(w, low)
            face!(line, first(r):thisind(str, last(r)) + ncodeunits(str[thisind(str, last(r))]) - 1, :REPL_DocBrowser_match)
        end
    end
end

function frame(b::Browser)
    empty!(b.hits)
    H, W = b.height, b.width
    if H < 6 || W < 24
        return [rpad(first(i == 1 ? " Terminal too small" : "", W), W) for i in 1:H]
    end
    rows = Vector{String}(undef, H)
    panes = layout(b)
    cols = Tuple{AnnotatedString{String},Vector{AnnotatedString{String}}}[]
    # Draw the doc pane first, since drawing it updates the current page.
    order = sortperm([pane === :doc ? 0 : 1 for (pane, _, _) in panes])
    built = Vector{Any}(undef, length(panes))
    for i in order
        pane, x, w = panes[i]
        lines = pane === :doc ? draw_doc(b, x, w) : draw_list(b, pane, x, w)
        built[i] = (pane_title(b, pane, x, w), lines)
    end
    sep = styled("│", :REPL_DocBrowser_border)
    for r in 1:content_rows(b)+1
        parts = AnnotatedString{String}[]
        for (i, (title, lines)) in enumerate(built)
            i > 1 && push!(parts, sep)
            push!(parts, r == 1 ? title : lines[r-1])
        end
        rows[r+1] = ansi(annotatedstring(parts...), b.color)
    end
    rows[1] = ansi(header_bar(b), b.color)
    rows[H] = ansi(footer_bar(b), b.color)
    return rows
end

ansi(s::AnnotatedString, color::Bool=true) = sprint(print, s; context = :color => color)

function draw(b::Browser)
    rows = frame(b)
    buf = IOBuffer()
    print(buf, "\e[?2026h")
    for (i, r) in enumerate(rows)
        if i > length(b.prev) || b.prev[i] != r
            print(buf, "\e[", i, ";1H\e[0m", r, "\e[0m")
        end
    end
    print(buf, "\e[?2026l")
    write(b.out, take!(buf))
    flush(b.out)
    b.prev = rows
    nothing
end

## Terminal setup ##

const ENTER_SCREEN = "\e[?1049h\e[?25l\e[?7l\e[?1000h\e[?1002h\e[?1003h\e[?1006h\e[?2004h\e[H\e[2J"
const LEAVE_SCREEN = "\e[?2004l\e[?1006l\e[?1003l\e[?1002l\e[?1000l\e[?7h\e[?25h\e[?1049l"

function set_raw(b::Browser, raw::Bool)
    try
        REPL.Terminals.raw!(b.term, raw)
    catch
    end
end

function enter_screen(b::Browser)
    set_raw(b, true)
    write(b.out, ENTER_SCREEN)
    flush(b.out)
    b.prev = String[]
end

function leave_screen(b::Browser, drain::Bool=false)
    write(b.out, LEAVE_SCREEN)
    flush(b.out)
    # Mouse reports sent before reporting stopped would reach the prompt. They must be read
    # while still in raw mode, which delivers them without waiting for a newline.
    drain && drain_input(b.in)
    set_raw(b, false)
end

function check_size!(b::Browser)
    h, w = displaysize(b.term)
    (h, w) == (b.height, b.width) && return false
    b.height, b.width = h, w
    write(b.out, "\e[H\e[2J")
    b.prev = String[]
    return true
end

## Selecting text ##

# The page line and column under a point of the documentation pane. With `clamped`, a point
# outside the pane counts as the nearest point inside it, so a drag can leave the pane.
function doc_point(b::Browser, row::Int, col::Int, clamped::Bool=false)
    n = content_rows(b)
    for (pane, x, w) in layout(b)
        pane === :doc || continue
        pw = max(10, w - 1) # the last column holds the scroll bar
        if clamped
            row = clamp(row, 3, n + 2)
            col = clamp(col, x, x + pw - 1)
        end
        (3 <= row <= n + 2 && x <= col < x + pw) || return nothing
        return (b.doc_scroll + row - 2, col - x + 1)
    end
    return nothing
end

# The columns of page line `li` that are selected.
function selected_cols(b::Browser, li::Int)
    b.sel === nothing && return nothing
    l1, c1, l2, c2 = ordered_selection(b.sel)
    l1 <= li <= l2 || return nothing
    return (li == l1 ? c1 : 1):(li == l2 ? c2 : typemax(Int) ÷ 2)
end

function ordered_selection(s::NTuple{4,Int})
    l1, c1, l2, c2 = s
    return (l2, c2) < (l1, c1) ? (l2, c2, l1, c1) : (l1, c1, l2, c2)
end

# The characters of `s` that start in columns `from` to `to`.
function col_slice(s::String, from::Int, to::Int)
    io = IOBuffer()
    col = 1
    for c in s
        from <= col <= to && print(io, c)
        col += textwidth(c)
    end
    return String(take!(io))
end

# Characters that make up a word for a double-click, so a path, a URL or `Base.sort!` is one word.
is_word_char(c::Char) = isletter(c) || isdigit(c) || c in "_!@.-/~+:\\#%=?&"

# The columns of the word at column `col` of `s`, without punctuation that ends a sentence.
function word_cols(s::String, col::Int)
    starts = Int[]
    chars = Char[]
    at = 1
    for c in s
        push!(starts, at)
        push!(chars, c)
        at += textwidth(c)
    end
    i = findlast(<=(col), starts)
    # past the end of the line, there is no word
    (i === nothing || col >= starts[i] + max(1, textwidth(chars[i]))) && return nothing
    is_word_char(chars[i]) || return nothing
    a = z = i
    while a > 1 && is_word_char(chars[a-1])
        a -= 1
    end
    while z < length(chars) && is_word_char(chars[z+1])
        z += 1
    end
    while z > a && chars[z] in ".:,?"
        z -= 1
    end
    return starts[a]:(starts[z] + textwidth(chars[z]) - 1)
end

"""
    selection_text(b) -> String

The selected text, without the indentation that the page adds to every line.
"""
function selection_text(b::Browser)
    b.sel === nothing && return ""
    l1, c1, l2, c2 = ordered_selection(b.sel)
    p = page(b, current_key(b), doc_width(b))
    lines = String[]
    for li in l1:min(l2, length(p.lines))
        cols = selected_cols(b, li)::UnitRange{Int}
        push!(lines, rstrip(col_slice(String(p.lines[li]), first(cols), last(cols))))
    end
    # A selection that starts mid-line keeps its first line as it is. One that starts in the
    # indentation starts at the line's text.
    first_line = l1 <= length(p.lines) ? String(p.lines[l1]) : ""
    first_indent = textwidth(first_line) - textwidth(lstrip(first_line))
    if !isempty(lines) && 1 < c1 <= first_indent + 1
        lines[1] = rstrip(col_slice(first_line, 1, last(selected_cols(b, l1)::UnitRange{Int})))
    end
    whole = c1 > first_indent + 1 ? (2:length(lines)) : eachindex(lines)
    indent = typemax(Int)
    for i in whole
        isempty(lines[i]) || (indent = min(indent, length(lines[i]) - length(lstrip(lines[i]))))
    end
    if indent != typemax(Int)
        for i in whole
            lines[i] = chop(lines[i]; head=min(indent, length(lines[i])), tail=0)
        end
    end
    return String(strip(join(lines, '\n'), '\n'))
end

# The key that makes the terminal select text itself, rather than pass the mouse on.
function native_select_key()
    term = get(ENV, "TERM_PROGRAM", "")
    return term == "iTerm.app" ? "Option" : term == "Apple_Terminal" ? "Fn" : "Shift"
end

"""
    copy_to_clipboard(b, text) -> String

Copy `text` to the clipboard, and say how. Over SSH the local clipboard is not the user's, so
the text goes to the terminal instead, as an OSC 52 sequence.
"""
function copy_to_clipboard(b::Browser, text::String)
    if !haskey(ENV, "SSH_CONNECTION")
        try
            InteractiveUtils.clipboard(text)
            return "to the clipboard"
        catch
        end
    end
    write(b.out, "\e]52;c;", base64encode(text), "\a")
    return "through the terminal"
end

# How selections are copied. Tests replace it, so they do not touch the clipboard.
const COPY = Ref{Any}(copy_to_clipboard)

function copy_selection!(b::Browser)
    text = selection_text(b)
    isempty(text) && return true
    how = COPY[](b, text)
    n = count(==('\n'), text) + 1
    what = n == 1 ? "$(length(text)) characters" : "$n lines"
    b.status = "Copied $what $how · $(native_select_key())-drag selects with the terminal"
    return true
end

# A press in the text of the documentation. One click starts a selection, two select a word
# and three a line.
function press_doc!(b::Browser, row::Int, col::Int)
    pt = doc_point(b, row, col)
    pt === nothing && return false
    t, r, c, n = b.clicks
    now = time()
    n = now - t < 0.4 && (r, c) == (row, col) ? n + 1 : 1.0
    b.clicks = (now, row, col, n)
    b.focus = :doc
    li, col = pt
    p = page(b, current_key(b), doc_width(b))
    text = li <= length(p.lines) ? String(p.lines[li]) : ""
    if n == 1
        b.sel = (li, col, li, col)
        b.dragging = true
        return true
    end
    cols = n == 2 ? word_cols(text, col) : 1:max(1, textwidth(text))
    cols === nothing && return true
    b.sel = (li, first(cols), li, last(cols))
    return copy_selection!(b)
end

function drag_selection!(b::Browser, row::Int, col::Int)
    # Dragging past the top or bottom of the pane scrolls it.
    row < 3 && scroll_doc!(b, -1)
    row > content_rows(b) + 2 && scroll_doc!(b, 1)
    pt = doc_point(b, row, col, true)
    (pt === nothing || b.sel === nothing) && return false
    b.sel = (b.sel[1], b.sel[2], pt[1], pt[2])
    return true
end

function release_selection!(b::Browser)
    b.dragging = false
    s = b.sel
    s === nothing && return false
    if (s[1], s[2]) == (s[3], s[4])
        b.sel = nothing # a click, not a selection
        return true
    end
    return copy_selection!(b)
end

## Events ##

function move!(b::Browser, pane::Symbol, delta::Int)
    ls = pane === :side ? b.side : b.names
    len = pane === :side ? length(b.side_rows) : length(b.name_rows)
    len == 0 && return
    ls.cursor = clamp(ls.cursor + delta, 1, len)
    reveal!(ls, content_rows(b), list_offset(b, pane))
    b.override = nothing
    b.source = pane
    pane === :side && sync_side!(b)
    pane === :names && show_heading!(b)
end

"""
    open_manual!(b, t::ManualTarget)

Show a page of the manual, at the heading that `t` names. A page in the sidebar is picked
there, so that the names pane lists its headings; any other page, such as the manual page of
a package, is shown on its own.
"""
open_manual!(b::Browser, t::ManualTarget) = open_manual!(b, t.path, anchor_heading(b.cat, t.path, t.anchor))

function open_manual!(b::Browser, path::String, k::Int)
    i = manual_page_of(b.cat, path)
    navigate!(b) do
        b.query === nothing || close_search!(b)
        b.override = nothing
        b.focus = :doc
        i === nothing && (b.override = ManualTarget(path, ""))
        i === nothing && return
        p = b.cat.manual[i]
        pkg = p.role === :pkgdocs
        unfold_to!(b, pkg ? :stdlibdocs : p.role; open=true)
        j = findfirst(r -> pkg ? r.role === :pkgdocs : r.page == p, b.side_rows)
        j === nothing || (b.side.cursor = j)
        reveal!(b.side, content_rows(b))
        b.source = :side
        sync_side!(b)
        if pkg
            # Unfold the page in the names pane and pick it, so that it is the page shown.
            push!(b.expanded, Symbol(path))
            refresh_names!(b)
            n = findfirst(r -> r.kind === :page && r.hit.path == path, b.name_rows)
            n === nothing || (b.names.cursor = n)
        end
    end
    scroll_to_heading!(b, k)
    i === nothing && return # the names pane lists something else
    # Pick the heading in the names pane too.
    r = findfirst(r -> r.kind === :heading && r.count == k && (r.hit === nothing || r.hit.path == path), b.name_rows)
    r === nothing || (b.names.cursor = r; reveal!(b.names, content_rows(b), names_offset(b)))
end

# When a heading of a manual page is picked, scroll the page to it.
function show_heading!(b::Browser)
    row = name_row(b)
    row !== nothing && row.kind === :hit && return scroll_to_heading!(b, hit_heading(b.cat, row.hit))
    (row === nothing || side_page(b) === nothing || b.query !== nothing) && return
    row.kind === :heading ? scroll_to_heading!(b, row.count) : scroll_to_heading!(b, 0)
end

function scroll_to_heading!(b::Browser, k::Int)
    key = current_key(b)
    p = page(b, key, doc_width(b))
    b.doc_key = key # so that drawing does not scroll back to the top
    b.doc_scroll = k in eachindex(p.headings) ? p.headings[k] - 1 : 0
    b.link = 0
end

function scroll_doc!(b::Browser, delta::Int)
    b.doc_scroll = max(0, b.doc_scroll + delta)
end

function cycle_link!(b::Browser, dir::Int)
    p = page(b, current_key(b), doc_width(b))
    isempty(p.spots) && return (b.status = "No links on this page"; nothing)
    n = content_rows(b)
    if b.link == 0
        first_visible = last_visible = 0
        for (i, s) in enumerate(p.spots)
            b.doc_scroll < s.line <= b.doc_scroll + n || continue
            first_visible == 0 && (first_visible = i)
            last_visible = i
        end
        b.link = first_visible == 0 ? (dir > 0 ? 1 : length(p.spots)) :
                 dir > 0 ? first_visible : last_visible
    else
        b.link = mod1(b.link + dir, length(p.spots))
    end
    line = p.spots[b.link].line
    if !(b.doc_scroll < line <= b.doc_scroll + n)
        b.doc_scroll = max(0, line - n ÷ 3)
    end
end

function toggle_section!(b::Browser, role::Symbol)
    role in b.expanded ? delete!(b.expanded, role) : push!(b.expanded, role)
    refresh_side!(b)
end

function toggle_page!(b::Browser, path::String)
    key = Symbol(path)
    key in b.expanded ? delete!(b.expanded, key) : push!(b.expanded, key)
    refresh_names!(b)
end

function toggle_group!(b::Browser, group::Symbol)
    group in b.collapsed ? delete!(b.collapsed, group) : push!(b.collapsed, group)
    refresh_names!(b)
end

function activate!(b::Browser, pane::Symbol)
    if pane === :side
        row = side_row(b)
        row === nothing && return
        if is_header(row)
            toggle_section!(b, row.role)
        elseif row.unloaded !== nothing
            load_package!(b, row.unloaded)
        else
            b.focus = :names
            b.source = :names
        end
    elseif pane === :names
        row = name_row(b)
        row === nothing && return
        if row.kind === :header
            toggle_group!(b, row.group)
        elseif row.kind === :page
            toggle_page!(b, row.hit.path)
        elseif row.kind === :about || row.kind === :heading
            b.focus = :doc
        elseif row.kind === :hit
            open_manual!(b, row.hit.path, hit_heading(b.cat, row.hit))
        elseif row.kind === :more
            toggle_indirect!(b)
        else
            e = row.entry::NameEntry
            if b.query !== nothing
                open_entry!(b, e)
            elseif e.kind === :module
                open_entry!(b, e)
            else
                b.focus = :doc
            end
        end
    else
        key = current_key(b)
        p = page(b, key, doc_width(b))
        if b.link in eachindex(p.spots)
            follow!(b, p.spots[b.link].target)
        elseif key isa LoadTarget
            load_package!(b, key.id)
        else
            b.status = "Press ⇥ to pick a link"
        end
    end
end

function focus_left!(b::Browser)
    if b.focus === :doc
        b.focus = b.source === :side ? :side : :names
    elseif b.focus === :names
        if b.query === nothing && side_page(b) === nothing && package_of(b.cat, b.mod) !== nothing &&
                get(b.cat.bymod, b.mod, nothing) === nothing
            # In a submodule, step out to its parent.
            child = b.mod
            navigate!(b) do
                show_module!(b, parentmodule(child); entry=nameof(child))
            end
        else
            b.focus = :side
            b.source = :side
        end
    end
end

function focus_right!(b::Browser)
    if b.focus === :side
        row = side_row(b)
        row === nothing ? nothing : is_header(row) ? toggle_section!(b, row.role) :
            row.unloaded !== nothing ? (b.focus = :doc) : (b.focus = :names; b.source = :names)
    elseif b.focus === :names
        row = name_row(b)
        if row !== nothing && row.kind === :entry && row.entry.kind === :module
            open_entry!(b, row.entry)
        else
            b.focus = :doc
        end
    end
end

function toggle_flag!(b::Browser, flag::Symbol)
    key = current_key(b)
    key isa NameEntry || return
    flags = get!(Set{Symbol}, b.flags, key)
    flag in flags ? delete!(flags, flag) : push!(flags, flag)
end

function toggle_textsearch!(b::Browser)
    b.textsearch = !b.textsearch
    b.query === nothing || update_search!(b)
    b.status = b.textsearch ? "Searching docstring text" : "Searching names"
end

function jump_heading!(b::Browser, dir::Int)
    p = page(b, current_key(b), doc_width(b))
    hs = dir > 0 ? filter(>(b.doc_scroll + 1), p.headings) : filter(<(b.doc_scroll + 1), p.headings)
    isempty(hs) && return (b.status = "No more headings"; nothing)
    b.doc_scroll = (dir > 0 ? first(hs) : last(hs)) - 1
    b.focus = :doc
end

function start_search!(b::Browser, query::String="")
    b.query = query
    b.typing = true
    b.focus = :names
    b.source = :names
    b.override = nothing
    update_search!(b)
end

function run_search!(b::Browser)
    q = b.query
    found = q === nothing ? NameEntry[] : b.textsearch ? search_text(b.cat, q; internal=b.internal) :
        search(b.cat, q; internal=b.internal)
    b.results = b.indirect ? found : filter(e -> !is_indirect(b.cat, e), found)
    b.hidden = length(found) - length(b.results)
    b.manual_hits = q === nothing ? ManualHit[] : search_manual(b.cat, q; text=b.textsearch)
end

function is_indirect(cat::Catalog, e::NameEntry)
    p = package_of(cat, e.mod)
    # The standard libraries are left in, since many are loaded by Julia itself.
    return p !== nothing && p.role === :indirect && !Base.is_stdlib(p.id)
end

# Whether searches include indirect dependencies, kept for the next time the browser opens.
const SEARCH_INDIRECT = Ref(false)

function toggle_indirect!(b::Browser)
    b.indirect = SEARCH_INDIRECT[] = !b.indirect
    b.query === nothing || update_search!(b)
    b.status = b.indirect ? "Searching indirect dependencies too" : "Leaving indirect dependencies out of searches"
end

function update_search!(b::Browser)
    run_search!(b)
    b.names = ListState(1, 0)
    refresh_names!(b)
    # Start on the best result, not on the heading of its group.
    first_result = findfirst(r -> r.kind !== :header, b.name_rows)
    first_result === nothing || (b.names.cursor = first_result)
    show_heading!(b)
end

function handle_typing!(b::Browser, ev::KeyEvent)
    q = something(b.query, "")
    if ev.key === :char && ev.mods & (CTRL | ALT) == 0
        b.query = q * ev.char
        update_search!(b)
    elseif ev.key === :backspace
        isempty(q) && return true
        b.query = q[1:prevind(q, lastindex(q))]
        update_search!(b)
    elseif ev.key === :char && ev.mods == CTRL && ev.char == 'u'
        b.query = ""
        update_search!(b)
    elseif ev.key === :char && ev.mods == CTRL && ev.char == 't'
        toggle_textsearch!(b)
    elseif ev.key === :char && ev.mods == CTRL && ev.char == 'a'
        toggle_indirect!(b)
    elseif ev.key === :up || (ev.key === :char && ev.mods == CTRL && ev.char == 'p')
        move!(b, :names, -1)
    elseif ev.key === :down || (ev.key === :char && ev.mods == CTRL && ev.char == 'n')
        move!(b, :names, 1)
    elseif ev.key in (:pageup, :pagedown)
        move!(b, :names, (ev.key === :pageup ? -1 : 1) * content_rows(b))
    elseif ev.key === :enter
        row = name_row(b)
        if row !== nothing && row.kind === :hit
            b.typing = false
            open_manual!(b, row.hit.path, hit_heading(b.cat, row.hit))
        elseif row !== nothing && row.kind === :more
            toggle_indirect!(b)
        elseif row === nothing || row.entry === nothing
            b.status = "No matches"
        else
            b.typing = false
            open_entry!(b, row.entry::NameEntry)
        end
    elseif ev.key in (:tab, :right)
        b.typing = false
    elseif ev.key === :escape || (ev.key === :char && ev.mods == CTRL && ev.char in ('c', 'g'))
        close_search!(b)
    end
    return true
end

isplain(ev::KeyEvent, c::Char) = ev.key === :char && ev.char == c && ev.mods & (CTRL | ALT) == 0
isctrl(ev::KeyEvent, c::Char) = ev.key === :char && ev.char == c && ev.mods == CTRL

function handle!(b::Browser, ev::KeyEvent)
    if b.sel !== nothing && !(ev.key in (:up, :down, :pageup, :pagedown))
        b.sel = nothing
        b.dragging = false
        ev.key === :escape && (b.status = ""; return true)
    end
    b.status = ""
    b.hover = nothing
    FIRST_OPEN[] = false
    if b.help
        if ev.key in (:up, :down, :pageup, :pagedown) || isplain(ev, ' ')
            ev.key === :up ? scroll_doc!(b, -1) : ev.key === :down ? scroll_doc!(b, 1) :
                scroll_doc!(b, (ev.key === :pageup ? -1 : 1) * content_rows(b))
        else
            b.help = false
        end
        return true
    end
    b.typing && return handle_typing!(b, ev)
    pane = b.focus
    n = content_rows(b)
    if isplain(ev, 'q') || isctrl(ev, 'c') || isctrl(ev, 'd')
        b.quit = true
    elseif ev.key === :escape
        if b.query !== nothing
            close_search!(b)
        elseif b.link != 0
            b.link = 0
        else
            b.quit = true
        end
    elseif isplain(ev, '?') || ev.key === :f1
        b.help = true
    elseif isplain(ev, '/') || isctrl(ev, 'f')
        start_search!(b, something(b.query, ""))
    elseif ev.key === :up || isplain(ev, 'k') || isctrl(ev, 'p')
        pane === :doc ? scroll_doc!(b, -1) : move!(b, pane, -1)
    elseif ev.key === :down || isplain(ev, 'j') || isctrl(ev, 'n')
        pane === :doc ? scroll_doc!(b, 1) : move!(b, pane, 1)
    elseif ev.key === :pageup || isctrl(ev, 'b') || (ev.key === :char && ev.char == 'v' && ev.mods == ALT)
        pane === :doc ? scroll_doc!(b, -(n - 2)) : move!(b, pane, -n)
    elseif ev.key === :pagedown || isplain(ev, ' ') || isctrl(ev, 'v')
        pane === :doc ? scroll_doc!(b, n - 2) : move!(b, pane, n)
    elseif ev.key === :home || isplain(ev, 'g')
        pane === :doc ? (b.doc_scroll = 0) : move!(b, pane, -typemax(Int) ÷ 2)
    elseif ev.key === :end || isplain(ev, 'G')
        pane === :doc ? (b.doc_scroll = typemax(Int) ÷ 2) : move!(b, pane, typemax(Int) ÷ 2)
    elseif ev.key === :left && ev.mods & ALT != 0 || isplain(ev, '[')
        go_back!(b)
    elseif ev.key === :right && ev.mods & ALT != 0 || isplain(ev, ']')
        go_forward!(b)
    elseif ev.key === :char && ev.mods == ALT && ev.char == 'b'
        go_back!(b)
    elseif ev.key === :char && ev.mods == ALT && ev.char == 'f'
        go_forward!(b)
    elseif ev.key === :left || isplain(ev, 'h')
        focus_left!(b)
    elseif ev.key === :right || isplain(ev, 'l')
        focus_right!(b)
    elseif ev.key === :enter
        activate!(b, pane)
    elseif ev.key === :tab
        pane === :doc ? cycle_link!(b, 1) :
            (b.focus = pane === :side && side_placeholder(b) === nothing ? :names : :doc)
    elseif ev.key === :backtab
        pane === :doc ? cycle_link!(b, -1) :
            (b.focus = pane === :names || side_placeholder(b) !== nothing ? :side : :names)
    elseif ev.key === :backspace
        go_back!(b)
    elseif isctrl(ev, 'a')
        toggle_indirect!(b)
    elseif isctrl(ev, 't')
        toggle_textsearch!(b)
    elseif isplain(ev, 'x')
        key = current_key(b)
        key isa NameEntry ? toggle_flag!(b, :extended) : (b.status = "No extended help here")
    elseif isplain(ev, '{')
        jump_heading!(b, -1)
    elseif isplain(ev, '}')
        jump_heading!(b, 1)
    elseif isplain(ev, 'w')
        b.layout = LAYOUTS[mod1(findfirst(==(b.layout), LAYOUTS) + 1, length(LAYOUTS))]
        LAYOUT[] = b.layout
        b.status = b.layout === :auto ? "Layout: fit the window" : "Layout: $(b.layout) pane" * (b.layout === :one ? "" : "s")
        write(b.out, "\e[H\e[2J")
        b.prev = String[]
    elseif isplain(ev, 'i')
        b.internal = !b.internal
        b.query === nothing ? refresh_names!(b) : update_search!(b)
        b.status = b.internal ? "Showing internal names" : "Hiding internal names"
    elseif isplain(ev, 'e')
        x = selected_subject(b)
        src = x === nothing ? page_source(b) : source_of(b, x)
        src === nothing ? (b.status = "No source location for that") : edit_source!(b, src)
    elseif isplain(ev, 'o')
        p = page(b, current_key(b), doc_width(b))
        if b.link in eachindex(p.spots) && p.spots[b.link].target isa URLTarget
            open_url(b, p.spots[b.link].target.url)
        else
            b.status = "Pick a web link with ⇥ first"
        end
    elseif isplain(ev, 'y')
        name = current_name(b)
        if name === nothing
            b.status = "Nothing to insert here"
        else
            b.result = name
            b.quit = true
        end
    elseif isplain(ev, 'a')
        all_open = all(r -> r in b.expanded, SECTION_ORDER)
        b.expanded = all_open ? Set(DEFAULT_EXPANDED) : Set(SECTION_ORDER)
        refresh_side!(b)
    elseif isctrl(ev, 'l')
        write(b.out, "\e[H\e[2J")
        b.prev = String[]
    else
        return false
    end
    return true
end

function handle!(b::Browser, ev::PasteEvent)
    text = strip(replace(ev.text, r"\s+" => ' '))
    isempty(text) && return false
    start_search!(b, String(text))
    return true
end

function hit_at(b::Browser, row::Int, col::Int)
    for h in Iterators.reverse(b.hits)
        h.row == row && col in h.cols && return h
    end
    return nothing
end

function pane_at(b::Browser, row::Int, col::Int)
    3 <= row <= b.height - 1 || return nothing
    for (pane, x, w) in layout(b)
        x <= col < x + w && return pane
    end
    return nothing
end

function handle!(b::Browser, ev::MouseEvent)
    if b.dragging && ev.action === :drag
        return drag_selection!(b, ev.row, ev.col)
    elseif b.dragging && ev.action === :release
        return release_selection!(b)
    elseif ev.action === :move || ev.action === :drag
        h = hit_at(b, ev.row, ev.col)
        if ev.action === :drag && h !== nothing && h.action isa Tuple && first(h.action) === :scrollbar
            return click!(b, h, false)
        end
        hover = h === nothing ? nothing : h.action
        hover == b.hover && return false
        b.hover = hover
        return true
    elseif ev.action in (:wheelup, :wheeldown)
        pane = pane_at(b, ev.row, ev.col)
        pane === nothing && return false
        delta = ev.action === :wheelup ? -3 : 3
        if pane === :doc
            scroll_doc!(b, delta)
        else
            ls = pane === :side ? b.side : b.names
            len = pane === :side ? length(b.side_rows) : length(b.name_rows)
            len -= list_offset(b, pane)
            ls.scroll = clamp(ls.scroll + delta, 0, max(0, len - content_rows(b)))
        end
        return true
    elseif ev.action === :press
        ev.button in (8, 9) && (b.help = false)
        ev.button == 8 && return go_back!(b)
        ev.button == 9 && return go_forward!(b)
        # Terminals do not report Cmd, so right-click, Alt-click and Ctrl-click open the source.
        if ev.button == 3 || (ev.button == 1 && ev.mods & (ALT | CTRL) != 0)
            h = hit_at(b, ev.row, ev.col)
            return edit_subject!(b, h === nothing ? nothing : hit_subject(b, h.action))
        end
        ev.button == 1 || return false
        b.status = ""
        b.sel = nothing
        FIRST_OPEN[] = false
        h = hit_at(b, ev.row, ev.col)
        # Clicking anywhere but the search field ends typing in it.
        (h === nothing || !(first(h.action) in (:search, :toggle_text))) && (b.typing = false)
        # Anything that navigates closes the help, so what it shows is not hidden underneath.
        h === nothing || first(h.action) in (:help, :scrollbar, :spot) || (b.help = false)
        if h === nothing
            press_doc!(b, ev.row, ev.col) && return true
            pane = pane_at(b, ev.row, ev.col)
            pane === nothing || (pane === :names && side_placeholder(b) !== nothing) || (b.focus = pane)
            b.help = false
            return true
        end
        now = time()
        double = b.last_click[2] == h.action && now - b.last_click[1] < 0.4
        b.last_click = (now, h.action)
        return click!(b, h, double)
    end
    return false
end

function click!(b::Browser, h::Hit, double::Bool)
    a = h.action
    kind = first(a)
    if kind in (:side, :names)
        pane, i = a
        b.help = false
        ls = pane === :side ? b.side : b.names
        again = b.focus === pane && ls.cursor == i
        ls.cursor = i
        b.focus = pane
        b.source = pane
        b.override = nothing
        pane === :side && sync_side!(b)
        pane === :names && show_heading!(b)
        rows = pane === :side ? b.side_rows : b.name_rows
        header = pane === :side ? is_header(rows[i]) : rows[i].kind in (:header, :page)
        (header || double || (again && pane === :names)) && activate!(b, pane)
    elseif kind === :section
        follow!(b, SectionTarget(a[2]))
    elseif kind === :spot
        p = page(b, current_key(b), doc_width(b))
        _, k = a
        k in eachindex(p.spots) || return false
        # A link in the help leaves the help, for the page underneath.
        b.help ? (b.help = false) : (b.link = k)
        follow!(b, p.spots[k].target)
    elseif kind === :scrollbar
        p = page(b, current_key(b), doc_width(b))
        b.doc_scroll = round(Int, a[2] * max(0, length(p.lines) - content_rows(b)))
    elseif kind === :crumb
        m = a[2]
        m === b.mod && b.query === nothing && return false
        navigate!(b) do
            b.focus = :names
            show_module!(b, m)
        end
    elseif kind === :load
        load_package!(b, a[2])
    elseif kind === :project
        follow!(b, SectionTarget(:direct))
    elseif kind === :toggle_text
        toggle_textsearch!(b)
        b.query === nothing && start_search!(b)
    elseif kind === :focus
        a[2] === :names && side_placeholder(b) !== nothing && return false
        b.focus = a[2]
    elseif kind === :back
        go_back!(b)
    elseif kind === :forward
        go_forward!(b)
    elseif kind === :search
        start_search!(b, something(b.query, ""))
    elseif kind === :help
        b.help = !b.help
    elseif kind === :quit
        b.quit = true
    elseif kind === :key
        return handle!(b, a[2])
    end
    return true
end

## Help ##

const HELP_TEXT = """
This browser shows Julia's manual, under *Manual*, *Base reference* and *Developer docs*, and the docstrings of the loaded modules, with a package's README when it has no module docstring. A package's page links to its own manual page when it ships one, as *manual*, and to its full documentation online as *docs*. Julia's manual is also at <$(julia_docs())>.

# Moving around

  * `↑` `↓` or `j` `k`: move in a list, or scroll the documentation.
  * `←` `→` or `h` `l`: move between the panes. In a submodule, `←` goes up to its parent.
  * `⏎`: open what is selected. On a section heading, fold or unfold it.
  * `⇥` and `⇧⇥` in the documentation: step through its links. `⏎` follows the selected link.
  * `⌫`, `[` or `Alt-←`: go back. `]` or `Alt-→`: go forward.
  * `PgUp` `PgDn` `Space` `g` `G`: page up and down, top and bottom.
  * `{` `}`: jump to the previous or next heading. Long pages start with a list of their headings.
  * `x`: show or hide the extended help of a docstring.
  * `w`: change the layout: three panes, two, one, or fit the window.

# Finding things

  * `/`: search the names of every loaded module. `Plots.pl` searches inside modules matching `Plots`.
  * `Ctrl-T`: switch between searching names and searching the text of docstrings.
  * `Ctrl-A`: include indirect dependencies in searches, or leave them out again.
  * `i`: show or hide internal (non-public) names.
  * `a`: unfold every package section, including indirect dependencies.

# Leaving the browser

  * `y`: close the browser and put the name being viewed at the REPL prompt.
  * ⧉ paste at the prompt, under an example: close the browser and put the example's code at the prompt.
  * `e`: open the source of the selected link or name, or else of the page, in your editor.
  * Right-click, `Alt`-click or `Ctrl`-click: open the source of what is under the pointer.
  * `o`: open the selected web link in your web browser.
  * `q` or `Esc`: close the browser.

# The mouse

Click to select, and click again (or double-click) to open. Click links to follow them, and the breadcrumbs at the top to go up. The wheel scrolls the pane under the pointer, and the back and forward mouse buttons move through history.

Drag across the documentation to select text, which is copied when you let go. Double-click selects a word, and triple-click a line. To select with your terminal instead, hold NATIVE_KEY while you drag.

# What the marks mean

  * `●` exported: available after `using`.
  * `○` public: part of the API, but needs qualifying, as in `Module.name`.
  * `·` internal: not public, and may change without notice.

Dimmed names have no docstring of their own, and struck-out names are deprecated. Mentions of packages, such as `Example.jl`, lead to the package's documentation, to its load page, or to a web search.

# More help

At the REPL, type `?` followed by a name for its documentation, `;` for a shell, and `]` to manage packages.
"""

function help_page(cat::Catalog, width::Int)
    pb = PageBuilder(width)
    addline!(pb)
    add_markdown!(pb, Markdown.parse(replace(HELP_TEXT, "NATIVE_KEY" => native_select_key())), Base, cat)
    addline!(pb)
    return Page(pb)
end

## Entry point ##

const LAYOUT = Ref(:auto)
# Where the browser was when it last closed, so that it opens there again.
const LAST = Ref{Any}(nothing)

"""
    browse([term]; query=nothing, context=Main) -> Union{String, Nothing}
    browse([term,] x)

Open the documentation browser on `term`. It shows every loaded module, split into the
active project's direct dependencies, Julia's own modules, the standard library and
indirect dependencies, along with each module's names and their documentation.

`query` picks where to start: a name such as `"sum"` or `"Base.Iterators"` opens its
documentation, and anything else starts a search. Passing an object `x` instead opens the
documentation of `x`: a module, a function, a type, or else the type of `x`. With neither,
the browser opens where it was last closed.

Returns the name or the code chosen with `y` or a paste link, or `nothing`.
"""
function browse(term::REPL.Terminals.TextTerminal = REPL.TerminalMenus.default_terminal();
                query::Union{Nothing,AbstractString} = nothing, context::Module = Main)
    b = Browser(term)
    if query !== nothing && !isempty(strip(query))
        t = resolve_ref(query, context, b.cat)
        if t === nothing || t isa URLTarget
            start_search!(b, String(strip(query)))
        else
            follow!(b, t)
            empty!(b.back)
        end
    else
        reopen!(b)
    end
    return run!(b)
end

browse(x; context::Module = Main) = browse(REPL.TerminalMenus.default_terminal(), x; context)

function browse(term::REPL.Terminals.TextTerminal, x; context::Module = Main)
    x isa AbstractString && return browse(term; query = x, context)
    b = Browser(term)
    t = object_target(x)
    if t !== nothing
        follow!(b, t)
        empty!(b.back)
    end
    return run!(b)
end

function object_target(@nospecialize x)
    x isa Module && return ModuleTarget(x)
    if x isa Function
        m, s = parentmodule(x), nameof(x)
        visible_binding(m, s) && getglobal(m, s) === x && return NameTarget(m, s)
    end
    return type_target(x isa Type ? x : typeof(x))
end

function reopen!(b::Browser)
    last = LAST[]
    last === nothing && return
    mod, name, focus = last
    try
        show_module!(b, mod; entry = name)
        b.focus = focus
        focus === :side && (b.source = :side)
    catch
    end
end

function await_release(io::IO)
    while true
        ev = try
            read_event(io)
        catch
            return
        end
        ev isa MouseEvent && ev.action in (:drag, :move) && continue
        return
    end
end

function drain_input(io::IO)
    try
        while wait_bytes(io, 0.05)
            read_event(io)
        end
    catch
    end
end

function run!(b::Browser)
    enter_screen(b)
    timer = Timer(0.25; interval=0.25) do _
        try
            @lock b.lock begin
                # Packages loaded from the browser are only visible in the latest world.
                b.quit || (check_size!(b) && Base.invokelatest(draw, b))
            end
        catch
        end
    end
    mouse_exit = false
    try
        @lock b.lock draw(b)
        while !b.quit
            ev = try
                read_event(b.in)
            catch err
                err isa EOFError || rethrow()
                break
            end
            ev === nothing && continue
            @lock b.lock begin
                changed = try
                    Base.invokelatest(handle!, b, ev)
                catch err
                    err isa InterruptException && rethrow()
                    b.status = "Error: " * first(split(sprint(showerror, err), '\n'))
                    true
                end
                changed && !b.quit && Base.invokelatest(draw, b)
            end
            # A click that closes the browser is followed by its release, which would
            # otherwise be typed at the prompt.
            mouse_exit = b.quit && ev isa MouseEvent
            mouse_exit && ev.action === :press && await_release(b.in)
        end
    finally
        close(timer)
        leave_screen(b, mouse_exit)
    end
    key = current_key(b)
    LAST[] = (b.mod, key isa NameEntry ? key.name : nothing, b.focus)
    return b.result
end
