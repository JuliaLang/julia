# This file is a part of Julia. License is MIT: https://julialang.org/license

# The manual: the Markdown pages that Julia's HTML documentation is built from, installed in
# `share/doc/julia/src`, and the manual pages of the standard libraries.

# Where the Markdown source of the manual is installed. A source build links it there too.
manual_dir() = normpath(Sys.BINDIR, Base.DOCDIR, "src")

"""
    manual_pages(dir) -> Vector{ManualPage}

The pages of the manual, in the order that `pages.toml` gives. Pages that are listed but not
installed are left out.
"""
function manual_pages(dir::String=manual_dir())
    pages = ManualPage[]
    toml = joinpath(dir, "pages.toml")
    isfile(toml) || return pages
    d = try
        Base.parsed_toml(toml)
    catch
        return pages
    end
    for (key, role) in (("manual", :manual), ("base", :basedocs))
        for rel in get(d, key, String[])
            add_manual_page!(pages, joinpath(dir, rel), role, "")
        end
    end
    # The standard libraries ship their manual pages with their source.
    stdlibs = ManualPage[]
    for name in readdir(Sys.STDLIB)
        name == "Pkg" && continue # it has a section of its own
        path = stdlib_page(name)
        path === nothing || add_manual_page!(stdlibs, path, :stdlibdocs, "")
    end
    solvers = joinpath(Sys.STDLIB, "SparseArrays", "docs", "src", "solvers.md")
    isfile(solvers) && add_manual_page!(stdlibs, solvers, :stdlibdocs, "")
    append!(pages, sort_by!(stdlibs, p -> lowercase(p.title)))
    for file in pkg_doc_files()
        add_manual_page!(pages, joinpath(pkg_docs_dir(), file), :pkgdocs, "")
    end
    for g in get(d, "devdocs", Any[])
        g isa AbstractDict || continue
        for rel in get(g, "pages", String[])
            add_manual_page!(pages, joinpath(dir, rel), :devdocs, String(get(g, "title", "")))
        end
    end
    return pages
end

# The manual page of a standard library, as the HTML manual picks it.
function stdlib_page(name::AbstractString)
    src = joinpath(Sys.STDLIB, name, "docs", "src")
    return isfile(joinpath(src, "index.md")) ? joinpath(src, "index.md") : nothing
end

pkg_docs_dir() = joinpath(Sys.STDLIB, "Pkg", "docs", "src")

# The pages of Pkg's manual, in the order that its documentation build lists them.
function pkg_doc_files()
    dir = pkg_docs_dir()
    isdir(dir) || return String[]
    make = joinpath(dir, "..", "make.jl")
    listed = isfile(make) ? [m[1] for m in eachmatch(r"\"([\w-]+\.md)\"", read(make, String))] : String[]
    # Some pages are made by the build, so are not installed.
    files = filter(f -> isfile(joinpath(dir, f)), listed)
    isempty(files) || return files
    files = filter(f -> endswith(f, ".md") && f != "basedocs.md", readdir(dir))
    return sort_by!(files, f -> (f != "index.md", f))
end

# A link to Pkg's online manual, as the page that is installed.
function pkgdocs_target(url::AbstractString)
    m = match(r"^https?://pkgdocs\.julialang\.org/[^/#]+/?(?:([\w-]+)/?)?(?:#(.*))?$", url)
    m === nothing && return nothing
    path = normpath(pkg_docs_dir(), something(m[1], "index") * ".md")
    return isfile(path) ? ManualTarget(path, something(m[2], "")) : nothing
end

function add_manual_page!(pages::Vector{ManualPage}, path::String, role::Symbol, group::String)
    isfile(path) || return
    path = normpath(path) # so that it compares equal to the targets of links
    headings = scan_headings(path; limit=1)
    # A standard library's page goes by the library's name, which is what people look for.
    title = if role === :stdlibdocs && basename(path) != "solvers.md"
        basename(dirname(dirname(dirname(path))))
    else
        isempty(headings) ? splitext(basename(path))[1] : headings[1].text
    end
    # Pkg numbers its chapters, which would skip the pages its build makes.
    role === :pkgdocs && (title = replace(title, r"^(\*\*)?\d+\.(\*\*)?\s+" => ""))
    push!(pages, ManualPage(path, title, role, group))
end

const HEADING = r"^(#{1,6})\s+(.*?)\s*#*\s*$"
const ID_HEADING = r"^\[(.*)\]\(@id\s+([^)]+)\)$"

# The headings of a page, found quickly without parsing it, with their lines. This is close to
# what the page renders, and is used to search and to find pages; what a page shows comes from
# parsing it. With `limit`, stop after that many.
scan_headings(path::String; limit::Int=typemax(Int)) = open(io -> scan_headings(io, limit), path)

function scan_headings(io::IO, limit::Int)
    headings = Heading[]
    fence = ""
    prev = ""
    for (n, line) in enumerate(eachline(io))
        if !isempty(fence)
            # As in the Markdown parser, a fence closes with a line that starts with it exactly.
            startswith(line, fence) && !startswith(line, fence * fence[1]) && (fence = "")
            continue
        end
        m = match(r"^ {0,3}(```+|~~~+)(.*)$", line)
        # A backtick fence cannot have backticks after it, so that is inline code.
        if m !== nothing && !(startswith(m[1], '`') && occursin('`', m[2]))
            fence = m[1]
            prev = ""
            continue
        end
        m = match(HEADING, line)
        if m !== nothing
            push_heading!(headings, length(m[1]), m[2], n)
        elseif occursin(r"^(=+|-+)\s*$", line) && !isempty(strip(prev)) && !startswith(lstrip(prev), r"[-*+>|]")
            # an underlined heading
            push_heading!(headings, startswith(line, '=') ? 1 : 2, strip(prev), n - 1)
        end
        length(headings) >= limit && break
        prev = line
    end
    return headings
end

function push_heading!(headings::Vector{Heading}, level::Int, text::AbstractString, line::Int)
    id = ""
    idm = match(ID_HEADING, text)
    idm === nothing || ((text, id) = (idm[1], strip(idm[2])))
    # Headings are shown as plain text, without the Markdown of their links and code.
    text = replace(replace(text, r"\[([^\]]*)\]\([^)]*\)" => s"\1"), "`" => "", "**" => "", "__" => "")
    push!(headings, Heading(level, strip(text), id, line))
end

"""
    page_headings(cat, path) -> Vector{Heading}

The headings of a page as it is shown: those of its parsed Markdown, in order, without those
that have no letters or digits, which are not shown as headings. These line up with the
headings of the rendered page, which the names pane and links scroll to.
"""
function page_headings(cat::Catalog, path::String)
    get!(cat.headings, path) do
        isfile(path) || return Heading[]
        md = try
            Markdown.parse(read(path, String))
        catch
            return Heading[]
        end
        headings = Heading[]
        walk_md(md) do x
            x isa Markdown.Header || return
            text, id = x.text, ""
            if length(text) == 1 && text[1] isa Markdown.Link && startswith(text[1].url, "@id")
                id = String(strip(chopprefix(text[1].url, "@id")))
                text = text[1].text
            end
            t = String(strip(replace(md_text(text), r"\s+" => ' ')))
            any(c -> isletter(c) || isdigit(c), t) && push!(headings, Heading(heading_level(x), t, id, 0))
        end
        headings
    end
end

heading_level(::Markdown.Header{l}) where {l} = l

# The place among a page's shown headings of the heading with this text, or 0.
function heading_index(cat::Catalog, path::String, text::AbstractString)
    isempty(text) && return 0
    return something(findfirst(h -> h.text == text, page_headings(cat, path)), 0)
end

"""
    manual_anchors(cat) -> Dict{String,String}

The page of every heading of the manual, by its `@id`, its text and its slug: the names that
Documenter's `@ref` links use for a heading.
"""
function manual_anchors(cat::Catalog)
    cat.anchors === nothing || return cat.anchors
    anchors = Dict{String,String}()
    for p in cat.manual
        for h in scan_headings(p.path)
            isempty(h.id) || get!(anchors, h.id, p.path)
            get!(anchors, h.text, p.path)
            get!(anchors, slugify(h.text), p.path)
        end
    end
    cat.anchors = anchors
    return anchors
end

# How Documenter turns a heading into the name that `@ref` and `#` links use.
function slugify(s::AbstractString)
    s = replace(s, r"\s+" => "-", "&" => "-and-")
    s = replace(s, r"[^\p{L}\p{P}\d\-]+" => "")
    return String(strip(replace(s, r"\-\-+" => "-"), '-'))
end

# The heading of page `path` that `anchor` names, as a place among its headings, or 0 for the top.
function anchor_heading(cat::Catalog, path::String, anchor::String)
    isempty(anchor) && return 0
    for (k, h) in enumerate(page_headings(cat, path))
        (h.id == anchor || h.text == anchor || slugify(h.text) == anchor) && return k
    end
    return 0
end

manual_page_of(cat::Catalog, path::String) = findfirst(p -> p.path == normpath(path), cat.manual)

# The first page of a package's Documenter manual, which standard libraries and many packages
# ship with their source.
function package_manual(m::Module)
    dir = pkgdir(m)
    dir === nothing && return nothing
    in_stdlib_dir(dir) && return stdlib_page(basename(dir))
    path = joinpath(dir, "docs", "src", "index.md")
    return isfile(path) ? normpath(path) : nothing
end

# The module that names on a manual page are looked up in: the standard library it documents,
# or else `Main`.
function manual_context(path::String)
    in_stdlib_dir(path) || return Main
    name = first(splitpath(relpath(path, normpath(Sys.STDLIB))))
    for m in Base.loaded_modules_array()
        String(nameof(m)) == name && return m
    end
    return Main
end

# Links between manual pages point at their files, as only file and web links survive rendering.
const MANUAL_LINK = "file://"
manual_url(path::AbstractString, anchor::AbstractString="") = string(MANUAL_LINK, path, '#', anchor)

# The page and anchor of a link to a Markdown file, or `nothing`. The anchor follows the last
# `#`, as a path may have one too.
function manual_target(url::AbstractString)
    startswith(url, MANUAL_LINK) || return nothing
    s = chopprefix(url, MANUAL_LINK)
    endswith(s, ".md") && return ManualTarget(String(s), "")
    i = findlast('#', s)
    i === nothing && return nothing
    path = s[1:prevind(s, i)]
    return endswith(path, ".md") ? ManualTarget(String(path), String(s[nextind(s, i):end])) : nothing
end

is_manual_link(url::AbstractString) = manual_target(url) !== nothing

"""
    documenter!(md, path, ctx, cat) -> md

Turn the Documenter syntax of a manual page into plain Markdown that the browser can show:

  - `@docs` blocks become lists of the names they document, which are links;
  - `@example` and `@repl` blocks become Julia code, without their output;
  - blocks that only set up the build, such as `@meta`, `@setup` and `@eval`, are left out;
  - headings lose their `@id`, and `@ref` links to headings, and links to other pages, lead
    to those pages.
"""
function documenter!(md::Markdown.MD, path::String, ctx::Module, cat::Catalog)
    md.content = documenter_blocks(md.content)
    walk_md(md) do x
        x isa Markdown.Link && (x.url = manual_link(x, path, ctx, cat))
    end
    return md
end

function documenter_blocks(blocks::Vector)
    out = Any[]
    for x in blocks
        if x isa Markdown.Code && startswith(x.language, "@")
            lang = first(split(x.language))
            if lang in ("@example", "@repl")
                push!(out, Markdown.Code("julia", x.code))
            elseif lang == "@docs"
                items = Any[]
                for line in split(x.code, '\n')
                    name = strip(line)
                    isempty(name) || push!(items, Any[Markdown.Paragraph(Any[Markdown.Code(String(name))])])
                end
                isempty(items) || push!(out, Markdown.List(items, -1, false))
            end
        elseif x isa Markdown.Header
            if length(x.text) == 1 && x.text[1] isa Markdown.Link && startswith(x.text[1].url, "@id")
                x.text = x.text[1].text
            end
            push!(out, x)
        elseif x isa Markdown.Admonition
            x.content = documenter_blocks(x.content)
            push!(out, x)
        else
            push!(out, x)
        end
    end
    return out
end

function manual_link(link::Markdown.Link, path::String, ctx::Module, cat::Catalog)
    url = link.url
    if startswith(url, "@ref")
        target = strip(strip(chopprefix(url, "@ref")), '"')
        key = String(isempty(target) ? strip(replace(md_text(link.text), r"\s+" => ' ')) : target)
        # As in Documenter, a link written as code names a docstring, and one in words a heading.
        code = length(link.text) == 1 && link.text[1] isa Markdown.Code
        anchors = manual_anchors(cat)
        page = code ? nothing : something(get(anchors, key, nothing), get(anchors, slugify(key), nothing), Some(nothing))
        page === nothing || return manual_url(page, key)
        resolve_ref(strip(key, '`'), ctx, cat) === nothing || return url
        page = something(get(anchors, key, nothing), get(anchors, slugify(key), nothing), Some(nothing))
        page === nothing || return manual_url(page, key)
        return anchor_heading(cat, path, key) == 0 ? url : manual_url(path, key)
    end
    t = pkgdocs_target(url)
    t === nothing || return manual_url(t.path, t.anchor)
    occursin(r"^[a-z][a-z0-9+.-]*:"i, url) && return url
    file, anchor = occursin('#', url) ? split(url, '#'; limit=2) : (url, "")
    file = isempty(file) ? path : normpath(dirname(path), file)
    endswith(file, ".md") && isfile(file) || return url
    return manual_url(file, anchor)
end

"""
    manual_page(cat, path, width) -> Page

A manual page, with its Documenter syntax made plain.
"""
function manual_page(cat::Catalog, path::String, width::Int)
    pb = PageBuilder(width)
    addline!(pb)
    ctx = manual_context(path)
    if in_stdlib_dir(path) && ctx === Main
        # The page of a standard library that is not loaded: its names can only be links once it is.
        name = first(splitpath(relpath(path, normpath(Sys.STDLIB))))
        id = Base.identify_package(name)
        if id !== nothing
            addline!(pb, "  ", ("$name is not loaded, so the names on this page are not links.  ", :REPL_DocBrowser_dim),
                     (" Load $name ", :REPL_DocBrowser_link_active, LoadTarget(id)))
            addline!(pb)
        end
    end
    md = Markdown.parse(read(path, String))
    documenter!(md, path, ctx, cat)
    add_markdown!(pb, md, ctx, cat)
    addline!(pb)
    return Page(pb, SourceTarget(path, 1))
end

## Searching the manual ##

# How well `q` matches `s`, both lowercase: the whole of it, its start, a word in it, or any part.
function match_score(q::AbstractString, s::AbstractString)
    s == q && return 300
    startswith(s, q) && return 200
    occursin(" " * q, s) && return 150
    return occursin(q, s) ? 100 : 0
end

# A page's short name: the standard library it documents, or else its file. Pkg's pages are
# named for Pkg too, so that searching for it finds them.
function page_name(p::ManualPage)
    p.role === :stdlibdocs && return basename(dirname(dirname(dirname(p.path))))
    name = splitext(basename(p.path))[1]
    p.role === :pkgdocs || return name
    return name == "index" ? "Pkg" : "Pkg " * name
end

"""
    search_manual(cat, query; text=false) -> Vector{ManualHit}

The pages and headings of the manual that match `query`, best first. Pages come before
headings. With `text`, the words of each section are searched instead, as for docstrings.
"""
function search_manual(cat::Catalog, query::AbstractString; text::Bool=false, limit::Int=30)
    q = lowercase(strip(query))
    hits = ManualHit[]
    isempty(q) && return hits
    if text
        words = split(q)
        for (path, heading, body) in manual_sections(cat)
            all(w -> occursin(w, body), words) || continue
            score = sum(w -> count(w, body), words) + (occursin(q, body) ? 50 : 0)
            push!(hits, ManualHit(path, heading, score))
        end
    else
        for p in cat.manual
            # A match on the title counts for more than one on the file name.
            s = max(match_score(q, lowercase(p.title)), match_score(q, lowercase(page_name(p))) - 50)
            s > 0 && push!(hits, ManualHit(p.path, "", 1000 + s))
            for (k, h) in enumerate(index_headings(cat, p.path))
                k == 1 && h.level == 1 && continue # the title
                s = match_score(q, lowercase(h.text))
                s > 0 && push!(hits, ManualHit(p.path, h.text, s))
            end
        end
    end
    sort_by!(hits, h -> -h.score)
    length(hits) > limit && resize!(hits, limit)
    return hits
end

# The lowercased text of every section of the manual, with its page and heading.
function manual_sections(cat::Catalog)
    cat.sections === nothing || return cat.sections
    sections = Tuple{String,String,String}[]
    for p in cat.manual
        lines = readlines(p.path)
        hs = index_headings(cat, p.path)
        starts = [1; Int[h.line for h in hs]]
        for k in 0:length(hs)
            stop = k < length(hs) ? starts[k+2] - 1 : length(lines)
            body = lowercase(join(view(lines, starts[k+1]:stop), ' '))
            push!(sections, (p.path, k == 0 ? "" : hs[k].text, body))
        end
    end
    cat.sections = sections
    return sections
end

hit_title(cat::Catalog, h::ManualHit) = isempty(h.heading) ? manual_title(cat, h.path) : h.heading

# The heading a hit is at, among those its page shows.
hit_heading(cat::Catalog, h::ManualHit) = heading_index(cat, h.path, h.heading)

# The line of the file that a hit's heading is on.
function hit_line(cat::Catalog, h::ManualHit)
    i = findfirst(x -> x.text == h.heading, index_headings(cat, h.path))
    return i === nothing ? 1 : index_headings(cat, h.path)[i].line
end

# The scanned headings of a page, kept for searching.
index_headings(cat::Catalog, path::String) = get!(() -> scan_headings(path), cat.index_headings, path)

function manual_title(cat::Catalog, path::String)
    i = manual_page_of(cat, path)
    return i === nothing ? basename(path) : cat.manual[i].title
end
