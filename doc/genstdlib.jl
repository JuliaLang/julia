using .Markdown
using Base.Markdown: MD

cd(dirname(@__FILE__))

isop(func) = ismatch(r"[^\w@!.]|^!$", func)

ident(mod, x) = "$mod.$(isop(x) ? "(:($x))" : x)"

all_docs = ObjectIdDict()

function add_all_docs(it)
    for (k, v) in it
        all_docs[v] = k
    end
end

function add_all_docs(it, k)
    for (_, v) in it
        all_docs[v] = k
    end
end

function sym_exported(obj::ANY, m, exports)
    if isa(obj, Union{Function,DataType,Module})
        return symbol(string(obj)) in exports
    elseif isa(obj, IntrinsicFunction)
        # Only example seems to be cglobal for now
        return true
    elseif isa(obj, Docs.Binding)
        return (obj::Docs.Binding).var in exports
    else
        return false
    end
end

function add_all_docs_meta(m::Module,meta)
    exports = names(m)
    for (obj, d) in meta
        isexported = sym_exported(obj, m, exports)
        if isa(d, Base.Docs.FuncDoc) || isa(d, Base.Docs.TypeDoc)
            add_all_docs(d.meta, (obj, isexported))
        else
            all_docs[d] = (obj, isexported)
        end
    end
end

mod_added = ObjectIdDict()

function add_all_docs_mod(m::Module)
    mod_added[m] = m
    try
        add_all_docs_meta(m,Docs.meta(m))
    end
    for name in names(m, true)
        try
            sub_m = getfield(m,name)
            isa(sub_m, Module) || continue
            if !(sub_m in keys(mod_added))
                add_all_docs_mod(sub_m)
            end
        end
    end
end

add_all_docs_mod(Base)
# Most of the keywords are not functions and they are easy to check by hand
# add_all_docs(Docs.keywords)

println("Collect $(length(all_docs)) Docs")

function find_docs(v)
    docs = []
    for mod in keys(mod_added)
        try
            meta = Docs.meta(mod)[v]
            if isa(meta, Base.Docs.FuncDoc) || isa(meta, Base.Docs.TypeDoc)
                append!(docs, collect(values(meta.meta)))
            else
                push!(docs, meta)
            end
        end
    end
    if isempty(docs)
        error("Cannot find doc for $v")
    end
    return docs
end

function getdoc(mod, x)
    try
        x = unescape_string(x)
        if symbol(x) in keys(Docs.keywords)
            return Any[Docs.keywords[symbol(x)]]
        end
        if x[1] != '@'
            v = eval(parse(ident(mod, x)))
            isa(v, Colon) && (v = Base.colon)
            return find_docs(v)
        else
            for m in [eval(parse(mod)); collect(keys(mod_added));]
                try
                    v = Docs.Binding(m, symbol(x))
                    return find_docs(v)
                end
            end
            error("Cannot find doc for $x")
        end
    catch e
        println(e)
        warn("Mod $mod $x")
    end
    []
end

flat_content(md) = md
flat_content(xs::Vector) = reduce((xs, x) -> vcat(xs,flat_content(x)), [], xs)
flat_content(md::MD) = flat_content(md.content)

flatten(md::MD) = MD(flat_content(md))
function flatten(doc::Base.DocObj)
    if isa(doc.content, MD)
        md = doc.content
    elseif doc.parser == :md || doc.parser == :Markdown
        md = Markdown.parse(doc.content)
    elseif doc.parser == :catdoc
        md = MD(map(flatten, doc.content)...)
    elseif doc.parser == :string
        md = MD(doc.content)
    else
        assert("can't flatten unknown format $doc")
    end
    return MD(flat_content(MD))
end

isrst(md) =
    length(flatten(md).content) == 1 &&
    isa(flatten(md).content[1], Markdown.Code) &&
    flatten(md).content[1].language == "rst"

function tryrst(md, remove_decl)
    try
        if remove_decl && isa(md.content[1], Markdown.Code) && md.content[1].language == ""
            shift!(md.content)
        end
        return Markdown.rst(md)
    catch e
        warn("Error converting docstring:")
#        display(md)
        println(e)
        return
    end
end

torst(md,remove_decl) = isrst(md) ? flatten(md).content[1].code : tryrst(md, remove_decl)

function split_decl_rst(md, decl)
    if isrst(md)
        rst_text = flatten(md).content[1].code
        ls = split(rst_text, "\n")
        body_start = 1
        if startswith(ls[1], ".. ") && !endswith(ls[1], "::")
            decl = ".. function:: " * replace(ls[1], r"^.. *", "")
            body_start += 1
            while startswith(ls[body_start], "   ")
                decl *= replace(ls[body_start], r"^ *", "\n              ")
                body_start += 1
            end
            while ls[body_start] == ""
                body_start += 1
            end
            return decl, join(ls[body_start:end], "\n")
        end
        return decl, rst_text
    else
        if isa(md.content[1], Markdown.Code) && md.content[1].language == ""
            decl = ".. function:: " * replace(shift!(md.content).code, "\n",
                                              "\n              ")
        end
        return decl, Markdown.rst(md)
    end
end

# Disable by default since it is hard to eliminate false positives
const warn_doc_between_func = "JULIA_WARN_DOC_BETWEEN_FUNC" in keys(ENV)

function translate(file)
    @assert(isfile(file))
    ls = split(readall(file), "\n")[1:end-1]
    doccing = false
    func = nothing
    mod = "Base"
    modidx = -1
    open(file, "w+") do io
        has_func_doc = false
        missing_func_doc = []
        cur_func = ""
        function warn_missing_func_doc()
            if (warn_doc_between_func && has_func_doc &&
                !isempty(missing_func_doc))
                doc_str = join(missing_func_doc, "\n")
                warn("Possible missing document for `$cur_func` in `$file`:")
                info(doc_str)
            end
            missing_func_doc = []
        end
        function start_new_section()
            warn_missing_func_doc()
            has_func_doc = false
        end
        function start_func_doc(func_line)
            warn_missing_func_doc()
            cur_func = func_line
            has_func_doc = true
        end
        function push_non_func_line(l)
            has_func_doc || return
            if (startswith(l, ".. data:: ") ||
                startswith(l, ".. _") ||
                ismatch(r"^[A-Z][a-z]* implemented", l))
                start_new_section()
            elseif (startswith(l, "----") ||
                    startswith(l, "====") ||
                    startswith(l, "~~~~"))
                if !isempty(missing_func_doc)
                    pop!(missing_func_doc)
                    start_new_section()
                end
            end
            has_func_doc || return
            push!(missing_func_doc, l)
        end
        for (i,l) in enumerate(ls)
            if ismatch(r"^\.\. (current)?module::", l)
                start_new_section()
                mod = match(r"^\.\. (current)?module:: ([\w\.]+)", l).captures[2]
                modidx = i
                println(io, l)
            elseif startswith(l, ".. function::")
                func = match(r"^\.\. function:: (@?[^\(\s\{]+)(.*)", l)
                func == nothing && (warn("bad function $l"); continue)
                funcname = func.captures[1]
                full = funcname * func.captures[2]
                if !('(' in full || '@' in full)
                    ex = parse(full)
                    if isa(ex, Expr)
                        if (ex.head == :(||) || ex.head == :(&&))
                            funcname = string(ex.head)
                        elseif ex.head == :macrocall
                            funcname = string(ex.args[1])
                        end
                    end
                end
                doc = nothing
                for mdoc in getdoc(mod, funcname)
                    trst = tryrst(mdoc, false)
                    trst !== nothing || continue
                    if contains(replace(trst, r"[\n ][\n ]+", " "),
                                " " * replace(full, r"[\n ][\n ]+", " "))
                        if doc != nothing
                            error("duplicate $full $l")
                        end
                        doc = mdoc
                    else
                        #@show trst full
                    end
                end
                if doc == nothing || torst(doc, false) == nothing
                    info("$file: no docs for $full in $mod")
                    println(io, l)
                    doccing = false
                    start_new_section()
                    continue
                end
                delete!(all_docs, doc)
                doccing = true
                start_func_doc(full)
                decl, body = split_decl_rst(doc, l)
                println(io, decl)
                println(io)
                println(io, "   .. Docstring generated from Julia source\n")
                for l in split(body, "\n")
                    ismatch(r"^\s*$", l) ? println(io) : println(io, "   ", l)
                end
                isrst(doc) && println(io)
            elseif doccing && (ismatch(r"^\s+", l) || ismatch(r"^\s*$", l))
                modidx == i-1 && println(io)
            else
                doccing = false
                push_non_func_line(l)
                println(io, l)
            end
        end
    end
end

for folder in ["stdlib", "manual", "devdocs"]
    println("\nConverting $folder/\n")
    for file in readdir("$folder")
        translate("$folder/$file")
    end
end

println()

exported_missing_count = 0
unexported_missing_count = 0

for (d, v) in all_docs
    if length(v) == 2
        val,isexported = v
    else
        val,isexported = v,true
    end
    isa(val, ObjectIdDict) && continue # No idea what these are
    isa(val, Int) && continue # We don't document `0` as a function
    if isexported
        warn("Exported method missing doc for $val")
        exported_missing_count += 1
    else
        # info("Unexported method missing doc for $val")
        unexported_missing_count += 1
    end
    # println(tryrst(d, false))
    # # Generate todo list ;-p
    # println("- [ ] `$v`")
end

if (exported_missing_count + unexported_missing_count) > 0
    println()
    warn("Missing $exported_missing_count exported doc strings")
    info("Missing $unexported_missing_count unexported doc strings")
end
