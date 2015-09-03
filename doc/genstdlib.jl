using .Markdown
using Base.Markdown: MD

cd(dirname(@__FILE__))

isop(func) = ismatch(r"[^\w@!.]|^!$", func)

ident(mod, x) = "$mod.$(isop(x) ? "(:($x))" : x)"

function getdoc(mod, x)
    try
        x = unescape_string(x)
        if symbol(x) in keys(Docs.keywords)
            return Any[Docs.keywords[symbol(x)]]
        end
        v = if x[1] != '@'
            eval(parse(ident(mod, x)))
        else
            Docs.Binding(eval(parse(mod)), symbol(x))
        end
        if isa(v, Colon)
            v = Base.colon
        end
        M = Docs.meta(Base)
        if isa(M[v], Base.Docs.FuncDoc) || isa(M[v], Base.Docs.TypeDoc)
            return collect(values(M[v].meta))
        else
            return Any[M[v]]
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

function translate(file)
    @assert(isfile(file))
    ls = split(readall(file), "\n")[1:end-1]
    doccing = false
    func = nothing
    mod = "Base"
    modidx = -1
    open(file, "w+") do io
        for (i,l) in enumerate(ls)
            if ismatch(r"^\.\. (current)?module::", l)
                mod = match(r"^\.\. (current)?module:: ([\w\.]+)", l).captures[2]
                modidx = i
                println(io, l)
            elseif startswith(l, ".. function::")
                func = match(r"^\.\. function:: (@?[^\(\s\{]+)(.*)", l)
                func == nothing && (warn("bad function $l"); continue)
                funcname = func.captures[1]
                rest = func.captures[2]
                full = funcname*rest
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
                    info("no docs for $(ident(mod, funcname)) $rest")
                    println(io, l)
                    doccing = false
                    continue
                end
                doccing = true
                println(io, l)
                println(io)
                println(io, "   .. Docstring generated from Julia source\n")
                for l in split(torst(doc,true), "\n")
                    ismatch(r"^\s*$", l) ? println(io) : println(io, "   ", l)
                end
                isrst(doc) && println(io)
            elseif doccing && (startswith(l, "   ") || ismatch(r"^\s*$", l))
                modidx == i-1 && println(io)
            else
                doccing = false
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
