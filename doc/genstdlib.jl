using .Markdown
using Base.Markdown: MD

cd(dirname(@__FILE__))

isop(func) = ismatch(r"[^\w@!.]|^!$", func)

ident(mod, x) = "$mod.$(isop(x) ? "(:($x))" : x)"

getdoc(mod, x) = try eval(parse("@doc $(ident(mod, x))")) catch e end

flat_content(md) = md
flat_content(xs::Vector) = reduce((xs, x) -> vcat(xs,flat_content(x)), [], xs)
flat_content(md::MD) = flat_content(md.content)
flatten(md::MD) = MD(flat_content(md))

# issig(md) = isa(md, Markdown.Code) && length(split(md.code, "\n")) == 1

# function splitsig(md)
#   md = flatten(md)
#   sig = nothing
#   if !isempty(md.content) && issig(md.content[1])
#       sig = shift!(md.content)
#   end
#   return md, sig
# end

isrst(md) =
    length(flatten(md).content) == 1 &&
    isa(flatten(md).content[1], Markdown.Code) &&
    flatten(md).content[1].language == "rst"

torst(md) = isrst(md) ? flatten(md).content[1].code : Markdown.rst(md)

function remsig(l, doc)
    sig = l[15:end]
    ls = split(doc, "\n")
    contains(ls[2], sig) && (ls = ls[3:end])
    while ls[1] == "" shift!(ls) end
    startswith(ls[1], " "^11) && (ls = ["::", ls...])
    join(ls, "\n")
end

function translate(file)
    @assert isfile(file)
    ls = split(readall(file), "\n")[1:end-1]
    doccing = false
    func = nothing
    mod = "Base"

    open(file, "w+") do io
        for l in ls
            if ismatch(r"^\.\. (current)?module::", l)
                mod = match(r"^\.\. (current)?module:: ([\w\.]+)", l).captures[2]
                println(io, l)
            elseif startswith(l, ".. function::")
                func = match(r".. function:: (@?[^\(\s\{]+)", l)
                func == nothing && (warn("bad function $l"); continue)
                func = func.captures[1]
                doc = getdoc(mod, func)

                if doc == nothing || !isrst(doc)
                    doc == nothing ?
                        info("no docs for $(ident(mod, func))") :
                        info("can't convert docs for $(ident(mod, func))")
                    println(io, l)
                    doccing = false
                    continue
                end

                doccing = true
                println(io, l)
                println(io)
                for l in split(remsig(l, torst(doc)), "\n")
                    ismatch(r"^\s*$", l) ? println(io) : println(io, "   ", l)
                end
                println(io)
            elseif doccing && (startswith(l, "   ") || ismatch(r"^\s*$", l))
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
