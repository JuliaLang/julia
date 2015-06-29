cd(dirname(@__FILE__))

# Load docs from RST into memory

typealias Docs Dict{UTF8String, Vector{UTF8String}}

function parsedoc!(docs::Dict{UTF8String, Docs}, file)
    doccing = false
    mod = "Base"
    func = ""
    doc = IOBuffer()
    for l in split(readall(file), "\n")

        if doccing && (startswith(l, "   ") || ismatch(r"^\s*$", l))
            println(doc, startswith(l, "   ") ? l[4:end] : l)
        else
            doccing = false
            func != "" &&
            push!(get!(get!(docs, mod, Docs()), func, UTF8String[]), rstrip(takebuf_string(doc)))
            func = ""
        end

        if startswith(l, ".. function::")
            name = match(r"^\.\. function:: (@?[^\(\s\{]+)", l)
            name == nothing && (warn("bad function $l"); continue)
            doccing = true
            func = name.captures[1]
            println(doc, "::")
            println(doc, " "^11, l[15:end])
        elseif ismatch(r"^\.\. (current)?module::", l)
            mod = match(r"^\.\. (current)?module:: ([\w\.]+)", l).captures[2]
        end
    end
    func != "" &&
    push!(get!(get!(docs, mod, Docs()), func, UTF8String[]), rstrip(takebuf_string(doc)))
    return docs
end

function alldocs()
    docs = Dict{UTF8String, Docs}()
    for folder in ["stdlib", "manual", "devdocs"]
        for f in readdir(folder)
            parsedoc!(docs, "$folder/$f")
        end
    end
    return docs
end

map(kv->length(kv[2]), alldocs()) |> sum
map(kv -> sum(map(kv -> length(kv[2]), kv[2])), alldocs()) |> sum

# Dump in helpdb.jl

exceptions = ["ans", "CPU_CORES", "JULIA_HOME", "STDOUT", "STDERR", "STDIN",
              "help", "apropos", "Help", "x", "build_sysimg", ".\\\\", "\\:",
              "\\", "\\\\", "munmap", "mmap", "FormatMessage", "GetLastError"]

qualify = ["ccall", "in", "<:", "|>", "*", "\\", "*", "/", "^", ".+", ".-", ".*",
           "./", ".\\", ".^", "//", "<<", ">>", ">>>", "==", "!=", "===", "!==",
           "<", "<=", ">", ">=", ".==", ".!=", ".<", ".<=", ".>", ".>=", "|", "*",
           "^", ":", "!"]

isop(func) = ismatch(r"[^\w@!.]|^!$", func)

identifier(mod, func) =
    func in qualify ? "$mod.$(isop(func) ? "(:($func))" : func)" :
    mod == "Base" ? func :
    "$(replace(mod, "Base.", "")).$func"

macquote(n) =
  startswith(n, "@") ? ":$n" :
  contains(n, "@") ? ":($n)" :
  n

open("../base/docs/helpdb.jl", "w") do io
    for (mod, docs) in alldocs()
        println(io, "# $mod\n")
        for (func, docs) in docs
            func in exceptions && continue
            println(io, "doc\"\"\"\n```rst")
            for (i, doc) in enumerate(docs)
                println(io, doc)
                i < length(docs) && println(io)
            end
            println(io, "```\n\"\"\"")
            println(io, macquote(identifier(mod, func)))
            println(io)
        end
    end
end

# let count = 0, hand = 0
#   for (mod, docs) in alldocs()
#     for (func, docs) in docs
#       count += 1
#       doc = join(docs, "\n")
#       if ismatch(r":\w+:", doc)
#         hand += 1
#       end
#     end
#   end
#   hand, count
# end
