# This file is a part of Julia. License is MIT: http://julialang.org/license

# Method and method-table pretty-printing

const  filler = :_ # e.g. Array{_,_}
function replace_tvars(t::DataType, tv)
    # Returns a string representation of t with its TypeVars replaced
    # with the ones in tv, if found, else with *.
    length(t.parameters)==0 && return string(t.name)
    outstr = string(t.name) * "{"
    for p in t.parameters
        nstr = if isa(p,Union(DataType,TypeConstructor))
             replace_tvars(p, tv)
        elseif !isa(p,TypeVar)
            string(p)
        elseif isa(p,TypeVar)
            # find which TypeVar features the method parameter
            i = find([p===t for t in tv])
            length(i)==1 ? string(tv[i[1]].name) : string(filler)
        else
            error("hu?")
        end
        outstr = outstr * nstr *","
    end
    return outstr[1:end-1]*"}"
end
function replace_tvars(t::TypeConstructor, tv)
    tt = TypeVar(filler)
    return string(t{[tt for i=1:length(t.parameters)]...})
end

function argtype_decl(n, t, tv) # -> (argname, argtype)
    if isa(n,Expr)
        n = n.args[1]  # handle n::T in arg list
    end
    s = string(n)
    i = search(s,'#')
    if i > 0
        s = s[1:i-1]
    end
    if t === Any && !isempty(s)
        return s, ""
    end
    if isvarargtype(t)
        if t.parameters[1] === Any
            return string(s, "..."), ""
        else
            return s, string(t.parameters[1], "...")
        end
    end
    # If DataType/TypeConstructor replace all unbound TypeVars with *
    isa(t,Union(DataType,TypeConstructor)) &&  return s, replace_tvars(t, tv)
    return s, string(t)
end

function arg_decl_parts(m::Method)
    tv = m.tvars
    if !isa(tv,SimpleVector)
        tv = svec(tv)
    end
    li = m.func.code
    e = uncompressed_ast(li)
    argnames = e.args[1]
    s = symbol("?")
    decls = [argtype_decl(get(argnames,i,s), m.sig.types[i], tv) for i=1:length(m.sig.types)]
    return tv, decls, li.file, li.line
end

function show(io::IO, m::Method)
    print(io, m.func.code.name)
    tv, decls, file, line = arg_decl_parts(m)
    if !isempty(tv)
        show_delim_array(io, tv, '{', ',', '}', false)
    end
    print(io, "(")
    print_joined(io, [isempty(d[2]) ? d[1] : d[1]*"::"*d[2] for d in decls],
                 ",", ",")
    print(io, ")")
    if line > 0
        print(io, " at ", file, ":", line)
    end
end

function show_method_table(io::IO, mt::MethodTable, max::Int=-1, header::Bool=true)
    name = mt.name
    n = length(mt)
    if header
        m = n==1 ? "method" : "methods"
        print(io, "# $n $m for generic function \"$name\":")
    end
    d = mt.defs
    n = rest = 0
    while d !== nothing
        if max==-1 || n<max || (rest==0 && n==max && d.next === nothing)
            println(io)
            show(io, d)
            n += 1
        else
            rest += 1
        end
        d = d.next
    end
    if rest > 0
        println(io)
        print(io,"... $rest methods not shown (use methods($name) to see them all)")
    end
end

show(io::IO, mt::MethodTable) = show_method_table(io, mt)

inbase(m::Module) = m == Base ? true : m == Main ? false : inbase(module_parent(m))
fileurl(file) = let f = find_source_file(file); f == nothing ? "" : "file://"*f; end
function url(m::Method)
    M = m.func.code.module
    (m.func.code.file == :null || m.func.code.file == :string) && return ""
    file = string(m.func.code.file)
    line = m.func.code.line
    line <= 0 || ismatch(r"In\[[0-9]+\]", file) && return ""
    if inbase(M)
        return "https://github.com/JuliaLang/julia/tree/$(Base.GIT_VERSION_INFO.commit)/base/$file#L$line"
    else
        try
            d = dirname(file)
            u = Git.readchomp(`config remote.origin.url`, dir=d)
            u = match(Git.GITHUB_REGEX,u).captures[1]
            root = cd(d) do # dir=d confuses --show-toplevel, apparently
                Git.readchomp(`rev-parse --show-toplevel`)
            end
            if startswith(file, root)
                commit = Git.readchomp(`rev-parse HEAD`, dir=d)
                return "https://github.com/$u/tree/$commit/"*file[length(root)+2:end]*"#L$line"
            else
                return fileurl(file)
            end
        catch
            return fileurl(file)
        end
    end
end

function writemime(io::IO, ::MIME"text/html", m::Method)
    print(io, m.func.code.name)
    tv, decls, file, line = arg_decl_parts(m)
    if !isempty(tv)
        print(io,"<i>")
        show_delim_array(io, tv, '{', ',', '}', false)
        print(io,"</i>")
    end
    print(io, "(")
    print_joined(io, [isempty(d[2]) ? d[1] : d[1]*"::<b>"*d[2]*"</b>"
                      for d in decls], ",", ",")
    print(io, ")")
    if line > 0
        u = url(m)
        if isempty(u)
            print(io, " at ", file, ":", line)
        else
            print(io, """ at <a href="$u" target="_blank">""",
                  file, ":", line, "</a>")
        end
    end
end

function writemime(io::IO, mime::MIME"text/html", mt::MethodTable)
    name = mt.name
    n = length(mt)
    meths = n==1 ? "method" : "methods"
    print(io, "$n $meths for generic function <b>$name</b>:<ul>")
    d = mt.defs
    while d !== nothing
        print(io, "<li> ")
        writemime(io, mime, d)
        d = d.next
    end
    print(io, "</ul>")
end

# pretty-printing of Vector{Method} for output of methodswith:

function writemime(io::IO, mime::MIME"text/html", mt::AbstractVector{Method})
    print(io, summary(mt))
    if !isempty(mt)
        print(io, ":<ul>")
        for d in mt
            print(io, "<li> ")
            writemime(io, mime, d)
        end
        print(io, "</ul>")
    end
end

# override usual show method for Vector{Method}: don't abbreviate long lists
writemime(io::IO, mime::MIME"text/plain", mt::AbstractVector{Method}) =
    showarray(io, mt, limit=false)
