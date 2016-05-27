# This file is a part of Julia. License is MIT: http://julialang.org/license

# Method and method table pretty-printing

function argtype_decl(env, n, t) # -> (argname, argtype)
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
        tt, tn = t.parameters[1], t.parameters[2]
        if isa(tn, TypeVar) && !tn.bound
            if tt === Any || (isa(tt, TypeVar) && !tt.bound)
                return string(s, "..."), ""
            else
                return s, string_with_env(env, tt) * "..."
            end
        end
        return s, string_with_env(env, "Vararg{", tt, ",", tn, "}")
    elseif t == String
        return s, "String"
    end
    return s, string_with_env(env, t)
end

function argtype_decl_vararg(env, n, t)
    if isa(n, Expr)
        s = string(n.args[1])
        if n.args[2].head == :...
            # x... or x::T... declaration
            if t.parameters[1] === Any
                return string(s, "..."), ""
            else
                return s, string_with_env(env, t.parameters[1]) * "..."
            end
        elseif t == String
            return s, "String"
        end
    end
    # x::Vararg, x::Vararg{T}, or x::Vararg{T,N} declaration
    s, length(n.args[2].args) < 4 ?
       string_with_env(env, "Vararg{", t.parameters[1], "}") :
       string_with_env(env, "Vararg{", t.parameters[1], ",", t.parameters[2], "}")
end

function arg_decl_parts(m::Method)
    tv = m.tvars
    if !isa(tv,SimpleVector)
        tv = Any[tv]
    else
        tv = Any[tv...]
    end
    li = m.lambda_template
    file, line = "", 0
    if li !== nothing
        argnames = li.slotnames[1:li.nargs]
        s = Symbol("?")
        decls = Any[argtype_decl(:tvar_env => tv, get(argnames, i, s), m.sig.parameters[i])
                    for i = 1:length(m.sig.parameters)]
        if isdefined(li, :def)
            file, line = li.def.file, li.def.line
        end
    else
        decls = Any["" for i = 1:length(m.sig.parameters)]
    end
    return tv, decls, file, line
end

function kwarg_decl(sig::ANY, kwtype::DataType)
    sig = Tuple{kwtype, Array, sig.parameters...}
    kwli = ccall(:jl_methtable_lookup, Any, (Any, Any), kwtype.name.mt, sig)
    if kwli !== nothing
        kwli = kwli::Method
        return filter(x->!('#' in string(x)), kwli.lambda_template.slotnames[kwli.lambda_template.nargs+1:end])
    end
    return ()
end

function show(io::IO, m::Method; kwtype::Nullable{DataType}=Nullable{DataType}())
    tv, decls, file, line = arg_decl_parts(m)
    ft = m.sig.parameters[1]
    d1 = decls[1]
    if ft <: Function &&
            isdefined(ft.name.module, ft.name.mt.name) &&
            ft == typeof(getfield(ft.name.module, ft.name.mt.name))
        print(io, ft.name.mt.name)
    elseif isa(ft, DataType) && is(ft.name, Type.name) && isleaftype(ft)
        f = ft.parameters[1]
        if isa(f, DataType) && isempty(f.parameters)
            print(io, f)
        else
            print(io, "(", d1[1], "::", d1[2], ")")
        end
    else
        print(io, "(", d1[1], "::", d1[2], ")")
    end
    if !isempty(tv)
        show_delim_array(io, tv, '{', ',', '}', false)
    end
    print(io, "(")
    join(io, [isempty(d[2]) ? d[1] : d[1]*"::"*d[2] for d in decls[2:end]],
                 ", ", ", ")
    if !isnull(kwtype)
        kwargs = kwarg_decl(m.sig, get(kwtype))
        if !isempty(kwargs)
            print(io, "; ")
            join(io, kwargs, ", ", ", ")
        end
    end
    print(io, ")")
    if line > 0
        print(io, " at ", file, ":", line)
    end
end

function show_method_table(io::IO, ms::MethodList, max::Int=-1, header::Bool=true)
    mt = ms.mt
    name = mt.name
    isself = isdefined(mt.module, name) &&
             typeof(getfield(mt.module, name)) <: Function
    n = length(ms)
    if header
        m = n==1 ? "method" : "methods"
        ns = isself ? string(name) : string("(::", name, ")")
        what = startswith(ns, '@') ? "macro" : "generic function"
        print(io, "# $n $m for ", what, " \"", ns, "\":")
    end
    kwtype = isdefined(mt, :kwsorter) ? Nullable{DataType}(typeof(mt.kwsorter)) : Nullable{DataType}()
    n = rest = 0
    local last
    for meth in ms
       if max==-1 || n<max
            println(io)
            show(io, meth; kwtype=kwtype)
            n += 1
        else
            rest += 1
            last = meth
        end
    end
    if rest > 0
        println(io)
        if rest == 1
            show(io, last; kwtype=kwtype)
        else
            print(io,"... $rest methods not shown (use methods($name) to see them all)")
        end
    end
end

show(io::IO, ms::MethodList) = show_method_table(io, ms)
show(io::IO, mt::MethodTable) = show_method_table(io, MethodList(mt))

function inbase(m::Module)
    if m == Base
        true
    else
        parent = module_parent(m)
        parent === m ? false : inbase(parent)
    end
end
fileurl(file) = let f = find_source_file(file); f === nothing ? "" : "file://"*f; end

function url(m::Method)
    M = m.module
    (m.file == :null || m.file == :string) && return ""
    file = string(m.file)
    line = m.line
    line <= 0 || ismatch(r"In\[[0-9]+\]", file) && return ""
    if inbase(M)
        if isempty(Base.GIT_VERSION_INFO.commit)
            # this url will only work if we're on a tagged release
            return "https://github.com/JuliaLang/julia/tree/v$VERSION/base/$file#L$line"
        else
            return "https://github.com/JuliaLang/julia/tree/$(Base.GIT_VERSION_INFO.commit)/base/$file#L$line"
        end
    else
        try
            d = dirname(file)
            return LibGit2.with(LibGit2.GitRepoExt(d)) do repo
                LibGit2.with(LibGit2.GitConfig(repo)) do cfg
                    u = LibGit2.get(cfg, "remote.origin.url", "")
                    u = match(LibGit2.GITHUB_REGEX,u).captures[1]
                    commit = string(LibGit2.head_oid(repo))
                    root = LibGit2.path(repo)
                    if startswith(file, root)
                        "https://github.com/$u/tree/$commit/"*file[length(root)+1:end]*"#L$line"
                    else
                        fileurl(file)
                    end
                end
            end
        catch
            return fileurl(file)
        end
    end
end

function writemime(io::IO, ::MIME"text/html", m::Method; kwtype::Nullable{DataType}=Nullable{DataType}())
    tv, decls, file, line = arg_decl_parts(m)
    ft = m.sig.parameters[1]
    d1 = decls[1]
    if ft <: Function &&
            isdefined(ft.name.module, ft.name.mt.name) &&
            ft == typeof(getfield(ft.name.module, ft.name.mt.name))
        print(io, ft.name.mt.name)
    elseif isa(ft, DataType) && is(ft.name, Type.name) && isleaftype(ft)
        f = ft.parameters[1]
        if isa(f, DataType) && isempty(f.parameters)
            print(io, f)
        else
            print(io, "(", d1[1], "::<b>", d1[2], "</b>)")
        end
    else
        print(io, "(", d1[1], "::<b>", d1[2], "</b>)")
    end
    if !isempty(tv)
        print(io,"<i>")
        show_delim_array(io, tv, '{', ',', '}', false)
        print(io,"</i>")
    end
    print(io, "(")
    join(io, [isempty(d[2]) ? d[1] : d[1]*"::<b>"*d[2]*"</b>"
                      for d in decls[2:end]], ", ", ", ")
    if !isnull(kwtype)
        kwargs = kwarg_decl(m.sig, get(kwtype))
        if !isempty(kwargs)
            print(io, "; <i>")
            join(io, kwargs, ", ", ", ")
            print(io, "</i>")
        end
    end
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

function writemime(io::IO, mime::MIME"text/html", ms::MethodList)
    mt = ms.mt
    name = mt.name
    n = length(ms)
    meths = n==1 ? "method" : "methods"
    ns = string(name)
    what = startswith(ns, '@') ? "macro" : "generic function"
    print(io, "$n $meths for ", what, " <b>$ns</b>:<ul>")
    kwtype = isdefined(mt, :kwsorter) ? Nullable{DataType}(typeof(mt.kwsorter)) : Nullable{DataType}()
    for meth in ms
        print(io, "<li> ")
        writemime(io, mime, meth; kwtype=kwtype)
        print(io, "</li> ")
    end
    print(io, "</ul>")
end

writemime(io::IO, mime::MIME"text/html", mt::MethodTable) = writemime(io, mime, MethodList(mt))

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
