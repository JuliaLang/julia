# This file is a part of Julia. License is MIT: https://julialang.org/license

# Method and method table pretty-printing

function argtype_decl(env, n, sig::DataType, i::Int, nargs, isva::Bool) # -> (argname, argtype)
    t = sig.parameters[i]
    if i == nargs && isva && !isvarargtype(t)
        t = Vararg{t,length(sig.parameters)-nargs+1}
    end
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
        v1, v2 = nothing, nothing
        if isa(t, UnionAll)
            v1 = t.var
            t = t.body
            if isa(t, UnionAll)
                v2 = t.var
                t = t.body
            end
        end
        ut = unwrap_unionall(t)
        tt, tn = ut.parameters[1], ut.parameters[2]
        if isa(tn, TypeVar) && (tn === v1 || tn === v2)
            if tt === Any || (isa(tt, TypeVar) && (tt === v1 || tt === v2))
                return string(s, "..."), ""
            else
                return s, string_with_env(env, tt) * "..."
            end
        end
        return s, string_with_env(env, "Vararg{", tt, ",", tn, "}")
    end
    return s, string_with_env(env, t)
end

function method_argnames(m::Method)
    if !isdefined(m, :source) && isdefined(m, :generator)
        return m.generator.argnames
    end
    argnames = Vector{Any}(uninitialized, m.nargs)
    ccall(:jl_fill_argnames, Void, (Any, Any), m.source, argnames)
    return argnames
end

function arg_decl_parts(m::Method)
    tv = Any[]
    sig = m.sig
    while isa(sig, UnionAll)
        push!(tv, sig.var)
        sig = sig.body
    end
    file = m.file
    line = m.line
    if isdefined(m, :source) || isdefined(m, :generator)
        argnames = method_argnames(m)
        show_env = ImmutableDict{Symbol, Any}()
        for t in tv
            show_env = ImmutableDict(show_env, :unionall_env => t)
        end
        decls = Any[argtype_decl(show_env, argnames[i], sig, i, m.nargs, m.isva)
                    for i = 1:m.nargs]
    else
        decls = Any[("", "") for i = 1:length(sig.parameters)]
    end
    return tv, decls, file, line
end

function kwarg_decl(m::Method, kwtype::DataType)
    sig = rewrap_unionall(Tuple{kwtype, NamedTuple, unwrap_unionall(m.sig).parameters...}, m.sig)
    kwli = ccall(:jl_methtable_lookup, Any, (Any, Any, UInt), kwtype.name.mt, sig, typemax(UInt))
    if kwli !== nothing
        kwli = kwli::Method
        src = uncompressed_ast(kwli)
        kws = filter(x -> !('#' in string(x)), src.slotnames[(kwli.nargs + 1):end])
        # ensure the kwarg... is always printed last. The order of the arguments are not
        # necessarily the same as defined in the function
        i = findfirst(x -> endswith(string(x), "..."), kws)
        i == 0 && return kws
        push!(kws, kws[i])
        return deleteat!(kws, i)
    end
    return ()
end

function show_method_params(io::IO, tv)
    if !isempty(tv)
        print(io, " where ")
        if length(tv) == 1
            show(io, tv[1])
        else
            show_delim_array(io, tv, '{', ',', '}', false)
        end
    end
end

function show(io::IO, m::Method; kwtype::Nullable{DataType}=Nullable{DataType}())
    tv, decls, file, line = arg_decl_parts(m)
    sig = unwrap_unionall(m.sig)
    ft0 = sig.parameters[1]
    ft = unwrap_unionall(ft0)
    d1 = decls[1]
    if sig === Tuple
        print(io, m.name)
        decls = Any[(), ("...", "")]
    elseif ft <: Function && isa(ft, DataType) &&
            isdefined(ft.name.module, ft.name.mt.name) &&
                # TODO: more accurate test? (tn.name === "#" name)
            ft0 === typeof(getfield(ft.name.module, ft.name.mt.name))
        print(io, ft.name.mt.name)
    elseif isa(ft, DataType) && ft.name === Type.body.name
        f = ft.parameters[1]
        if isa(f, DataType) && isempty(f.parameters)
            print(io, f)
        else
            print(io, "(", d1[1], "::", d1[2], ")")
        end
    else
        print(io, "(", d1[1], "::", d1[2], ")")
    end
    print(io, "(")
    join(io, [isempty(d[2]) ? d[1] : d[1]*"::"*d[2] for d in decls[2:end]],
                 ", ", ", ")
    if !isnull(kwtype)
        kwargs = kwarg_decl(m, get(kwtype))
        if !isempty(kwargs)
            print(io, "; ")
            join(io, kwargs, ", ", ", ")
        end
    end
    print(io, ")")
    show_method_params(io, tv)
    print(io, " in ", m.module)
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
        sname = string(name)
        ns = (isself || '#' in sname) ? sname : string("(::", name, ")")
        what = startswith(ns, '@') ? "macro" : "generic function"
        print(io, "# $n $m for ", what, " \"", ns, "\":")
    end
    kwtype = isdefined(mt, :kwsorter) ? Nullable{DataType}(typeof(mt.kwsorter)) : Nullable{DataType}()
    n = rest = 0
    local last

    resize!(LAST_SHOWN_LINE_INFOS, 0)
    for meth in ms
       if max==-1 || n<max
            n += 1
            println(io)
            print(io, "[$(n)] ")
            show(io, meth; kwtype=kwtype)
            push!(LAST_SHOWN_LINE_INFOS, (string(meth.file), meth.line))
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
    Sys.iswindows() && (file = replace(file, '\\', '/'))
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
                    if startswith(file, root) || startswith(realpath(file), root)
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

function show(io::IO, ::MIME"text/html", m::Method; kwtype::Nullable{DataType}=Nullable{DataType}())
    tv, decls, file, line = arg_decl_parts(m)
    sig = unwrap_unionall(m.sig)
    ft0 = sig.parameters[1]
    ft = unwrap_unionall(ft0)
    d1 = decls[1]
    if ft <: Function && isa(ft, DataType) &&
            isdefined(ft.name.module, ft.name.mt.name) &&
            ft0 === typeof(getfield(ft.name.module, ft.name.mt.name))
        print(io, ft.name.mt.name)
    elseif isa(ft, DataType) && ft.name === Type.body.name
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
        kwargs = kwarg_decl(m, get(kwtype))
        if !isempty(kwargs)
            print(io, "; <i>")
            join(io, kwargs, ", ", ", ")
            print(io, "</i>")
        end
    end
    print(io, ")")
    print(io, " in ", m.module)
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

function show(io::IO, mime::MIME"text/html", ms::MethodList)
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
        show(io, mime, meth; kwtype=kwtype)
        print(io, "</li> ")
    end
    print(io, "</ul>")
end

show(io::IO, mime::MIME"text/html", mt::MethodTable) = show(io, mime, MethodList(mt))

# pretty-printing of AbstractVector{Method} for output of methodswith:
function show(io::IO, mime::MIME"text/plain", mt::AbstractVector{Method})
    resize!(LAST_SHOWN_LINE_INFOS, 0)
    for (i, m) in enumerate(mt)
        print(io, "[$(i)] ")
        show(io, m)
        println(io)
        push!(LAST_SHOWN_LINE_INFOS, (string(m.file), m.line))
    end
end

function show(io::IO, mime::MIME"text/html", mt::AbstractVector{Method})
    print(io, summary(mt))
    if !isempty(mt)
        print(io, ":<ul>")
        for d in mt
            print(io, "<li> ")
            show(io, mime, d)
        end
        print(io, "</ul>")
    end
end
