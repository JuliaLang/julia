# This file is a part of Julia. License is MIT: http://julialang.org/license

# Method and method-table pretty-printing

function get_lambda(m::TypeMapEntry)
    isdefined(m, :func) || return nothing
    isa(m.func, Method) && return m.func.lambda_template
    isa(m.func, LambdaInfo) && return m.func
    return nothing
end

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
        if t.parameters[1] === Any
            return string(s, "..."), ""
        else
            return s, string_with_env(env, t.parameters[1]) * "..."
        end
    elseif t == ByteString
        return s, "ByteString"
    end
    return s, string_with_env(env, t)
end

function arg_decl_parts(m::TypeMapEntry)
    tv = m.tvars
    if !isa(tv,SimpleVector)
        tv = Any[tv]
    else
        tv = Any[tv...]
    end
    li = get_lambda(m)
    file, line = "", 0
    if li !== nothing
        argnames = li.slotnames[1:li.nargs]
        s = symbol("?")
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

function kwarg_decl(m::TypeMapEntry, kwtype::DataType)
    sig = Tuple{kwtype, Array, m.sig.parameters...}
    kwli = ccall(:jl_methtable_lookup, Any, (Any, Any), kwtype.name.mt, sig)
    if kwli !== nothing
        kwli = kwli::Method
        return filter(x->!('#' in string(x)), kwli.lambda_template.slotnames[kwli.lambda_template.nargs+1:end])
    end
    return ()
end

function show(io::IO, m::Method)
    print(io, m.module, '.', m.name, m.isstaged ? " [@generated] at " : " at ", m.file, ":", m.line)
end

function show(io::IO, m::TypeMapEntry; kwtype::Nullable{DataType}=Nullable{DataType}())
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
    print_joined(io, [isempty(d[2]) ? d[1] : d[1]*"::"*d[2] for d in decls[2:end]],
                 ", ", ", ")
    if !isnull(kwtype)
        kwargs = kwarg_decl(m, get(kwtype))
        if !isempty(kwargs)
            print(io, "; ")
            print_joined(io, kwargs, ", ", ", ")
        end
    end
    print(io, ")")
    if line > 0
        print(io, " at ", file, ":", line)
    end
end

function show_method_table(io::IO, mt::MethodTable, max::Int=-1, header::Bool=true)
    name = mt.name
    isself = isdefined(mt.module, name) &&
             typeof(getfield(mt.module, name)) <: Function
    n = length(mt)
    if header
        m = n==1 ? "method" : "methods"
        ns = isself ? string(name) : string("(::", name, ")")
        what = startswith(ns, '@') ? "macro" : "generic function"
        print(io, "# $n $m for ", what, " \"", ns, "\":")
    end
    kwtype = isdefined(mt, :kwsorter) ? Nullable{DataType}(typeof(mt.kwsorter)) : Nullable{DataType}()
    n = rest = 0
    local last
    visit(mt) do d
        if max==-1 || n<max
            println(io)
            show(io, d; kwtype=kwtype)
            n += 1
        else
            rest += 1
            last = d
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

show(io::IO, mt::MethodTable) = show_method_table(io, mt)

function inbase(m::Module)
    if m == Base
        true
    else
        parent = module_parent(m)
        parent === m ? false : inbase(parent)
    end
end
fileurl(file) = let f = find_source_file(file); f === nothing ? "" : "file://"*f; end

url(m::TypeMapEntry) = url(m.func)
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

function writemime(io::IO, ::MIME"text/html", m::TypeMapEntry; kwtype::Nullable{DataType}=Nullable{DataType}())
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
    print_joined(io, [isempty(d[2]) ? d[1] : d[1]*"::<b>"*d[2]*"</b>"
                      for d in decls[2:end]], ", ", ", ")
    if !isnull(kwtype)
        kwargs = kwarg_decl(m, get(kwtype))
        if !isempty(kwargs)
            print(io, "; <i>")
            print_joined(io, kwargs, ", ", ", ")
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

function writemime(io::IO, mime::MIME"text/html", mt::MethodTable)
    name = mt.name
    n = length(mt)
    meths = n==1 ? "method" : "methods"
    ns = string(name)
    what = startswith(ns, '@') ? "macro" : "generic function"
    print(io, "$n $meths for ", what, " <b>$ns</b>:<ul>")
    kwtype = isdefined(mt, :kwsorter) ? Nullable{DataType}(typeof(mt.kwsorter)) : Nullable{DataType}()
    visit(mt) do d
        print(io, "<li> ")
        writemime(io, mime, d; kwtype=kwtype)
        print(io, "</li> ")
    end
    print(io, "</ul>")
end

# pretty-printing of Vector{TypeMapEntry} for output of methodswith:

function writemime(io::IO, mime::MIME"text/html", mt::AbstractVector{TypeMapEntry})
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

# override usual show method for Vector{TypeMapEntry}: don't abbreviate long lists
writemime(io::IO, mime::MIME"text/plain", mt::AbstractVector{TypeMapEntry}) =
    showarray(IOContext(io, :limit_output => false), mt)
