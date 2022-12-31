# This file is a part of Julia. License is MIT: https://julialang.org/license

# Method and method table pretty-printing

const empty_sym = Symbol("")
function strip_gensym(sym)
    if sym === :var"#self#" || sym === :var"#unused#"
        return empty_sym
    end
    return Symbol(replace(String(sym), r"^(.*)#(.*#)?\d+$" => s"\1"))
end

function argtype_decl(env, n, @nospecialize(sig::DataType), i::Int, nargs, isva::Bool) # -> (argname, argtype)
    t = unwrapva(sig.parameters[min(i, end)])
    if i == nargs && isva
        va = sig.parameters[end]
        if isvarargtype(va) && (!isdefined(va, :N) || !isa(va.N, Int))
            t = va
        else
            ntotal = length(sig.parameters)
            isvarargtype(va) && (ntotal += va.N - 1)
            t = Vararg{t,ntotal-nargs+1}
        end
    end
    if isa(n,Expr)
        n = n.args[1]  # handle n::T in arg list
    end
    n = strip_gensym(n)
    local s
    if n === empty_sym
        s = ""
    else
        s = sprint(show_sym, n)
        t === Any && return s, ""
    end
    if isvarargtype(t)
        if !isdefined(t, :N)
            if unwrapva(t) === Any
                return string(s, "..."), ""
            else
                return s, string_with_env(env, unwrapva(t)) * "..."
            end
        end
        return s, string_with_env(env, "Vararg{", t.T, ", ", t.N, "}")
    end
    return s, string_with_env(env, t)
end

function method_argnames(m::Method)
    argnames = ccall(:jl_uncompress_argnames, Vector{Symbol}, (Any,), m.slot_syms)
    isempty(argnames) && return argnames
    return argnames[1:m.nargs]
end

function arg_decl_parts(m::Method, html=false)
    tv = Any[]
    sig = m.sig
    while isa(sig, UnionAll)
        push!(tv, sig.var)
        sig = sig.body
    end
    file, line = updated_methodloc(m)
    argnames = method_argnames(m)
    if length(argnames) >= m.nargs
        show_env = ImmutableDict{Symbol, Any}()
        for t in tv
            show_env = ImmutableDict(show_env, :unionall_env => t)
        end
        decls = Tuple{String,String}[argtype_decl(show_env, argnames[i], sig, i, m.nargs, m.isva)
                    for i = 1:m.nargs]
        decls[1] = ("", sprint(show_signature_function, unwrapva(sig.parameters[1]), false, decls[1][1], html,
                               context = show_env))
    else
        decls = Tuple{String,String}[("", "") for i = 1:length(sig.parameters::SimpleVector)]
    end
    return tv, decls, file, line
end

# NOTE: second argument is deprecated and is no longer used
function kwarg_decl(m::Method, kwtype = nothing)
    if m.sig !== Tuple # OpaqueClosure or Builtin
        kwtype = typeof(Core.kwcall)
        sig = rewrap_unionall(Tuple{kwtype, Any, (unwrap_unionall(m.sig)::DataType).parameters...}, m.sig)
        kwli = ccall(:jl_methtable_lookup, Any, (Any, Any, UInt), kwtype.name.mt, sig, get_world_counter())
        if kwli !== nothing
            kwli = kwli::Method
            slotnames = ccall(:jl_uncompress_argnames, Vector{Symbol}, (Any,), kwli.slot_syms)
            kws = filter(x -> !(x === empty_sym || '#' in string(x)), slotnames[(kwli.nargs + 1):end])
            # ensure the kwarg... is always printed last. The order of the arguments are not
            # necessarily the same as defined in the function
            i = findfirst(x -> endswith(string(x)::String, "..."), kws)
            if i !== nothing
                push!(kws, kws[i])
                deleteat!(kws, i)
            end
            return kws
        end
    end
    return Symbol[]
end

function show_method_params(io::IO, tv)
    if !isempty(tv)
        print(io, " where ")
        if length(tv) == 1
            show(io, tv[1])
        else
            print(io, "{")
            for i = 1:length(tv)
                if i > 1
                    print(io, ", ")
                end
                x = tv[i]
                show(io, x)
                io = IOContext(io, :unionall_env => x)
            end
            print(io, "}")
        end
    end
end

# In case the line numbers in the source code have changed since the code was compiled,
# allow packages to set a callback function that corrects them.
# (Used by Revise and perhaps other packages.)
# Any function `f` stored here must be consistent with the signature
#    f(m::Method)::Tuple{Union{Symbol,String}, Union{Int32,Int64}}
const methodloc_callback = Ref{Union{Function, Nothing}}(nothing)

function fixup_stdlib_path(path::String)
    # The file defining Base.Sys gets included after this file is included so make sure
    # this function is valid even in this intermediary state
    if isdefined(@__MODULE__, :Sys)
        BUILD_STDLIB_PATH = Sys.BUILD_STDLIB_PATH::String
        STDLIB = Sys.STDLIB::String
        if BUILD_STDLIB_PATH != STDLIB
            # BUILD_STDLIB_PATH gets defined in sysinfo.jl
            npath = normpath(path)
            npath′ = replace(npath, normpath(BUILD_STDLIB_PATH) => normpath(STDLIB))
            return npath == npath′ ? path : npath′
        end
    end
    return path
end

# This function does the method location updating
function updated_methodloc(m::Method)::Tuple{String, Int32}
    file, line = m.file, m.line
    if methodloc_callback[] !== nothing
        try
            file, line = invokelatest(methodloc_callback[], m)::Tuple{Union{Symbol,String}, Union{Int32,Int64}}
        catch
        end
    end
    file = fixup_stdlib_path(string(file))
    return file, Int32(line)
end

functionloc(m::Core.MethodInstance) = functionloc(m.def)

"""
    functionloc(m::Method)

Return a tuple `(filename,line)` giving the location of a `Method` definition.
"""
function functionloc(m::Method)
    file, ln = updated_methodloc(m)
    if ln <= 0
        error("could not determine location of method definition")
    end
    return (find_source_file(string(file)), ln)
end

"""
    functionloc(f::Function, types)

Return a tuple `(filename,line)` giving the location of a generic `Function` definition.
"""
functionloc(@nospecialize(f), @nospecialize(types)) = functionloc(which(f,types))

function functionloc(@nospecialize(f))
    mt = methods(f)
    if isempty(mt)
        if isa(f, Function)
            error("function has no definitions")
        else
            error("object is not callable")
        end
    end
    if length(mt) > 1
        error("function has multiple methods; please specify a type signature")
    end
    return functionloc(first(mt))
end

function sym_to_string(sym)
    s = String(sym)
    if endswith(s, "...")
        return string(sprint(show_sym, Symbol(s[1:end-3])), "...")
    else
        return sprint(show_sym, sym)
    end
end


function kwsortermethod_kwargs(m::Method)
    kwargs = Symbol[]
    for x in Iterators.drop(eachsplit(m.slot_syms, '\0'), m.nargs)
        if !isempty(x) && !occursin('#', x)
            push!(kwargs, Symbol(x))
        end
    end
    return kwargs
    println('\n', m, '\n', collect(Iterators.drop(eachsplit(m.slot_syms, '\0'), m.nargs)), '\n')
end

# type for pretty-printing methods corresponding to the same definition site with different
# optional arguments
struct FactoredMethod
    m::Vector{Method} # the different methods, by order of increasing nargs
    kwargs::Vector{Symbol}
end
function FactoredMethod(m::Method, kwsortermethod)
    FactoredMethod([m], kwsortermethod ? kwsortermethod_kwargs(m) : kwarg_decl(m))
end

struct FactoredMethodList
    list::Vector{FactoredMethod}
    positions::Vector{Vector{Int}}
    ms::MethodList
end

function FactoredMethodList(ms::MethodList, kwsortermethod=false)
    isempty(ms) && return FactoredMethodList(FactoredMethod[], Vector{Int}[], ms)
    I = sortperm(ms.ms; by=m->(m.line, m.file, m.nargs))
    list = FactoredMethod[FactoredMethod(ms[I[1]], kwsortermethod)]
    positions = Vector{Int}[[I[1]]]
    for _i in 2:length(ms)
        i = I[_i]
        last_m = last(last(list).m)
        m = ms[i]
        newmethod = true
        # Check whether m and last_m could refer to the same method except for the
        # addition of a new optional argument
        if !last_m.isva && # the vararg variant is always last
                isempty(last(list).kwargs) && # the kwargs variant is always last
                m.file == last_m.file && m.line == last_m.line && # same location
                startswith(m.slot_syms, last_m.slot_syms) && # same argument names so far
                (m.nargs == last_m.nargs + 1 || (m.isva && m.nargs == last_m.nargs + 2))
                # number of arguments consistent with one new optional argument (+ slurp)
            unwrapped = Base.unwrap_unionall(m.sig).types
            truncated = Tuple{unwrapped[1:last_m.nargs]...}
            rewrapped = Base.rewrap_unionall(truncated, m.sig)
            if rewrapped == last_m.sig # same argument types so far
                # At this point, we have determined that method m has a signature identical
                # to that of last_m, except for one additional argument (+ slurp)
                newmethod = false
                push!(last(list).m, m)
                 # we assert that only one of the methods has kwargs, if any
                append!(last(list).kwargs, kwsortermethod ? kwsortermethod_kwargs(m) : kwarg_decl(m))
                push!(last(positions), i)
            end
        end
        if newmethod
            push!(list, FactoredMethod(m, kwsortermethod))
            push!(positions, [i])
        end
    end
    J = sortperm(positions)
    return FactoredMethodList(list[J], positions[J], ms)
end


# default compact view
show(io::IO, m::Union{Method,FactoredMethod}; kwargs...) = show_method(IOContext(io, :compact=>true), m; kwargs...)

show(io::IO, ::MIME"text/plain", m::Union{Method,FactoredMethod}; kwargs...) = show_method(io, m; kwargs...)

show(io::IO, ::MIME"text/html", m::Union{Method,FactoredMethod}; kwargs...) = show_method(io, m; html=true, kwargs...)

function show_method(io::IO, f::Union{Method,FactoredMethod}; modulecolor=:light_black, digit_align_width=1, html::Bool=false)
    m = f isa FactoredMethod ? last(f.m) : f
    tv, decls, file, line = arg_decl_parts(m, html)
    sig = unwrap_unionall(m.sig)
    if sig === Tuple
        # Builtin
        print(io, m.name, "(...)")
        file = "none"
        line = 0
    else
        print(io, decls[1][2], "(")
        supmandatory = 1 + (f isa FactoredMethod ? first(f.m).nargs : m.nargs)
        last_optional = m.nargs >= supmandatory ? m.nargs - m.isva : 0
        for i in 2:m.nargs
            i == supmandatory && print(io, '[')
            d = decls[i]
            printstyled(io, d[1], color=:light_black)
            if !isempty(d[2])
                print(io, "::")
                html && print(io, "<b>")
                print_type_bicolor(io, d[2], color=:bold, inner_color=:normal)
                html && print(io, "</b>")
            end
            i == last_optional && print(io, ']')
            i == m.nargs || print(io, ", ")
        end
        kwargs = f isa FactoredMethod ? f.kwargs : kwarg_decl(m)
        if !isempty(kwargs)
            print(io, html ? "; <i>" : "; ")
            join(io, map(sym_to_string, kwargs), ", ", ", ")
            html && print(io, "</i>")
        end
        print(io, ")")
        if !isempty(tv)
            html && print(io,"<i>")
            show_method_params(io, tv)
            html && print(io,"</i>")
        end
    end

    if !(get(io, :compact, false)::Bool) # single-line mode
        println(io)
        digit_align_width += 4
    end
    # module & file, re-using function from errorshow.jl
    print_module_path_file(io, m.module, string(file), line; modulecolor, digit_align_width, htmlurl=(html ? url(m) : ""))
end

function show_method_list_header(io::IO, ms::MethodList, namefmt::Function)
    mt = ms.mt
    name = mt.name
    hasname = isdefined(mt.module, name) &&
              typeof(getfield(mt.module, name)) <: Function
    n = length(ms)
    m = n==1 ? "method" : "methods"
    print(io, "# $n $m")
    sname = string(name)
    namedisplay = namefmt(sname)
    if hasname
        what = (startswith(sname, '@') ?
                    "macro"
               : mt.module === Core && mt.defs isa Core.TypeMapEntry && (mt.defs.func::Method).sig === Tuple ?
                    "builtin function"
               : # else
                    "generic function")
        print(io, " for ", what, " ", namedisplay, " from ")

        col = get!(() -> popfirst!(STACKTRACE_MODULECOLORS), STACKTRACE_FIXEDCOLORS, parentmodule_before_main(ms.mt.module))

        printstyled(io, ms.mt.module, color=col)
    elseif '#' in sname
        print(io, " for anonymous function ", namedisplay)
    elseif mt === _TYPE_NAME.mt
        print(io, " for type constructor")
    else
        print(io, " for callable object")
    end
    !iszero(n) && print(io, ":")
end

# cluster into consecutive numbers, e.g. [4, 11, 7, 9, 12, 2, 1, 8] into [1:2, 4, 7:9, 11:12]
function _cluster_consecutive(list)
    l = sort(list)
    result = Union{Int,UnitRange{Int}}[]
    ref = last = popfirst!(l)
    for x in l
        if x == last + 1
            last = x
        else
            ref == last ? push!(result, ref) : push!(result, ref:last)
            ref = last = x
        end
    end
    ref == last ? push!(result, ref) : push!(result, ref:last)
    return result
end

function show_method_table(io::IO, ml::Union{MethodList,FactoredMethodList}, max::Int=-1, header::Bool=true)
    ms = ml isa FactoredMethodList ? ml.ms : ml
    mt = ms.mt
    name = mt.name
    hasname = isdefined(mt.module, name) &&
              typeof(getfield(mt.module, name)) <: Function
    if header
        show_method_list_header(io, ms, str -> "\""*str*"\"")
    end
    rest = 0
    local last_meth

    last_shown_line_infos = get(io, :last_shown_line_infos, nothing)
    last_shown_line_infos === nothing || empty!(last_shown_line_infos)

    modul = if mt === _TYPE_NAME.mt && length(ms) > 0 # type constructor
            which(ms.ms[1].module, ms.ms[1].name)
        else
            mt.module
        end

    if ml isa FactoredMethodList
        positions = Vector{Union{Int,UnitRange{Int}}}[_cluster_consecutive(x) for x in ml.positions]
        digit_align_width = 2 + maximum(x -> length(x) - 1 + sum(y -> y isa Int ? ndigits(y) : ndigits(first(y)) + ndigits(last(y)), x), positions; init=0)
        methods = ml.list
    else
        positions = eachindex(ml.ms)
        digit_align_width = isempty(ms) ? 0 : (2 + ndigits(length(ms)))
        methods = ml.ms
    end

    for (n, meth) in zip(positions, methods)
        if max == -1 || first(n) < max
            println(io)
            print(io, ' ', lpad('['*join(n, ',')*']', digit_align_width), ' ')
            meth_module = ml isa FactoredMethodList ? first(meth.m).module : meth.module
            modulecolor = if meth_module == modul
                nothing
            else
                m = parentmodule_before_main(meth_module)
                get!(() -> popfirst!(STACKTRACE_MODULECOLORS), STACKTRACE_FIXEDCOLORS, m)
            end
            show_method(io, meth; modulecolor)

            file, line = updated_methodloc(ml isa FactoredMethodList ? last(meth.m) : meth)
            if last_shown_line_infos !== nothing
                push!(last_shown_line_infos, (string(file), line))
            end
        else
            rest += 1
            last_meth = meth
        end
    end
    if rest > 0
        println(io)
        if rest == 1
            show_method(io, last)
        else
            print(io, "... $rest methods not shown")
            if hasname
                print(io, " (use methods($name) to see them all)")
            end
        end
    end
    nothing
end

show(io::IO, ml::FactoredMethodList) = show_method_table(io, ml)
show(io::IO, ms::MethodList) = show_method_table(io, FactoredMethodList(ms))
show(io::IO, ::MIME"text/plain", ml::FactoredMethodList) = show_method_table(io, ml)
show(io::IO, ::MIME"text/plain", ml::MethodList) = show_method_table(io, FactoredMethodList(ml))
show(io::IO, mt::Core.MethodTable) = show_method_table(io, MethodList(mt))

function inbase(m::Module)
    if m == Base
        true
    else
        parent = parentmodule(m)
        parent === m ? false : inbase(parent)
    end
end
fileurl(file) = let f = find_source_file(file); f === nothing ? "" : "file://"*f; end

function url(m::Method)
    M = parentmodule(m)
    (m.file === :null || m.file === :string) && return ""
    file = string(m.file)
    line = m.line
    line <= 0 || occursin(r"In\[[0-9]+\]", file) && return ""
    Sys.iswindows() && (file = replace(file, '\\' => '/'))
    libgit2_id = PkgId(UUID((0x76f85450_5226_5b5a,0x8eaa_529ad045b433)), "LibGit2")
    if inbase(M)
        if isempty(Base.GIT_VERSION_INFO.commit)
            # this url will only work if we're on a tagged release
            return "https://github.com/JuliaLang/julia/tree/v$VERSION/base/$file#L$line"
        else
            return "https://github.com/JuliaLang/julia/tree/$(Base.GIT_VERSION_INFO.commit)/base/$file#L$line"
        end
    elseif root_module_exists(libgit2_id)
        LibGit2 = root_module(libgit2_id)
        try
            d = dirname(file)
            return LibGit2.with(LibGit2.GitRepoExt(d)) do repo
                LibGit2.with(LibGit2.GitConfig(repo)) do cfg
                    u = LibGit2.get(cfg, "remote.origin.url", "")
                    u = (match(LibGit2.GITHUB_REGEX,u)::AbstractMatch).captures[1]
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
    else
        return fileurl(file)
    end
end

function show(io::IO, ::MIME"text/html", m::Method)
    tv, decls, file, line = arg_decl_parts(m, true)
    sig = unwrap_unionall(m.sig)
    if sig === Tuple
        # Builtin
        print(io, m.name, "(...) in ", parentmodule(m))
        return
    end
    print(io, decls[1][2], "(")
    join(
        io,
        String[
            isempty(d[2]) ? string(d[1]) : string(d[1], "::<b>", d[2] , "</b>") for d in decls[2:end]
        ],
        ", ",
        ", ",
    )
    kwargs = kwarg_decl(m)
    if !isempty(kwargs)
        print(io, "; <i>")
        join(io, map(sym_to_string, kwargs), ", ", ", ")
        print(io, "</i>")
    end
    print(io, ")")
    if !isempty(tv)
        print(io,"<i>")
        show_method_params(io, tv)
        print(io,"</i>")
    end
    print(io, " in ", parentmodule(m))
    if line > 0
        file, line = updated_methodloc(m)
        u = url(m)
        if isempty(u)
            print(io, " at ", file, ":", line)
        else
            print(io, """ at <a href="$u" target="_blank">""",
                  file, ":", line, "</a>")
        end
    end
end

function show(io::IO, mime::MIME"text/html", ml::Union{MethodList,FactoredMethodList})
    ms = ml isa FactoredMethodList ? ml.ms : ml
    mt = ms.mt
    show_method_list_header(io, ms, str -> "<b>"*str*"</b>")
    print(io, "<ul>")
    for meth in (ml isa FactoredMethodList ? ml.list : ms)
        print(io, "<li> ")
        show(io, mime, meth)
        print(io, "</li> ")
    end
    print(io, "</ul>")
end

show(io::IO, mime::MIME"text/html", mt::Core.MethodTable) = show(io, mime, MethodList(mt))

# pretty-printing of AbstractVector{Method}
function show(io::IO, mime::MIME"text/plain", mt::AbstractVector{Method})
    last_shown_line_infos = get(io, :last_shown_line_infos, nothing)
    last_shown_line_infos === nothing || empty!(last_shown_line_infos)
    first = true
    for (i, m) in enumerate(mt)
        first || println(io)
        first = false
        print(io, "[$(i)] ")
        show(io, m)
        file, line = updated_methodloc(m)
        if last_shown_line_infos !== nothing
            push!(last_shown_line_infos, (string(file), line))
        end
    end
    first && summary(io, mt)
    nothing
end

function show(io::IO, mime::MIME"text/html", mt::AbstractVector{Method})
    summary(io, mt)
    if !isempty(mt)
        print(io, ":<ul>")
        for d in mt
            print(io, "<li> ")
            show(io, mime, d)
        end
        print(io, "</ul>")
    end
    nothing
end
