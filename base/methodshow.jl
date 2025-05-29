# This file is a part of Julia. License is MIT: https://julialang.org/license

# Method and method table pretty-printing

const empty_sym = Symbol("")
function strip_gensym(sym)
    if sym === :var"#self#" || sym === :var"#unused#"
        return empty_sym
    end
    return Symbol(replace(String(sym), r"^(.*)#(.*#)?\d+$"sa => s"\1"))
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
        sig = rewrap_unionall(Tuple{kwtype, NamedTuple, (unwrap_unionall(m.sig)::DataType).parameters...}, m.sig)
        kwli = ccall(:jl_methtable_lookup, Any, (Any, UInt), sig, get_world_counter())
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
            isempty(kws) && push!(kws,  :var"...")
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
        if Sys.BUILD_STDLIB_PATH != Sys.STDLIB
            # BUILD_STDLIB_PATH gets defined in sysinfo.jl
            npath = normpath(path)
            npath′ = replace(npath, normpath(Sys.BUILD_STDLIB_PATH) => normpath(Sys.STDLIB))
            path = npath == npath′ ? path : npath′
        end
        if isdefined(@__MODULE__, :Core) && isdefined(Core, :Compiler)
            compiler_folder = dirname(String(Base.moduleloc(Core.Compiler).file))
            if dirname(path) == compiler_folder
                return abspath(Sys.STDLIB, "..", "..", "Compiler", "src", basename(path))
            end
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
    if sym === :var"..."
        return "..."
    end
    s = String(sym)
    if endswith(s, "...")
        return string(sprint(show_sym, Symbol(s[1:end-3])), "...")
    else
        return sprint(show_sym, sym)
    end
end

# default compact view
show(io::IO, m::Method; kwargs...) = show_method(IOContext(io, :compact=>true), m; kwargs...)

show(io::IO, ::MIME"text/plain", m::Method; kwargs...) = show_method(io, m; kwargs...)

function show_method(io::IO, m::Method; modulecolor = :light_black, digit_align_width = 1)
    tv, decls, file, line = arg_decl_parts(m)
    sig = unwrap_unionall(m.sig)
    if sig === Tuple
        # Builtin
        print(io, m.name, "(...)")
        file = "none"
        line = 0
    else
        print(io, decls[1][2], "(")

        # arguments
        for (i,d) in enumerate(decls[2:end])
            printstyled(io, d[1], color=:light_black)
            if !isempty(d[2])
                print(io, "::")
                print_type_bicolor(io, d[2], color=:bold, inner_color=:normal)
            end
            i < length(decls)-1 && print(io, ", ")
        end

        kwargs = kwarg_decl(m)
        if !isempty(kwargs)
            print(io, "; ")
            for kw in kwargs
                skw = sym_to_string(kw)
                print(io, skw)
                if kw != last(kwargs)
                    print(io, ", ")
                end
            end
        end
        print(io, ")")
        show_method_params(io, tv)
    end

    if !(get(io, :compact, false)::Bool) # single-line mode
        println(io)
        digit_align_width += 4
    end
    # module & file, re-using function from errorshow.jl
    print_module_path_file(io, parentmodule(m), string(file), line; modulecolor, digit_align_width)
end

function show_method_list_header(io::IO, ms::MethodList, namefmt::Function)
    tn = ms.tn
    name = tn.singletonname
    hasname = isdefined(tn.module, name) &&
              typeof(getfield(tn.module, name)) <: Function
    n = length(ms)
    m = n==1 ? "method" : "methods"
    print(io, "# $n $m")
    sname = string(name)
    namedisplay = namefmt(sname)
    if hasname
        what = (startswith(sname, '@') ?
                    "macro"
               : tn.module === Core && tn.wrapper <: Core.Builtin ?
                    "builtin function"
               : # else
                    "generic function")
        print(io, " for ", what, " ", namedisplay, " from ")

        col = get!(() -> popfirst!(STACKTRACE_MODULECOLORS), STACKTRACE_FIXEDCOLORS, parentmodule_before_main(tn.module))

        printstyled(io, tn.module, color=col)
    elseif '#' in sname
        print(io, " for anonymous function ", namedisplay)
    elseif tn === _TYPE_NAME || iskindtype(tn.wrapper)
        print(io, " for type constructor")
    else
        print(io, " for callable object")
    end
    !iszero(n) && print(io, ":")
end

# Determine the `modulecolor` value to pass to `show_method`
function _modulecolor(method::Method)
    mmt = get_methodtable(method)
    # TODO: this looks like a buggy bit of internal hacking, so disable for now
    return nothing
    if mmt === nothing || mmt.module === parentmodule(method)
        return nothing
    end
    # `mmt` is only particularly relevant for external method tables. Since the primary
    # method table is shared, we now need to distinguish "primary" methods by trying to
    # check if there is a primary `DataType` to identify it with. c.f. how `jl_method_def`
    # would derive this same information (for the name).
    ft = argument_datatype((unwrap_unionall(method.sig)::DataType).parameters[1])
    # `ft` should be the type associated with the first argument in the method signature.
    # If it's `Type`, try to unwrap it again.
    if isType(ft)
        ft = argument_datatype(ft.parameters[1])
    end
    if ft === nothing || parentmodule(method) === parentmodule(ft) !== Core
        return nothing
    end
    m = parentmodule_before_main(method)
    return get!(() -> popfirst!(STACKTRACE_MODULECOLORS), STACKTRACE_FIXEDCOLORS, m)
end

function show_method_table(io::IO, ms::MethodList, max::Int=-1, header::Bool=true)
    tn = ms.tn
    name = tn.singletonname
    hasname = isdefined(tn.module, name) &&
              typeof(getfield(tn.module, name)) <: Function
    if header
        show_method_list_header(io, ms, str -> "\""*str*"\"")
    end
    n = rest = 0
    local last

    last_shown_line_infos = get(io, :last_shown_line_infos, nothing)
    last_shown_line_infos === nothing || empty!(last_shown_line_infos)

    digit_align_width = length(string(max > 0 ? max : length(ms)))

    for meth in ms
        if max == -1 || n < max
            n += 1
            println(io)

            print(io, " ", lpad("[$n]", digit_align_width + 2), " ")

            show_method(io, meth; modulecolor=_modulecolor(meth))

            file, line = updated_methodloc(meth)
            if last_shown_line_infos !== nothing
                push!(last_shown_line_infos, (string(file), line))
            end
        else
            rest += 1
            last = meth
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

show(io::IO, ms::MethodList) = show_method_table(io, ms)
show(io::IO, ::MIME"text/plain", ms::MethodList) = show_method_table(io, ms)
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
    line <= 0 || occursin(r"In\[[0-9]+\]"a, file) && return ""
    Sys.iswindows() && (file = replace(file, '\\' => '/'))
    if inbase(M)
        if isempty(Base.GIT_VERSION_INFO.commit)
            # this url will only work if we're on a tagged release
            return "https://github.com/JuliaLang/julia/tree/v$VERSION/base/$file#L$line"
        else
            return "https://github.com/JuliaLang/julia/tree/$(Base.GIT_VERSION_INFO.commit)/base/$file#L$line"
        end
    end
    libgit2_id = PkgId(UUID((0x76f85450_5226_5b5a,0x8eaa_529ad045b433)), "LibGit2")
    LibGit2 = maybe_root_module(libgit2_id)
    if LibGit2 isa Module
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
            # oops, this was a bad idea
        end
    end
    return fileurl(file)
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

function show(io::IO, mime::MIME"text/html", ms::MethodList)
    show_method_list_header(io, ms, str -> "<b>"*str*"</b>")
    print(io, "<ul>")
    for meth in ms
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
