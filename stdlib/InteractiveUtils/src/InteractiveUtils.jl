# This file is a part of Julia. License is MIT: https://julialang.org/license

module InteractiveUtils

Base.Experimental.@optlevel 1

export apropos, edit, less, code_warntype, code_llvm, code_native, methodswith, varinfo,
    versioninfo, subtypes, supertypes, @which, @edit, @less, @functionloc, @code_warntype,
    @code_typed, @code_lowered, @code_llvm, @code_native, clipboard

import Base.Docs.apropos

using Base: unwrap_unionall, rewrap_unionall, isdeprecated, Bottom, show_unquoted, summarysize,
    to_tuple_type, signature_type, format_bytes

using Markdown

include("editless.jl")
include("codeview.jl")
include("macros.jl")
include("clipboard.jl")

"""
    varinfo(m::Module=Main, pattern::Regex=r""; all::Bool = false, imported::Bool = false, sortby::Symbol = :name)

Return a markdown table giving information about exported global variables in a module, optionally restricted
to those matching `pattern`.

The memory consumption estimate is an approximate lower bound on the size of the internal structure of the object.

- `all` : also list non-exported objects defined in the module, deprecated objects, and compiler-generated objects.
- `imported` : also list objects explicitly imported from other modules.
- `recursive` : recursively include objects in sub-modules, observing the same settings in each.
- `sortby` : the column to sort results by. Options are `:name` (default), `:size`, and `:summary`.
"""
function varinfo(m::Module=Main, pattern::Regex=r""; all::Bool = false, imported::Bool = false, sortby::Symbol = :name, recursive::Bool = false)
    @assert sortby in [:name, :size, :summary] "Unrecognized `sortby` value `:$sortby`. Possible options are `:name`, `:size`, and `:summary`"
    function _populate_rows(m2::Module, allrows, include_self::Bool, prep::String)
        newrows = Any[
            let
                value = getfield(m2, v)
                ssize_str, ssize = if value===Base || value===Main || value===Core
                    ("", typemax(Int))
                else
                    ss = summarysize(value)
                    (format_bytes(ss), ss)
                end
                Any[string(prep, v), ssize_str, summary(value), ssize]
            end
            for v in names(m2; all, imported)
            if (string(v) != split(string(m2), ".")[end] || include_self) && isdefined(m2, v) && occursin(pattern, string(v)) ]
        append!(allrows, newrows)
        if recursive
            for row in newrows
                if row[3] == "Module" && !in(split(row[1], ".")[end], [split(string(m2), ".")[end], "Base", "Main", "Core"])
                    _populate_rows(getfield(m2, Symbol(split(row[1], ".")[end])), allrows, false, prep * "$(row[1]).")
                end
            end
        end
        return allrows
    end
    rows = _populate_rows(m, Vector{Any}[], true, "")
    if sortby == :name
        col, reverse = 1, false
    elseif sortby == :size
        col, reverse = 4, true
    elseif sortby == :summary
        col, reverse = 3, false
    end
    rows = sort!(rows, by=r->r[col], rev=reverse)
    pushfirst!(rows, Any["name", "size", "summary"])

    return Markdown.MD(Any[Markdown.Table(map(r->r[1:3], rows), Symbol[:l, :r, :l])])
end
varinfo(pat::Regex; kwargs...) = varinfo(Main, pat, kwargs...)

"""
    versioninfo(io::IO=stdout; verbose::Bool=false)

Print information about the version of Julia in use. The output is
controlled with boolean keyword arguments:

- `verbose`: print all additional information

See also: [`VERSION`](@ref).
"""
function versioninfo(io::IO=stdout; verbose::Bool=false)
    println(io, "Julia Version $VERSION")
    if !isempty(Base.GIT_VERSION_INFO.commit_short)
        println(io, "Commit $(Base.GIT_VERSION_INFO.commit_short) ($(Base.GIT_VERSION_INFO.date_string))")
    end
    if ccall(:jl_is_debugbuild, Cint, ())!=0
        println(io, "DEBUG build")
    end
    println(io, "Platform Info:")
    println(io, "  OS: ", Sys.iswindows() ? "Windows" : Sys.isapple() ?
        "macOS" : Sys.KERNEL, " (", Sys.MACHINE, ")")

    if verbose
        lsb = ""
        if Sys.islinux()
            try lsb = readchomp(pipeline(`lsb_release -ds`, stderr=devnull)); catch; end
        end
        if Sys.iswindows()
            try lsb = strip(read(`$(ENV["COMSPEC"]) /c ver`, String)); catch; end
        end
        if !isempty(lsb)
            println(io, "      ", lsb)
        end
        if Sys.isunix()
            println(io, "  uname: ", readchomp(`uname -mprsv`))
        end
    end

    if verbose
        cpuio = IOBuffer() # print cpu_summary with correct alignment
        Sys.cpu_summary(cpuio)
        for (i, line) in enumerate(split(String(take!(cpuio)), "\n"))
            prefix = i == 1 ? "  CPU: " : "       "
            println(io, prefix, line)
        end
    else
        cpu = Sys.cpu_info()
        println(io, "  CPU: ", cpu[1].model)
    end

    if verbose
        println(io, "  Memory: $(Sys.total_memory()/2^30) GB ($(Sys.free_memory()/2^20) MB free)")
        try println(io, "  Uptime: $(Sys.uptime()) sec"); catch; end
        print(io, "  Load Avg: ")
        Base.print_matrix(io, Sys.loadavg()')
        println(io)
    end
    println(io, "  WORD_SIZE: ", Sys.WORD_SIZE)
    println(io, "  LIBM: ",Base.libm_name)
    println(io, "  LLVM: libLLVM-",Base.libllvm_version," (", Sys.JIT, ", ", Sys.CPU_NAME, ")")

    env_strs = [String[ "  $(k) = $(v)" for (k,v) in ENV if occursin(r"JULIA", k)];
                (verbose ?
                 String[ "  $(k) = $(v)" for (k,v) in ENV if occursin(r"PATH|FLAG|^TERM$|HOME", k)] :
                 [])]
    if !isempty(env_strs)
        println(io, "Environment:")
        for str in env_strs
            println(io, str)
        end
    end
end


function type_close_enough(@nospecialize(x), @nospecialize(t))
    x == t && return true
    # TODO: handle UnionAll properly
    return (isa(x, DataType) && isa(t, DataType) && x.name === t.name && x <: t) ||
           (isa(x, Union) && isa(t, DataType) && (type_close_enough(x.a, t) || type_close_enough(x.b, t)))
end

# `methodswith` -- shows a list of methods using the type given
"""
    methodswith(typ[, module or function]; supertypes::Bool=false])

Return an array of methods with an argument of type `typ`.

The optional second argument restricts the search to a particular module or function
(the default is all top-level modules).

If keyword `supertypes` is `true`, also return arguments with a parent type of `typ`,
excluding type `Any`.
"""
function methodswith(t::Type, f::Function, meths = Method[]; supertypes::Bool=false)
    for d in methods(f)
        if any(function (x)
                   let x = rewrap_unionall(x, d.sig)
                       (type_close_enough(x, t) ||
                        (supertypes ? (isa(x, Type) && t <: x && (!isa(x,TypeVar) || x.ub != Any)) :
                         (isa(x,TypeVar) && x.ub != Any && t == x.ub)) &&
                        x != Any)
                   end
               end,
               unwrap_unionall(d.sig).parameters)
            push!(meths, d)
        end
    end
    return meths
end

function _methodswith(t::Type, m::Module, supertypes::Bool)
    meths = Method[]
    for nm in names(m)
        if isdefined(m, nm)
            f = getfield(m, nm)
            if isa(f, Function)
                methodswith(t, f, meths; supertypes = supertypes)
            end
        end
    end
    return unique(meths)
end

methodswith(t::Type, m::Module; supertypes::Bool=false) = _methodswith(t, m, supertypes)

function methodswith(t::Type; supertypes::Bool=false)
    meths = Method[]
    for mod in Base.loaded_modules_array()
        append!(meths, _methodswith(t, mod, supertypes))
    end
    return unique(meths)
end

# subtypes
function _subtypes(m::Module, x::Type, sts=Base.IdSet{Any}(), visited=Base.IdSet{Module}())
    push!(visited, m)
    xt = unwrap_unionall(x)
    if !isa(xt, DataType)
        return sts
    end
    xt = xt::DataType
    for s in names(m, all = true)
        if isdefined(m, s) && !isdeprecated(m, s)
            t = getfield(m, s)
            if isa(t, DataType)
                t = t::DataType
                if t.name.name === s && supertype(t).name == xt.name
                    ti = typeintersect(t, x)
                    ti != Bottom && push!(sts, ti)
                end
            elseif isa(t, UnionAll)
                t = t::UnionAll
                tt = unwrap_unionall(t)
                isa(tt, DataType) || continue
                tt = tt::DataType
                if tt.name.name === s && supertype(tt).name == xt.name
                    ti = typeintersect(t, x)
                    ti != Bottom && push!(sts, ti)
                end
            elseif isa(t, Module)
                t = t::Module
                in(t, visited) || _subtypes(t, x, sts, visited)
            end
        end
    end
    return sts
end

function _subtypes_in(mods::Array, x::Type)
    if !isabstracttype(x)
        # Fast path
        return Type[]
    end
    sts = Base.IdSet{Any}()
    visited = Base.IdSet{Module}()
    for m in mods
        _subtypes(m, x, sts, visited)
    end
    return sort!(collect(sts), by=string)
end

subtypes(m::Module, x::Type) = _subtypes_in([m], x)

"""
    subtypes(T::DataType)

Return a list of immediate subtypes of DataType `T`. Note that all currently loaded subtypes
are included, including those not visible in the current module.

See also [`supertype`](@ref), [`supertypes`](@ref), [`methodswith`](@ref).

# Examples
```jldoctest
julia> subtypes(Integer)
3-element Vector{Any}:
 Bool
 Signed
 Unsigned
```
"""
subtypes(x::Type) = _subtypes_in(Base.loaded_modules_array(), x)

"""
    supertypes(T::Type)

Return a tuple `(T, ..., Any)` of `T` and all its supertypes, as determined by
successive calls to the [`supertype`](@ref) function, listed in order of `<:`
and terminated by `Any`.

See also [`subtypes`](@ref).

# Examples
```jldoctest
julia> supertypes(Int)
(Int64, Signed, Integer, Real, Number, Any)
```
"""
function supertypes(T::Type)
    S = supertype(T)
    # note: we return a tuple here, not an Array as for subtypes, because in
    #       the future we could evaluate this function statically if desired.
    return S === T ? (T,) : (T, supertypes(S)...)
end

# dumptype is for displaying abstract type hierarchies,
# based on Jameson Nash's typetree.jl in https://github.com/JuliaArchive/Examples
function dumptype(io::IO, @nospecialize(x), n::Int, indent)
    print(io, x)
    n == 0 && return  # too deeply nested
    isa(x, DataType) && x.abstract && dumpsubtypes(io, x, Main, n, indent)
    nothing
end

directsubtype(a::DataType, b::DataType) = supertype(a).name === b.name
directsubtype(a::UnionAll, b::DataType) = directsubtype(a.body, b)
directsubtype(a::Union, b::DataType) = directsubtype(a.a, b) || directsubtype(a.b, b)
# Fallback to handle TypeVar's
directsubtype(a, b::DataType) = false
function dumpsubtypes(io::IO, x::DataType, m::Module, n::Int, indent)
    for s in names(m, all = true)
        if isdefined(m, s) && !isdeprecated(m, s)
            t = getfield(m, s)
            if t === x || t === m
                continue
            elseif isa(t, Module) && nameof(t) === s && parentmodule(t) === m
                # recurse into primary module bindings
                dumpsubtypes(io, x, t, n, indent)
            elseif isa(t, UnionAll) && directsubtype(t::UnionAll, x)
                dt = unwrap_unionall(t)
                println(io)
                if isa(dt, DataType) && dt.name.wrapper === t
                    # primary type binding
                    print(io, indent, "  ")
                    dumptype(io, dt, n - 1, string(indent, "  "))
                else
                    # aliases to types
                    print(io, indent, "  ", m, ".", s, "{")
                    tvar_io::IOContext = io
                    tp = t
                    while true
                        show(tvar_io, tp.var)
                        tvar_io = IOContext(tvar_io, :unionall_env => tp.var)
                        tp = tp.body
                        if isa(tp, UnionAll)
                            print(io, ", ")
                        else
                            print(io, "} = ")
                            break
                        end
                    end
                    show(tvar_io, tp)
                end
            elseif isa(t, Union) && directsubtype(t::Union, x)
                println(io)
                print(io, indent, "  ", m, ".", s, " = ", t)
            elseif isa(t, DataType) && directsubtype(t::DataType, x)
                println(io)
                if t.name.module !== m || t.name.name != s
                    # aliases to types
                    print(io, indent, "  ", m, ".", s, " = ")
                    show(io, t)
                else
                    # primary type binding
                    print(io, indent, "  ")
                    dumptype(io, t, n - 1, string(indent, "  "))
                end
            end
        end
    end
    nothing
end

# TODO: @deprecate peakflops to LinearAlgebra
export peakflops
"""
    peakflops(n::Integer=2000; parallel::Bool=false)

`peakflops` computes the peak flop rate of the computer by using double precision
[`gemm!`](@ref LinearAlgebra.BLAS.gemm!). For more information see
[`LinearAlgebra.peakflops`](@ref).

!!! compat "Julia 1.1"
    This function will be moved from `InteractiveUtils` to `LinearAlgebra` in the
    future. In Julia 1.1 and later it is available as `LinearAlgebra.peakflops`.
"""
function peakflops(n::Integer=2000; parallel::Bool=false)
    # Base.depwarn("`peakflop`s have moved to the LinearAlgebra module, " *
    #              "add `using LinearAlgebra` to your imports.", :peakflops)
    let LinearAlgebra = Base.require(Base.PkgId(
            Base.UUID((0x37e2e46d_f89d_539d,0xb4ee_838fcccc9c8e)), "LinearAlgebra"))
        return LinearAlgebra.peakflops(n; parallel = parallel)
    end
end

function report_bug(kind)
    @info "Loading BugReporting package..."
    BugReportingId = Base.PkgId(
        Base.UUID((0xbcf9a6e7_4020_453c,0xb88e_690564246bb8)), "BugReporting")
    # Check if the BugReporting package exists in the current environment
    local BugReporting
    if Base.locate_package(BugReportingId) === nothing
        @info "Package `BugReporting` not found - attempting temporary installation"
        # Create a temporary environment and add BugReporting
        let Pkg = Base.require(Base.PkgId(
            Base.UUID((0x44cfe95a_1eb2_52ea,0xb672_e2afdf69b78f)), "Pkg"))
            mktempdir() do tmp
                old_load_path = copy(LOAD_PATH)
                push!(empty!(LOAD_PATH), joinpath(tmp, "Project.toml"))
                old_active_project = Base.ACTIVE_PROJECT[]
                Base.ACTIVE_PROJECT[] = nothing
                Pkg.add(Pkg.PackageSpec(BugReportingId.name, BugReportingId.uuid))
                BugReporting = Base.require(BugReportingId)
                append!(empty!(LOAD_PATH), old_load_path)
                Base.ACTIVE_PROJECT[] = old_active_project
            end
        end
    else
        BugReporting = Base.require(BugReportingId)
    end
    return Base.invokelatest(BugReporting.make_interactive_report, kind, ARGS)
end

end
