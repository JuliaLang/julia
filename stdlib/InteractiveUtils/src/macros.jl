# This file is a part of Julia. License is MIT: https://julialang.org/license

# macro wrappers for various reflection functions

using Base: insert!, replace_ref_begin_end!,
    infer_return_type, infer_exception_type, infer_effects, code_ircode, isexpr

# defined in Base so it's possible to time all imports, including InteractiveUtils and its deps
# via. `Base.@time_imports` etc.
import Base: @time_imports, @trace_compile, @trace_dispatch

separate_kwargs(args...; kwargs...) = (args, values(kwargs))

# This structure allows encoding the type of an argument
# without having a value, used to support type annotations
# in calls such as `f(1, ::Float64)` for introspection macros.
struct InterpolatedType
    T::Type
    InterpolatedType(@nospecialize(T::Type)) = new(T)
    InterpolatedType(@nospecialize(val)) = new(Core.Typeof(val))
end

_typeof(t::InterpolatedType) = t.T
_typeof(@nospecialize(t)) = Core.Typeof(t)
typesof(@nospecialize args...) = Tuple{Any[_typeof(arg) for arg in args]...}

"""
Transform a dot expression into one where each argument has been replaced by a
variable "xj" (with j an integer from 1 to the returned i).
The list `args` contains the original arguments that have been replaced.
"""
function recursive_dotcalls!(ex, args, i=1)
    if !(ex isa Expr) || ((ex.head !== :. || !(ex.args[2] isa Expr)) &&
                          (ex.head !== :call || string(ex.args[1])[1] != '.'))
        newarg = Symbol('x', i)
        if isexpr(ex, :...)
            push!(args, only(ex.args))
            return Expr(:..., newarg), i+1
        else
            push!(args, ex)
            return newarg, i+1
        end
    end
    (start, branches) = ex.head === :. ? (1, ex.args[2].args) : (2, ex.args)
    length_branches = length(branches)::Int
    for j in start:length_branches
        branch, i = recursive_dotcalls!(branches[j], args, i)
        branches[j] = branch
    end
    return ex, i
end

is_interpolated_type(arg) = isexpr(arg, :call, 2) && arg.args[1] === InterpolatedType
function _typeof_expr(ex)
    is_interpolated_type(ex) && return esc(ex.args[2])
    Core.Typeof(ex)
end

function extract_farg(arg)
    !is_interpolated_type(arg) && return esc(arg)
    fT = esc(arg.args[2])
    :($construct_callable($fT))
end

function construct_callable(@nospecialize(func::Type))
    # Support function singleton types such as `(::typeof(f))(args...)`
    Base.issingletontype(func) && isdefined(func, :instance) && return func.instance
    # Don't support type annotations otherwise, we don't want to give wrong answers
    # for callables such as `(::Returns{Int})(args...)` where using `Returns{Int}`
    # would give us code for the constructor, not for the callable object.
    throw(ArgumentError("If a function type is explicitly provided, it must be a singleton whose only instance is the callable object"))
end

typesof_expr(args) = :(typesof($(esc.(args)...)))

function process_type_annotations!(ex::Expr)
    for (i, subex) in enumerate(ex.args)
        isa(subex, Expr) || continue
        if isexpr(subex, :(::), 1)
            ex.args[i] = :($InterpolatedType($(subex.args[1])))
        elseif isexpr(subex, :...) && isexpr(subex.args[1], :(::), 1)
            T = subex.args[1].args[1]
            ex.args[i] = :(Vararg{$T})
        else
            process_type_annotations!(subex)
        end
    end
    ex
end

function gen_call_with_extracted_types(__module__, fcn, ex0, kws=Expr[])
    if isexpr(ex0, :ref)
        ex0 = replace_ref_begin_end!(ex0)
    end
    # assignments get bypassed: @edit a = f(x) <=> @edit f(x)
    if isa(ex0, Expr) && ex0.head == :(=) && isa(ex0.args[1], Symbol) && isempty(kws)
        return gen_call_with_extracted_types(__module__, fcn, ex0.args[2])
    end
    if isa(ex0, Expr)
        process_type_annotations!(ex0)
        if ex0.head === :do && isexpr(get(ex0.args, 1, nothing), :call)
            if length(ex0.args) != 2
                return Expr(:call, :error, "ill-formed do call")
            end
            i = findlast(a->(isexpr(a, :kw) || isexpr(a, :parameters)), ex0.args[1].args)
            args = copy(ex0.args[1].args)
            insert!(args, (isnothing(i) ? 2 : 1+i::Int), ex0.args[2])
            ex0 = Expr(:call, args...)
        end
        if ex0.head === :. || (ex0.head === :call && ex0.args[1] !== :.. && string(ex0.args[1])[1] == '.')
            codemacro = startswith(string(fcn), "code_")
            if codemacro && (ex0.head === :call || ex0.args[2] isa Expr)
                # Manually wrap a dot call in a function
                args = Any[]
                ex, i = recursive_dotcalls!(copy(ex0), args)
                xargs = [Symbol('x', j) for j in 1:i-1]
                dotfuncname = gensym("dotfunction")
                dotfuncdef = Expr(:local, Expr(:(=), Expr(:call, dotfuncname, xargs...), ex))
                return quote
                    $(esc(dotfuncdef))
                    $(fcn)($(esc(dotfuncname)), $(typesof_expr(args)); $(kws...))
                end
            elseif !codemacro
                fully_qualified_symbol = true # of the form A.B.C.D
                ex1 = ex0
                while ex1 isa Expr && ex1.head === :.
                    fully_qualified_symbol = (length(ex1.args) == 2 &&
                                              ex1.args[2] isa QuoteNode &&
                                              ex1.args[2].value isa Symbol)
                    fully_qualified_symbol || break
                    ex1 = ex1.args[1]
                end
                fully_qualified_symbol &= ex1 isa Symbol
                if fully_qualified_symbol || is_interpolated_type(ex1)
                    return quote
                        local arg1 = $(esc(ex0.args[1]))
                        if isa(arg1, Module)
                            $(if string(fcn) == "which"
                                  :(which(arg1, $(ex0.args[2])))
                              else
                                  :(error("expression is not a function call"))
                              end)
                        else
                            local args = $(typesof_expr(ex0.args))
                            $(fcn)(Base.getproperty, args)
                        end
                    end
                else
                    return Expr(:call, :error, "dot expressions are not lowered to "
                                * "a single function call, so @$fcn cannot analyze "
                                * "them. You may want to use Meta.@lower to identify "
                                * "which function call to target.")
                end
            end
        end
        if any(a->(isexpr(a, :kw) || isexpr(a, :parameters)), ex0.args)
            return quote
                local arg1 = $(esc(ex0.args[1]))
                local args, kwargs = $separate_kwargs($(map(esc, ex0.args[2:end])...))
                $(fcn)(Core.kwcall, typesof(kwargs, arg1, args...); $(kws...))
            end
        elseif ex0.head === :call
            argtypes = ex0.args[2:end]
            if ex0.args[1] === :^ && length(ex0.args) >= 3 && isa(ex0.args[3], Int)
                farg = :(Base.literal_pow)
                pushfirst!(argtypes, :^)
                argtypes[3] = InterpolatedType(Val(ex0.args[3]))
            else
                farg = extract_farg(ex0.args[1])
            end
            return Expr(:call, fcn, farg, typesof_expr(argtypes), kws...)
        elseif ex0.head === :(=) && length(ex0.args) == 2
            lhs, rhs = ex0.args
            if isa(lhs, Expr)
                if lhs.head === :(.)
                    return Expr(:call, fcn, Base.setproperty!,
                                typesof_expr(Any[lhs.args..., rhs]), kws...)
                elseif lhs.head === :ref
                    return Expr(:call, fcn, Base.setindex!,
                                typesof_expr(Any[lhs.args[1], rhs, lhs.args[2:end]...]), kws...)
                end
            end
        elseif ex0.head === :vcat || ex0.head === :typed_vcat
            if ex0.head === :vcat
                f, hf = Base.vcat, Base.hvcat
                args = ex0.args
            else
                f, hf = Base.typed_vcat, Base.typed_hvcat
                args = ex0.args[2:end]
            end
            if any(a->isa(a,Expr) && a.head === :row, args)
                rows = Any[ (isa(x,Expr) && x.head === :row ? x.args : Any[x]) for x in args ]
                lens = map(length, rows)
                args = Any[Expr(:tuple, lens...); vcat(rows...)]
                ex0.head === :typed_vcat && pushfirst!(args, ex0.args[1])
                return Expr(:call, fcn, hf, typesof_expr(args), kws...)
            else
                return Expr(:call, fcn, f, typesof_expr(ex0.args), kws...)
            end
        else
            for (head, f) in (:ref => Base.getindex, :hcat => Base.hcat, :(.) => Base.getproperty, :vect => Base.vect, Symbol("'") => Base.adjoint, :typed_hcat => Base.typed_hcat, :string => string)
                if ex0.head === head
                    return Expr(:call, fcn, f, typesof_expr(ex0.args), kws...)
                end
            end
        end
    end
    if isa(ex0, Expr) && ex0.head === :macrocall # Make @edit @time 1+2 edit the macro by using the types of the *expressions*
        return Expr(:call, fcn, extract_farg(ex0.args[1]), :(Tuple{#=__source__=#LineNumberNode, #=__module__=#Module, $(Any[_typeof_expr(a) for a in ex0.args[3:end]]...)}), kws...)
    end

    ex = Meta.lower(__module__, ex0)
    if !isa(ex, Expr)
        return Expr(:call, :error, "expression is not a function call or symbol")
    end

    exret = Expr(:none)
    if ex.head === :call
        if any(e->(isa(e, Expr) && e.head === :(...)), ex0.args) &&
            (ex.args[1] === GlobalRef(Core,:_apply_iterate) ||
             ex.args[1] === GlobalRef(Base,:_apply_iterate))
            # check for splatting
            exret = Expr(:call, ex.args[2], fcn,
                        Expr(:tuple, extract_farg(ex.args[3]), typesof_expr(ex.args[4:end])))
        else
            exret = Expr(:call, fcn, extract_farg(ex.args[1]), typesof_expr(ex.args[2:end]), kws...)
        end
    end
    if ex.head === :thunk || exret.head === :none
        exret = Expr(:call, :error, "expression is not a function call, "
                                  * "or is too complex for @$fcn to analyze; "
                                  * "break it down to simpler parts if possible. "
                                  * "In some cases, you may want to use Meta.@lower.")
    end
    return exret
end

"""
Same behaviour as `gen_call_with_extracted_types` except that keyword arguments
of the form "foo=bar" are passed on to the called function as well.
The keyword arguments must be given before the mandatory argument.
"""
function gen_call_with_extracted_types_and_kwargs(__module__, fcn, ex0)
    kws = Expr[]
    arg = ex0[end] # Mandatory argument
    for i in 1:length(ex0)-1
        x = ex0[i]
        if x isa Expr && x.head === :(=) # Keyword given of the form "foo=bar"
            if length(x.args) != 2
                return Expr(:call, :error, "Invalid keyword argument: $x")
            end
            push!(kws, Expr(:kw, esc(x.args[1]), esc(x.args[2])))
        else
            return Expr(:call, :error, "@$fcn expects only one non-keyword argument")
        end
    end
    return gen_call_with_extracted_types(__module__, fcn, arg, kws)
end

for fname in [:which, :less, :edit, :functionloc]
    @eval begin
        macro ($fname)(ex0)
            gen_call_with_extracted_types(__module__, $(Expr(:quote, fname)), ex0)
        end
    end
end

macro which(ex0::Symbol)
    ex0 = QuoteNode(ex0)
    return :(which($__module__, $ex0))
end

for fname in [:code_warntype, :code_llvm, :code_native,
              :infer_return_type, :infer_effects, :infer_exception_type]
    @eval macro ($fname)(ex0...)
        gen_call_with_extracted_types_and_kwargs(__module__, $(QuoteNode(fname)), ex0)
    end
end

for fname in [:code_typed, :code_lowered, :code_ircode]
    @eval macro ($fname)(ex0...)
        thecall = gen_call_with_extracted_types_and_kwargs(__module__, $(QuoteNode(fname)), ex0)
        quote
            local results = $thecall
            length(results) == 1 ? results[1] : results
        end
    end
end

"""
    @functionloc

Applied to a function or macro call, it evaluates the arguments to the specified call, and
returns a tuple `(filename,line)` giving the location for the method that would be called for those arguments.
It calls out to the [`functionloc`](@ref) function.
"""
:@functionloc

"""
    @which

Applied to a function or macro call, it evaluates the arguments to the specified call, and
returns the `Method` object for the method that would be called for those arguments. Applied
to a variable, it returns the module in which the variable was bound. It calls out to the
[`which`](@ref) function.

See also: [`@less`](@ref), [`@edit`](@ref).
"""
:@which

"""
    @less

Evaluates the arguments to the function or macro call, determines their types, and calls the [`less`](@ref)
function on the resulting expression.

See also: [`@edit`](@ref), [`@which`](@ref), [`@code_lowered`](@ref).
"""
:@less

"""
    @edit

Evaluates the arguments to the function or macro call, determines their types, and calls the [`edit`](@ref)
function on the resulting expression.

See also: [`@less`](@ref), [`@which`](@ref).
"""
:@edit

"""
    @code_typed

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_typed`](@ref) on the resulting expression. Use the optional argument `optimize` with

    @code_typed optimize=true foo(x)

to control whether additional optimizations, such as inlining, are also applied.

See also: [`code_typed`](@ref), [`@code_warntype`](@ref), [`@code_lowered`](@ref), [`@code_llvm`](@ref), [`@code_native`](@ref).
"""
:@code_typed

"""
    @code_lowered

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_lowered`](@ref) on the resulting expression.

See also: [`code_lowered`](@ref), [`@code_warntype`](@ref), [`@code_typed`](@ref), [`@code_llvm`](@ref), [`@code_native`](@ref).
"""
:@code_lowered

"""
    @code_warntype

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_warntype`](@ref) on the resulting expression.

See also: [`code_warntype`](@ref), [`@code_typed`](@ref), [`@code_lowered`](@ref), [`@code_llvm`](@ref), [`@code_native`](@ref).
"""
:@code_warntype

"""
    @code_llvm

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_llvm`](@ref) on the resulting expression.
Set the optional keyword arguments `raw`, `dump_module`, `debuginfo`, `optimize`
by putting them and their value before the function call, like this:

    @code_llvm raw=true dump_module=true debuginfo=:default f(x)
    @code_llvm optimize=false f(x)

`optimize` controls whether additional optimizations, such as inlining, are also applied.
`raw` makes all metadata and dbg.* calls visible.
`debuginfo` may be one of `:source` (default) or `:none`,  to specify the verbosity of code comments.
`dump_module` prints the entire module that encapsulates the function.

See also: [`code_llvm`](@ref), [`@code_warntype`](@ref), [`@code_typed`](@ref), [`@code_lowered`](@ref), [`@code_native`](@ref).
"""
:@code_llvm

"""
    @code_native

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_native`](@ref) on the resulting expression.

Set any of the optional keyword arguments `syntax`, `debuginfo`, `binary` or `dump_module`
by putting it before the function call, like this:

    @code_native syntax=:intel debuginfo=:default binary=true dump_module=false f(x)

* Set assembly syntax by setting `syntax` to `:intel` (default) for Intel syntax or `:att` for AT&T syntax.
* Specify verbosity of code comments by setting `debuginfo` to `:source` (default) or `:none`.
* If `binary` is `true`, also print the binary machine code for each instruction precedented by an abbreviated address.
* If `dump_module` is `false`, do not print metadata such as rodata or directives.

See also: [`code_native`](@ref), [`@code_warntype`](@ref), [`@code_typed`](@ref), [`@code_lowered`](@ref), [`@code_llvm`](@ref).
"""
:@code_native

"""
    @time_imports

A macro to execute an expression and produce a report of any time spent importing packages and their
dependencies. Any compilation time will be reported as a percentage, and how much of which was recompilation, if any.

One line is printed per package or package extension. The duration shown is the time to import that package itself, not including the time to load any of its dependencies.

On Julia 1.9+ [package extensions](@ref man-extensions) will show as Parent → Extension.

!!! note
    During the load process a package sequentially imports all of its dependencies, not just its direct dependencies.

```julia-repl
julia> @time_imports using CSV
     50.7 ms  Parsers 17.52% compilation time
      0.2 ms  DataValueInterfaces
      1.6 ms  DataAPI
      0.1 ms  IteratorInterfaceExtensions
      0.1 ms  TableTraits
     17.5 ms  Tables
     26.8 ms  PooledArrays
    193.7 ms  SentinelArrays 75.12% compilation time
      8.6 ms  InlineStrings
     20.3 ms  WeakRefStrings
      2.0 ms  TranscodingStreams
      1.4 ms  Zlib_jll
      1.8 ms  CodecZlib
      0.8 ms  Compat
     13.1 ms  FilePathsBase 28.39% compilation time
   1681.2 ms  CSV 92.40% compilation time
```

!!! compat "Julia 1.8"
    This macro requires at least Julia 1.8

"""
:@time_imports

"""
    @trace_compile

A macro to execute an expression and show any methods that were compiled (or recompiled in yellow),
like the julia args `--trace-compile=stderr --trace-compile-timing` but specifically for a call.

```julia-repl
julia> @trace_compile rand(2,2) * rand(2,2)
#=   39.1 ms =# precompile(Tuple{typeof(Base.rand), Int64, Int64})
#=  102.0 ms =# precompile(Tuple{typeof(Base.:(*)), Array{Float64, 2}, Array{Float64, 2}})
2×2 Matrix{Float64}:
 0.421704  0.864841
 0.211262  0.444366
```

!!! compat "Julia 1.12"
    This macro requires at least Julia 1.12

"""
:@trace_compile

"""
    @trace_dispatch

A macro to execute an expression and report methods that were compiled via dynamic dispatch,
like the julia arg `--trace-dispatch=stderr` but specifically for a call.

!!! compat "Julia 1.12"
    This macro requires at least Julia 1.12

"""
:@trace_dispatch

"""
    @activate Component

Activate a newly loaded copy of an otherwise builtin component. The `Component`
to be activated will be resolved using the ordinary rules of module resolution
in the current environment.

When using `@activate`, additional options for a component may be specified in
square brackets `@activate Compiler[:option1, :option]`

Currently `@activate Compiler` is the only available component that may be
activatived.

For `@activate Compiler`, the following options are available:
1. `:reflection` - Activate the compiler for reflection purposes only.
                   The ordinary reflection functionality in `Base` and `InteractiveUtils`.
                   Will use the newly loaded compiler. Note however, that these reflection
                   functions will still interact with the ordinary native cache (both loading
                   and storing). An incorrect compiler implementation may thus corrupt runtime
                   state if reflection is used. Use external packages like `Cthulhu.jl`
                   introspecting compiler behavior with a separated cache partition.

2. `:codegen`   - Activate the compiler for internal codegen purposes. The new compiler
                  will be invoked whenever the runtime requests compilation.

`@activate Compiler` without options is equivalent to `@activate Compiler[:reflection]`.

"""
macro activate(what)
    options = Symbol[]
    if isexpr(what, :ref)
        Component = what.args[1]
        for i = 2:length(what.args)
            arg = what.args[i]
            if !isa(arg, QuoteNode) || !isa(arg.value, Symbol)
                error("Usage Error: Option $arg is not a symbol")
            end
            push!(options, arg.value)
        end
    else
        Component = what
    end
    if !isa(Component, Symbol)
        error("Usage Error: Component $Component is not a symbol")
    end
    allowed_components = (:Compiler,)
    if !(Component in allowed_components)
        error("Usage Error: Component $Component is not recognized. Expected one of $allowed_components")
    end
    s = gensym()
    if Component === :Compiler && isempty(options)
        push!(options, :reflection)
    end
    options = map(options) do opt
        Expr(:kw, opt, true)
    end
    Expr(:toplevel,
        esc(:(import $Component as $s)),
        esc(:($s.activate!(;$(options...)))))
end
