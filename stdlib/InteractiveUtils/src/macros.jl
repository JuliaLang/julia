# This file is a part of Julia. License is MIT: https://julialang.org/license

# macro wrappers for various reflection functions

import Base: typesof, insert!

separate_kwargs(args...; kwargs...) = (args, kwargs.data)

gen_call_with_extracted_types(__module__, fcn, ex0; kws...) = _gen_call_with_extracted_types(__module__, fcn, ex0)[1]

const _cat_msg = "constant interpolation not yet allowed for concat syntax"
const _kw_msg = "constant interpolation not yet allowed for keyword arguments"
const _disallowed_msg = "constant interpolation is not yet supported for this macro"
function strip_dollar_add_error!(setup, exarg, msg = _kw_msg)
    if isa(exarg, Expr) && exarg.head == :$
        # TODO: Make this work
        push!(setup.args, Expr(:call, :error, msg))
        # Strip `$` for better error message
        return esc(exarg.args[1])
    end
    return esc(exarg)
end

function process_args!(setup, exargs, allow_const_interp)
    allow_const_interp && push!(setup.args, :(resize!(argvals, $(length(exargs)))))
    found_any = false
    for (idx, arg) in pairs(exargs)
        if isa(arg, Expr) && arg.head === :$
            if allow_const_interp
                sym = gensym()
                push!(setup.args, quote
                    $sym = $(esc(arg.args[1]))
                    argvals[$idx] = $sym
                end)
                exargs[idx] = sym
                found_any = true
            else
                exargs[idx] = strip_dollar_add_error!(setup,
                    arg, _disallowed_msg)
            end
        elseif isa(arg, Expr) && arg.head == :kw
            exargs[idx] = Expr(:kw, arg.args[1],
                strip_dollar_add_error!(setup, arg.args[2]))
        elseif isa(arg, Expr) && arg.head == :parameters
            for i in 1:length(arg.args)
                parg = arg.args[i]
                if isa(parg, Expr) && parg.head == :kw
                    parg = Expr(:kw, parg.args[1],
                        strip_dollar_add_error!(setup, parg.args[2]))
                else
                    parg = strip_dollar_add_error!(setup, parg)
                end
                arg.args[i] = parg
            end
        elseif arg !== nothing
            exargs[idx] = esc(exargs[idx])
        end
    end
    return found_any
end

function combine_setup_call(setup, call)
    # It would be ok to always include the setup code, but we sometimes
    # show the output of the (simple form of) these macros to show how
    # they work under the hood and we'd like that to be as clean as
    # possible.
    all_code = isempty(setup.args) ? call : Expr(:block, setup, call)
    (all_code, call)
end
function _gen_call_with_extracted_types(__module__, fcn, ex0; allow_const_interp=false)
    setup = allow_const_interp ? quote
        argvals = Vector{Any}(undef, 0)
    end : Expr(:block)
    found_any = false
    if isa(ex0, Expr)
        if any(a->(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex0.args)
            exargs = copy(ex0.args)
            found_any = process_args!(setup, exargs, allow_const_interp)
            push!(setup.args, quote
                local arg1 = $(exargs[1])
                local args, kwargs = $separate_kwargs($(exargs[2:end]...))
            end)
            call = :($(fcn)(Core.kwfunc(arg1),
                       Tuple{typeof(kwargs), Core.Typeof(arg1), map(Core.Typeof, args)...},
                       $((found_any ? (:argvals,) : ())...)))
            return combine_setup_call(setup, call)
        elseif ex0.head === :call
            exargs = copy(ex0.args)
            found_any = process_args!(setup, exargs, allow_const_interp)
            call = Expr(:call, fcn, exargs[1],
                        Expr(:call, typesof, exargs[2:end]...),
                        (found_any ? (:argvals,) : ())...)
            return combine_setup_call(setup, call)
        elseif ex0.head === :(=) && length(ex0.args) == 2
            lhs, rhs = ex0.args
            if isa(lhs, Expr) && lhs.head in (:(.), :ref)
                if lhs.head === :(.)
                    exargs = Any[nothing, lhs.args..., rhs]
                    # For setproperty!, if the element we're setting is a symbol,
                    # automatically pass that symbol through as a constant.
                    sym = exargs[3]
                    if isa(sym, QuoteNode) && isa(sym.value, Symbol) && allow_const_interp
                        found_any = true
                        push!(setup.args, quote
                            resize!(argvals, 3)
                            argvals[3] = $(sym)
                        end)
                    end
                    f = Base.setproperty!
                else
                    exargs = Any[nothing, lhs.args[1], rhs, lhs.args[2:end]...]
                    f = Base.setindex!
                end
                found_any |= process_args!(setup, exargs, allow_const_interp)
                call = Expr(:call, fcn, f,
                    Expr(:call, typesof, exargs[2:end]...),
                    (found_any ? (:argvals,) : ())...)
                return combine_setup_call(setup, call)
            end
        elseif ex0.head === :vcat || ex0.head === :typed_vcat
            args = ex0.args
            if ex0.head === :vcat
                f, hf = Base.vcat, Base.hvcat
            else
                f, hf = Base.typed_vcat, Base.typed_hvcat
            end
            if any(a->isa(a,Expr) && a.head === :row, args)
                rows = Any[ (isa(x,Expr) && x.head === :row ? x.args : Any[x]) for x in (
                    f === Base.vcat ? args : args[2:end])]
                lens = map(length, rows)
                rowargs = vcat(rows...)
                rowargs = map(arg->strip_dollar_add_error!(setup, arg, _cat_msg), rowargs)
                targ = ()
                if f === Base.typed_vcat
                    targ = (strip_dollar_add_error!(setup, args[1], _cat_msg),)
                end
                call = Expr(:call, fcn, hf,
                            Expr(:call, typesof,
                                 targ...,
                                 Expr(:tuple, lens...),
                                 rowargs...))
            else
                args = map(arg->strip_dollar_add_error!(setup, arg, _cat_msg), args)
                call = Expr(:call, fcn, f,  Expr(:call, typesof, args...))
            end
            return combine_setup_call(setup, call)
        else
            for (head, f) in (:ref => Base.getindex, :hcat => Base.hcat, :(.) => Base.getproperty, :vect => Base.vect, Symbol("'") => Base.adjoint, :typed_hcat => Base.typed_hcat, :string => string)
                if ex0.head === head
                    exargs = Any[nothing, ex0.args...]
                    if f === Base.getproperty && allow_const_interp
                        # For getproperty, if the element we're setting is a symbol,
                        # automatically pass that symbol through as a constant.
                        sym = exargs[3]
                        if isa(sym, QuoteNode) && isa(sym.value, Symbol)
                            found_any = true
                            push!(setup.args, quote
                                resize!(argvals, 3)
                                argvals[3] = $(sym)
                            end)
                        end
                    end
                    found_any |= process_args!(setup, exargs, allow_const_interp)
                    call = Expr(:call, fcn, f,
                                Expr(:call, typesof, exargs[2:end]...),
                                (found_any ? (:argvals,) : ())...)
                    return combine_setup_call(setup, call)
                end
            end
        end
    end

    if isa(ex0, Expr) && ex0.head === :macrocall # Make @edit @time 1+2 edit the macro by using the types of the *expressions*
        call = Expr(:call, fcn, esc(ex0.args[1]), Tuple{#=__source__=#LineNumberNode, #=__module__=#Module, Any[ Core.Typeof(a) for a in ex0.args[3:end] ]...})
        return combine_setup_call(setup, call)
    end

    ex = Meta.lower(__module__, ex0)
    if !isa(ex, Expr)
        call = Expr(:call, :error, "expression is not a function call or symbol")
        return combine_setup_call(setup, call)
    end

    exret = Expr(:none)
    if ex.head === :call
        if any(e->(isa(e, Expr) && e.head === :(...)), ex0.args) &&
            (ex.args[1] === GlobalRef(Core,:_apply) ||
             ex.args[1] === GlobalRef(Base,:_apply))
            # check for splatting
            exret = Expr(:call, ex.args[1], fcn,
                        Expr(:tuple, esc(ex.args[2]),
                            Expr(:call, typesof, map(esc, ex.args[3:end])...)))
        else
            exret = Expr(:call, fcn, esc(ex.args[1]),
                         Expr(:call, typesof, map(esc, ex.args[2:end])...))
        end
    end
    if ex.head === :thunk || exret.head === :none
        exret = Expr(:call, :error, "expression is not a function call, "
                                  * "or is too complex for @$fcn to analyze; "
                                  * "break it down to simpler parts if possible")
    end
    return (exret, exret)
end

"""
Same behaviour as gen_call_with_extracted_types except that keyword arguments
of the form "foo=bar" are passed on to the called function as well.
The keyword arguments must be given before the mandatory argument.
"""
function gen_call_with_extracted_types_and_kwargs(__module__, fcn, ex0; kw...)
    kwargs = Vector{Any}[]
    arg = ex0[end] # Mandatory argument
    for i in 1:length(ex0)-1
        x = ex0[i]
        if x isa Expr && x.head == :(=) # Keyword given of the form "foo=bar"
            push!(kwargs, x.args)
        else
            return Expr(:call, :error, "@$fcn expects only one non-keyword argument")
        end
    end
    (code, thecall) = _gen_call_with_extracted_types(__module__, fcn, arg; kw...)
    for kwarg in kwargs
        if length(kwarg) != 2
            x = string(Expr(:(=), kwarg...))
            return Expr(:call, :error, "Invalid keyword argument: $x")
        end
        push!(thecall.args, Expr(:kw, kwarg[1], kwarg[2]))
    end
    return code
end

for fname in [:which, :less, :edit, :functionloc, :code_warntype, :code_native]
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

macro code_llvm(ex0...)
    gen_call_with_extracted_types_and_kwargs(__module__, :code_llvm, ex0)
end

macro code_typed(ex0...)
    thecall = gen_call_with_extracted_types_and_kwargs(__module__, :code_typed, ex0; allow_const_interp=true)
    quote
        results = $thecall
        length(results) == 1 ? results[1] : results
    end
end

macro code_lowered(ex0)
    thecall = gen_call_with_extracted_types(__module__, :code_lowered, ex0)
    quote
        results = $thecall
        length(results) == 1 ? results[1] : results
    end
end

"""
    @functionloc

Applied to a function or macro call, it evaluates the arguments to the specified call, and
returns a tuple `(filename,line)` giving the location for the method that would be called for those arguments.
It calls out to the `functionloc` function.
"""
:@functionloc

"""
    @which

Applied to a function or macro call, it evaluates the arguments to the specified call, and
returns the `Method` object for the method that would be called for those arguments. Applied
to a variable, it returns the module in which the variable was bound. It calls out to the
`which` function.
"""
:@which

"""
    @less

Evaluates the arguments to the function or macro call, determines their types, and calls the `less`
function on the resulting expression.
"""
:@less

"""
    @edit

Evaluates the arguments to the function or macro call, determines their types, and calls the `edit`
function on the resulting expression.
"""
:@edit

"""
    @code_typed

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_typed`](@ref) on the resulting expression. Use the optional argument `optimize` with

    @code_typed optimize=true foo(x)

to control whether additional optimizations, such as inlining, are also applied.
"""
:@code_typed

"""
    @code_lowered

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_lowered`](@ref) on the resulting expression.
"""
:@code_lowered

"""
    @code_warntype

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_warntype`](@ref) on the resulting expression.
"""
:@code_warntype

"""
    @code_llvm

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_llvm`](@ref) on the resulting expression.
Set the optional keyword arguments `raw`, `dump_module` and `optimize` by putting them and
their value before the function call, like this:

    @code_llvm raw=true dump_module=true f(x)
    @code_llvm optimize=false f(x)

`optimize` controls whether additional optimizations, such as inlining, are also applied.
`raw` makes all metadata and dbg.* calls visible.
`dump_module` prints the entire module that encapsulates the function, with debug info and metadata.
"""
:@code_llvm

"""
    @code_native

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_native`](@ref) on the resulting expression.
"""
:@code_native
