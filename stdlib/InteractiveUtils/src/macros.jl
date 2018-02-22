# This file is a part of Julia. License is MIT: https://julialang.org/license

# macro wrappers for various reflection functions

import Base.typesof

separate_kwargs(args...; kwargs...) = (args, kwargs.data)

function gen_call_with_extracted_types(__module__, fcn, ex0)
    if isa(ex0, Expr)
        if any(a->(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex0.args)
            return quote
                local arg1 = $(esc(ex0.args[1]))
                local args, kwargs = $separate_kwargs($(map(esc, ex0.args[2:end])...))
                $(fcn)(Core.kwfunc(arg1),
                       Tuple{typeof(kwargs), Core.Typeof(arg1), map(Core.Typeof, args)...})
            end
        elseif ex0.head == :call
            return Expr(:call, fcn, esc(ex0.args[1]),
                        Expr(:call, typesof, map(esc, ex0.args[2:end])...))
        elseif ex0.head == :(.)
            return Expr(:call, fcn, Base.getproperty,
                        Expr(:call, typesof, map(esc, ex0.args)...))
        elseif ex0.head == :(=) && length(ex0.args) == 2 && ex0.args[1].head == :(.)
            return Expr(:call, fcn, Base.setproperty!,
                        Expr(:call, typesof, map(esc, [ex0.args[1].args..., ex0.args[2]])...))
        end
    end
    if isa(ex0, Expr) && ex0.head == :macrocall # Make @edit @time 1+2 edit the macro by using the types of the *expressions*
        return Expr(:call, fcn, esc(ex0.args[1]), Tuple{#=__source__=#LineNumberNode, #=__module__=#Module, Any[ Core.Typeof(a) for a in ex0.args[3:end] ]...})
    end
    ex = Meta.lower(__module__, ex0)
    exret = Expr(:none)
    if !isa(ex, Expr)
        exret = Expr(:call, :error, "expression is not a function call or symbol")
    elseif ex.head == :call
        if any(e->(isa(e, Expr) && e.head==:(...)), ex0.args) &&
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
    elseif ex.head == :body
        a1 = ex.args[1]
        if isa(a1, Expr) && a1.head == :call
            a11 = a1.args[1]
            if a11 == :setindex!
                exret = Expr(:call, fcn, a11,
                             Expr(:call, typesof, map(esc, a1.args[2:end])...))
            end
        end
    end
    if ex.head == :thunk || exret.head == :none
        exret = Expr(:call, :error, "expression is not a function call, "
                                  * "or is too complex for @$fcn to analyze; "
                                  * "break it down to simpler parts if possible")
    end
    return exret
end

for fname in [:which, :less, :edit, :functionloc, :code_warntype,
              :code_llvm, :code_llvm_raw, :code_native]
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

for fname in [:code_typed, :code_lowered]
    @eval begin
        macro ($fname)(ex0)
            thecall = gen_call_with_extracted_types(__module__, $(Expr(:quote, fname)), ex0)
            quote
                results = $thecall
                length(results) == 1 ? results[1] : results
            end
        end
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
[`code_typed`](@ref) on the resulting expression.
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
"""
:@code_llvm

"""
    @code_native

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_native`](@ref) on the resulting expression.
"""
:@code_native
