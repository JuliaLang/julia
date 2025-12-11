# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    replace_ref_begin_end!(ex)

Recursively replace occurrences of the symbols `:begin` and `:end` in a "ref" expression
(i.e. `A[...]`) `ex` with the appropriate function calls (`firstindex` or `lastindex`).
Replacement uses the closest enclosing ref, so

    A[B[end]]

should transform to

    A[B[lastindex(B)]]

"""
replace_ref_begin_end!(__module__::Module, @nospecialize ex) = replace_ref_begin_end_!(__module__, ex, nothing, false, 0)[1]
# replace_ref_begin_end_!(...) returns (new ex, whether withex was used)
function replace_ref_begin_end_!(__module__::Module, ex, withex, in_quote_context::Bool, escs::Int)
    @nospecialize
    used_withex = false
    function escapes(@nospecialize(ex), escs::Int)
        for i = 1:escs
            ex = esc(ex)
        end
        return ex
    end
    function handle_refexpr!(__module__::Module, ref_ex::Expr, main_ex::Expr, withex, in_quote_context, escs::Int)
        @assert !in_quote_context
        local used_withex
        ref_ex.args[1], used_withex = replace_ref_begin_end_!(__module__, ref_ex.args[1], withex, in_quote_context, escs)
        S = gensym(:S) # temp var to cache ex.args[1] if needed. if S is a global or expression, then it has side effects to use
        assignments = []
        used_S = false # whether we actually need S
        # new :ref, so redefine withex
        nargs = length(ref_ex.args) - 1
        if nargs == 0
            return main_ex, used_withex
        elseif nargs == 1
            # replace with lastindex(S)
            ref_ex.args[2], used_S = replace_ref_begin_end_!(__module__, ref_ex.args[2], (:($firstindex($S)),:($lastindex($S))), in_quote_context, escs)
        else
            ni = 1
            nx = 0
            J = nargs + 1
            need_temps = false # whether any arg needs temporaries

            # First pass: determine if any argument will needs temporaries
            for j = 2:J
                exj = ref_ex.args[j]
                if isexpr(exj, :...)
                    need_temps = true
                    break
                end
            end

            # Second pass: if any need temps, create temps for all args
            temp_vars = Tuple{Int,Symbol}[]
            for j = 2:J
                n = nx === 0 ? ni : :($nx + $ni)
                exj, used = replace_ref_begin_end_!(__module__, ref_ex.args[j], (:($firstindex($S,$n)),:($lastindex($S,$n))), in_quote_context, escs)
                used_S |= used
                ref_ex.args[j] = exj
                ni += 1
                if need_temps
                    isva = isexpr(exj, :...) # implied need_temps
                    if isva
                        exj = exj.args[1]
                    end
                    if isa_ast_node(exj) # create temp to preserve evaluation order and count in case `used` gets set later
                        exj = gensym(:arg)
                        push!(temp_vars, (j, exj))
                    end
                    if isva
                        ni -= 1
                        nx = nx === 0 ? :(length($exj)) : :($nx + length($exj))
                    end
                end
            end

            # Third pass: if `used`, need to actually make those temp assignments now
            if used_S
                for (j, temp_var) in temp_vars
                    exj = ref_ex.args[j]
                    isva = isexpr(exj, :...) # implied need_temps
                    if isva
                        exj = exj.args[1]
                    end
                    push!(assignments, :(local $temp_var = $exj))
                    ref_ex.args[j] = isva ? Expr(:..., temp_var) : temp_var
                end
            end
        end

        if used_S
            S0 = ref_ex.args[1]
            S = escapes(S, escs)
            ref_ex.args[1] = S
            main_ex = :(local $S = $S0; $(assignments...); $main_ex)
        end
        return main_ex, used_withex
    end
    if ex isa Expr && ex.head === :macrocall
        # Blithly modifying the arguments to another macro is unwise, so call
        # macroexpand first on it.
        # Unfortunately, macroexpand itself corrupts the scope of variables in
        # the result by calling macroexpand.scm before returning which cannot be
        # avoided since `jl_expand_macros` is private and somewhat difficult to
        # reimplement correctly.
        ex = macroexpand(__module__, ex)
    end
    if isa(ex,Symbol)
        if !in_quote_context
            if ex === :begin
                withex === nothing && error("Invalid use of begin outside []")
                return escapes((withex::NTuple{2,Expr})[1], escs), true
            elseif ex === :end
                withex === nothing && error("Invalid use of end outside []")
                return escapes((withex::NTuple{2,Expr})[2], escs), true
            end
        end
    elseif isa(ex,Expr)
        if !in_quote_context && ex.head === :ref # n.b. macroexpand.scm design is incapable of tracking :begin and :end scope, so emulate that here too and ignore escs
            return handle_refexpr!(__module__, ex, ex, withex, in_quote_context, escs)
        elseif ex.head === :$
            # no longer an executable expression (handle all equivalent forms of :inert, :quote, and QuoteNode the same way)
            in_quote_context = false
        elseif ex.head === :quote
            # executable again
            in_quote_context = true
        elseif ex.head === :var"hygienic-scope"
            # no longer our expression
            escs += 1
        elseif ex.head === :escape
            # our expression again once zero
            escs == 0 && return ex, used_withex
            escs -= 1
        elseif ex.head === :meta || ex.head === :inert
            return ex, used_withex
        elseif !in_quote_context && last(string(ex.head)) == '=' && Meta.isexpr(ex.args[1], :ref)
            for i = eachindex(ex.args)
                if i == 1
                    # we'll deal with the ref expression later
                    continue
                end
                ex.args[i], used = replace_ref_begin_end_!(__module__, ex.args[i], withex, in_quote_context, escs)
                used_withex |= used
            end
            ex, used = handle_refexpr!(__module__, ex.args[1]::Expr, ex, withex, in_quote_context, escs)
            used_withex |= used
            return ex, used_withex
        end
        # recursive search
        for i = eachindex(ex.args)
            ex.args[i], used = replace_ref_begin_end_!(__module__, ex.args[i], withex, in_quote_context, escs)
            used_withex |= used
        end
    end
    return ex, used_withex
end

"""
    @view A[inds...]

Transform the indexing expression `A[inds...]` into the equivalent [`view`](@ref) call.

This can only be applied directly to a single indexing expression and is particularly
helpful for expressions that include the special `begin` or `end` indexing syntaxes
like `A[begin, 2:end-1]` (as those are not supported by the normal [`view`](@ref)
function).

Note that `@view` cannot be used as the target of a regular assignment (e.g.,
`@view(A[1, 2:end]) = ...`), nor would the un-decorated
[indexed assignment](@ref man-indexed-assignment) (`A[1, 2:end] = ...`)
or broadcasted indexed assignment (`A[1, 2:end] .= ...`) make a copy.  It can be useful,
however, for _updating_ broadcasted assignments like `@view(A[1, 2:end]) .+= 1`
because this is a simple syntax for `@view(A[1, 2:end]) .= @view(A[1, 2:end]) + 1`,
and the indexing expression on the right-hand side would otherwise make a
copy without the `@view`.

See also [`@views`](@ref) to switch an entire block of code to use views for non-scalar indexing.

!!! compat "Julia 1.5"
    Using `begin` in an indexing expression to refer to the first index requires at least
    Julia 1.5.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> b = @view A[:, 1]
2-element view(::Matrix{Int64}, :, 1) with eltype Int64:
 1
 3

julia> fill!(b, 0)
2-element view(::Matrix{Int64}, :, 1) with eltype Int64:
 0
 0

julia> A
2×2 Matrix{Int64}:
 0  2
 0  4
```
"""
macro view(ex)
    Meta.isexpr(ex, :ref) || throw(ArgumentError(
        "Invalid use of @view macro: argument must be a reference expression A[...]."))
    ex = replace_ref_begin_end!(__module__, ex)
    # NOTE We embed `view` as a function object itself directly into the AST.
    #      By doing this, we prevent the creation of function definitions like
    #      `view(A, idx) = xxx` in cases such as `@view(A[idx]) = xxx.`
    if Meta.isexpr(ex, :ref)
        ex = Expr(:call, view, ex.args...)
    elseif Meta.isexpr(ex, :block)
        arg2 = ex.args[end]
        Meta.isexpr(arg2, :ref) || error("unsupported replace_ref_begin_end result")
        # ex replaced by let ...; foo[...]; end
        ex.args[end] = Expr(:call, view, arg2.args...)
    else
        error("unsupported replace_ref_begin_end result")
    end
    return esc(ex)
end

############################################################################
# @views macro code:

# maybeview is like getindex, but returns a view for slicing operations
# (while remaining equivalent to getindex for scalar indices and non-array types)
@propagate_inbounds maybeview(A, args...) = getindex(A, args...)
@propagate_inbounds maybeview(A::AbstractArray, args...) = view(A, args...)
@propagate_inbounds maybeview(A::AbstractArray, args::Union{Number,AbstractCartesianIndex}...) = getindex(A, args...)
@propagate_inbounds maybeview(A) = getindex(A)
@propagate_inbounds maybeview(A::AbstractArray) = getindex(A)

# _views implements the transformation for the @views macro.
# @views calls esc(_views(...)) to work around #20241,
# so any function calls we insert (to maybeview, or to
# firstindex and lastindex in replace_ref_begin_end!) must be interpolated
# as values rather than as symbols to ensure that they are called
# from Base rather than from the caller's scope.
_views(x) = x
function _views(ex::Expr)
    if ex.head in (:(=), :(.=))
        # don't use view for ref on the lhs of an assignment,
        # but still use views for the args of the ref:
        arg1 = ex.args[1]
        Expr(ex.head, Meta.isexpr(arg1, :ref) ?
                        Expr(:ref, mapany(_views, (arg1::Expr).args)...) : _views(arg1),
                _views(ex.args[2]))
    elseif ex.head === :ref
        Expr(:call, maybeview, mapany(_views, ex.args)...)::Expr
    else
        h = string(ex.head)
        # don't use view on the lhs of an op-assignment a[i...] += ...
        if last(h) == '=' && Meta.isexpr(ex.args[1], :ref)
            lhs = ex.args[1]::Expr

            # temp vars to avoid recomputing a and i,
            # which will be assigned in a let block:
            i = Symbol[Symbol(:i, k) for k = 1:length(lhs.args)-1]

            # for splatted indices like a[i, j...], we need to
            # splat the corresponding temp var.
            I = similar(i, Any)
            for k = 1:length(i)
                argk1 = lhs.args[k+1]
                if Meta.isexpr(argk1, :...)
                    I[k] = Expr(:..., i[k])
                    lhs.args[k+1] = (argk1::Expr).args[1] # unsplat
                else
                    I[k] = i[k]
                end
            end

            Expr(:var"hygienic-scope", # assign a and i to the macro's scope
                 Expr(:let,
                      Expr(:block,
                           :(a = $(esc(_views(lhs.args[1])))),
                           Any[:($(i[k]) = $(esc(_views(lhs.args[k+1])))) for k=1:length(i)]...),
                      Expr(first(h) == '.' ? :(.=) : :(=), :(a[$(I...)]),
                           Expr(:call, esc(Symbol(h[1:end-1])),
                                :($maybeview(a, $(I...))),
                                mapany(e -> esc(_views(e)), ex.args[2:end])...))), Base)
        else
            exprarray(ex.head, mapany(_views, ex.args))
        end
    end
end

"""
    @views expression

Convert every array-slicing operation in the given expression
(which may be a `begin`/`end` block, loop, function, etc.)
to return a view. Scalar indices, non-array types, and
explicit [`getindex`](@ref) calls (as opposed to `array[...]`) are
unaffected.

Similarly, `@views` converts string slices into [`SubString`](@ref) views.

!!! note
    The `@views` macro only affects `array[...]` expressions
    that appear explicitly in the given `expression`, not array slicing that
    occurs in functions called by that code.

!!! compat "Julia 1.5"
    Using `begin` in an indexing expression to refer to the first index was implemented
    in Julia 1.4, but was only supported by `@views` starting in Julia 1.5.

# Examples
```jldoctest
julia> A = zeros(3, 3);

julia> @views for row in 1:3
           b = A[row, :] # b is a view, not a copy
           b .= row      # assign every element to the row index
       end

julia> A
3×3 Matrix{Float64}:
 1.0  1.0  1.0
 2.0  2.0  2.0
 3.0  3.0  3.0
```
"""
macro views(x)
    esc(_views(replace_ref_begin_end!(__module__, x)))
end
