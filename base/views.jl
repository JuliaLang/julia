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
replace_ref_begin_end!(ex) = replace_ref_begin_end_!(ex, nothing)[1]
# replace_ref_begin_end_!(ex,withex) returns (new ex, whether withex was used)
function replace_ref_begin_end_!(ex, withex)
    used_withex = false
    if isa(ex,Symbol)
        if ex === :begin
            withex === nothing && error("Invalid use of begin")
            return withex[1], true
        elseif ex === :end
            withex === nothing && error("Invalid use of end")
            return withex[2], true
        end
    elseif isa(ex,Expr)
        if ex.head === :ref
            ex.args[1], used_withex = replace_ref_begin_end_!(ex.args[1], withex)
            S = isa(ex.args[1],Symbol) ? ex.args[1]::Symbol : gensym(:S) # temp var to cache ex.args[1] if needed
            used_S = false # whether we actually need S
            # new :ref, so redefine withex
            nargs = length(ex.args)-1
            if nargs == 0
                return ex, used_withex
            elseif nargs == 1
                # replace with lastindex(S)
                ex.args[2], used_S = replace_ref_begin_end_!(ex.args[2], (:($firstindex($S)),:($lastindex($S))))
            else
                n = 1
                J = lastindex(ex.args)
                for j = 2:J
                    exj, used = replace_ref_begin_end_!(ex.args[j], (:($firstindex($S)),:($lastindex($S,$n))))
                    used_S |= used
                    ex.args[j] = exj
                    if isa(exj,Expr) && exj.head === :...
                        # splatted object
                        exjs = exj.args[1]
                        n = :($n + length($exjs))
                    elseif isa(n, Expr)
                        # previous expression splatted
                        n = :($n + 1)
                    else
                        # an integer
                        n += 1
                    end
                end
            end
            if used_S && S !== ex.args[1]
                S0 = ex.args[1]
                ex.args[1] = S
                ex = Expr(:let, :($S = $S0), ex)
            end
        else
            # recursive search
            for i = eachindex(ex.args)
                ex.args[i], used = replace_ref_begin_end_!(ex.args[i], withex)
                used_withex |= used
            end
        end
    end
    ex, used_withex
end

"""
    @view A[inds...]

Creates a `SubArray` from an indexing expression. This can only be applied directly to a
reference expression (e.g. `@view A[1,2:end]`), and should *not* be used as the target of
an assignment (e.g. `@view(A[1,2:end]) = ...`).  See also [`@views`](@ref)
to switch an entire block of code to use views for slicing.

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
    if Meta.isexpr(ex, :ref)
        ex = replace_ref_begin_end!(ex)
        if !Meta.isexpr(ex, :let)
            # wrap in a let block to prevent accidentally redefining `view` if used on the LHS, e.g.
            #   @view(A[x]) = 2
            ex = Expr(:let, Expr(:block), ex)
        end
        @assert Meta.isexpr(ex.args[2], :ref)
        ex.args[1] = esc(ex.args[1])
        ex.args[2] = :(view($(map(esc, ex.args[2].args)...)))
        return esc
    else
        throw(ArgumentError("Invalid use of @view macro: argument must be a reference expression A[...]."))
    end
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
        lhs = ex.args[1]
        Expr(ex.head, Meta.isexpr(lhs, :ref) ?
                      Expr(:ref, mapany(_views, lhs.args)...) : _views(lhs),
             _views(ex.args[2]))
    elseif ex.head === :ref
        Expr(:call, maybeview, mapany(_views, ex.args)...)
    else
        h = string(ex.head)
        # don't use view on the lhs of an op-assignment a[i...] += ...
        if last(h) == '=' && Meta.isexpr(ex.args[1], :ref)
            lhs = ex.args[1]

            # temp vars to avoid recomputing a and i,
            # which will be assigned in a let block:
            a = gensym(:a)
            i = [gensym(:i) for k = 1:length(lhs.args)-1]

            # for splatted indices like a[i, j...], we need to
            # splat the corresponding temp var.
            I = similar(i, Any)
            for k = 1:length(i)
                if Meta.isexpr(lhs.args[k+1], :...)
                    I[k] = Expr(:..., i[k])
                    lhs.args[k+1] = lhs.args[k+1].args[1] # unsplat
                else
                    I[k] = i[k]
                end
            end

            Expr(:let,
                 Expr(:block,
                      :($a = $(_views(lhs.args[1]))),
                      [:($(i[k]) = $(_views(lhs.args[k+1]))) for k=1:length(i)]...),
                 Expr(first(h) == '.' ? :(.=) : :(=), :($a[$(I...)]),
                      Expr(:call, Symbol(h[1:end-1]),
                           :($maybeview($a, $(I...))),
                           mapany(_views, ex.args[2:end])...)))
        else
            Expr(ex.head, mapany(_views, ex.args)...)
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

!!! note
    The `@views` macro only affects `array[...]` expressions
    that appear explicitly in the given `expression`, not array slicing that
    occurs in functions called by that code.

!!! compat "Julia 1.5"
    Using `begin` in an indexing expression to refer to the first index requires at least
    Julia 1.5.

# Examples
```jldoctest
julia> A = zeros(3, 3);

julia> @views for row in 1:3
           b = A[row, :]
           b[:] .= row
       end

julia> A
3×3 Matrix{Float64}:
 1.0  1.0  1.0
 2.0  2.0  2.0
 3.0  3.0  3.0
```
"""
macro views(x)
    esc(_views(replace_ref_begin_end!(x)))
end
