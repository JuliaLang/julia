# This file is a part of Julia. License is MIT: http://julialang.org/license

# Base.Cartesian

"""
    @nall N expr

`@nall 3 d->(i_d > 1)` would generate the expression `(i_1 > 1 && i_2 > 1 && i_3 > 1)`. This
can be convenient for bounds-checking.
"""
:(Cartesian.@nall)

"""
    @nloops N itersym rangeexpr bodyexpr
    @nloops N itersym rangeexpr preexpr bodyexpr
    @nloops N itersym rangeexpr preexpr postexpr bodyexpr

Generate `N` nested loops, using `itersym` as the prefix for the iteration variables.
`rangeexpr` may be an anonymous-function expression, or a simple symbol `var` in which case
the range is `1:size(var,d)` for dimension `d`.

Optionally, you can provide "pre" and "post" expressions. These get executed first and last,
respectively, in the body of each loop. For example, :

    @nloops 2 i A d->j_d=min(i_d,5) begin
        s += @nref 2 A j
    end

would generate :

    for i_2 = 1:size(A, 2)
        j_2 = min(i_2, 5)
        for i_1 = 1:size(A, 1)
            j_1 = min(i_1, 5)
            s += A[j_1,j_2]
        end
    end

If you want just a post-expression, supply `nothing` for the pre-expression. Using
parenthesis and semicolons, you can supply multi-statement expressions.
"""
:(Cartesian.@nloops)

"""
    @ntuple N expr

Generates an `N`-tuple. `@ntuple 2 i` would generate `(i_1, i_2)`, and `@ntuple 2 k->k+1`
would generate `(2,3)`.
"""
:(Cartesian.@ntuple)

"""
    @nif N conditionexpr expr
    @nif N conditionexpr expr elseexpr

Generates a sequence of `if ... elseif ... else ... end` statements. For example:

    @nif 3 d->(i_d >= size(A,d)) d->(error("Dimension ", d, " too big")) d->println("All OK")

would generate:

    if i_1 > size(A, 1)
        error("Dimension ", 1, " too big")
    elseif i_2 > size(A, 2)
        error("Dimension ", 2, " too big")
    else
        println("All OK")
    end
"""
:(Cartesian.@nif)

"""
    @nref N A indexexpr

Generate expressions like `A[i_1,i_2,...]`. `indexexpr` can either be an iteration-symbol
prefix, or an anonymous-function expression.
"""
:(Cartesian.@nref)

"""
    @nexprs N expr

Generate `N` expressions. `expr` should be an anonymous-function expression.
"""
:(Cartesian.@nexprs)
