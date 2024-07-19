# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    @static

Partially evaluate an expression at macro expansion time.

This is useful in cases where a construct would be invalid in some cases, such as a `ccall`
to an os-dependent function, or macros defined in packages that are not imported.

`@static` requires a conditional. The conditional can be in an `if` statement, a ternary
operator, or `&&`\`||`. The conditional is evaluated by recursively expanding macros,
lowering and executing the resulting expressions. Then, the matching branch (if any) is
returned. All the other branches of the conditional are deleted before they are
macro-expanded (and lowered or executed).

# Example

Suppose we want to parse an expression `expr` that is valid only on macOS. We could solve
this problem using `@static` with `@static if Sys.isapple() expr end`. In case we had
`expr_apple` for macOS and `expr_others` for the other operating systems, the solution with
`@static` would be `@static Sys.isapple() ? expr_apple : expr_others`.
"""
macro static(ex)
    if isa(ex, Expr)
        @label loop
        hd = ex.head
        if hd ∈ (:if, :elseif, :&&, :||)
            cond = Core.eval(__module__, ex.args[1])::Bool
            if xor(cond, hd === :||)
                return esc(ex.args[2])
            elseif length(ex.args) == 3
                br = ex.args[3]
                if br isa Expr && br.head === :elseif
                    ex = br
                    @goto loop
                else
                    return esc(ex.args[3])
                end
            elseif hd ∈ (:if, :elseif)
                return nothing
            else
                return cond
            end
        end
    end
    throw(ArgumentError("invalid @static macro"))
end
