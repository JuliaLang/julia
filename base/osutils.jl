# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    @static

Partially evaluate an expression at parse time.

For example, `@static Sys.iswindows() ? foo : bar` will evaluate `Sys.iswindows()` and insert
either `foo` or `bar` into the expression.
This is useful in cases where a construct would be invalid on other platforms,
such as a `ccall` to a non-existent function.
`@static if Sys.isapple() foo end` and `@static foo <&&,||> bar` are also valid syntax.
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
        if hd == :for && ex.args[1].head == :(=)
            var = ex.args[1].args[1]
            itr = Core.eval(__module__, ex.args[1].args[2])
            blk = quote end
            for val in itr
                let_blk = :(let $var = $val end)
                let_blk.args[2] = ex.args[2]
                push!(blk.args, let_blk)
            end
            return esc(blk)
        end
    end
    throw(ArgumentError("invalid @static macro usage"))
end
