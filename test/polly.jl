# This file is a part of Julia. License is MIT: http://julialang.org/license

function is_syntax_equal(x::Expr, y::Expr)
    if x.head === y.head
        if x.head === :line
            # `:line` expressions are treated as syntactically equivalent
            # regardless of their actual arguments
            return true
        else
            return is_syntax_of_args_equal(x, y)
        end
    end
    return false
end

is_syntax_equal(x, y) = x == y

function is_syntax_of_args_equal(x::Expr, y::Expr)
    if length(x.args) != length(y.args)
        return false
    end

    for i in eachindex(x.args)
        if !is_syntax_equal(x.args[i], y.args[i])
            return false
        end
    end

    return true
end

# Test whether `Base.Polly.canonicalize!()` works for a `UnitRange`-based loop.
let single_unit_range_loop = quote
        for i = 1:10
        end
    end

    expected = quote
        for i = Base.Polly.UnitRange(1,10)
        end
    end

    Base.Polly.canonicalize!(single_unit_range_loop)
    @test is_syntax_equal(single_unit_range_loop, expected)
end

# Test whether `Base.Polly.canonicalize!()` works for a `StepRange`-based loop.
let single_step_range_loop = quote
        for i = 1:2:10
        end
    end

    expected = quote
        for i = Base.Polly.StepRange(1,2,10)
        end
    end

    Base.Polly.canonicalize!(single_step_range_loop)
    @test is_syntax_equal(single_step_range_loop, expected)
end

# Test whether `Base.Polly.canonicalize!()` works for nested range-based loops.
let nested_loops = quote
        for i = 1:10, j = i:3:20
            for k = i:j
            end
        end
    end

    expected = quote
        for i = Base.Polly.UnitRange(1,10), j = Base.Polly.StepRange(i,3,20)
            for k = Base.Polly.UnitRange(i,j)
            end
        end
    end

    Base.Polly.canonicalize!(nested_loops)
    @test is_syntax_equal(nested_loops, expected)
end

# Test whether `Base.Polly.canonicalize!()` works for successive range-based
# loops.
let successive_loops = quote
        for i = 1:10
        end

        for j = 1:2:10
        end
    end

    expected = quote
        for i = Base.Polly.UnitRange(1,10)
        end

        for j = Base.Polly.StepRange(1,2,10)
        end
    end

    Base.Polly.canonicalize!(successive_loops)
    @test is_syntax_equal(successive_loops, expected)
end

# Test whether `Base.Polly.canonicalize!()` works for loops nested inside
# `if`-statements
let loops_inside_if = quote
        if some_condition
            for i = 1:10
            end
        else
            for j = 1:2:10
            end
        end
    end

    expected = quote
        if some_condition
            for i = Base.Polly.UnitRange(1,10)
            end
        else
            for j = Base.Polly.StepRange(1,2,10)
            end
        end
    end

    Base.Polly.canonicalize!(loops_inside_if)
    @test is_syntax_equal(loops_inside_if, expected)
end

# Test whether `Base.Polly.canonicalize!()` works for a more complex AST.
let trmm = quote
       function trmm(alpha, A, B)
           m,n = size(B)
           for i = 1:m, j = 1:n
               for k = (i+1):m
                   B[i,j] += A[k,i] * B[k,j]
               end
               B[i,j] = alpha * B[i,j]
           end
       end
    end

    expected = quote
        function trmm(alpha, A, B)
            m,n = size(B)
            for i = Base.Polly.UnitRange(1,m), j = Base.Polly.UnitRange(1,n)
                for k = Base.Polly.UnitRange((i+1),m)
                    B[i,j] += A[k,i] * B[k,j]
                end
                B[i,j] = alpha * B[i,j]
            end
        end
    end

    Base.Polly.canonicalize!(trmm)
    @test is_syntax_equal(trmm, expected)
end
