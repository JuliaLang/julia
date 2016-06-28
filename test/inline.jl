# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "inline" begin
"""
Helper to walk the AST and call a function on every node.
"""
function walk(func, expr)
    func(expr)
    if isa(expr, Expr)
        func(expr.head)
        for o in expr.args
            walk(func, o)
        end
    end
end

"""
Helper to test that every slot is in range after inlining.
"""
function test_inlined_symbols(func, argtypes)
    linfo = code_typed(func, argtypes)[1]
    nl = length(linfo.slottypes)
    ast = Expr(:body); ast.args = Base.uncompressed_ast(linfo)
    walk(ast) do e
        if isa(e, Slot)
            @test 1 <= e.id <= nl
        end
        if isa(e, NewvarNode)
            @test 1 <= e.slot.id <= nl
        end
    end
end

# Test case 1:
# Make sure that all symbols are properly escaped after inlining
# https://github.com/JuliaLang/julia/issues/12620
@inline function test_inner(count)
    x = 1
    i = 0
    while i <= count
        y = x
        x = x + y
        i += 1
    end
end
function test_outer(a)
    test_inner(a)
end
test_inlined_symbols(test_outer, Tuple{Int64})

# Test case 2:
# Make sure that an error is thrown for the undeclared
# y in the else branch.
# https://github.com/JuliaLang/julia/issues/12620
@inline function foo_inl(x)
    if x
        y = 2
    else
        return y
    end
end
function bar12620()
    for i = 1:3
        foo_inl(i==1)
    end
end
@test_throws UndefVarError bar12620()

# issue #16165
@inline f16165(x) = (x = UInt(x) + 1)
g16165(x) = f16165(x)
@test g16165(1) === (UInt(1) + 1)

end
