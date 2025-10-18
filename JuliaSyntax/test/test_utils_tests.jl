# Tests for the test_utils go here to allow the utils to be included on their
# own without invoking the tests.
@testset "Reference parser bugs" begin
    # `0x1.8p0f`
    @test exprs_roughly_equal(1.5,
                              Expr(:call, :*, 1.5, :f))
    @test exprs_roughly_equal(1.5,
                              Expr(:call, :*, 1.5, :f0))
    # `@f(a=1) do \n end`
    @test exprs_roughly_equal(Expr(:do, Expr(:macrocall, Symbol("@f"), LineNumberNode(1), Expr(:kw, :a, 1)),
                                   Expr(:->, Expr(:tuple), Expr(:block, LineNumberNode(1)))),
                              Expr(:do, Expr(:macrocall, Symbol("@f"), LineNumberNode(1), Expr(:(=), :a, 1)),
                                   Expr(:->, Expr(:tuple), Expr(:block, LineNumberNode(1)))))
    # `"""\n  a\n \n  b"""`
    @test exprs_roughly_equal("a\n \nb", " a\n\n b")
    @test !exprs_roughly_equal("a\n x\nb", " a\n x\n b")
    @test exprs_roughly_equal("a\n x\nb", "a\n x\nb")
    # `(a; b,)`
    @test exprs_roughly_equal(Expr(:block, :a, LineNumberNode(1), :b),
                              Expr(:tuple, Expr(:parameters, :b), :a))
    @test !exprs_roughly_equal(Expr(:block, :a, LineNumberNode(1), :b),
                               Expr(:tuple, Expr(:parameters, :c), :a))
    @test !exprs_roughly_equal(Expr(:block, :a, LineNumberNode(1), :b),
                               Expr(:tuple, Expr(:parameters, :b), :c))
    @test !exprs_roughly_equal(Expr(:block, :a, LineNumberNode(1), :b, :c),
                               Expr(:tuple, Expr(:parameters, :b), :a))

    # Line numbers for short form function defs in `for` :-(
    @test exprs_roughly_equal(Expr(:for, Expr(:(=),
                                              Expr(:call, :f),
                                              1),
                                   Expr(:block, LineNumberNode(1))),
                              Expr(:for, Expr(:(=),
                                              Expr(:call, :f),
                                              Expr(:block, LineNumberNode(1), 1)),
                                   Expr(:block, LineNumberNode(1))))
end

