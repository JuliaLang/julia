# issue #9684
let
    for (ex1, ex2) in [("5.≠x", "5.!=x"),
                       ("5.≥x", "5.>=x"),
                       ("5.≤x", "5.<=x")]
        ex1 = parse(ex1); ex2 = parse(ex2)
        @test ex1.head === :comparison && (ex1.head === ex2.head)
        @test ex1.args[1] === 5 && ex2.args[1] === 5
        @test is(eval(Main, ex1.args[2]), eval(Main, ex2.args[2]))
        @test ex1.args[3] === :x && (ex1.args[3] === ex2.args[3])
    end
end

# issue #9704
let a = :a
    @test :(try
            catch $a
            end) == :(try
                      catch a
                      end)
    @test :(module $a
            end) == :(module a
                      end)
end

# string literals
macro test999_str(args...); args; end
@test test999"a"b == ("a","b")
@test test999"""a"""b == ("a","b")
@test test999"
    a
    b" == ("
    a
    b",)
@test test999"""
    a
    b""" == ("a\nb",)

# issue #5997
@test_throws ParseError parse(": x")
@test_throws ParseError parse("d[: 2]")

# issue #6770
@test_throws ParseError parse("x.3")

# issue #8763
@test_throws ParseError parse("sqrt(16)2")
@test_throws ParseError parse("x' y")
@test_throws ParseError parse("x 'y")
@test parse("x'y") == Expr(:call, :*, Expr(symbol("'"), :x), :y)

# issue #8301
@test_throws ParseError parse("&*s")

# issue #10677
@test_throws ParseError parse("/1")
@test_throws ParseError parse("/pi")
@test parse("- = 2") == Expr(:(=), :(-), 2)
@test parse("/ = 2") == Expr(:(=), :(/), 2)
@test_throws ParseError parse("< : 2")
@test_throws ParseError parse("+ : 2")
@test_throws ParseError parse("< :2")
@test parse("+ :2") == Expr(:call, :(+), QuoteNode(2))
