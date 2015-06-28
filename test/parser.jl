# This file is a part of Julia. License is MIT: http://julialang.org/license

function parseall(str)
    pos = start(str)
    exs = []
    while !done(str, pos)
        ex, pos = parse(str, pos)
        push!(exs, ex)
    end
    if length(exs) == 0
        throw(ParseError("end of input"))
    elseif length(exs) == 1
        return exs[1]
    else
        return Expr(:block, exs...)
    end
end

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
    @test :(module $a end) == :(module a
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

# issue #10900
@test_throws ParseError parse("+=")
@test_throws ParseError parse(".")
@test_throws ParseError parse("...")

# issue #10901
@test parse("/([1], 1)[1]") == :(([1] / 1)[1])

# issue #10997
@test parse(":(x.\$f[i])") == Expr(:quote,
                                   Expr(:ref,
                                        Expr(symbol("."), :x,
                                             Expr(:$, Expr(:call, TopNode(:Expr),
                                                           QuoteNode(:quote),
                                                           :f))),
                                        :i))

# issue #10994
@test parse("1 + #= \0 =# 2") == :(1 + 2)

# issue #10985
@test expand(:(f(::Int...) = 1)).head == :method

# issue #10910
@test parse(":(using A)") == Expr(:quote, Expr(:using, :A))
@test parse(":(using A.b, B)") == Expr(:quote,
                                       Expr(:toplevel,
                                            Expr(:using, :A, :b),
                                            Expr(:using, :B)))
@test parse(":(using A: b, c.d)") == Expr(:quote,
                                          Expr(:toplevel,
                                               Expr(:using, :A, :b),
                                               Expr(:using, :A, :c, :d)))

@test parse(":(importall A)") == Expr(:quote, Expr(:importall, :A))

@test parse(":(import A)") == Expr(:quote, Expr(:import, :A))
@test parse(":(import A.b, B)") == Expr(:quote,
                                        Expr(:toplevel,
                                             Expr(:import, :A, :b),
                                             Expr(:import, :B)))
@test parse(":(import A: b, c.d)") == Expr(:quote,
                                           Expr(:toplevel,
                                                Expr(:import, :A, :b),
                                                Expr(:import, :A, :c, :d)))

# issue #11332
@test parse("export \$(symbol(\"A\"))") == :(export $(Expr(:$, :(symbol("A")))))
@test parse("export \$A") == :(export $(Expr(:$, :A)))
@test parse("using \$a.\$b") == Expr(:using, Expr(:$, :a), Expr(:$, :b))
@test parse("using \$a.\$b, \$c") == Expr(:toplevel, Expr(:using, Expr(:$, :a),
                                                          Expr(:$, :b)),
                                          Expr(:using, Expr(:$, :c)))
@test parse("using \$a: \$b, \$c.\$d") ==
    Expr(:toplevel, Expr(:using, Expr(:$, :a), Expr(:$, :b)),
         Expr(:using, Expr(:$, :a), Expr(:$, :c), Expr(:$, :d)))

# fix pr #11338 and test for #11497
@test parseall("using \$\na") == Expr(:block, Expr(:using, :$), :a)
@test parseall("using \$,\na") == Expr(:toplevel, Expr(:using, :$),
                                       Expr(:using, :a))
@test parseall("using &\na") == Expr(:block, Expr(:using, :&), :a)

@test parseall("a = &\nb") == Expr(:block, Expr(:(=), :a, :&), :b)
@test parseall("a = \$\nb") == Expr(:block, Expr(:(=), :a, :$), :b)
@test parseall(":(a = &\nb)") == Expr(:quote, Expr(:(=), :a, Expr(:&, :b)))
@test parseall(":(a = \$\nb)") == Expr(:quote, Expr(:(=), :a, Expr(:$, :b)))
