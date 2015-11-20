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

# issue 11970
@test parseall("""
macro f(args...) end; @f ""
""") == Expr(:toplevel,
            Expr(:macro, Expr(:call, :f, Expr(:..., :args)), Expr(:block,)),
            Expr(:macrocall, symbol("@f"), ""))

# integer parsing
@test is(parse(Int32,"0",36),Int32(0))
@test is(parse(Int32,"1",36),Int32(1))
@test is(parse(Int32,"9",36),Int32(9))
@test is(parse(Int32,"A",36),Int32(10))
@test is(parse(Int32,"a",36),Int32(10))
@test is(parse(Int32,"B",36),Int32(11))
@test is(parse(Int32,"b",36),Int32(11))
@test is(parse(Int32,"F",36),Int32(15))
@test is(parse(Int32,"f",36),Int32(15))
@test is(parse(Int32,"Z",36),Int32(35))
@test is(parse(Int32,"z",36),Int32(35))

@test parse(Int,"0") == 0
@test parse(Int,"-0") == 0
@test parse(Int,"1") == 1
@test parse(Int,"-1") == -1
@test parse(Int,"9") == 9
@test parse(Int,"-9") == -9
@test parse(Int,"10") == 10
@test parse(Int,"-10") == -10
@test parse(Int64,"3830974272") == 3830974272
@test parse(Int64,"-3830974272") == -3830974272
@test parse(Int,'3') == 3
@test parse(Int,'3', 8) == 3

parsebin(s) = parse(Int,s,2)
parseoct(s) = parse(Int,s,8)
parsehex(s) = parse(Int,s,16)

@test parsebin("0") == 0
@test parsebin("-0") == 0
@test parsebin("1") == 1
@test parsebin("-1") == -1
@test parsebin("10") == 2
@test parsebin("-10") == -2
@test parsebin("11") == 3
@test parsebin("-11") == -3
@test parsebin("1111000011110000111100001111") == 252645135
@test parsebin("-1111000011110000111100001111") == -252645135

@test parseoct("0") == 0
@test parseoct("-0") == 0
@test parseoct("1") == 1
@test parseoct("-1") == -1
@test parseoct("7") == 7
@test parseoct("-7") == -7
@test parseoct("10") == 8
@test parseoct("-10") == -8
@test parseoct("11") == 9
@test parseoct("-11") == -9
@test parseoct("72") == 58
@test parseoct("-72") == -58
@test parseoct("3172207320") == 434704080
@test parseoct("-3172207320") == -434704080

@test parsehex("0") == 0
@test parsehex("-0") == 0
@test parsehex("1") == 1
@test parsehex("-1") == -1
@test parsehex("9") == 9
@test parsehex("-9") == -9
@test parsehex("a") == 10
@test parsehex("-a") == -10
@test parsehex("f") == 15
@test parsehex("-f") == -15
@test parsehex("10") == 16
@test parsehex("-10") == -16
@test parsehex("0BADF00D") == 195948557
@test parsehex("-0BADF00D") == -195948557
@test parse(Int64,"BADCAB1E",16) == 3135023902
@test parse(Int64,"-BADCAB1E",16) == -3135023902
@test parse(Int64,"CafeBabe",16) == 3405691582
@test parse(Int64,"-CafeBabe",16) == -3405691582
@test parse(Int64,"DeadBeef",16) == 3735928559
@test parse(Int64,"-DeadBeef",16) == -3735928559

@test parse(Int,"2\n") == 2
@test parse(Int,"   2 \n ") == 2
@test parse(Int," 2 ") == 2
@test parse(Int,"2 ") == 2
@test parse(Int," 2") == 2
@test parse(Int,"+2\n") == 2
@test parse(Int,"-2") == -2
@test_throws ArgumentError parse(Int,"   2 \n 0")
@test_throws ArgumentError parse(Int,"2x")
@test_throws ArgumentError parse(Int,"-")

# multibyte spaces
@test parse(Int, "3\u2003\u202F") == 3
@test_throws ArgumentError parse(Int, "3\u2003\u202F,")

@test parse(Int,'a') == 10
@test_throws ArgumentError parse(Int,typemax(Char))

@test parse(Int,"1234") == 1234
@test parse(Int,"0x1234") == 0x1234
@test parse(Int,"0o1234") == 0o1234
@test parse(Int,"0b1011") == 0b1011
@test parse(Int,"-1234") == -1234
@test parse(Int,"-0x1234") == -Int(0x1234)
@test parse(Int,"-0o1234") == -Int(0o1234)
@test parse(Int,"-0b1011") == -Int(0b1011)

## FIXME: #4905, do these tests for Int128/UInt128!
for T in (Int8, Int16, Int32, Int64)
    @test parse(T,string(typemin(T))) == typemin(T)
    @test parse(T,string(typemax(T))) == typemax(T)
    @test_throws OverflowError parse(T,string(big(typemin(T))-1))
    @test_throws OverflowError parse(T,string(big(typemax(T))+1))
end

for T in (UInt8,UInt16,UInt32,UInt64)
    @test parse(T,string(typemin(T))) == typemin(T)
    @test parse(T,string(typemax(T))) == typemax(T)
    @test_throws ArgumentError parse(T,string(big(typemin(T))-1))
    @test_throws OverflowError parse(T,string(big(typemax(T))+1))
end

@test parse("1 == 2|>3") == Expr(:comparison, 1, :(==), Expr(:call, :(|>), 2, 3))

# issue #12501 and pr #12502
parse("""
      baremodule A
      "a" in b
      end
      """)
parse("""
      baremodule A
      "a"
      end
      """)

# issue #12626
@test parse("a .÷ 1") == Expr(:call, :.÷, :a, 1)
@test parse("a .÷= 1") == Expr(:.÷=, :a, 1)

# issue #12771
@test -(3)^2 == -9

# issue #13302
let p = parse("try
           a
       catch
           b, c = t
       end")
    @test isa(p,Expr) && p.head === :try
    @test p.args[2] === false
    @test p.args[3].args[end] == parse("b,c = t")
end

# pr #13078
@test parse("a in b in c") == Expr(:comparison, :a, :in, :b, :in, :c)
@test parse("a||b→c&&d") == Expr(:call, :→,
                                 Expr(symbol("||"), :a, :b),
                                 Expr(symbol("&&"), :c, :d))

# issue #11988 -- normalize \r and \r\n in literal strings to \n
@test "foo\nbar" == parse("\"\"\"\r\nfoo\r\nbar\"\"\"") == parse("\"\"\"\nfoo\nbar\"\"\"") == parse("\"\"\"\rfoo\rbar\"\"\"") == parse("\"foo\r\nbar\"") == parse("\"foo\rbar\"") == parse("\"foo\nbar\"")
@test '\r' == first("\r") == first("\r\n") # still allow explicit \r
