# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests for parser and syntax lowering

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
    undot(op) = Symbol(string(op)[2:end])
    for (ex1, ex2) in [("5 .≠ x", "5 .!= x"),
                       ("5 .≥ x", "5 .>= x"),
                       ("5 .≤ x", "5 .<= x")]
        ex1 = parse(ex1); ex2 = parse(ex2)
        @test ex1.head === :call && (ex1.head === ex2.head)
        @test ex1.args[2] === 5 && ex2.args[2] === 5
        @test eval(Main, undot(ex1.args[1])) === eval(Main, undot(ex2.args[1]))
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
@test parse("x'y") == Expr(:call, :*, Expr(Symbol("'"), :x), :y)

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
                                        Expr(Symbol("."), :x,
                                             QuoteNode(Expr(:$, :f))),
                                        :i))

# issue #10994
@test parse("1 + #= \0 =# 2") == :(1 + 2)

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
@test parse("export \$(Symbol(\"A\"))") == :(export $(Expr(:$, :(Symbol("A")))))
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
    macro f(args...) end; @f "macro argument"
""") == Expr(:toplevel,
             Expr(:macro, Expr(:call, :f, Expr(:..., :args)), Expr(:block, LineNumberNode(1, :none))),
             Expr(:macrocall, Symbol("@f"), LineNumberNode(1, :none), "macro argument"))

# blocks vs. tuples
@test parse("()") == Expr(:tuple)
@test parse("(;)") == Expr(:block)
@test parse("(;;;;)") == Expr(:block)
@test_throws ParseError parse("(,)")
@test_throws ParseError parse("(;,)")
@test_throws ParseError parse("(,;)")
@test parse("(x;)") == Expr(:block, :x)
@test parse("(;x)") == Expr(:tuple, Expr(:parameters, :x))
@test parse("(;x,)") == Expr(:tuple, Expr(:parameters, :x))
@test parse("(x,)") == Expr(:tuple, :x)
@test parse("(x,;)") == Expr(:tuple, :x)
@test parse("(x;y)") == Expr(:block, :x, :y)
@test parse("(x=1;y=2)") == Expr(:block, Expr(:(=), :x, 1), Expr(:(=), :y, 2))
@test parse("(x,;y)") == Expr(:tuple, Expr(:parameters, :y), :x)
@test parse("(x,;y=1)") == Expr(:tuple, Expr(:parameters, Expr(:kw, :y, 1)), :x)
@test parse("(x,a;y=1)") == Expr(:tuple, Expr(:parameters, Expr(:kw, :y, 1)), :x, :a)
@test parse("(x,a;y=1,z=2)") == Expr(:tuple, Expr(:parameters, Expr(:kw,:y,1), Expr(:kw,:z,2)), :x, :a)
@test parse("(a=1, b=2)") == Expr(:tuple, Expr(:(=), :a, 1), Expr(:(=), :b, 2))
@test_throws ParseError parse("(1 2)") # issue #15248

# integer parsing
@test parse(Int32,"0",36) === Int32(0)
@test parse(Int32,"1",36) === Int32(1)
@test parse(Int32,"9",36) === Int32(9)
@test parse(Int32,"A",36) === Int32(10)
@test parse(Int32,"a",36) === Int32(10)
@test parse(Int32,"B",36) === Int32(11)
@test parse(Int32,"b",36) === Int32(11)
@test parse(Int32,"F",36) === Int32(15)
@test parse(Int32,"f",36) === Int32(15)
@test parse(Int32,"Z",36) === Int32(35)
@test parse(Int32,"z",36) === Int32(35)

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

# Issue 20587
for T in vcat(subtypes(Signed), subtypes(Unsigned))
    T === BigInt && continue # TODO: make BigInt pass this test
    for s in ["", " ", "  "]
        # Without a base (handles things like "0x00001111", etc)
        result = @test_throws ArgumentError parse(T, s)
        exception_without_base = result.value
        if T == Bool
            if s == ""
                @test exception_without_base.msg == "input string is empty"
            else
                @test exception_without_base.msg == "input string only contains whitespace"
            end
        else
            @test exception_without_base.msg == "input string is empty or only contains whitespace"
        end

        # With a base
        result = @test_throws ArgumentError parse(T, s, 16)
        exception_with_base = result.value
        if T == Bool
            if s == ""
                @test exception_with_base.msg == "input string is empty"
            else
                @test exception_with_base.msg == "input string only contains whitespace"
            end
        else
            @test exception_with_base.msg == "input string is empty or only contains whitespace"
        end
    end

    # Test `tryparse_internal` with part of a string
    let b = "                   "
        result = @test_throws ArgumentError get(Base.tryparse_internal(Bool, b, 7, 11, 0, true))
        exception_bool = result.value
        @test exception_bool.msg == "input string only contains whitespace"

        result = @test_throws ArgumentError get(Base.tryparse_internal(Int, b, 7, 11, 0, true))
        exception_int = result.value
        @test exception_int.msg == "input string is empty or only contains whitespace"

        result = @test_throws ArgumentError get(Base.tryparse_internal(UInt128, b, 7, 11, 0, true))
        exception_uint = result.value
        @test exception_uint.msg == "input string is empty or only contains whitespace"
    end

    # Test that the entire input string appears in error messages
    let s = "     false    true     "
        result = @test_throws(ArgumentError,
            get(Base.tryparse_internal(Bool, s, start(s), endof(s), 0, true)))
        @test result.value.msg == "invalid Bool representation: $(repr(s))"
    end

    # Test that leading and trailing whitespace is ignored.
    for v in (1, 2, 3)
        @test parse(Int, "    $v"    ) == v
        @test parse(Int, "    $v\n"  ) == v
        @test parse(Int, "$v    "    ) == v
        @test parse(Int, "    $v    ") == v
    end
    for v in (true, false)
        @test parse(Bool, "    $v"    ) == v
        @test parse(Bool, "    $v\n"  ) == v
        @test parse(Bool, "$v    "    ) == v
        @test parse(Bool, "    $v    ") == v
    end
    for v in (0.05, -0.05, 2.5, -2.5)
        @test parse(Float64, "    $v"    ) == v
        @test parse(Float64, "    $v\n"  ) == v
        @test parse(Float64, "$v    "    ) == v
        @test parse(Float64, "    $v    ") == v
    end
    @test parse(Float64, "    .5"    ) == 0.5
    @test parse(Float64, "    .5\n"  ) == 0.5
    @test parse(Float64, "    .5    ") == 0.5
    @test parse(Float64, ".5    "    ) == 0.5
end

@test parse(Bool, "\u202f true") === true
@test parse(Bool, "\u202f false") === false

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

# parsing numbers with _ and .
@test parse("1_2.3_4") == 12.34
@test_throws ParseError parse("1._")
@test_throws ParseError parse("1._5")
@test_throws ParseError parse("1e.3")
@test_throws ParseError parse("1e3.")
@test parse("2e_1") == Expr(:call, :*, 2, :e_1)
# issue #17705
@test parse("2e3_") == Expr(:call, :*, 2e3, :_)
@test parse("2e-3_") == Expr(:call, :*, 2e-3, :_)
@test parse("2e3_\"x\"") == Expr(:call, :*, 2e3, Expr(:macrocall, Symbol("@__str"), LineNumberNode(1, :none), "x"))

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

for T in (Int8, Int16, Int32, Int64, Int128)
    @test parse(T,string(typemin(T))) == typemin(T)
    @test parse(T,string(typemax(T))) == typemax(T)
    @test_throws OverflowError parse(T,string(big(typemin(T))-1))
    @test_throws OverflowError parse(T,string(big(typemax(T))+1))
end

for T in (UInt8,UInt16,UInt32,UInt64,UInt128)
    @test parse(T,string(typemin(T))) == typemin(T)
    @test parse(T,string(typemax(T))) == typemax(T)
    @test_throws ArgumentError parse(T,string(big(typemin(T))-1))
    @test_throws OverflowError parse(T,string(big(typemax(T))+1))
end

@test parse("1 == 2|>3") == Expr(:call, :(==), 1, Expr(:call, :(|>), 2, 3))

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
                                 Expr(Symbol("||"), :a, :b),
                                 Expr(Symbol("&&"), :c, :d))

# issue #11988 -- normalize \r and \r\n in literal strings to \n
@test "foo\nbar" == parse("\"\"\"\r\nfoo\r\nbar\"\"\"") ==
    parse("\"\"\"\nfoo\nbar\"\"\"") == parse("\"\"\"\rfoo\rbar\"\"\"") ==
    parse("\"foo\r\nbar\"") == parse("\"foo\rbar\"") == parse("\"foo\nbar\"")
@test '\r' == first("\r") == first("\r\n") # still allow explicit \r

# issue #14561 - generating 0-method generic function def
let fname = :f
    @test :(function $fname end) == Expr(:function, :f)
end

# issue #14977
@test parse("x = 1", 1) == (:(x = 1), 6)
@test parse("x = 1", 6) == (nothing, 6)
@test_throws BoundsError parse("x = 1", 0)
@test_throws BoundsError parse("x = 1", -1)
@test_throws BoundsError parse("x = 1", 7)

# issue #14683
@test_throws ParseError parse("'\\A\"'")
@test parse("'\"'") == parse("'\\\"'") == '"' == "\""[1] == '\42'

@test_throws ParseError parse("f(2x for x=1:10, y")

# issue #15223
call0(f) = f()
call1(f,x) = f(x)
call2(f,x,y) = f(x,y)
@test (call0() do; 42 end) == 42
@test (call1(42) do x; x+1 end) == 43
@test (call2(42,1) do x,y; x+y+1 end) == 44

# definitions using comparison syntax
let a⊂b = reduce(&, x ∈ b for x in a) && length(b)>length(a)
    @test [1,2] ⊂ [1,2,3,4]
    @test !([1,2] ⊂ [1,3,4])
    @test !([1,2] ⊂ [1,2])
end

# issue #9503
@test parse("x<:y") == Expr(:(<:), :x, :y)
@test parse("x>:y") == Expr(:(>:), :x, :y)
@test parse("x<:y<:z").head === :comparison
@test parse("x>:y<:z").head === :comparison

# reason PR #19765, <- operator, was reverted
@test -2<-1 # DO NOT ADD SPACES

# issue #11169
uncalled(x) = @test false
fret() = uncalled(return true)
@test fret()

# issue #9617
let p = 15
    @test 2p+1 == 31  # not a hex float literal
end

# issue #15597
function test_parseerror(str, msg)
    try
        parse(str)
        @test false
    catch e
        @test isa(e,ParseError) && e.msg == msg
    end
end
test_parseerror("0x", "invalid numeric constant \"0x\"")
test_parseerror("0b", "invalid numeric constant \"0b\"")
test_parseerror("0o", "invalid numeric constant \"0o\"")
test_parseerror("0x0.1", "hex float literal must contain \"p\" or \"P\"")
test_parseerror("0x1.0p", "invalid numeric constant \"0x1.0\"")

# issue #15798
@test expand(Main, Base.parse_input_line("""
              try = "No"
           """)) == Expr(:error, "unexpected \"=\"")

# issue #19861 make sure macro-expansion happens in the newest world for top-level expression
@test eval(Base.parse_input_line("""
           macro X19861()
               return 23341
           end
           @X19861
           """)::Expr) == 23341

# test parse_input_line for a streaming IO input
let b = IOBuffer("""
                 let x = x
                     x
                 end
                 f()
                 """)
    @test Base.parse_input_line(b) == Expr(:let, Expr(:(=), :x, :x), Expr(:block, LineNumberNode(2, :none), :x))
    @test Base.parse_input_line(b) == Expr(:call, :f)
    @test Base.parse_input_line(b) === nothing
end

# issue #15763
test_parseerror("if\nfalse\nend", "missing condition in \"if\" at none:1")
test_parseerror("if false\nelseif\nend", "missing condition in \"elseif\" at none:2")

# issue #15828
@test expand(Main, parse("x...")) == Expr(:error, "\"...\" expression outside call")

# issue #15830
@test expand(Main, parse("foo(y = (global x)) = y")) == Expr(:error, "misplaced \"global\" declaration")

# issue #15844
function f15844(x)
    x
end

g15844 = let
    local function f15844(x::Int32)
        2x
    end
end

function add_method_to_glob_fn!()
    global function f15844(x::Int64)
        3x
    end
end

add_method_to_glob_fn!()
@test g15844 !== f15844
@test g15844(Int32(1)) == 2
@test f15844(Int32(1)) == 1
@test f15844(Int64(1)) == 3

# issue #15661
@test_throws ParseError parse("function catch() end")
@test_throws ParseError parse("function end() end")
@test_throws ParseError parse("function finally() end")

# PR #16170
@test expand(Main, parse("true(x) = x")) == Expr(:error, "invalid function name \"true\"")
@test expand(Main, parse("false(x) = x")) == Expr(:error, "invalid function name \"false\"")

# issue #16355
@test expand(Main, :(f(d:Int...) = nothing)) == Expr(:error, "\"d:Int\" is not a valid function argument name")

# issue #16517
@test (try error(); catch; 0; end) === 0
@test (try error(); catch; false; end) === false  # false and true are Bool literals, not variables
@test (try error(); catch; true; end) === true
f16517() = try error(); catch; 0; end
@test f16517() === 0

# issue #16671
@test parse("1.") === 1.0

isline(x) = isa(x, LineNumberNode)

# issue #16672
@test count(isline, parse("begin end").args) == 1
@test count(isline, parse("begin; end").args) == 1
@test count(isline, parse("begin; x+2; end").args) == 1
@test count(isline, parse("begin; x+2; y+1; end").args) == 2

# issue #16736
let
    local lineoffset0 = @__LINE__() + 1
    local lineoffset1 = @__LINE__()
    local lineoffset2 = @__LINE__() - 1
    @test lineoffset0 == lineoffset1 == lineoffset2
end

# issue #16686
@test parse("try x
             catch; test()
                 y
             end") == Expr(:try,
                           Expr(:block,
                                LineNumberNode(1, :none),
                                :x),
                           false,
                           Expr(:block,
                                LineNumberNode(2, :none),
                                Expr(:call, :test),
                                LineNumberNode(3, :none),
                                :y))

# test that pre 0.5 deprecated syntax is a parse error
@test_throws ParseError parse("Int [1,2,3]")
@test_throws ParseError parse("Int [x for x in 1:10]")
@test_throws ParseError parse("foo (x) = x")
@test_throws ParseError parse("foo {T<:Int}(x::T) = x")

@test_throws ParseError parse("Foo .bar")

@test_throws ParseError parse("import x .y")
@test_throws ParseError parse("using x .y")

@test_throws ParseError parse("--x")
@test_throws ParseError parse("stagedfunction foo(x); end")

@test parse("A=>B") == Expr(:call, :(=>), :A, :B)

@test parse("{1,2,3}") == Expr(:braces, 1, 2, 3)
@test parse("{1 2 3 4}") == Expr(:bracescat, Expr(:row, 1, 2, 3, 4))
@test parse("{1 2; 3 4}") == Expr(:bracescat, Expr(:row, 1, 2), Expr(:row, 3, 4))
@test parse("{x for x in 1:10}") == Expr(:braces, :(x for x in 1:10))
@test parse("{x=>y for (x,y) in zip([1,2,3],[4,5,6])}") == Expr(:braces, :(x=>y for (x,y) in zip([1,2,3],[4,5,6])))
@test parse("{:a=>1, :b=>2}") == Expr(:braces, Expr(:call, :(=>), QuoteNode(:a), 1),
                                      Expr(:call, :(=>), QuoteNode(:b), 2))

@test parse("[a,b;c]")  == Expr(:vect, Expr(:parameters, :c), :a, :b)
@test parse("[a,;c]")   == Expr(:vect, Expr(:parameters, :c), :a)
@test parse("a[b,c;d]") == Expr(:ref, :a, Expr(:parameters, :d), :b, :c)
@test parse("a[b,;d]")  == Expr(:ref, :a, Expr(:parameters, :d), :b)
@test_throws ParseError parse("[a,;,b]")
@test parse("{a,b;c}")  == Expr(:braces, Expr(:parameters, :c), :a, :b)
@test parse("{a,;c}")   == Expr(:braces, Expr(:parameters, :c), :a)
@test parse("a{b,c;d}") == Expr(:curly, :a, Expr(:parameters, :d), :b, :c)
@test parse("a{b,;d}")  == Expr(:curly, :a, Expr(:parameters, :d), :b)

# this now is parsed as getindex(Pair{Any,Any}, ...)
@test_throws MethodError eval(parse("(Any=>Any)[]"))
@test_throws MethodError eval(parse("(Any=>Any)[:a=>1,:b=>2]"))

# make sure base can be any Integer
for T in (Int, BigInt)
    let n = parse(T, "123", Int8(10))
        @test n == 123
        @test isa(n, T)
    end
end

# issue #16720
let err = try
    include_string(@__MODULE__, "module A

        function broken()

            x[1] = some_func(

        end

        end")
    catch e
        e
    end
    @test err.line == 7
end

# issue #17065
@test parse(Int, "2") === 2
@test parse(Bool, "true") === true
@test parse(Bool, "false") === false
@test get(tryparse(Bool, "true")) === get(Nullable{Bool}(true))
@test get(tryparse(Bool, "false")) === get(Nullable{Bool}(false))
@test_throws ArgumentError parse(Int, "2", 1)
@test_throws ArgumentError parse(Int, "2", 63)

# issue #17333: tryparse should still throw on invalid base
for T in (Int32, BigInt), base in (0,1,100)
    @test_throws ArgumentError tryparse(T, "0", base)
end

# error throwing branch from #10560
@test_throws ArgumentError Base.tryparse_internal(Bool, "foo", 1, 2, 10, true)

@test tryparse(Float64, "1.23") === Nullable(1.23)
@test tryparse(Float32, "1.23") === Nullable(1.23f0)
@test tryparse(Float16, "1.23") === Nullable(Float16(1.23))

# PR #17393
for op in (:.==, :.&, :.|, :.≤)
    @test parse("a $op b") == Expr(:call, op, :a, :b)
end
for op in (:.=, :.+=)
    @test parse("a $op b") == Expr(op, :a, :b)
end

# issue #17489
let m_error, error_out, filename = Base.source_path()
    m_error = try @eval method_c6(a::(:A)) = 1; catch e; e; end
    error_out = sprint(showerror, m_error)
    @test startswith(error_out, "ArgumentError: invalid type for argument a in method definition for method_c6 at $filename:")

    m_error = try @eval method_c6(::(:A)) = 2; catch e; e; end
    error_out = sprint(showerror, m_error)
    @test startswith(error_out, "ArgumentError: invalid type for argument number 1 in method definition for method_c6 at $filename:")

    m_error = try @eval method_c6(A; B) = 3; catch e; e; end
    error_out = sprint(showerror, m_error)
    @test error_out == "syntax: keyword argument \"B\" needs a default value"

    # issue #20614
    m_error = try @eval foo(types::NTuple{N}, values::Vararg{Any,N}, c) where {N} = nothing; catch e; e; end
    error_out = sprint(showerror, m_error)
    @test startswith(error_out, "ArgumentError: Vararg on non-final argument")
end

# issue #7272
@test expand(Main, parse("let
              global x = 2
              local x = 1
              end")) == Expr(:error, "variable \"x\" declared both local and global")

@test expand(Main, parse("let
              local x = 2
              local x = 1
              end")) == Expr(:error, "local \"x\" declared twice")

@test expand(Main, parse("let x
                  local x = 1
              end")) == Expr(:error, "local \"x\" declared twice")

@test expand(Main, parse("let x = 2
                  local x = 1
              end")) == Expr(:error, "local \"x\" declared twice")

# issue #23673
@test :(let $([:(x=1),:(y=2)]...); x+y end) == :(let x = 1, y = 2; x+y end)

# make sure front end can correctly print values to error messages
let ex = expand(Main, parse("\"a\"=1"))
    @test ex == Expr(:error, "invalid assignment location \"\"a\"\"")
end

# make sure that incomplete tags are detected correctly
# (i.e. error messages in src/julia-parser.scm must be matched correctly
# by the code in base/client.jl)
for (str, tag) in Dict("" => :none, "\"" => :string, "#=" => :comment, "'" => :char,
                       "`" => :cmd, "begin;" => :block, "quote;" => :block,
                       "let;" => :block, "for i=1;" => :block, "function f();" => :block,
                       "f() do x;" => :block, "module X;" => :block, "mutable struct X;" => :block,
                       "struct X;" => :block, "(" => :other, "[" => :other,
                       "begin" => :other, "quote" => :other,
                       "let" => :other, "for" => :other, "function" => :other,
                       "f() do" => :other, "module" => :other, "mutable struct" => :other,
                       "struct" => :other)
    @test Base.incomplete_tag(parse(str, raise=false)) == tag
end

# meta nodes for optional positional arguments
@test expand(Main, :(@inline f(p::Int=2) = 3)).args[2].args[3].inlineable

# issue #16096
module M16096
macro iter()
    return quote
        @inline function foo16096(sub)
            it = 1
        end
    end
end
end
let ex = expand(M16096, :(@iter))
    @test isa(ex, Expr) && ex.head === :thunk
end
let ex = expand(Main, :($M16096.@iter))
    @test isa(ex, Expr) && ex.head === :thunk
end
let thismodule = @__MODULE__,
    ex = expand(thismodule, :(@M16096.iter))
    @test isa(ex, Expr) && ex.head === :thunk
    @test !isdefined(M16096, :foo16096)
    local_foo16096 = eval(@__MODULE__, ex)
    @test local_foo16096(2.0) == 1
    @test !@isdefined foo16096
    @test !@isdefined it
    @test !isdefined(M16096, :foo16096)
    @test !isdefined(M16096, :it)
    @test typeof(local_foo16096).name.module === thismodule
    @test typeof(local_foo16096).name.mt.module === thismodule
    @test getfield(thismodule, typeof(local_foo16096).name.mt.name) === local_foo16096
    @test getfield(thismodule, typeof(local_foo16096).name.name) === typeof(local_foo16096)
    @test !isdefined(M16096, typeof(local_foo16096).name.mt.name)
    @test !isdefined(M16096, typeof(local_foo16096).name.name)
end

macro f16096()
    quote
        g16096($(esc(:x))) = 2x
    end
end
let g = @f16096
    @test g(3) == 6
end
macro f16096_2()
    quote
        g16096_2(; $(esc(:x))=2) = 2x
    end
end
let g = @f16096_2
    @test g() == 4
end

# issue #15838
module A15838
    macro f() end
    const x = :a
end
module B15838
    import ..A15838.@f
    macro f(x); return :x; end
    const x = :b
end
@test A15838.@f() === nothing
@test A15838.@f(1) === :b
let ex = :(A15838.@f(1, 2)), __source__ = LineNumberNode(@__LINE__, Symbol(@__FILE__))
    nometh = try
        macroexpand(@__MODULE__, ex)
        false
    catch ex
        ex
    end::LoadError
    @test nometh.file === string(__source__.file)
    @test nometh.line === __source__.line
    e = nometh.error::MethodError
    @test e.f === getfield(A15838, Symbol("@f"))
    @test e.args === (__source__, @__MODULE__, 1, 2)
end

# issue 10046
for op in ["+", "-", "\$", "|", ".+", ".-", "*", ".*"]
    @test_throws ParseError parse("$op in [+, -]")
end

# issue #17701
@test expand(Main, :(i==3 && i+=1)) == Expr(:error, "invalid assignment location \"==(i, 3) && i\"")

# issue #18667
@test expand(Main, :(true = 1)) == Expr(:error, "invalid assignment location \"true\"")
@test expand(Main, :(false = 1)) == Expr(:error, "invalid assignment location \"false\"")

# PR #15592
let str = "[1] [2]"
    @test_throws ParseError parse(str)
end

# issue 15896 and PR 15913
@test_throws ErrorException eval(:(macro test15896(d; y=0) end))

# Issue #16578 (Lowering) mismatch between push_loc and pop_loc
module TestMeta_16578
using Test
function get_expr_list(ex::CodeInfo)
    return ex.code::Array{Any,1}
end
function get_expr_list(ex::Expr)
    if ex.head == :thunk
        return get_expr_list(ex.args[1])
    else
        return ex.args
    end
end

function count_meta_loc(exprs)
    push_count = 0
    pop_count = 0
    for expr in exprs
        Meta.isexpr(expr, :meta) || continue
        expr = expr::Expr
        if expr.args[1] === :push_loc
            push_count += 1
        elseif expr.args[1] === :pop_loc
            pop_count += 1
        end
        @test push_count >= pop_count
    end
    @test push_count == pop_count
    return push_count
end

function is_return_ssavalue(ex::Expr)
    ex.head === :return && isa(ex.args[1], SSAValue)
end

function is_pop_loc(ex::Expr)
    ex.head === :meta && ex.args[1] === :pop_loc
end

# Macros
macro m1()
    quote
        sin(1)
    end
end
macro m2()
    quote
        1
    end
end
include_string(@__MODULE__, """
macro m3()
    quote
        @m1
    end
end
macro m4()
    quote
        @m2
    end
end
""", "another_file.jl")
m1_exprs = get_expr_list(expand(@__MODULE__, :(@m1)))
m2_exprs = get_expr_list(expand(@__MODULE__, :(@m2)))
m3_exprs = get_expr_list(expand(@__MODULE__, :(@m3)))
m4_exprs = get_expr_list(expand(@__MODULE__, :(@m4)))

# Check the expanded expresion has expected number of matching push/pop
# and the return is handled correctly
@test count_meta_loc(m1_exprs) == 1
@test is_return_ssavalue(m1_exprs[end])
@test is_pop_loc(m1_exprs[end - 1])

@test count_meta_loc(m2_exprs) == 1
@test m2_exprs[end] == :(return 1)
@test is_pop_loc(m2_exprs[end - 1])

@test count_meta_loc(m3_exprs) == 2
@test is_return_ssavalue(m3_exprs[end])
@test is_pop_loc(m3_exprs[end - 1])

@test count_meta_loc(m4_exprs) == 2
@test m4_exprs[end] == :(return 1)
@test is_pop_loc(m4_exprs[end - 1])

function f1(a)
    b = a + 100
    b
end

@generated function f2(a)
    quote
        b = a + 100
        b
    end
end

f1_exprs = get_expr_list(@code_typed(f1(1))[1])
f2_exprs = get_expr_list(@code_typed(f2(1))[1])

@test Meta.isexpr(f1_exprs[end], :return)
@test is_pop_loc(f2_exprs[end])
@test Meta.isexpr(f2_exprs[end - 1], :return)

if Base.JLOptions().code_coverage != 0 && Base.JLOptions().can_inline != 0
    @test count_meta_loc(f1_exprs) == 1
    @test count_meta_loc(f2_exprs) == 2
else
    @test count_meta_loc(f1_exprs) == 0
    @test count_meta_loc(f2_exprs) == 1
end

# Check that string and command literals are parsed to the appropriate macros
@test :(x"s") == :(@x_str "s")
@test :(x"s"flag) == :(@x_str "s" "flag")
@test :(x"s\"`\x\$\\") == :(@x_str "s\"`\\x\\\$\\\\")
@test :(x`s`) == :(@x_cmd "s")
@test :(x`s`flag) == :(@x_cmd "s" "flag")
@test :(x`s\`"\x\$\\`) == :(@x_cmd "s`\"\\x\\\$\\\\")

# Check multiline command literals
@test :(@cmd "multiline\ncommand\n") == :```
multiline
command
```

macro julia_cmd(s)
    Meta.quot(parse(s))
end
@test julia```
if test + test == test
    println(test)
end
```.head == :if

end

# issue 18756
module Mod18756
mutable struct Type
end
end
@test method_exists(Mod18756.Type, ())

# issue 18002
@test parse("Foo{T} = Bar{T}") == Expr(:(=), Expr(:curly, :Foo, :T), Expr(:curly, :Bar, :T))

# don't insert push_loc for filename `none` at the top level
let ex = expand(Main, parse("""
begin
    x = 1
end"""))
    @test !any(x->(x == Expr(:meta, :push_loc, :none)), ex.args)
end

# Check qualified string macros
Base.r"regex" == r"regex"

module QualifiedStringMacro
module SubModule
macro x_str(x)
    1
end
macro y_cmd(x)
    2
end
end
end

@test QualifiedStringMacro.SubModule.x"" === 1
@test QualifiedStringMacro.SubModule.y`` === 2

let ..(x,y) = x + y
    @test 3 .. 4 === 7
end

# issue #7669
@test parse("@a(b=1, c=2)") == Expr(:macrocall, Symbol("@a"), LineNumberNode(1, :none), :(b=1), :(c=2))

# issue #19685
let f = function (x; kw...)
            return (x, kw)
        end,
    g = function (x; a = 2)
            return (x, a)
        end
    @test f(1) == (1, Any[])
    @test g(1) == (1, 2)
end

# normalization of Unicode symbols (#19464)
let ε=1, μ=2, x=3, î=4
    # issue #5434 (mu vs micro):
    @test parse("\u00b5") === parse("\u03bc")
    @test µ == μ == 2
    # NFC normalization of identifiers:
    @test parse("\u0069\u0302") === parse("\u00ee")
    @test î == 4
    # latin vs greek ε (#14751)
    @test parse("\u025B") === parse("\u03B5")
    @test ɛ == ε == 1
end

# issue #8925
let
    global const (c8925, d8925) = (3, 4)
end
@test c8925 == 3 && isconst(@__MODULE__, :c8925)
@test d8925 == 4 && isconst(@__MODULE__, :d8925)

# issue #18754: parse ccall as a regular function
@test parse("ccall([1], 2)[3]") == Expr(:ref, Expr(:call, :ccall, Expr(:vect, 1), 2), 3)
@test parse("ccall(a).member") == Expr(:., Expr(:call, :ccall, :a), QuoteNode(:member))

# Check that the body of a `where`-qualified short form function definition gets
# a :block for its body
short_where_call = :(f(x::T) where T = T)
@test short_where_call.args[2].head == :block

# `where` with multi-line anonymous functions
let f = function (x::T) where T
            T
        end
    @test f(:x) === Symbol
end

let f = function (x::T, y::S) where T<:S where S
            (T,S)
        end
    @test f(0,1) === (Int,Int)
end

# issue #20541
@test parse("[a .!b]") == Expr(:hcat, :a, Expr(:call, :(.!), :b))

@test expand(Main, :(a{1} = b)) == Expr(:error, "invalid type parameter name \"1\"")
@test expand(Main, :(a{2<:Any} = b)) == Expr(:error, "invalid type parameter name \"2\"")

# issue #20653
@test_throws UndefVarError Base.call(::Int) = 1
module Test20653
using Test
struct A
end
call(::A) = 1
const a = A()
@test_throws MethodError a()
@test call(a) === 1
end

# issue #20729
macro m20729()
    ex = Expr(:head)
    resize!(ex.args, 1)
    return ex
end

@test_throws ErrorException eval(@__MODULE__, :(@m20729))
@test expand(@__MODULE__, :(@m20729)) == Expr(:error, "undefined reference in AST")

macro err20000()
    return Expr(:error, "oops!")
end

@test expand(@__MODULE__, :(@err20000)) == Expr(:error, "oops!")

# issue #20000
@test parse("@m(a; b=c)") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1, :none),
                                  Expr(:parameters, Expr(:kw, :b, :c)), :a)

# issue #21054
macro make_f21054(T)
    quote
        $(esc(:f21054))(X::Type{<:$T}) = 1
    end
end
@eval @make_f21054 $Array
@test isa(f21054, Function)
g21054(>:) = >:2
@test g21054(-) == -2

# issue #21168
@test expand(Main, :(a.[1])) == Expr(:error, "invalid syntax a.[1]")
@test expand(Main, :(a.{1})) == Expr(:error, "invalid syntax a.{1}")

# Issue #21225
let abstr = parse("abstract type X end")
    @test parse("abstract type X; end") == abstr
    @test parse(string("abstract type X", ";"^5, " end")) == abstr
    @test parse("abstract type X\nend") == abstr
    @test parse(string("abstract type X", "\n"^5, "end")) == abstr
end
let prim = parse("primitive type X 8 end")
    @test parse("primitive type X 8; end") == prim
    @test parse(string("primitive type X 8", ";"^5, " end")) == prim
    @test parse("primitive type X 8\nend") == prim
    @test parse(string("primitive type X 8", "\n"^5, "end")) == prim
end

# issue #21155
@test filter(!isline,
             parse("module B
                        using ..x,
                              ..y
                    end").args[3].args)[1] ==
      Expr(:toplevel,
           Expr(:using, Symbol("."), Symbol("."), :x),
           Expr(:using, Symbol("."), Symbol("."), :y))

@test filter(!isline,
             parse("module A
                        using .B,
                              .C
                    end").args[3].args)[1] ==
      Expr(:toplevel,
           Expr(:using, Symbol("."), :B),
           Expr(:using, Symbol("."), :C))

# issue #21440
@test parse("+(x::T,y::T) where {T} = 0") == parse("(+)(x::T,y::T) where {T} = 0")
@test parse("a::b::c") == Expr(:(::), Expr(:(::), :a, :b), :c)

# issue #21545
f21545(::Type{<: AbstractArray{T,N} where N}) where T = T
@test f21545(Array{Int8}) === Int8
@test parse("<:{T} where T") == Expr(:where, Expr(:curly, :(<:), :T), :T)
@test parse("<:(T) where T") == Expr(:where, Expr(:(<:), :T), :T)
@test parse("<:{T}(T) where T") == Expr(:where, Expr(:call, Expr(:curly, :(<:), :T), :T), :T)

# issue #21586
macro m21586(x)
    Expr(:kw, esc(x), 42)
end

f21586(; @m21586(a), @m21586(b)) = a + b
@test f21586(a=10) == 52

# issue #21604
@test_nowarn @eval module Test21604
    const Foo = Any
    struct X
        x::Foo
    end
end
@test Test21604.X(1.0) === Test21604.X(1.0)

# issue #20575
@test_throws ParseError parse("\"a\"x")
@test_throws ParseError parse("\"a\"begin end")

# comment 298107224 on pull #21607
module Test21607
    using Test
    const Any = Integer

    # check that X <: Core.Any, not Integer
    mutable struct X; end
    @test supertype(X) === Core.Any

    # check that return type is Integer
    f()::Any = 1.0
    @test f() === 1

    # check that constructor accepts Any
    struct Y
        x
    end
    @test Y(1.0) !== Y(1)

    # check that function default argument type is Any
    g(x) = x
    @test g(1.0) === 1.0

    # check that asserted variable type is Integer
    @test let
        x::Any = 1.0
        x
    end === 1

    # check that unasserted variable type is not Integer
    @test let
        x = 1.0
        x
    end === 1.0
end

# issue #16937
@test expand(Main, :(f(2, a=1, w=3, c=3, w=4, b=2))) ==
    Expr(:error, "keyword argument \"w\" repeated in call to \"f\"")

let f(x) =
      g(x) = 1
    @test functionloc(f(1))[2] > functionloc(f)[2]
end

# let-bound functions with `where` and static parameters
@test let f()::Int = 2.0
    f()
end === 2
@test let (f(x::T)::Tuple{Int,Any}) where {T} = (3.0, T)
    f("")
end === (3, String)

# operator suffixes
@test parse("3 +̂ 4") == Expr(:call, :+̂, 3, 4)
@test parse("3 +̂′ 4") == Expr(:call, :+̂′, 3, 4)
@test parse("3 +⁽¹⁾ 4") == Expr(:call, :+⁽¹⁾, 3, 4)
@test parse("3 +₍₀₎ 4") == Expr(:call, :+₍₀₎, 3, 4)
for bad in ('=', '$', ':', "||", "&&", "->", "<:")
    @test_throws ParseError parse("3 $(bad)⁽¹⁾ 4")
end
@test Base.operator_precedence(:+̂) == Base.operator_precedence(:+)

# issue #19351
# adding return type decl should not affect parse of function body
@test :(t(abc) = 3).args[2] == :(t(abc)::Int = 3).args[2]

# issue #7314
@test parse("local x, y = 1, 2") == Expr(:local, Expr(:(=),
                                                      Expr(:tuple, :x, :y),
                                                      Expr(:tuple, 1, 2)))

@test_throws ParseError parse("[2for i=1:10]")
@test_throws ParseError parse("[1 for i in 1:2for j in 2]")
@test_throws ParseError parse("(1 for i in 1:2for j in 2)")
# issue #20441
@test_throws ParseError parse("[x.2]")
@test_throws ParseError parse("x.2")
@test parse("[x;.2]") == Expr(:vcat, :x, 0.2)

# issue #22840
@test parse("[:a :b]") == Expr(:hcat, QuoteNode(:a), QuoteNode(:b))

# issue #22868
@test_throws ParseError parse("x@time 2")
@test_throws ParseError parse("@ time")

# issue #7479
@test expand(Main, parse("(true &&& false)")) == Expr(:error, "misplaced \"&\" expression")

# if an indexing expression becomes a cat expression, `end` is not special
@test_throws ParseError parse("a[end end]")
@test_throws ParseError parse("a[end;end]")
#@test_throws ParseError parse("a[end;]")  # this is difficult to fix
let a = rand(8), i = 3
    @test a[[1:i-1; i+1:end]] == a[[1,2,4,5,6,7,8]]
end

# issue #18935
@test [begin
          @inbounds for i = 1:10 end
       end for i = 1:5] == fill(nothing, 5)

# issue #18912
@test_throws ParseError parse("(::)")
@test parse(":(::)") == QuoteNode(Symbol("::"))
@test_throws ParseError parse("f(::) = ::")
@test parse("(::A)") == Expr(Symbol("::"), :A)
@test_throws ParseError parse("(::, 1)")
@test_throws ParseError parse("(1, ::)")

# issue #18650
let ex = parse("maximum(@elapsed sleep(1) for k = 1:10)")
    @test isa(ex, Expr) && ex.head === :call && ex.args[2].head === :generator &&
        ex.args[2].args[1].head === :macrocall
end

# issue #23173
@test_throws ErrorException("invalid module path") eval(:(import $(:.)))

# issue #23234
let
    f = function (x=0)
        x
    end
    @test f() == 0
    @test f(2) == 2
end

# issue #18730
@test expand(Main, quote
        function f()
            local Int
            x::Int -> 2
        end
    end) == Expr(:error, "local variable Int cannot be used in closure declaration")
