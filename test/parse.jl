# This file is a part of Julia. License is MIT: http://julialang.org/license

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
    for (ex1, ex2) in [("5.≠x", "5.!=x"),
                       ("5.≥x", "5.>=x"),
                       ("5.≤x", "5.<=x")]
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
macro f(args...) end; @f ""
""") == Expr(:toplevel,
             Expr(:macro, Expr(:call, :f, Expr(:..., :args)), Expr(:block, Expr(:line, 1, :none))),
             Expr(:macrocall, Symbol("@f"), ""))

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
@test parse("2e3_\"x\"") == Expr(:call, :*, 2e3, Expr(:macrocall, Symbol("@__str"), "x"))

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
@test "foo\nbar" == parse("\"\"\"\r\nfoo\r\nbar\"\"\"") == parse("\"\"\"\nfoo\nbar\"\"\"") == parse("\"\"\"\rfoo\rbar\"\"\"") == parse("\"foo\r\nbar\"") == parse("\"foo\rbar\"") == parse("\"foo\nbar\"")
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
@test expand(Base.parse_input_line("""
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
    @test Base.parse_input_line(b) == Expr(:let, Expr(:block, Expr(:line, 2, :none), :x), Expr(:(=), :x, :x))
    @test Base.parse_input_line(b) == Expr(:call, :f)
    @test Base.parse_input_line(b) === nothing
end

# issue #15763
# TODO enable post-0.5
#test_parseerror("if\nfalse\nend", "missing condition in \"if\" at none:1")
test_parseerror("if false\nelseif\nend", "missing condition in \"elseif\" at none:2")

# issue #15828
@test expand(parse("x...")) == Expr(:error, "\"...\" expression outside call")

# issue #15830
@test expand(parse("foo(y = (global x)) = y")) == Expr(:error, "misplaced \"global\" declaration")

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
@test expand(parse("true(x) = x")) == Expr(:error, "invalid function name \"true\"")
@test expand(parse("false(x) = x")) == Expr(:error, "invalid function name \"false\"")

# issue #16355
@test expand(:(f(d:Int...)=nothing)) == Expr(:error, "\"d:Int\" is not a valid function argument name")

# issue #16517
@test (try error(); catch 0; end) === 0
@test (try error(); catch false; end) === false  # false and true are Bool literals, not variables
@test (try error(); catch true; end) === true
f16517() = try error(); catch 0; end
@test f16517() === 0

# issue #16671
@test parse("1.") === 1.0

# issue #16672
let isline(x) = isa(x,Expr) && x.head === :line
    @test count(isline, parse("begin end").args) == 1
    @test count(isline, parse("begin; end").args) == 1
    @test count(isline, parse("begin; x+2; end").args) == 1
    @test count(isline, parse("begin; x+2; y+1; end").args) == 2
end

# issue #16736
let
    local lineoffset0 = @__LINE__ + 1
    local lineoffset1 = @__LINE__
    local lineoffset2 = @__LINE__ - 1
    @test lineoffset0 == lineoffset1 == lineoffset2
end

# issue #16686
@test parse("try x
             catch test()
                 y
             end") == Expr(:try,
                           Expr(:block,
                                Expr(:line, 1, :none),
                                :x),
                           false,
                           Expr(:block,
                                Expr(:line, 2, :none),
                                Expr(:call, :test),
                                Expr(:line, 3, :none),
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

#@test_throws ParseError parse("{1,2,3}")
#@test_throws ParseError parse("{1 2 3 4}")
#@test_throws ParseError parse("{1,2; 3,4}")
@test_throws ParseError parse("{x for x in 1:10}")
@test_throws ParseError parse("{x=>y for (x,y) in zip([1,2,3],[4,5,6])}")
#@test_throws ParseError parse("{:a=>1, :b=>2}")

# this now is parsed as getindex(Pair{Any,Any}, ...)
@test_throws MethodError eval(parse("(Any=>Any)[]"))
@test_throws MethodError eval(parse("(Any=>Any)[:a=>1,:b=>2]"))
# to be removed post 0.5
#@test_throws MethodError eval(parse("(Any=>Any)[x=>y for (x,y) in zip([1,2,3],[4,5,6])]"))

# make sure base can be any Integer
for T in (Int, BigInt)
    let n = parse(T, "123", Int8(10))
        @test n == 123
        @test isa(n, T)
    end
end

# issue #16720
let err = try
    include_string("module A

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
end

# issue #7272
@test expand(parse("let
              global x = 2
              local x = 1
              end")) == Expr(:error, "variable \"x\" declared both local and global")

@test expand(parse("let
              local x = 2
              local x = 1
              end")) == Expr(:error, "local \"x\" declared twice")

@test expand(parse("let x
                  local x = 1
              end")) == Expr(:error, "local \"x\" declared twice")

@test expand(parse("let x = 2
                  local x = 1
              end")) == Expr(:error, "local \"x\" declared twice")


# make sure front end can correctly print values to error messages
let ex = expand(parse("\"a\"=1"))
    @test ex == Expr(:error, "invalid assignment location \"\"a\"\"")
end

# make sure that incomplete tags are detected correctly
# (i.e. error messages in src/julia-parser.scm must be matched correctly
# by the code in base/client.jl)
for (str, tag) in Dict("" => :none, "\"" => :string, "#=" => :comment, "'" => :char,
                       "`" => :cmd, "begin;" => :block, "quote;" => :block,
                       "let;" => :block, "for i=1;" => :block, "function f();" => :block,
                       "f() do x;" => :block, "module X;" => :block, "type X;" => :block,
                       "immutable X;" => :block, "(" => :other, "[" => :other,
                       "begin" => :other, "quote" => :other,
                       "let" => :other, "for" => :other, "function" => :other,
                       "f() do" => :other, "module" => :other, "type" => :other,
                       "immutable" => :other)
    @test Base.incomplete_tag(parse(str, raise=false)) == tag
end

# meta nodes for optional positional arguments
@test expand(:(@inline f(p::Int=2) = 3)).args[2].args[3].inlineable

# issue #16096
module M16096
macro iter()
    quote
        @inline function foo(sub)
            it = 1
        end
    end
end
end
let ex = expand(:(@M16096.iter))
    @test !(isa(ex,Expr) && ex.head === :error)
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
let nometh = expand(:(A15838.@f(1, 2)))
    @test (nometh::Expr).head === :error
    @test length(nometh.args) == 1
    e = nometh.args[1]::MethodError
    @test e.f === getfield(A15838, Symbol("@f"))
    @test e.args === (1,2)
end

# issue 10046
for op in ["+", "-", "\$", "|", ".+", ".-", "*", ".*"]
    @test_throws ParseError parse("$op in [+, -]")
end

# issue #17701
@test expand(:(i==3 && i+=1)) == Expr(:error, "invalid assignment location \"==(i,3)&&i\"")

# issue #18667
@test expand(:(true = 1)) == Expr(:error, "invalid assignment location \"true\"")
@test expand(:(false = 1)) == Expr(:error, "invalid assignment location \"false\"")

# PR #15592
let str = "[1] [2]"
    @test_throws ParseError parse(str)
end

# issue 15896 and PR 15913
@test_throws ErrorException eval(:(macro test15896(d; y=0) end))

# Issue #16578 (Lowering) mismatch between push_loc and pop_loc
module TestMeta_16578
using Base.Test
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
include_string("""
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
m1_exprs = get_expr_list(expand(:(@m1)))
m2_exprs = get_expr_list(expand(:(@m2)))
m3_exprs = get_expr_list(expand(:(@m3)))
m4_exprs = get_expr_list(expand(:(@m4)))

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
@test is_pop_loc(f2_exprs[end - 1])
@test Meta.isexpr(f2_exprs[end], :return)

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
@test :```
multiline
command
``` == :(@cmd "multiline\ncommand\n")

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
type Type
end
end
@test method_exists(Mod18756.Type, ())

# issue 18002
@test parse("typealias a (Int)") == Expr(:typealias, :a, :Int)
@test parse("typealias b (Int,)") == Expr(:typealias, :b, Expr(:tuple, :Int))
@test parse("typealias Foo{T} Bar{T}") == Expr(:typealias, Expr(:curly, :Foo, :T), Expr(:curly, :Bar, :T))

# don't insert push_loc for filename `none` at the top level
let ex = expand(parse("""
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
@test parse("@a(b=1, c=2)") == Expr(:macrocall, Symbol("@a"), :(b=1), :(c=2))

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
@test c8925 == 3 && isconst(:c8925)
@test d8925 == 4 && isconst(:d8925)

# issue #18754: parse ccall as a regular function
@test parse("ccall([1], 2)[3]") == Expr(:ref, Expr(:call, :ccall, Expr(:vect, 1), 2), 3)
@test parse("ccall(a).member") == Expr(:., Expr(:call, :ccall, :a), QuoteNode(:member))
