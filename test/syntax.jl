# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests for parser and syntax lowering

using Random

import Base.Meta.ParseError

function parseall(str)
    pos = firstindex(str)
    exs = []
    while pos <= lastindex(str)
        ex, pos = Meta.parse(str, pos)
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
        ex1 = Meta.parse(ex1); ex2 = Meta.parse(ex2)
        @test ex1.head === :call && (ex1.head === ex2.head)
        @test ex1.args[2] === 5 && ex2.args[2] === 5
        @test Core.eval(Main, undot(ex1.args[1])) === Core.eval(Main, undot(ex2.args[1]))
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
@test_throws ParseError Meta.parse(": x")
@test_throws ParseError Meta.parse("""begin
    :
    x""")
@test_throws ParseError Meta.parse("d[: 2]")

# issue #6770
@test_throws ParseError Meta.parse("x.3")

# issue #8763
@test_throws ParseError Meta.parse("sqrt(16)2")
@test_throws ParseError Meta.parse("x' y")
@test_throws ParseError Meta.parse("x 'y")
@test Meta.parse("x'y") == Expr(:call, :*, Expr(Symbol("'"), :x), :y)

# issue #18851
@test Meta.parse("-2[m]") == Expr(:call, :-, Expr(:ref, 2, :m))
@test Meta.parse("+2[m]") == Expr(:call, :+, Expr(:ref, 2, :m))
@test Meta.parse("!2[3]") == Expr(:call, :!, Expr(:ref, 2, 3))
@test Meta.parse("-2{m}") == Expr(:call, :-, Expr(:curly, 2, :m))
@test Meta.parse("+2{m}") == Expr(:call, :+, Expr(:curly, 2, :m))
@test Meta.parse("-2(m)") == Expr(:call, :*, -2, :m)

# issue #8301
@test_throws ParseError Meta.parse("&*s")

# issue #10677
@test_throws ParseError Meta.parse("/1")
@test_throws ParseError Meta.parse("/pi")
@test Meta.parse("- = 2") == Expr(:(=), :(-), 2)
@test Meta.parse("/ = 2") == Expr(:(=), :(/), 2)
@test_throws ParseError Meta.parse("< : 2")
@test_throws ParseError Meta.parse("+ : 2")
@test_throws ParseError Meta.parse("< :2")
@test Meta.parse("+ :2") == Expr(:call, :(+), QuoteNode(2))

# issue #10900
@test_throws ParseError Meta.parse("+=")
@test_throws ParseError Meta.parse(".")
@test_throws ParseError Meta.parse("...")

# issue #10901
@test Meta.parse("/([1], 1)[1]") == :(([1] / 1)[1])

# issue #10997
@test Meta.parse(":(x.\$f[i])") == Expr(:quote,
                                   Expr(:ref,
                                        Expr(Symbol("."), :x,
                                             QuoteNode(Expr(:$, :f))),
                                        :i))

# issue #10994
@test Meta.parse("1 + #= \0 =# 2") == :(1 + 2)

# issue #10910
@test Meta.parse(":(using A)") == Expr(:quote, Expr(:using, Expr(:., :A)))
@test Meta.parse(":(using A.b, B)") == Expr(:quote,
                                       Expr(:using,
                                            Expr(:., :A, :b),
                                            Expr(:., :B)))
@test Meta.parse(":(using A: b, c.d)") == Expr(:quote,
                                          Expr(:using,
                                               Expr(:(:),
                                                    Expr(:., :A),
                                                    Expr(:., :b),
                                                    Expr(:., :c, :d))))

@test Meta.parse(":(import A)") == Expr(:quote, Expr(:import, Expr(:., :A)))
@test Meta.parse(":(import A.b, B)") == Expr(:quote,
                                        Expr(:import,
                                             Expr(:., :A, :b),
                                             Expr(:., :B)))
@test Meta.parse(":(import A: b, c.d)") == Expr(:quote,
                                           Expr(:import,
                                                Expr(:(:),
                                                     Expr(:., :A),
                                                     Expr(:., :b),
                                                     Expr(:., :c, :d))))

# issue #11332
@test Meta.parse("export \$(Symbol(\"A\"))") == :(export $(Expr(:$, :(Symbol("A")))))
@test Meta.parse("export \$A") == :(export $(Expr(:$, :A)))
@test Meta.parse("using \$a.\$b") == Expr(:using, Expr(:., Expr(:$, :a), Expr(:$, :b)))
@test Meta.parse("using \$a.\$b, \$c") == Expr(:using,
                                               Expr(:., Expr(:$, :a), Expr(:$, :b)),
                                               Expr(:., Expr(:$, :c)))
@test Meta.parse("using \$a: \$b, \$c.\$d") ==
    Expr(:using,
         Expr(:(:), Expr(:., Expr(:$, :a)), Expr(:., Expr(:$, :b)),
              Expr(:., Expr(:$, :c), Expr(:$, :d))))

# fix pr #11338 and test for #11497
@test parseall("using \$\na") == Expr(:block, Expr(:using, Expr(:., :$)), :a)
@test parseall("using \$,\na") == Expr(:using, Expr(:., :$), Expr(:., :a))
@test parseall("using &\na") == Expr(:block, Expr(:using, Expr(:., :&)), :a)

@test parseall("a = &\nb") == Expr(:block, Expr(:(=), :a, :&), :b)
@test parseall("a = \$\nb") == Expr(:block, Expr(:(=), :a, :$), :b)
@test parseall(":(a = &\nb)") == Expr(:quote, Expr(:(=), :a, Expr(:&, :b)))
@test parseall(":(a = \$\nb)") == Expr(:quote, Expr(:(=), :a, Expr(:$, :b)))

# issue 12027 - short macro name parsing vs _str suffix
@test parseall("""
    macro f(args...) end; @f "macro argument"
""") == Expr(:toplevel,
             Expr(:macro, Expr(:call, :f, Expr(:..., :args)),
                  Expr(:block, LineNumberNode(1, :none), LineNumberNode(1, :none))),
             Expr(:macrocall, Symbol("@f"), LineNumberNode(1, :none), "macro argument"))

# blocks vs. tuples
@test Meta.parse("()") == Expr(:tuple)
@test Meta.parse("(;)") == Expr(:tuple, Expr(:parameters))
@test Meta.parse("(;;)") == Expr(:block)
@test Meta.parse("(;;;;)") == Expr(:block)
@test_throws ParseError Meta.parse("(,)")
@test_throws ParseError Meta.parse("(;,)")
@test_throws ParseError Meta.parse("(,;)")
# TODO: would be nice to make these errors, but needed to parse e.g. `(x;y,)->x`
#@test_throws ParseError Meta.parse("(1;2,)")
#@test_throws ParseError Meta.parse("(1;2,;)")
#@test_throws ParseError Meta.parse("(1;2,;3)")
@test Meta.parse("(x;)") == Expr(:block, :x)
@test Meta.parse("(;x)") == Expr(:tuple, Expr(:parameters, :x))
@test Meta.parse("(;x,)") == Expr(:tuple, Expr(:parameters, :x))
@test Meta.parse("(x,)") == Expr(:tuple, :x)
@test Meta.parse("(x,;)") == Expr(:tuple, Expr(:parameters), :x)
@test Meta.parse("(x;y)") == Expr(:block, :x, LineNumberNode(1,:none), :y)
@test Meta.parse("(x...;)") == Expr(:tuple, Expr(:parameters), Expr(:(...), :x))
@test Meta.parse("(;x...)") == Expr(:tuple, Expr(:parameters, Expr(:(...), :x)))
@test Meta.parse("(x...;y)") == Expr(:tuple, Expr(:parameters, :y), Expr(:(...), :x))
@test Meta.parse("(x;y...)") == Expr(:block, :x, LineNumberNode(1,:none), Expr(:(...), :y))
@test Meta.parse("(x=1;y=2)") == Expr(:block, Expr(:(=), :x, 1), LineNumberNode(1,:none), Expr(:(=), :y, 2))
@test Meta.parse("(x,;y)") == Expr(:tuple, Expr(:parameters, :y), :x)
@test Meta.parse("(x,;y=1)") == Expr(:tuple, Expr(:parameters, Expr(:kw, :y, 1)), :x)
@test Meta.parse("(x,a;y=1)") == Expr(:tuple, Expr(:parameters, Expr(:kw, :y, 1)), :x, :a)
@test Meta.parse("(x,a;y=1,z=2)") == Expr(:tuple, Expr(:parameters, Expr(:kw,:y,1), Expr(:kw,:z,2)), :x, :a)
@test Meta.parse("(a=1, b=2)") == Expr(:tuple, Expr(:(=), :a, 1), Expr(:(=), :b, 2))
@test_throws ParseError Meta.parse("(1 2)") # issue #15248

@test Meta.parse("f(x;)") == Expr(:call, :f, Expr(:parameters), :x)

@test Meta.parse("1 == 2|>3") == Expr(:call, :(==), 1, Expr(:call, :(|>), 2, 3))

# issue #24153
@test Meta.parse("a|>b|>c|>d") == Meta.parse("((a|>b)|>c)|>d")
@test Meta.parse("a<|b<|c<|d") == Meta.parse("a<|(b<|(c<|d))")

# issue #12501 and pr #12502
Meta.parse("""
      baremodule A
      "a" in b
      end
      """)
Meta.parse("""
      baremodule A
      "a"
      end
      """)

# issue #12626
@test Meta.parse("a .÷ 1") == Expr(:call, :.÷, :a, 1)
@test Meta.parse("a .÷= 1") == Expr(:.÷=, :a, 1)

# issue #12771
@test -(3)^2 == -9

# issue #13302
let p = Meta.parse("try
            a
        catch
            b, c = t
        end")
    @test isa(p,Expr) && p.head === :try
    @test p.args[2] === false
    @test p.args[3].args[end] == Meta.parse("b,c = t")
end

# pr #13078
@test Meta.parse("a in b in c") == Expr(:comparison, :a, :in, :b, :in, :c)
@test Meta.parse("a||b→c&&d") == Expr(:call, :→,
                                 Expr(Symbol("||"), :a, :b),
                                 Expr(Symbol("&&"), :c, :d))

# issue #11988 -- normalize \r and \r\n in literal strings to \n
@test "foo\nbar" == Meta.parse("\"\"\"\r\nfoo\r\nbar\"\"\"") ==
    Meta.parse("\"\"\"\nfoo\nbar\"\"\"") == Meta.parse("\"\"\"\rfoo\rbar\"\"\"") ==
    Meta.parse("\"foo\r\nbar\"") == Meta.parse("\"foo\rbar\"") == Meta.parse("\"foo\nbar\"")
@test '\r' == first("\r") == first("\r\n") # still allow explicit \r

# allow invalid UTF-8 in string literals
@test "\ud800"[1] == Char(0xd800)
@test "\udfff"[1] == Char(0xdfff)
@test length("\xc0\xb0") == 1
@test "\xc0\xb0"[1] == reinterpret(Char, 0xc0b00000)

# issue #14561 - generating 0-method generic function def
let fname = :f
    @test :(function $fname end) == Expr(:function, :f)
end

# issue #14977
@test Meta.parse("x = 1", 1) == (:(x = 1), 6)
@test Meta.parse("x = 1", 6) == (nothing, 6)
@test_throws BoundsError Meta.parse("x = 1", 0)
@test_throws BoundsError Meta.parse("x = 1", -1)
@test_throws BoundsError Meta.parse("x = 1", 7)

# issue #14683
@test_throws ParseError Meta.parse("'\\A\"'")
@test Meta.parse("'\"'") == Meta.parse("'\\\"'") == '"' == "\""[1] == '\42'

# issue #24558
@test_throws ParseError Meta.parse("'\\xff'")
@test_throws ParseError Meta.parse("'\\x80'")
@test_throws ParseError Meta.parse("'ab'")
@test '\u2200' == "\u2200"[1]

@test_throws ParseError Meta.parse("f(2x for x=1:10, y")

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
@test Meta.parse("x<:y") == Expr(:(<:), :x, :y)
@test Meta.parse("x>:y") == Expr(:(>:), :x, :y)
@test Meta.parse("x<:y<:z").head === :comparison
@test Meta.parse("x>:y<:z").head === :comparison

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

function test_parseerror(str, msg)
    try
        Meta.parse(str)
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
@test Meta.lower(Main, Base.parse_input_line("""
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
    @test Base.parse_input_line(b).args[end] == Expr(:let, Expr(:(=), :x, :x), Expr(:block, LineNumberNode(2, :none), :x))
    @test Base.parse_input_line(b).args[end] == Expr(:call, :f)
    @test Base.parse_input_line(b) === nothing
end

# issue #15763
test_parseerror("if\nfalse\nend", "missing condition in \"if\" at none:1")
test_parseerror("if false\nelseif\nend", "missing condition in \"elseif\" at none:2")

# issue #15828
@test Meta.lower(Main, Meta.parse("x...")) == Expr(:error, "\"...\" expression outside call")

# issue #15830
@test Meta.lower(Main, Meta.parse("foo(y = (global x)) = y")) == Expr(:error, "misplaced \"global\" declaration")

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
    @eval global function f15844(x::Int64)
        3x
    end
end

add_method_to_glob_fn!()
@test g15844 !== f15844
@test g15844(Int32(1)) == 2
@test f15844(Int32(1)) == 1
@test f15844(Int64(1)) == 3

# issue #15661
@test_throws ParseError Meta.parse("function catch() end")
@test_throws ParseError Meta.parse("function end() end")
@test_throws ParseError Meta.parse("function finally() end")

# PR #16170
@test Meta.lower(Main, Meta.parse("true(x) = x")) == Expr(:error, "invalid function name \"true\"")
@test Meta.lower(Main, Meta.parse("false(x) = x")) == Expr(:error, "invalid function name \"false\"")

# issue #16355
@test Meta.lower(Main, :(f(d:Int...) = nothing)) == Expr(:error, "\"d:Int\" is not a valid function argument name")

# issue #16517
@test (try error(); catch; 0; end) === 0
@test (try error(); catch; false; end) === false  # false and true are Bool literals, not variables
@test (try error(); catch; true; end) === true
f16517() = try error(); catch; 0; end
@test f16517() === 0

# issue #16671
@test Meta.parse("1.") === 1.0

isline(x) = isa(x, LineNumberNode)

# issue #16672
@test count(isline, Meta.parse("begin end").args) == 1
@test count(isline, Meta.parse("begin; end").args) == 1
@test count(isline, Meta.parse("begin; x+2; end").args) == 1
@test count(isline, Meta.parse("begin; x+2; y+1; end").args) == 2

# issue #16736
let
    local lineoffset0 = @__LINE__() + 1
    local lineoffset1 = @__LINE__()
    local lineoffset2 = @__LINE__() - 1
    @test lineoffset0 == lineoffset1 == lineoffset2
end

# issue #16686
@test Meta.parse("try x
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
@test_throws ParseError Meta.parse("Int [1,2,3]")
@test_throws ParseError Meta.parse("Int [x for x in 1:10]")
@test_throws ParseError Meta.parse("foo (x) = x")
@test_throws ParseError Meta.parse("foo {T<:Int}(x::T) = x")

@test_throws ParseError Meta.parse("Foo .bar")

@test_throws ParseError Meta.parse("import x .y")
@test_throws ParseError Meta.parse("using x .y")

@test_throws ParseError Meta.parse("--x")
@test_throws ParseError Meta.parse("stagedfunction foo(x); end")

@test Meta.parse("A=>B") == Expr(:call, :(=>), :A, :B)

@test Meta.parse("{1,2,3}") == Expr(:braces, 1, 2, 3)
@test Meta.parse("{1 2 3 4}") == Expr(:bracescat, Expr(:row, 1, 2, 3, 4))
@test Meta.parse("{1 2; 3 4}") == Expr(:bracescat, Expr(:row, 1, 2), Expr(:row, 3, 4))
@test Meta.parse("{x for x in 1:10}") == Expr(:braces, :(x for x in 1:10))
@test Meta.parse("{x=>y for (x,y) in zip([1,2,3],[4,5,6])}") == Expr(:braces, :(x=>y for (x,y) in zip([1,2,3],[4,5,6])))
@test Meta.parse("{:a=>1, :b=>2}") == Expr(:braces, Expr(:call, :(=>), QuoteNode(:a), 1),
                                      Expr(:call, :(=>), QuoteNode(:b), 2))

@test Meta.parse("[a,b;c]")  == Expr(:vect, Expr(:parameters, :c), :a, :b)
@test Meta.parse("[a,;c]")   == Expr(:vect, Expr(:parameters, :c), :a)
@test Meta.parse("a[b,c;d]") == Expr(:ref, :a, Expr(:parameters, :d), :b, :c)
@test Meta.parse("a[b,;d]")  == Expr(:ref, :a, Expr(:parameters, :d), :b)
@test_throws ParseError Meta.parse("[a,;,b]")
@test Meta.parse("{a,b;c}")  == Expr(:braces, Expr(:parameters, :c), :a, :b)
@test Meta.parse("{a,;c}")   == Expr(:braces, Expr(:parameters, :c), :a)
@test Meta.parse("a{b,c;d}") == Expr(:curly, :a, Expr(:parameters, :d), :b, :c)
@test Meta.parse("a{b,;d}")  == Expr(:curly, :a, Expr(:parameters, :d), :b)

# this now is parsed as getindex(Pair{Any,Any}, ...)
@test_throws MethodError eval(Meta.parse("(Any=>Any)[]"))
@test_throws MethodError eval(Meta.parse("(Any=>Any)[:a=>1,:b=>2]"))

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

# PR #17393
for op in (:.==, :.&, :.|, :.≤)
    @test Meta.parse("a $op b") == Expr(:call, op, :a, :b)
end
for op in (:.=, :.+=)
    @test Meta.parse("a $op b") == Expr(op, :a, :b)
end

# issue #17489
let m_error, error_out, filename = Base.source_path()
    m_error = try @eval method_c6(a::(:A)) = 1; catch e; e; end
    error_out = sprint(showerror, m_error)
    @test startswith(error_out, "ArgumentError: invalid type for argument a in method definition for method_c6 at $filename:")

    m_error = try @eval method_c6(::(:A)) = 2; catch e; e; end
    error_out = sprint(showerror, m_error)
    @test startswith(error_out, "ArgumentError: invalid type for argument number 1 in method definition for method_c6 at $filename:")

    # issue #20614
    m_error = try @eval foo(types::NTuple{N}, values::Vararg{Any,N}, c) where {N} = nothing; catch e; e; end
    error_out = sprint(showerror, m_error)
    @test startswith(error_out, "ArgumentError: Vararg on non-final argument")
end

# issue #7272
@test Meta.lower(Main, Meta.parse("let
              global x = 2
              local x = 1
              end")) == Expr(:error, "variable \"x\" declared both local and global")
#=
@test Meta.lower(Main, Meta.parse("let
              local x = 2
              local x = 1
              end")) == Expr(:error, "local \"x\" declared twice")

@test Meta.lower(Main, Meta.parse("let x
                  local x = 1
              end")) == Expr(:error, "local \"x\" declared twice")

@test Meta.lower(Main, Meta.parse("let x = 2
                  local x = 1
              end")) == Expr(:error, "local \"x\" declared twice")
=#
# issue #23673
@test :(let $([:(x=1),:(y=2)]...); x+y end) == :(let x = 1, y = 2; x+y end)

# make sure front end can correctly print values to error messages
let ex = Meta.lower(Main, Meta.parse("\"a\"=1"))
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
    @test Base.incomplete_tag(Meta.parse(str, raise=false)) == tag
end

# meta nodes for optional positional arguments
@test Meta.lower(Main, :(@inline f(p::Int=2) = 3)).args[1].code[end-1].args[3].inlineable

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
let ex = Meta.lower(M16096, :(@iter))
    @test isa(ex, Expr)
end
let ex = Meta.lower(Main, :($M16096.@iter))
    @test isa(ex, Expr)
end
let thismodule = @__MODULE__,
    ex = Meta.lower(thismodule, :(@M16096.iter))
    @test isa(ex, Expr)
    @test !isdefined(M16096, :foo16096)
    local_foo16096 = Core.eval(@__MODULE__, ex)
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
    @test_throws ParseError Meta.parse("$op in [+, -]")
end

# issue #17701
@test Meta.lower(Main, :(i==3 && i+=1)) == Expr(:error, "invalid assignment location \"(i == 3) && i\"")

# issue #18667
@test Meta.lower(Main, :(true = 1)) == Expr(:error, "invalid assignment location \"true\"")
@test Meta.lower(Main, :(false = 1)) == Expr(:error, "invalid assignment location \"false\"")

# PR #15592
let str = "[1] [2]"
    @test_throws ParseError Meta.parse(str)
end

# issue 15896 and PR 15913
@test_throws ErrorException eval(:(macro test15896(d; y=0) end))

# Issue #16578 (Lowering) mismatch between push_loc and pop_loc
module TestMeta_16578
using Test
function get_expr_list(ex::Core.CodeInfo)
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
    return push_count
end

function is_return_ssavalue(ex::Expr)
    ex.head === :return && isa(ex.args[1], Core.SSAValue)
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
include_string(@__MODULE__, """
macro m3()
    quote
        @m1
    end
end
""", "another_file.jl")
m1_exprs = get_expr_list(Meta.lower(@__MODULE__, quote @m1 end))

# Check the expanded expression has expected number of matching push/pop
# and the return is handled correctly
# NOTE: we currently only emit push/pop locations for macros from other files
@test_broken count_meta_loc(m1_exprs) == 1
@test is_return_ssavalue(m1_exprs[end])

let low3 = Meta.lower(@__MODULE__, quote @m3 end)
    m3_exprs = get_expr_list(low3)
    ci = low3.args[1]::Core.CodeInfo
    @test ci.codelocs == [3, 1]
    @test is_return_ssavalue(m3_exprs[end])
end

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

f1_ci = code_typed(f1, (Int,), debuginfo=:source)[1][1]
f2_ci = code_typed(f2, (Int,), debuginfo=:source)[1][1]

f1_exprs = get_expr_list(f1_ci)
f2_exprs = get_expr_list(f2_ci)

if Base.JLOptions().can_inline != 0
    @test length(f1_ci.linetable) == 3
    @test length(f2_ci.linetable) >= 3
else
    @test length(f1_ci.linetable) == 2
    @test length(f2_ci.linetable) >= 3
end

# Check that string and command literals are parsed to the appropriate macros
@test :(x"s") == :(@x_str "s")
@test :(x"s"flag) == :(@x_str "s" "flag")
@test :(x"s\"`\x\$\\") == :(@x_str "s\"`\\x\\\$\\")
@test :(x`s`) == :(@x_cmd "s")
@test :(x`s`flag) == :(@x_cmd "s" "flag")
@test :(x`s\`"\x\$\\`) == :(@x_cmd "s`\"\\x\\\$\\")

# Check multiline command literals
@test Expr(:macrocall, GlobalRef(Core, Symbol("@cmd")), LineNumberNode(@__LINE__, Symbol(@__FILE__)), "multiline\ncommand\n") == :```
multiline
command
```

macro julia_cmd(s)
    Meta.quot(Meta.parse(s))
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
@test hasmethod(Mod18756.Type, ())

# issue 18002
@test Meta.parse("Foo{T} = Bar{T}") == Expr(:(=), Expr(:curly, :Foo, :T), Expr(:curly, :Bar, :T))

# don't insert push_loc for filename `none` at the top level
let ex = Meta.lower(Main, Meta.parse("""
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
@test Meta.parse("@a(b=1, c=2)") == Expr(:macrocall, Symbol("@a"), LineNumberNode(1, :none), :(b=1), :(c=2))

# issue #19685
let f = function (x; kw...)
            return (x, kw)
        end,
    g = function (x; a = 2)
            return (x, a)
        end
    @test f(1) == (1, pairs(NamedTuple()))
    @test g(1) == (1, 2)
end

# normalization of Unicode symbols (#19464)
let ε=1, μ=2, x=3, î=4
    # issue #5434 (mu vs micro):
    @test Meta.parse("\u00b5") === Meta.parse("\u03bc")
    @test µ == μ == 2
    # NFC normalization of identifiers:
    @test Meta.parse("\u0069\u0302") === Meta.parse("\u00ee")
    @test î == 4
    # latin vs greek ε (#14751)
    @test Meta.parse("\u025B") === Meta.parse("\u03B5")
    @test ɛ == ε == 1
end

# issue #8925
let
    global const (c8925, d8925) = (3, 4)
end
@test c8925 == 3 && isconst(@__MODULE__, :c8925)
@test d8925 == 4 && isconst(@__MODULE__, :d8925)

# issue #18754: parse ccall as a regular function
@test Meta.parse("ccall([1], 2)[3]") == Expr(:ref, Expr(:call, :ccall, Expr(:vect, 1), 2), 3)
@test Meta.parse("ccall(a).member") == Expr(:., Expr(:call, :ccall, :a), QuoteNode(:member))

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
@test Meta.parse("[a .!b]") == Expr(:hcat, :a, Expr(:call, :(.!), :b))

@test Meta.lower(Main, :(a{1} = b)) == Expr(:error, "invalid type parameter name \"1\"")
@test Meta.lower(Main, :(a{2<:Any} = b)) == Expr(:error, "invalid type parameter name \"2\"")

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

@test_throws ErrorException Core.eval(@__MODULE__, :(@m20729))
@test Meta.lower(@__MODULE__, :(@m20729)) == Expr(:error, "undefined reference in AST")

macro err20000()
    return Expr(:error, "oops!")
end

@test Meta.lower(@__MODULE__, :(@err20000)) == Expr(:error, "oops!")

# issue #20000
@test Meta.parse("@m(a; b=c)") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1, :none),
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
@test Meta.lower(Main, :(a.[1])) == Expr(:error, "invalid syntax \"a.[1]\"")
@test Meta.lower(Main, :(a.{1})) == Expr(:error, "invalid syntax \"a.{1}\"")

# Issue #21225
let abstr = Meta.parse("abstract type X end")
    @test Meta.parse("abstract type X; end") == abstr
    @test Meta.parse(string("abstract type X", ";"^5, " end")) == abstr
    @test Meta.parse("abstract type X\nend") == abstr
    @test Meta.parse(string("abstract type X", "\n"^5, "end")) == abstr
end
let prim = Meta.parse("primitive type X 8 end")
    @test Meta.parse("primitive type X 8; end") == prim
    @test Meta.parse(string("primitive type X 8", ";"^5, " end")) == prim
    @test Meta.parse("primitive type X 8\nend") == prim
    @test Meta.parse(string("primitive type X 8", "\n"^5, "end")) == prim
end

# issue #21155
@test filter(!isline,
             Meta.parse("module B
                        using ..x,
                              ..y
                    end").args[3].args)[1] ==
      Expr(:using,
           Expr(:., :., :., :x),
           Expr(:., :., :., :y))

@test filter(!isline,
             Meta.parse("module A
                        using .B,
                              .C
                    end").args[3].args)[1] ==
      Expr(:using,
           Expr(:., :., :B),
           Expr(:., :., :C))

# issue #21440
@test Meta.parse("+(x::T,y::T) where {T} = 0") == Meta.parse("(+)(x::T,y::T) where {T} = 0")
@test Meta.parse("a::b::c") == Expr(:(::), Expr(:(::), :a, :b), :c)

# issue #21545
f21545(::Type{<: AbstractArray{T,N} where N}) where T = T
@test f21545(Array{Int8}) === Int8
@test Meta.parse("<:{T} where T") == Expr(:where, Expr(:curly, :(<:), :T), :T)
@test Meta.parse("<:(T) where T") == Expr(:where, Expr(:(<:), :T), :T)
@test Meta.parse("<:{T}(T) where T") == Expr(:where, Expr(:call, Expr(:curly, :(<:), :T), :T), :T)

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
@test_throws ParseError Meta.parse("\"a\"x")
@test_throws ParseError Meta.parse("\"a\"begin end")
@test_throws ParseError Meta.parse("\"a\"begin end\"b\"")

# issue #16427
@test_throws ParseError Meta.parse("for i=1:1 end(3)")
@test_throws ParseError Meta.parse("begin end(3)")
@test_throws ParseError Meta.parse("while false end(3)")

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
@test Meta.lower(Main, :(f(2, a=1, w=3, c=3, w=4, b=2))) ==
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
@test Meta.parse("3 +̂ 4") == Expr(:call, :+̂, 3, 4)
@test Meta.parse("3 +̂′ 4") == Expr(:call, :+̂′, 3, 4)
@test Meta.parse("3 +⁽¹⁾ 4") == Expr(:call, :+⁽¹⁾, 3, 4)
@test Meta.parse("3 +₍₀₎ 4") == Expr(:call, :+₍₀₎, 3, 4)
for bad in ('=', '$', ':', "||", "&&", "->", "<:")
    @test_throws ParseError Meta.parse("3 $(bad)⁽¹⁾ 4")
end
@test Base.operator_precedence(:+̂) == Base.operator_precedence(:+)

@test Meta.parse("(x)ᵀ") == Expr(:call, :*, :x, :ᵀ)

# issue #19351
# adding return type decl should not affect parse of function body
@test :(t(abc) = 3).args[2] == :(t(abc)::Int = 3).args[2]

# issue #7314
@test Meta.parse("local x, y = 1, 2") == Expr(:local, Expr(:(=),
                                                      Expr(:tuple, :x, :y),
                                                      Expr(:tuple, 1, 2)))

@test_throws ParseError Meta.parse("[2for i=1:10]")
@test_throws ParseError Meta.parse("[1 for i in 1:2for j in 2]")
@test_throws ParseError Meta.parse("(1 for i in 1:2for j in 2)")
# issue #20441
@test_throws ParseError Meta.parse("[x.2]")
@test_throws ParseError Meta.parse("x.2")
@test Meta.parse("[x;.2]") == Expr(:vcat, :x, 0.2)

# issue #22840
@test Meta.parse("[:a :b]") == Expr(:hcat, QuoteNode(:a), QuoteNode(:b))

# issue #22868
@test_throws ParseError Meta.parse("x@time 2")
@test_throws ParseError Meta.parse("@ time")

# issue #7479
@test Meta.lower(Main, Meta.parse("(true &&& false)")) == Expr(:error, "invalid syntax &false")

# issue #34748
@test Meta.lower(Main, :(&(1, 2))) == Expr(:error, "invalid syntax &(1, 2)")

# if an indexing expression becomes a cat expression, `end` is not special
@test_throws ParseError Meta.parse("a[end end]")
@test_throws ParseError Meta.parse("a[end;end]")
#@test_throws ParseError Meta.parse("a[end;]")  # this is difficult to fix
let a = rand(8), i = 3
    @test a[[1:i-1; i+1:end]] == a[[1,2,4,5,6,7,8]]
end

# issue #18935
@test [begin
          @inbounds for i = 1:10 end
       end for i = 1:5] == fill(nothing, 5)

# issue #18912
@test_throws ParseError Meta.parse("(::)")
@test Meta.parse(":(::)") == QuoteNode(Symbol("::"))
@test_throws ParseError Meta.parse("f(::) = ::")
@test Meta.parse("(::A)") == Expr(Symbol("::"), :A)
@test_throws ParseError Meta.parse("(::, 1)")
@test_throws ParseError Meta.parse("(1, ::)")

# issue #18650
let ex = Meta.parse("maximum(@elapsed sleep(1) for k = 1:10)")
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
@test Meta.lower(Main, quote
        function f()
            local Int
            x::Int -> 2
        end
    end) == Expr(:error, "local variable Int cannot be used in closure declaration")

# some issues with backquote
# preserve QuoteNode and LineNumberNode
@test eval(Expr(:quote, QuoteNode(Expr(:tuple, 1, Expr(:$, :(1+2)))))) == QuoteNode(Expr(:tuple, 1, 3))
@test eval(Expr(:quote, Expr(:line, Expr(:$, :(1+2))))) === LineNumberNode(3, nothing)
# splicing at the top level should be an error
xs23917 = [1,2,3]
@test_throws ErrorException eval(:(:($(xs23917...))))
let ex2 = eval(:(:(:($$(xs23917...)))))
    @test ex2 isa Expr
    @test_throws ErrorException eval(ex2)
    @test eval(:($(xs23917...),)) == (1,2,3)  # adding a comma gives a tuple
end
# multi-unquote of splice in nested quote
let xs = [:(1+2), :(3+4), :(5+6)]
    ex = quote quote $$(xs...) end end
    @test ex.args[2].args[1].args[2].args[2] == :(3 + 4)
    ex2 = eval(ex)
    @test ex2.args[2:end] == [3,7,11]
end

let x = [3,2,1]
    @test :( $(x...,) ) == (3, 2, 1)
    @test :( $(x...), ) == Expr(:tuple, 3, 2, 1)
end

# issue #23519
@test Meta.parse("@foo[1]") == Meta.parse("@foo([1])")
@test Meta.parse("@foo[1 2; 3 4]") == Meta.parse("@foo([1 2; 3 4])")
@test Meta.parse("@foo[1] + [2]") == Meta.parse("@foo([1]) + [2]")
@test Meta.parse("@foo [1] + [2]") == Meta.parse("@foo([1] + [2])")
@test Meta.parse("@Mdl.foo[1] + [2]") == Meta.parse("@Mdl.foo([1]) + [2]")
@test Meta.parse("@Mdl.foo [1] + [2]") == Meta.parse("@Mdl.foo([1] + [2])")

# issue #24289
macro m24289()
    :(global $(esc(:x24289)) = 1)
end
@test (@macroexpand @m24289) == :(global x24289 = 1)

# parsing numbers with _ and .
@test Meta.parse("1_2.3_4") == 12.34
@test_throws ParseError Meta.parse("1._")
@test_throws ParseError Meta.parse("1._5")
@test_throws ParseError Meta.parse("1e.3")
@test_throws ParseError Meta.parse("1e3.")
@test Meta.parse("2e_1") == Expr(:call, :*, 2, :e_1)
# issue #17705
@test Meta.parse("2e3_") == Expr(:call, :*, 2e3, :_)
@test Meta.parse("2e-3_") == Expr(:call, :*, 2e-3, :_)
@test Meta.parse("2e3_\"x\"") == Expr(:call, :*, 2e3, Expr(:macrocall, Symbol("@__str"), LineNumberNode(1, :none), "x"))

# misplaced top-level expressions
@test_throws ErrorException("syntax: \"\$\" expression outside quote") Core.eval(@__MODULE__, Meta.parse("x->\$x"))
@test Meta.lower(@__MODULE__, Expr(:$, :x)) == Expr(:error, "\"\$\" expression outside quote")
@test Meta.lower(@__MODULE__, :(x->import Foo)) == Expr(:error, "\"import\" expression not at top level")
@test Meta.lower(@__MODULE__, :(x->module Foo end)) == Expr(:error, "\"module\" expression not at top level")
@test Meta.lower(@__MODULE__, :(x->struct Foo end)) == Expr(:error, "\"struct\" expression not at top level")
@test Meta.lower(@__MODULE__, :(x->abstract type Foo end)) == Expr(:error, "\"abstract type\" expression not at top level")

# caused by #24538. forms that lower to `call` should wrap with `call` before
# recursively calling expand-forms.
@test [(0,0)... 1] == [0 0 1]
@test Float32[(0,0)... 1] == Float32[0 0 1]

@testset "raw_str macro" begin
    @test raw"$" == "\$"
    @test raw"\n" == "\\n"
    @test raw"\t" == "\\t"

    s1 = raw"""
         lorem ipsum\n
         $x = 1$
         """

    s2 = """
         lorem ipsum\\n
         \$x = 1\$
         """

    @test s1 == s2

    # issue #22926
    @test raw"\\" == "\\"
    @test raw"\\\\" == "\\\\"
    @test raw"\"" == "\""
    @test raw"\\\"" == "\\\""
    @test raw"\\x\\" == "\\\\x\\"
    @test raw"x \\\" y" == "x \\\" y"
    @test raw"x \\\ y" == "x \\\\\\ y"
end

@test_throws ParseError("expected \"}\" or separator in arguments to \"{ }\"; got \"V)\"") Meta.parse("f(x::V) where {V) = x")
@test_throws ParseError("expected \"]\" or separator in arguments to \"[ ]\"; got \"1)\"") Meta.parse("[1)")

# issue #9972
@test Meta.lower(@__MODULE__, :(f(;3))) == Expr(:error, "invalid keyword argument syntax \"3\"")

# issue #25055, make sure quote makes new Exprs
function f25055()
    x = quote end
    return x
end
@test f25055() !== f25055()

# issue #25391
@test Meta.parse("0:-1, \"\"=>\"\"") == Meta.parse("(0:-1, \"\"=>\"\")") ==
    Expr(:tuple, Expr(:call, :(:), 0, -1), Expr(:call, :(=>), "", ""))
@test Meta.parse("a => b = c") == Expr(:(=), Expr(:call, :(=>), :a, :b), Expr(:block, LineNumberNode(1, :none), :c))
@test Meta.parse("a = b => c") == Expr(:(=), :a, Expr(:call, :(=>), :b, :c))

# issue #16239, hygiene of rest keyword name
macro foo16239(x)
    :($(esc(:blah))(args...; kwargs...) = $(esc(x)))
end
function bar16239()
    kwargs = 0
    f = @foo16239 kwargs
    f()
end
@test bar16239() == 0

# lowering of <: and >:
let args = (Int, Any)
    @test <:(args...)
    @test >:(reverse(args)...)
end

# issue #25947
let getindex = 0, setindex! = 1, colon = 2, vcat = 3, hcat = 4, hvcat = 5
    a = [10,9,8]
    @test a[2] == 9
    @test 1:2 isa AbstractRange
    a[1] = 1
    @test a[1] == 1
    @test length([1; 2]) == 2
    @test size([0 0]) == (1, 2)
    @test size([1 2; 3 4]) == (2, 2)
end

# issue #25020
@test_throws ParseError Meta.parse("using Colors()")

let ex = Meta.parse("md\"x\"
                     f(x) = x", 1)[1]  # custom string literal is not a docstring
    @test Meta.isexpr(ex, :macrocall)
    @test ex.args[1] === Symbol("@md_str")
    @test length(ex.args) == 3
end

let ex = Meta.parse("@doc raw\"
                     \"
                     f(x) = x")
    @test Meta.isexpr(ex, :macrocall)
    @test ex.args[1] === Symbol("@doc")
    @test length(ex.args) == 4
    @test Meta.isexpr(ex.args[4], :(=))
end

let ex = Meta.parse("@doc raw\"
                     \"

                     f(x) = x", 1)[1]
    @test Meta.isexpr(ex, :macrocall)
    @test ex.args[1] === Symbol("@doc")
    @test length(ex.args) == 3
end

@test Meta.parse("\"x\"
                  # extra line, not a doc string
                  f(x) = x", 1)[1] === "x"
@test Meta.parse("\"x\"

                  f(x) = x", 1)[1] === "x"

# issue #26137
# cases where parens enclose argument lists
@test Meta.parse("-()^2")      == Expr(:call, :^, Expr(:call, :-), 2)
@test Meta.parse("-(x,)^2")    == Expr(:call, :^, Expr(:call, :-, :x), 2)
@test Meta.parse("-(x,;)^2")   == Expr(:call, :^, Expr(:call, :-, Expr(:parameters), :x), 2)
@test Meta.parse("-(;x)^2")    == Expr(:call, :^, Expr(:call, :-, Expr(:parameters, :x)), 2)
@test Meta.parse("-(x,y)^2")   == Expr(:call, :^, Expr(:call, :-, :x, :y), 2)
@test Meta.parse("-(x...)^2")  == Expr(:call, :^, Expr(:call, :-, Expr(:(...), :x)), 2)
@test Meta.parse("-(x...;)^2") == Expr(:call, :^, Expr(:call, :-, Expr(:parameters), Expr(:(...), :x)), 2)
@test Meta.parse("-(x...;)")   == Expr(:call, :-, Expr(:parameters), Expr(:(...), :x))

# cases where parens are just grouping
@test Meta.parse("-(x)^2")     == Expr(:call, :-, Expr(:call, :^, :x, 2))
@test Meta.parse("-(a=1)^2")   == Expr(:call, :-, Expr(:call, :^, Expr(:(=), :a, 1), 2))
@test Meta.parse("-(x;y)^2")   == Expr(:call, :-, Expr(:call, :^, Expr(:block, :x, LineNumberNode(1,:none), :y), 2))
@test Meta.parse("-(;)^2")     == Expr(:call, :^, Expr(:call, :-, Expr(:parameters)), 2)
@test Meta.parse("-(;;;;)^2")  == Expr(:call, :-, Expr(:call, :^, Expr(:block), 2))
@test Meta.parse("-(x;;;)^2")  == Expr(:call, :-, Expr(:call, :^, Expr(:block, :x), 2))
@test Meta.parse("+((1,2))")   == Expr(:call, :+, Expr(:tuple, 1, 2))

@test_throws ParseError("space before \"(\" not allowed in \"+ (\" at none:1") Meta.parse("1 -+ (a=1, b=2)")
# issue #29781
@test_throws ParseError("space before \"(\" not allowed in \"sin. (\" at none:1") Meta.parse("sin. (1)")
# Parser errors for disallowed space contain line numbers
@test_throws ParseError("space before \"[\" not allowed in \"f() [\" at none:2") Meta.parse("\nf() [i]")
@test_throws ParseError("space before \"(\" not allowed in \"f() (\" at none:2") Meta.parse("\nf() (i)")
@test_throws ParseError("space before \".\" not allowed in \"f() .\" at none:2") Meta.parse("\nf() .i")
@test_throws ParseError("space before \"{\" not allowed in \"f() {\" at none:2") Meta.parse("\nf() {i}")
@test_throws ParseError("space before \"m\" not allowed in \"@ m\" at none:2") Meta.parse("\n@ m")
@test_throws ParseError("space before \".\" not allowed in \"a .\" at none:2") Meta.parse("\nusing a .b")
@test_throws ParseError("space before \".\" not allowed in \"a .\" at none:2") Meta.parse("\nusing a .b")
@test_throws ParseError("space before \"(\" not allowed in \"+ (\" at none:2") Meta.parse("\n+ (x, y)")

@test Meta.parse("1 -+(a=1, b=2)") == Expr(:call, :-, 1,
                                           Expr(:call, :+, Expr(:kw, :a, 1), Expr(:kw, :b, 2)))

@test Meta.parse("-(2)(x)") == Expr(:call, :*, Expr(:call, :-,  2), :x)
@test Meta.parse("-(x)y")   == Expr(:call, :*, Expr(:call, :-, :x), :y)
@test Meta.parse("-(x,)y")  == Expr(:call, :*, Expr(:call, :-, :x), :y)
@test Meta.parse("-(f)(x)") == Expr(:call, :-, Expr(:call, :f, :x))
@test Meta.parse("-(2)(x)^2") == Expr(:call, :*, Expr(:call, :-, 2), Expr(:call, :^, :x, 2))
@test Meta.parse("Y <- (x->true)(X)") ==
    Expr(:call, :<, :Y,
         Expr(:call, :-, Expr(:call, Expr(:->, :x, Expr(:block, LineNumberNode(1,:none), true)),
                              :X)))

# issue #27641
@test Meta.parse("√3x")   == Expr(:call, :*, Expr(:call, :√, 3), :x)
@test Meta.parse("2^√3x") == Expr(:call, :^, 2, Expr(:call, :*, Expr(:call, :√, 3), :x))
@test Meta.parse("√2^3")  == Expr(:call, :√, Expr(:call, :^, 2, 3))
@test Meta.parse("-√2")   == Expr(:call, :-, Expr(:call, :√, 2))
@test Meta.parse("√3x^2") == Expr(:call, :*, Expr(:call, :√, 3), Expr(:call, :^, :x, 2))
@test Meta.parse("-3x^2") == Expr(:call, :*, -3, Expr(:call, :^, :x, 2))
@test_throws ParseError Meta.parse("2!3")
@test_throws ParseError Meta.parse("2√3")

# issue #27914
@test Meta.parse("2f(x)")        == Expr(:call, :*, 2, Expr(:call, :f, :x))
@test Meta.parse("f(x)g(x)")     == Expr(:call, :*, Expr(:call, :f, :x), Expr(:call, :g, :x))
@test Meta.parse("2f(x)g(x)")    == Expr(:call, :*, 2, Expr(:call, :f, :x), Expr(:call, :g, :x))
@test Meta.parse("f(x)g(x)h(x)") == Expr(:call, :*, Expr(:call, :f, :x), Expr(:call, :g, :x), Expr(:call, :h, :x))
@test Meta.parse("2(x)")         == Expr(:call, :*, 2, :x)
@test Meta.parse("2(x)y")        == Expr(:call, :*, 2, :x, :y)

@test_throws ParseError Meta.parse("a.: b")
@test Meta.parse("a.:end") == Expr(:., :a, QuoteNode(:end))
@test Meta.parse("a.:catch") == Expr(:., :a, QuoteNode(:catch))
@test Meta.parse("a.end") == Expr(:., :a, QuoteNode(:end))
@test Meta.parse("a.catch") == Expr(:., :a, QuoteNode(:catch))
@test Meta.parse("a.function") == Expr(:., :a, QuoteNode(:function))

# issue #25994
@test Meta.parse("[a\nfor a in b]") == Expr(:comprehension, Expr(:generator, :a, Expr(:(=), :a, :b)))

# issue #27529
let len = 10
    @test [ i for i in 0:len -1 ] == [0:9;]
end

# Module name cannot be a reserved word.
@test_throws ParseError Meta.parse("module module end")

@test Meta.lower(@__MODULE__, :(global true)) == Expr(:error, "invalid syntax in \"global\" declaration")
@test Meta.lower(@__MODULE__, :(let ccall end)) == Expr(:error, "invalid identifier name \"ccall\"")
@test Meta.lower(@__MODULE__, :(cglobal = 0)) == Expr(:error, "invalid assignment location \"cglobal\"")

# issue #26507
@test Meta.parse("@try x") == Expr(:macrocall, Symbol("@try"), LineNumberNode(1,:none), :x)
@test Meta.parse("@catch x") == Expr(:macrocall, Symbol("@catch"), LineNumberNode(1,:none), :x)
@test Meta.parse("@\$x") == Expr(:macrocall, Symbol("@\$"), LineNumberNode(1,:none), :x)

# issue #26717
@test Meta.lower(@__MODULE__, :( :(:) = 2 )) == Expr(:error, "invalid assignment location \":(:)\"")

# issue #27690
# previously, this was allowed since it thought `end` was being used for indexing.
# however the quote should disable that context.
@test_throws ParseError Meta.parse("Any[:(end)]")

# issue #17781
let ex = Meta.lower(@__MODULE__, Meta.parse("
    A = function (s, o...)
        f(a, b) do
        end
    end,
    B = function (s, o...)
        f(a, b) do
        end
    end"))
    @test isa(ex, Expr) && ex.head === :error
    @test ex.args[1] == """
invalid assignment location "function (s, o...)
    # none, line 2
    # none, line 3
    f(a, b) do
        # none, line 4
    end
end\""""
end

# issue #15229
@test Meta.lower(@__MODULE__, :(function f(x); local x; 0; end)) ==
    Expr(:error, "local variable name \"x\" conflicts with an argument")
@test Meta.lower(@__MODULE__, :(function f(x); begin; local x; 0; end; end)) ==
    Expr(:error, "local variable name \"x\" conflicts with an argument")

# issue #27964
a27964(x) = Any[x for x in []]
@test a27964(0) == Any[]
function b27964(x)
    local y
    let
        local x
        x = 2
        y = x
    end
    return (x, y)
end
@test b27964(8) == (8, 2)
function c27964(x)
    local y
    let x = 2
        y = x
    end
    return (x, y)
end
@test c27964(8) == (8, 2)

# issue #26739
let exc = try Core.eval(@__MODULE__, :(sin.[1])) catch exc ; exc end
    @test exc isa ErrorException
    @test startswith(exc.msg, "syntax: invalid syntax \"sin.[1]\"")
end

# issue #26873
f26873 = 0
try
    include_string(@__MODULE__, """f26873."a" """)
    @test false
catch e
    @test e isa LoadError
    @test e.error isa MethodError
end

@test Meta.lower(@__MODULE__, :(if true; break; end for i = 1:1)) == Expr(:error, "break or continue outside loop")
@test Meta.lower(@__MODULE__, :([if true; break; end for i = 1:1])) == Expr(:error, "break or continue outside loop")
@test Meta.lower(@__MODULE__, :(Int[if true; break; end for i = 1:1])) == Expr(:error, "break or continue outside loop")
@test Meta.lower(@__MODULE__, :([if true; continue; end for i = 1:1])) == Expr(:error, "break or continue outside loop")
@test Meta.lower(@__MODULE__, :(Int[if true; continue; end for i = 1:1])) == Expr(:error, "break or continue outside loop")

@test Meta.lower(@__MODULE__, :(return 0 for i=1:2)) == Expr(:error, "\"return\" not allowed inside comprehension or generator")
@test Meta.lower(@__MODULE__, :([ return 0 for i=1:2 ])) == Expr(:error, "\"return\" not allowed inside comprehension or generator")
@test Meta.lower(@__MODULE__, :(Int[ return 0 for i=1:2 ])) == Expr(:error, "\"return\" not allowed inside comprehension or generator")
@test [ ()->return 42 for i = 1:1 ][1]() == 42
@test Function[ identity() do x; return 2x; end for i = 1:1 ][1](21) == 42

# issue #27155
macro test27155()
    quote
        MyTest27155{Arg} = Tuple{Arg}
        MyTest27155
    end
end
@test @test27155() == (Tuple{T} where T)

# issue #27521
macro test27521(f, x)
    :(($(esc(f)), $x))
end
let ex = Meta.parse("@test27521(2) do y; y; end")
    fex = Expr(:(->), Expr(:tuple, :y), Expr(:block, LineNumberNode(1,:none), :y))
    @test ex == Expr(:do, Expr(:macrocall, Symbol("@test27521"), LineNumberNode(1,:none), 2),
                     fex)
    @test macroexpand(@__MODULE__, ex) == Expr(:tuple, fex, 2)
end

# issue #27129
f27129(x = 1) = (@Base._inline_meta; x)
for meth in methods(f27129)
    @test ccall(:jl_uncompress_ir, Any, (Any, Ptr{Cvoid}, Any), meth, C_NULL, meth.source).inlineable
end

# issue #27710
struct Foo27710{T} end
function test27710()
    types(::Foo27710{T}) where T = T
    T = types(Foo27710{Int64}())
end
@test test27710() === Int64

# issue #29064
struct X29064
    X29064::Int
end
@test X29064(1) isa X29064

# issue #27268
function f27268()
    g(col::AbstractArray{<:Real}) = col
end
function f27268_2()
    g(col::AbstractArray{T} where T<:Real) = col
end
@test f27268()([1]) == [1]
@test f27268_2()([1]) == [1]
@test_throws MethodError f27268()([""])
@test_throws MethodError f27268_2()([""])

@test_throws ErrorException("syntax: local variable x cannot be used in closure declaration") @eval begin
    function g27268()
        x = 1
        h(::Val{x}) = 1
    end
end

types27268 = (Int64,Int8)
function h27268()
    function g(::Union{map(t->Array{t,N},types27268)...} where N)
    end
end
@test first(methods(h27268())).sig == Tuple{typeof(h27268()), Union{Array{Int64,N}, Array{Int8,N}} where N}

let val(::Type{Val{X}}) where {X} = X, f
    function f()
        function g(::Val{x->2x})
        end
    end
    @test val(first(methods(f())).sig.parameters[2])(21) == 42
end

# issue #27807
module A27807
macro m()
    quote
        function foo(x::T, y::S) where T<:Number where S<:Number
            return one(T), zero(S)
        end
    end
end
end
@test A27807.@m()(1,1.0) === (1, 0.0)

# issue #27896 / #29429
@test Meta.lower(@__MODULE__, quote
    function foo(a::A, b::B) where {A,B}
        B = eltype(A)
        return convert(B, b)
    end
end) == Expr(:error, "local variable name \"B\" conflicts with a static parameter")
# issue #32620
@test Meta.lower(@__MODULE__, quote
    function foo(a::T) where {T}
        for i = 1:1
            T = 0
        end
    end
end) == Expr(:error, "local variable name \"T\" conflicts with a static parameter")
function f32620(x::T) where T
    local y
    let T = 3
        T = 2
        y = T
    end
    return (T, y)
end
@test f32620(0) === (Int, 2)

# issue #28044
code28044(x) = 10x
begin
    function f28044(::Val{code28044}) where code28044
        code28044(2)
    end
    # make sure this assignment to `code28044` doesn't add an implicit-global
    Val{code28044} where code28044
end
@test f28044(Val(identity)) == 2

# issue #28244
macro foo28244(sym)
    x = :(bar())
    push!(x.args, Expr(sym))
    x
end
@test (@macroexpand @foo28244(kw)) == Expr(:call, GlobalRef(@__MODULE__,:bar), Expr(:kw))
@test eval(:(@macroexpand @foo28244($(Symbol("let"))))) == Expr(:error, "malformed expression")

# #16356
@test_throws ParseError Meta.parse("0xapi")

# #22523 #22712
@test_throws ParseError Meta.parse("a?b:c")
@test_throws ParseError Meta.parse("a ?b:c")
@test_throws ParseError Meta.parse("a ? b:c")
@test_throws ParseError Meta.parse("a ? b :c")
@test_throws ParseError Meta.parse("?")

# #13079
@test Meta.parse("1<<2*3") == :((1<<2)*3)

# #19987
@test_throws ParseError Meta.parse("try ; catch f() ; end")

# #23076
@test :([1,2;]) == Expr(:vect, Expr(:parameters), 1, 2)

# #24452
@test Meta.parse("(a...)") == Expr(Symbol("..."), :a)

# #19324
@test_throws UndefVarError(:x) eval(:(module M19324
                 x=1
                 for i=1:10
                     x += i
                 end
             end))

# #22314
function f22314()
    i = 0
    for i = 1:10
    end
    i
end
@test f22314() == 0

module M22314
i = 0
for i = 1:10
end
end
@test M22314.i == 0

# #6080
@test Meta.lower(@__MODULE__, :(ccall(:a, Cvoid, (Cint,), &x))) == Expr(:error, "invalid syntax &x")

@test_throws ParseError Meta.parse("x.'")
@test_throws ParseError Meta.parse("0.+1")

# #24221
@test Meta.isexpr(Meta.lower(@__MODULE__, :(a=_)), :error)

for ex in [:([x=1]), :(T{x=1})]
    @test Meta.lower(@__MODULE__, ex) == Expr(:error, string("misplaced assignment statement in \"", ex, "\""))
end

# issue #28576
@test Meta.isexpr(Meta.parse("1 == 2 ?"), :incomplete)
@test Meta.isexpr(Meta.parse("1 == 2 ? 3 :"), :incomplete)

# issue #28991
eval(Expr(:toplevel,
          Expr(:module, true, :Mod28991,
               Expr(:block,
                    Expr(:export, :Inner),
                    Expr(:abstract, :Inner)))))
@test names(Mod28991) == Symbol[:Inner, :Mod28991]

# issue #28593
macro a28593()
    quote
        abstract type A28593{S<:Real, V<:AbstractVector{S}} end
    end
end

macro b28593()
    quote
        struct B28593{S<:Real, V<:AbstractVector{S}} end
    end
end

macro c28593()
    quote
        primitive type C28593{S<:Real, V<:AbstractVector{S}} 32 end
    end
end

@a28593
@b28593
@c28593

@test A28593.var.name === :S
@test B28593.var.name === :S
@test C28593.var.name === :S

# issue #25955
macro noeffect25955(e)
    return e
end

struct foo25955
end

@noeffect25955 function (f::foo25955)()
    42
end

@test foo25955()() == 42

# issue #28833
macro m28833(expr)
    esc(:(global a28833))
end
@m28833 1+1

# issue #28900
macro foo28900(x)
    quote
        $x
    end
end
f28900(; kwarg) = kwarg
let g = @foo28900 f28900(kwarg = x->2x)
    @test g(10) == 20
end

# issue #26037
x26037() = 10
function test_26037()
    [x26037() for _ in 1:3]
    for x26037 in 1:3
       x26037 += x26037
    end
end
@test test_26037() === nothing  # no UndefVarError

# range and interval operators
@test Meta.parse("1…2") == Expr(:call, :…, 1, 2)
@test Meta.parse("1⁝2") == Expr(:call, :⁝, 1, 2)
@test Meta.parse("1..2") == Expr(:call, :.., 1, 2)
# we don't parse chains of these since the associativity and meaning aren't clear
@test_throws ParseError Meta.parse("1..2..3")

# issue #30048
@test Meta.isexpr(Meta.lower(@__MODULE__, :(for a in b
           c = try
               try
                   d() do
                       if  GC.@preserve c begin
                           end
                       end
                   end
               finally
               end
           finally
           end
       end)), :thunk)

# issue #28506
@test Meta.isexpr(Meta.parse("1,"), :incomplete)
@test Meta.isexpr(Meta.parse("1, "), :incomplete)
@test Meta.isexpr(Meta.parse("1,\n"), :incomplete)
@test Meta.isexpr(Meta.parse("1, \n"), :incomplete)
@test_throws LoadError include_string(@__MODULE__, "1,")
@test_throws LoadError include_string(@__MODULE__, "1,\n")

# issue #30062
let er = Meta.lower(@__MODULE__, quote if false end, b+=2 end)
    @test Meta.isexpr(er, :error)
    @test startswith(er.args[1], "invalid multiple assignment location \"if")
end

# issue #30030
let x = 0
    @test (a=1, b=2, c=(x=3)) == (a=1, b=2, c=3)
    @test x == 3
end

function captured_and_shadowed_sp(x::T) where T
    function g()
        (T,
         let T = 0
             T
         end)
    end
    g()
end
@test captured_and_shadowed_sp(1) === (Int, 0)

function capture_with_conditional_label()
    @goto foo
    x = 1
    if false
        @label foo
    end
    return y->x
end
let f = capture_with_conditional_label()  # should not throw
    @test_throws UndefVarError(:x) f(0)
end

# `_` should not create a global (or local)
f30656(T) = (t, _)::Pair -> t >= T
f30656(10)(11=>1)
@test !isdefined(@__MODULE__, :_)

# issue #30772
function f30772(a::T) where T
    function ()
        function (b::T)
        end
    end
end
let f = f30772(1.0), g = f()
    @test g(1.0) === nothing
    @test_throws MethodError g(1)
end

@test_throws ErrorException("syntax: malformed \"using\" statement")  eval(Expr(:using, :X))
@test_throws ErrorException("syntax: malformed \"import\" statement") eval(Expr(:import, :X))

# eval'ing :const exprs
eval(Expr(:const, :_var_30877))
@test !isdefined(@__MODULE__, :_var_30877)
@test isconst(@__MODULE__, :_var_30877)

# anonymous kw function in value position at top level
f30926 = function (;k=0)
    k
end
@test f30926(k=2) == 2

if false
elseif false
    g30926(x) = 1
end
@test !isdefined(@__MODULE__, :g30926)

@testset "closure conversion in testsets" begin
    p = (2, 3, 4)
    @test p == (2, 3, 4)
    allocs = (() -> @allocated identity(p))()
    @test allocs == 0
end

@test_throws UndefVarError eval(Symbol(""))
@test_throws UndefVarError eval(:(1+$(Symbol(""))))

# issue #31404
f31404(a, b; kws...) = (a, b, kws.data)
@test f31404(+, (Type{T} where T,); optimize=false) === (+, (Type,), (optimize=false,))

# issue #28992
macro id28992(x) x end
@test @id28992(1 .+ 2) == 3
@test Meta.isexpr(Meta.lower(@__MODULE__, :(@id28992((.+)(a,b) = 0))), :error)
@test @id28992([1] .< [2] .< [3]) == [true]
@test @id28992(2 ^ -2) == 0.25
@test @id28992(2 .^ -2) == 0.25

# issue #32121
@test @id28992((a=1, b=2)) === (a=1, b=2)
a32121 = 8
b32121 = 9
@test @id28992((a32121=a32121, b32121=b32121)) === (a32121=8, b32121=9)

# issue #31596
f31596(x; kw...) = x
@test f31596((a=1,), b = 1.0) === (a=1,)

# issue #32325
let
    struct a32325 end
    a32325(x) = a32325()
end
@test a32325(0) === a32325()

@test Meta.lower(Main, :(struct A; A() = new{Int}(); end)) == Expr(:error, "too many type parameters specified in \"new{...}\"")
@test Meta.lower(Main, :(struct A{T, S}; A() = new{Int}(); end)) == Expr(:error, "too few type parameters specified in \"new{...}\"")

# issue #32467
let f = identity(identity() do
                 x = 0
                 @inbounds for i = 1:2
                     x += i
                 end
                 x
                 end)
    @test f() == 3
end

# issue #32499
x32499 = begin
    struct S32499
        function S32499(; x=1)
            x
        end
    end
    S32499(x=2)
end
@test x32499 == 2

# issue #32626
@test Meta.parse("'a'..'b'") == Expr(:call, :(..), 'a', 'b')
@test Meta.parse(":a..:b") == Expr(:call, :(..), QuoteNode(:a), QuoteNode(:b))

# Non-standard identifiers (PR #32408)
@test Meta.parse("var\"#\"") === Symbol("#")
@test Meta.parse("var\"true\"") === Symbol("true")
@test Meta.parse("var\"false\"") === Symbol("false")
@test_throws ParseError Meta.parse("var\"#\"x") # Reject string macro-like suffix
@test_throws ParseError Meta.parse("var \"#\"")
@test_throws ParseError Meta.parse("var\"for\" i = 1:10; end")
# A few cases which would be ugly to deal with if var"#" were a string macro:
@test Meta.parse("var\"#\".var\"a-b\"") == Expr(:., Symbol("#"), QuoteNode(Symbol("a-b")))
@test Meta.parse("export var\"#\"") == Expr(:export, Symbol("#"))
@test Base.remove_linenums!(Meta.parse("try a catch var\"#\" b end")) ==
      Expr(:try, Expr(:block, :a), Symbol("#"), Expr(:block, :b))
@test Meta.parse("(var\"function\" = 1,)") == Expr(:tuple, Expr(:(=), Symbol("function"), 1))
# Non-standard identifiers require parens for string interpolation
@test Meta.parse("\"\$var\\\"#\\\"\"") == Expr(:string, :var, "\"#\"")
@test Meta.parse("\"\$(var\"#\")\"") == Expr(:string, Symbol("#"))
# Stream positioning after parsing var
@test Meta.parse("var'", 1, greedy=false) == (:var, 4)

# quoted names in import (#33158)
@test Meta.parse("import Base.:+") == :(import Base.+)
@test Meta.parse("import Base.Foo.:(==).bar") == :(import Base.Foo.==.bar)

# issue #33135
function f33135(x::T) where {C1, T}
    let C1 = 1, C2 = 2
        C1
    end
end
@test f33135(0) == 1

# issue #33227
@test Meta.isexpr(Meta.lower(Main, :((@label a; @goto a))), :thunk)

# issue #33250
@test Meta.lower(Main, :(f(b=b...))) == Expr(:error, "\"...\" expression cannot be used as keyword argument value")
@test Meta.lower(Main, :(f(;a=a,b=b...))) == Expr(:error, "\"...\" expression cannot be used as keyword argument value")
@test Meta.lower(Main, :((a=a,b=b...))) == Expr(:error, "\"...\" expression cannot be used as named tuple field value")
@test Meta.lower(Main, :(f(;a...,b...)=0)) == Expr(:error, "invalid \"...\" on non-final keyword argument")
@test Meta.lower(Main, :(f(;a...,b=0)=0)) == Expr(:error, "invalid \"...\" on non-final keyword argument")

# issue #31547
@test Meta.lower(Main, :(a := 1)) == Expr(:error, "unsupported assignment operator \":=\"")

# issue #33841
let a(; b) = b
    @test a(b=3) == 3
end

# issue #33987
f33987(args::(Vararg{Any, N} where N); kwargs...) = args
@test f33987(1,2,3) === (1,2,3)

macro id_for_kwarg(x); x; end
Xo65KdlD = @id_for_kwarg let x = 1
    function f(; x)
        x
    end
end
@test_throws UndefKeywordError(:x) Xo65KdlD()
i0xb23hG = @id_for_kwarg let x = 1
    function f(; x=2)
        x
    end
end
@test i0xb23hG() == 2
@test i0xb23hG(x=10) == 10

accepts__kwarg(;z1) = z1
@test (@id_for_kwarg let z1 = 41; accepts__kwarg(; z1); end) == 41

@test @eval let
    (z,)->begin
        $(Expr(:inbounds, true))
        $(Expr(:inbounds, :pop))
    end
    pop = 1
end == 1

# issue #29982
@test Meta.parse("'a'") == 'a'
@test Meta.parse("'\U0061'") == 'a'
test_parseerror("''", "invalid empty character literal")
test_parseerror("'abc'", "character literal contains multiple characters")

# optional soft scope: #28789, #33864

@test @eval begin
    $(Expr(:softscope, true))
    x28789 = 0   # new global included in same expression
    for i = 1:2
        x28789 += i
    end
    x28789
end == 3

y28789 = 1  # new global defined in separate top-level input
@eval begin
    $(Expr(:softscope, true))
    for i = 1:10
        y28789 += i
    end
end
@test y28789 == 56

@eval begin
    $(Expr(:softscope, true))
    for i = 10:10
        z28789 = i
    end
    @test z28789 == 10
    z28789 = 0  # new global assigned after loop but in same soft scope
end

@eval begin
    $(Expr(:softscope, true))
    let y28789 = 0  # shadowing with let
        y28789 = 1
    end
end
@test y28789 == 56

@eval begin
    $(Expr(:softscope, true))
    let
        y28789 = -8  # let is always a hard scope
    end
end
@test y28789 == 56

@eval begin
    $(Expr(:softscope, true))
    for y28789 in 0:0
        for x in 2:2
            for y in 3:3
                z28789 = 42  # assign to global despite several loops
            end
        end
    end
end
@test z28789 == 42

@eval begin
    $(Expr(:softscope, true))
    let x = 0
        ww28789 = 88  # not global
        let y = 3
            ww28789 = 89
        end
        @test ww28789 == 89
    end
end
@test !@isdefined(ww28789)

@eval begin
    $(Expr(:softscope, true))
    for x = 0
        ww28789 = 88  # not global
        for y = 3
            ww28789 = 89
        end
        @test ww28789 == 89
    end
end
@test !@isdefined(ww28789)

@eval begin
    $(Expr(:softscope, true))
    function f28789()
        z28789 = 43
    end
    f28789()
end
@test z28789 == 42

# issue #34673
# check that :toplevel still returns a value when nested inside something else
@test eval(Expr(:block, 0, Expr(:toplevel, 43))) == 43

# issue #16594
@test Meta.parse("@x a + \nb") == Meta.parse("@x a +\nb")
@test [1 +
       1] == [2]
@test [1 +1] == [1 1]

@testset "issue #16594" begin
    # note for the macro tests, order is important
    # because the line number is included as part of the expression
    # (i.e. both macros must start on the same line)
    @test :(@test((1+1) == 2)) == :(@test 1 +
                                          1 == 2)
    @test :(@x 1 +1 -1) == :(@x(1, +1, -1))
    @test :(@x 1 + 1 -1) == :(@x(1+1, -1))
    @test :(@x 1 + 1 - 1) == :(@x(1 + 1 - 1))
    @test :(@x(1 + 1 - 1)) == :(@x 1 +
                                   1 -
                                   1)
    @test :(@x(1 + 1 + 1)) == :(@x 1 +
                                   1 +
                                   1)
    @test :([x .+
              y]) == :([x .+ y])
end

# line break in : expression disallowed
@test_throws Meta.ParseError Meta.parse("[1 :\n2] == [1:2]")

# added ⟂ to operator precedence (#24404)
@test Meta.parse("a ⟂ b ⟂ c") == Expr(:comparison, :a, :⟂, :b, :⟂, :c)
@test Meta.parse("a ⟂ b ∥ c") == Expr(:comparison, :a, :⟂, :b, :∥, :c)

# only allow certain characters after interpolated vars (#25231)
@test Meta.parse("\"\$x෴  \"",raise=false) == Expr(:error, "interpolated variable \$x ends with invalid character \"෴\"; use \"\$(x)\" instead.")
@test Base.incomplete_tag(Meta.parse("\"\$foo", raise=false)) == :string

@testset "issue #30341" begin
    @test Meta.parse("x .~ y") == Expr(:call, :.~, :x, :y)
    # Ensure dotting binary doesn't break dotting unary
    @test Meta.parse(".~[1,2]") == Expr(:call, :.~, Expr(:vect, 1, 2))
end

@testset "operator precedence correctness" begin
    ops = map(Symbol, split("= => || && --> < <| |> : + * // << ^ :: ."))
    for f in ops, g in ops
        f == g && continue
        pf = Base.operator_precedence(f)
        pg = Base.operator_precedence(g)
        @test pf != pg
        expr = Meta.parse("x$(f)y$(g)z")
        @test expr == Meta.parse(pf > pg ? "(x$(f)y)$(g)z" : "x$(f)(y$(g)z)")
    end
end

# issue 34498
@testset "macro calls @foo{...}" begin
    @test :(@foo{}) == :(@foo {})
    @test :(@foo{bar}) == :(@foo {bar})
    @test :(@foo{bar,baz}) == :(@foo {bar,baz})
    @test :(@foo{bar}(baz)) == :((@foo{bar})(baz))
    @test :(@foo{bar}{baz}) == :((@foo{bar}){baz})
    @test :(@foo{bar}[baz]) == :((@foo{bar})[baz])
    @test :(@foo{bar} + baz) == :((@foo{bar}) + baz)
end

@testset "issue #34650" begin
    for imprt in [:using, :import]
        @test Meta.isexpr(Meta.parse("$imprt A, B"), imprt)
        @test Meta.isexpr(Meta.parse("$imprt A: x, y, z"), imprt)

        err = Expr(
            :error,
            "\":\" in \"$imprt\" syntax can only be used when importing a single module. " *
            "Split imports into multiple lines."
        )
        ex = Meta.parse("$imprt A, B: x, y", raise=false)
        @test ex == err

        ex = Meta.parse("$imprt A: x, B: y", raise=false)
        @test ex == err
    end
end

# Syntax desugaring pass errors contain line numbers
@test Meta.lower(@__MODULE__, Expr(:block, LineNumberNode(101, :some_file), :(f(x,x)=1))) ==
    Expr(:error, "function argument names not unique around some_file:101")

# Ensure file names don't leak between `eval`s
eval(LineNumberNode(11, :incorrect_file))
let exc = try eval(:(f(x,x)=1)) catch e ; e ; end
    @test !occursin("incorrect_file", exc.msg)
end

# issue #34967
@test_throws LoadError("string", 2, ErrorException("syntax: invalid UTF-8 sequence")) include_string(@__MODULE__,
                                      "x34967 = 1\n# Halloa\xf5b\nx34967 = 2")
@test x34967 == 1
@test_throws LoadError("string", 1, ErrorException("syntax: invalid UTF-8 sequence")) include_string(@__MODULE__,
                                      "x\xf5 = 3\n# Halloa\xf5b\nx34967 = 4")
@test_throws LoadError("string", 3, ErrorException("syntax: invalid UTF-8 sequence")) include_string(@__MODULE__,
                                      """
                                      # line 1
                                      # line 2
                                      # Hello\xf5b
                                      x34967 = 6
                                      """)

@test Meta.parse("aa\u200b_", raise=false) ==
    Expr(:error, "invisible character \\u200b near column 3")
@test Meta.parse("aa\UE0080", raise=false) ==
    Expr(:error, "invalid character \"\Ue0080\" near column 3")

# issue #31238
a31238, b31238 = let x
    return 1
end
@test !@isdefined(a31238) && !@isdefined(b31238)
@test @eval((a31238, b31238) = let x
    return 1
end) === 1

# issue #35201
h35201(x; k=1) = (x, k)
f35201(c) = h35201((;c...), k=true)
@test f35201(Dict(:a=>1,:b=>3)) === ((a=1,b=3), true)


@testset "issue #34544/35367" begin
    # Test these evals shouldnt segfault
    eval(Expr(:call, :eval, Expr(:quote, Expr(:module, true, :bar1, Expr(:block)))))
    eval(Expr(:module, true, :bar2, Expr(:block)))
    eval(Expr(:quote, Expr(:module, true, :bar3, Expr(:quote))))
    @test_throws ErrorException eval(Expr(:call, :eval, Expr(:quote, Expr(:module, true, :bar4, Expr(:quote)))))
    @test_throws ErrorException eval(Expr(:module, true, :bar5, Expr(:foo)))
    @test_throws ErrorException eval(Expr(:module, true, :bar6, Expr(:quote)))
end

# issue #35391
macro a35391(b)
    :(GC.@preserve ($(esc(b)),) )
end
@test @a35391(0) === (0,)

# global declarations from the top level are not inherited by functions.
# don't allow such a declaration to override an outer local, since it's not
# clear what it should do.
@test Meta.lower(Main, :(let
                           x = 1
                           let
                             global x
                           end
                         end)) == Expr(:error, "`global x`: x is a local variable in its enclosing scope")
# note: this `begin` block must be at the top level
_temp_33553 = begin
    global _x_this_remains_undefined
    let
        local _x_this_remains_undefined = 2
        _x_this_remains_undefined
    end
end
@test _temp_33553 == 2
@test !@isdefined(_x_this_remains_undefined)
