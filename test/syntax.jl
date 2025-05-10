# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests for parser and syntax lowering

using Random
using Base: remove_linenums!

using_JuliaSyntax = parentmodule(Core._parse) != Core.Compiler

macro test_parseerror(str, msg)
    if using_JuliaSyntax
        # Diagnostics are tested separately in JuliaSyntax
        ex = :(@test_throws Meta.ParseError Meta.parse($(esc(str))))
    else
        ex = :(@test_throws Meta.ParseError($(esc(msg))) Meta.parse($(esc(str))))
    end
    ex.args[2] = __source__
    return ex
end

macro test_parseerror(str)
    ex = :(@test_throws Meta.ParseError Meta.parse($(esc(str))))
    ex.args[2] = __source__
    return ex
end

function parseall_nolines(str)
    ex = Meta.parseall(str)
    filter!(e->!(e isa LineNumberNode), ex.args)
    return ex
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
    @test :(try catch $a end) == :(try catch a end)
    @test :(module $a end) == :(module a end)
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

# make sure a trailing integer, not just a symbol, is allowed also
@test test999"foo"123 == ("foo", 123)

# issue #5997
@test_parseerror ": x"
@test_parseerror """begin
    :
    x"""
@test_parseerror "d[: 2]"

# issue #6770
@test_parseerror "x.3"

# issue #8763
@test_parseerror "sqrt(16)2"
@test_parseerror "x' y"
@test_parseerror "x 'y"
@test Meta.parse("x'y") == Expr(:call, :*, Expr(Symbol("'"), :x), :y)

# issue #18851
@test Meta.parse("-2[m]") == Expr(:call, :-, Expr(:ref, 2, :m))
@test Meta.parse("+2[m]") == Expr(:call, :+, Expr(:ref, 2, :m))
@test Meta.parse("!2[3]") == Expr(:call, :!, Expr(:ref, 2, 3))
@test Meta.parse("-2{m}") == Expr(:call, :-, Expr(:curly, 2, :m))
@test Meta.parse("+2{m}") == Expr(:call, :+, Expr(:curly, 2, :m))
@test Meta.parse("-2(m)") == Expr(:call, :*, -2, :m)

# issue #8301
@test_parseerror "&*s"

# issue #10677
@test_parseerror "/1"
@test_parseerror "/pi"
@test Meta.parse("- = 2") == Expr(:(=), :(-), 2)
@test Meta.parse("/ = 2") == Expr(:(=), :(/), 2)
@test_parseerror "< : 2"
@test_parseerror "+ : 2"
@test_parseerror "< :2"
@test Meta.parse("+ :2") == Expr(:call, :(+), QuoteNode(2))

# issue #10900
@test_parseerror "+="
@test_parseerror "."
@test_parseerror "..."

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
@test parseall_nolines("using \$\na") == Expr(:toplevel, Expr(:using, Expr(:., :$)), :a)
@test parseall_nolines("using \$,\na") == Expr(:toplevel, Expr(:using, Expr(:., :$), Expr(:., :a)))
@test parseall_nolines("using &\na") == Expr(:toplevel, Expr(:using, Expr(:., :&)), :a)

@test parseall_nolines("a = &\nb") == Expr(:toplevel, Expr(:(=), :a, :&), :b)
@test parseall_nolines("a = \$\nb") == Expr(:toplevel, Expr(:(=), :a, :$), :b)
@test parseall_nolines(":(a = &\nb)") == Expr(:toplevel, Expr(:quote, Expr(:(=), :a, Expr(:&, :b))))
@test parseall_nolines(":(a = \$\nb)") == Expr(:toplevel, Expr(:quote, Expr(:(=), :a, Expr(:$, :b))))

# issue 12027 - short macro name parsing vs _str suffix
@test parseall_nolines("""
    macro f(args...) end\n@f "macro argument"
""") == Expr(:toplevel,
             Expr(:macro, Expr(:call, :f, Expr(:..., :args)),
                  Expr(:block, LineNumberNode(1, :none), LineNumberNode(1, :none))),
             Expr(:macrocall, Symbol("@f"), LineNumberNode(2, :none), "macro argument"))

# blocks vs. tuples
@test Meta.parse("()") == Expr(:tuple)
@test Meta.parse("(;)") == Expr(:tuple, Expr(:parameters))
@test Meta.parse("(;;)") == Expr(:block)
@test Meta.parse("(;;;;)") == Expr(:block)
@test_parseerror "(,)"
@test_parseerror "(;,)"
@test_parseerror "(,;)"
# TODO: would be nice to make these errors, but needed to parse e.g. `(x;y,)->x`
#@test_parseerror "(1;2,)"
#@test_parseerror "(1;2,;)"
#@test_parseerror "(1;2,;3)"
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
@test_parseerror "(1 2)" # issue #15248

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
@test_parseerror "'\\A\"'"
@test Meta.parse("'\"'") == Meta.parse("'\\\"'") == '"' == "\""[1] == '\42'

# issue #24558
@test '\u2200' == "\u2200"[1]

if !using_JuliaSyntax
    # This should be Expr(:incomplete)
    @test_parseerror "f(2x for x=1:10, y"
end

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

@test_parseerror("0x", "invalid numeric constant \"0x\"")
@test_parseerror("0b", "invalid numeric constant \"0b\"")
@test_parseerror("0o", "invalid numeric constant \"0o\"")
@test_parseerror("0x0.1", "hex float literal must contain \"p\" or \"P\"")
@test_parseerror("0x1.0p", "invalid numeric constant \"0x1.0\"")

# issue #15798
# lowering preserves Expr(:error)
@test Meta.lower(Main, Expr(:error, "no")) == Expr(:error, "no")

# issue #19861 make sure macro-expansion happens in the newest world for top-level expression
@test eval(Base.parse_input_line("""
           macro X19861()
               return 23341
           end
           @X19861
           """)::Expr) == 23341

# issue #15763
@test_parseerror("if\nfalse\nend", "missing condition in \"if\" at none:1")
@test_parseerror("if false\nelseif\nend", "missing condition in \"elseif\" at none:2")

# issue #15828
@test Meta.lower(Main, Meta.parse("x...")) == Expr(:error, "\"...\" expression outside call")

# issue #57153 - malformed "..." expr
@test Meta.lower(@__MODULE__, :(identity($(Expr(:(...), 1, 2, 3))))) ==
    (Expr(:error, "wrong number of expressions following \"...\""))

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
@test_parseerror "function catch() end"
@test_parseerror "function end() end"
@test_parseerror "function finally() end"

# PR #16170
@test Meta.lower(Main, Meta.parse("true(x) = x")) == Expr(:error, "\"true\" is not a valid function argument name")
@test Meta.lower(Main, Meta.parse("false(x) = x")) == Expr(:error, "\"false\" is not a valid function argument name")

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
@test_parseerror "Int [1,2,3]"
@test_parseerror "Int [x for x in 1:10]"
@test_parseerror "foo (x) = x"
@test_parseerror "foo {T<:Int}(x::T) = x"

@test_parseerror "Foo .bar"

@test_parseerror "import x .y"
@test_parseerror "using x .y"

@test_parseerror "--x"
@test_parseerror "stagedfunction foo(x); end"

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
@test_parseerror "[a,;,b]"
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
    @test err.line in (5, 7)
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

    m_error = try @eval method_c6(a::Vararg{:A}) = 1; catch e; e; end
    error_out = sprint(showerror, m_error)
    @test startswith(error_out, "ArgumentError: invalid type for argument a in method definition for method_c6 at $filename:")
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
                       "for" => :other, "function" => :other,
                       "f() do" => :other, "module" => :other, "mutable struct" => :other,
                       "struct" => :other,
                       "quote" => using_JuliaSyntax ? :block : :other,
                       "let" => using_JuliaSyntax ? :block : :other,
                       "begin" => using_JuliaSyntax ? :block : :other,
                      )
    @test Base.incomplete_tag(Meta.parse(str, raise=false)) == tag
end

# meta nodes for optional positional arguments
let code = Meta.lower(Main, :(@inline f(p::Int=2) = 3)).args[1].code
    local src
    for i = length(code):-1:1
        if Meta.isexpr(code[i], :method)
            src = code[i].args[3]
            break
        end
    end
    @test Core.Compiler.is_declared_inline(src)
end

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
    Core.@latestworld
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
    e = try
        macroexpand(@__MODULE__, ex)
        false
    catch ex
        ex
    end::MethodError
    @test e.f === getfield(A15838, Symbol("@f"))
    @test e.args === (__source__, @__MODULE__, 1, 2)
end

# issue 10046
for op in ["+", "-", "\$", "|", ".+", ".-", "*", ".*"]
    @test_parseerror "$op in [+, -]"
end

# issue #17701
@test Meta.lower(Main, :(i==3 && i+=1)) == Expr(:error, "invalid assignment location \"(i == 3) && i\"")

# issue #18667
@test Meta.lower(Main, :(true = 1)) == Expr(:error, "invalid assignment location \"true\"")
@test Meta.lower(Main, :(false = 1)) == Expr(:error, "invalid assignment location \"false\"")

# PR #15592
let str = "[1] [2]"
    @test_parseerror str
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
    if ex.head === :thunk
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

function is_return_ssavalue(ex)
    ex isa Core.ReturnNode && ex.val isa Core.SSAValue
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
    #@test ci.codelocs in ([4, 4, 0], [4, 0])
    @test is_return_ssavalue(m3_exprs[end])
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
```.head === :if

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
let ε=1, μ=2, x=3, î=4, ⋅=5, (-)=6
    # issue #5434 (mu vs micro):
    @test Meta.parse("\u00b5") === Meta.parse("\u03bc")
    @test µ == μ == 2
    # NFC normalization of identifiers:
    @test Meta.parse("\u0069\u0302") === Meta.parse("\u00ee")
    @test î == 4
    # latin vs greek ε (#14751)
    @test Meta.parse("\u025B") === Meta.parse("\u03B5")
    @test ɛ == ε == 1
    # middot char · or · vs math dot operator ⋅ (#25098)
    @test Meta.parse("\u00b7") === Meta.parse("\u0387") === Meta.parse("\u22c5")
    @test (·) == (·) == (⋅) == 5
    # minus − vs hyphen-minus - (#26193)
    @test Meta.parse("\u2212") === Meta.parse("-")
    @test Meta.parse("\u221242") === Meta.parse("-42")
    @test Meta.parse("\u2212 42") == Meta.parse("- 42")
    @test Meta.parse("\u2212x") == Meta.parse("-x")
    @test Meta.parse("x \u2212 42") == Meta.parse("x - 42")
    @test Meta.parse("x \u2212= 42") == Meta.parse("x -= 42")
    @test Meta.parse("100.0e\u22122") === Meta.parse("100.0E\u22122") === Meta.parse("100.0e-2")
    @test Meta.parse("100.0f\u22122") === Meta.parse("100.0f-2")
    @test Meta.parse("0x100p\u22128") === Meta.parse("0x100P\u22128") === Meta.parse("0x100p-8")
    @test (−) == (-) == 6
    # hbar ℏ to ħ - (#48870)
    @test :ℏ === :ħ
end

# issue #8925
let
    global const (c8925, d8925) = (3, 4)
end
@test c8925 == 3 && isconst(@__MODULE__, :c8925)
@test d8925 == 4 && isconst(@__MODULE__, :d8925)

# issue #47168
let t47168 = (;a47168 = 1, b47168 = 2);
    global const (;a47168, b47168) = t47168
    @test a47168 == 1 && isconst(@__MODULE__, :a47168)
    @test b47168 == 2 && isconst(@__MODULE__, :b47168)
end
@test (let x = (;x=1); let (;x) = x; x; end, x; end) == (1, (x = 1,))

# issue #18754: parse ccall as a regular function
@test Meta.parse("ccall([1], 2)[3]") == Expr(:ref, Expr(:call, :ccall, Expr(:vect, 1), 2), 3)
@test Meta.parse("ccall(a).member") == Expr(:., Expr(:call, :ccall, :a), QuoteNode(:member))

# Check that the body of a `where`-qualified short form function definition gets
# a :block for its body
short_where_call = :(f(x::T) where T = T)
@test short_where_call.args[2].head === :block

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

# issue #45506
@test :( function (a) where {B, C} end).args[1] == Expr(:where, Expr(:tuple, :a), :B, :C)
@test (function(::Type{Tuple{A45506, B45506}}) where {A45506 <: Any, B45506 <: Any}
    B45506
end)(Tuple{Int8, Int16}) == Int16

# issue #20541
@test Meta.parse("[a .!b]") == Expr(:hcat, :a, Expr(:call, :.!, :b))

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
@test_broken Meta.lower(Main, :(a.[1])) == Expr(:error, "invalid syntax \"a.[1]\"")
@test_broken Meta.lower(Main, :(a.{1})) == Expr(:error, "invalid syntax \"a.{1}\"")

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
@test_parseerror "\"a\"x"
@test_parseerror "\"a\"begin end"
@test_parseerror "\"a\"begin end\"b\""

# issue #16427
@test_parseerror "for i=1:1 end(3)"
@test_parseerror "begin end(3)"
@test_parseerror "while false end(3)"

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
    @test_parseerror "3 $(bad)⁽¹⁾ 4"
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

@test_parseerror "[2for i=1:10]"
@test_parseerror "[1 for i in 1:2for j in 2]"
@test_parseerror "(1 for i in 1:2for j in 2)"
# issue #20441
@test_parseerror "[x.2]"
@test_parseerror "x.2"
@test Meta.parse("[x;.2]") == Expr(:vcat, :x, 0.2)

# issue #22840
@test Meta.parse("[:a :b]") == Expr(:hcat, QuoteNode(:a), QuoteNode(:b))

# issue #22868
@test_parseerror "x@time 2"
@test_parseerror "@ time"

# issue #7479
@test Meta.lower(Main, Meta.parse("(true &&& false)")) == Expr(:error, "invalid syntax &false")

# issue #34748
@test Meta.lower(Main, :(&(1, 2))) == Expr(:error, "invalid syntax &(1, 2)")

# if an indexing expression becomes a cat expression, `end` is not special
@test_parseerror "a[end end]"
@test_parseerror "a[end;end]"
#@test_parseerror "a[end;]"  # this is difficult to fix
let a = rand(8), i = 3
    @test a[[1:i-1; i+1:end]] == a[[1,2,4,5,6,7,8]]
end

# issue #18935
@test [begin
          @inbounds for i = 1:10 end
       end for i = 1:5] == fill(nothing, 5)

# issue #18912
@test_parseerror "(::)"
@test Meta.parse(":(::)") == QuoteNode(Symbol("::"))
@test_parseerror "f(::) = ::"
@test Meta.parse("(::A)") == Expr(Symbol("::"), :A)
@test_parseerror "(::, 1)"
@test_parseerror "(1, ::)"

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
module M24289
macro m24289()
    :(global $(esc(:x24289)) = 1)
end
end
M24289.@m24289
@test x24289 === 1

# parsing numbers with _ and .
@test Meta.parse("1_2.3_4") == 12.34
@test_parseerror "1._"
@test_parseerror "1._5"
@test_parseerror "1e.3"
@test_parseerror "1e3."
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

# issue #43960, evaluation order of splatting in `ref`
let a = [], b = [4,3,2,1]
    f() = (push!(a, 1); 2)
    g() = (push!(a, 2); ())
    @test b[f(), g()...] == 3
    @test a == [1,2]
end

# issue #44239
struct KWGetindex end
Base.getindex(::KWGetindex, args...; kws...) = (args, NamedTuple(kws))
let A = KWGetindex(), a = [], b = [4,3,2,1]
    f() = (push!(a, 1); 2)
    g() = (push!(a, 2); ())
    @test A[f(), g()..., k = f()] === ((2,), (k = 2,))
    @test a == [1, 2, 1]
    @test A[var"end"=1] === ((), (var"end" = 1,))
end

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

@test_parseerror("f(x::V) where {V) = x",
                 "expected \"}\" or separator in arguments to \"{ }\"; got \"V)\"")
@test_parseerror("[1)",
                 "expected \"]\" or separator in arguments to \"[ ]\"; got \"1)\"")

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

# Chaining of <: and >: in `where`
@test isa(Vector{T} where Int<:T<:Number, UnionAll)
@test isa(Vector{T} where Number>:T>:Int, UnionAll)

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
@test_parseerror "using Colors()"

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

@test_parseerror "1 -+ (a=1, b=2)"  "space before \"(\" not allowed in \"+ (\" at none:1"
# issue #29781
@test_parseerror "sin. (1)"     "space before \"(\" not allowed in \"sin. (\" at none:1"
# Parser errors for disallowed space contain line numbers
@test_parseerror "\nf() [i]"    "space before \"[\" not allowed in \"f() [\" at none:2"
@test_parseerror "\nf() (i)"    "space before \"(\" not allowed in \"f() (\" at none:2"
@test_parseerror "\nf() .i"     "space before \".\" not allowed in \"f() .\" at none:2"
@test_parseerror "\nf() {i}"    "space before \"{\" not allowed in \"f() {\" at none:2"
@test_parseerror "\n@ m"        "space before \"m\" not allowed in \"@ m\" at none:2"
@test_parseerror "\nusing a .b" "space before \".\" not allowed in \"a .\" at none:2"
@test_parseerror "\nusing a .b" "space before \".\" not allowed in \"a .\" at none:2"
@test_parseerror "\n+ (x, y)"   "space before \"(\" not allowed in \"+ (\" at none:2"

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
@test_parseerror "2!3"

# issue #27914
@test Meta.parse("2f(x)")        == Expr(:call, :*, 2, Expr(:call, :f, :x))
@test Meta.parse("f(x)g(x)")     == Expr(:call, :*, Expr(:call, :f, :x), Expr(:call, :g, :x))
@test Meta.parse("2f(x)g(x)")    == Expr(:call, :*, 2, Expr(:call, :f, :x), Expr(:call, :g, :x))
@test Meta.parse("f(x)g(x)h(x)") == Expr(:call, :*, Expr(:call, :f, :x), Expr(:call, :g, :x), Expr(:call, :h, :x))
@test Meta.parse("2(x)")         == Expr(:call, :*, 2, :x)
@test Meta.parse("2(x)y")        == Expr(:call, :*, 2, :x, :y)

@test_parseerror "a.: b"
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
@test_parseerror "module module end"

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
@test_parseerror "Any[:(end)]"

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

let ex = Meta.lower(@__MODULE__, :(function g end = 1))
    @test isa(ex, Expr) && ex.head === :error
    @test ex.args[1] == """
invalid assignment location "function g
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
    @test_broken exc isa ErrorException
    @test_broken startswith(exc.msg, "syntax: invalid syntax \"sin.[1]\"")
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

# issue #43018
module M43018
    macro test43018(fn)
        quote $(fn)() end
    end
end
@test :(@M43018.test43018() do; end) == :(M43018.@test43018() do; end)
@test @macroexpand(@M43018.test43018() do; end) == @macroexpand(M43018.@test43018() do; end)
@test @M43018.test43018() do; 43018 end == 43018

# issue #27129
f27129(x = 1) = (@inline; x)
for method in methods(f27129)
    @test Core.Compiler.is_declared_inline(method)
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
    esc(x)
end
@test @macroexpand(@foo28244(kw)) == Expr(:call, :bar, Expr(:kw))
let x = @macroexpand @foo28244(var"let")
    @test Meta.lower(@__MODULE__, x) == Expr(:error, "malformed expression")
end

# #16356
@test_parseerror "0xapi"

# #22523 #22712
@test_parseerror "a?b:c"
@test_parseerror "a ?b:c"
@test_parseerror "a ? b:c"
@test_parseerror "a ? b :c"
@test_parseerror "?"

# #13079
@test Meta.parse("1<<2*3") == :((1<<2)*3)

# #19987
@test_parseerror "try ; catch f() ; end"

# #23076
@test :([1,2;]) == Expr(:vect, Expr(:parameters), 1, 2)

# #24452
@test Meta.parse("(a...)") == Expr(Symbol("..."), :a)

# #19324
@test_throws UndefVarError(:x, :local) eval(:(module M19324
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

@test Meta.lower(@__MODULE__, :(f(x) = (y = x + 1; ccall((:a, y), Cvoid, ())))) == Expr(:error, "ccall function name and library expression cannot reference local variables")

@test_parseerror "x.'"
@test_parseerror "0.+1"

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

# issue #51899
macro struct_macro_51899()
    quote
        mutable struct Struct51899
            const const_field
            const const_field_with_type::Int
            $(esc(Expr(:const, :(escaped_const_field::MyType))))
            @atomic atomic_field
            @atomic atomic_field_with_type::Int
        end
    end
end

let ex = @macroexpand @struct_macro_51899()
    const_field, const_field_with_type, escaped_const_field,
    atomic_field, atomic_field_with_type = filter(x -> isa(x, Expr), ex.args[end].args[end].args)
    @test Meta.isexpr(const_field, :const)
    @test const_field.args[1] === :const_field

    @test Meta.isexpr(const_field_with_type, :const)
    @test Meta.isexpr(const_field_with_type.args[1], :(::))
    @test const_field_with_type.args[1].args[1] === :const_field_with_type
    @test const_field_with_type.args[1].args[2] == GlobalRef(@__MODULE__, :Int)

    @test Meta.isexpr(escaped_const_field, :const)
    @test Meta.isexpr(const_field_with_type.args[1], :(::))
    @test escaped_const_field.args[1].args[1] === :escaped_const_field
    @test escaped_const_field.args[1].args[2] === :MyType

    @test Meta.isexpr(atomic_field, :atomic)
    @test atomic_field.args[1] === :atomic_field

    @test Meta.isexpr(atomic_field_with_type, :atomic)
    @test atomic_field_with_type.args[1].args[1] === :atomic_field_with_type
    @test atomic_field_with_type.args[1].args[2] == GlobalRef(@__MODULE__, :Int)
end

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
@test_parseerror "1..2..3"

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
    @test_throws UndefVarError(:x, :local) f(0)
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
@test !isconst(@__MODULE__, :_var_30877)

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
f31404(a, b; kws...) = (a, b, values(kws))
@test f31404(+, (Type{T} where T,); optimize=false) === (+, (Type,), (optimize=false,))

# issue #28992
macro id28992(x) x end
@test @id28992(1 .+ 2) == 3
@test Meta.@lower(.+(a,b) = 0) == Expr(:error, "invalid function name \".+\"")
@test Meta.@lower((.+)(a,b) = 0) == Expr(:error, "invalid function name \"(.+)\"")
let m = @__MODULE__
    @test Meta.lower(m, :($m.@id28992(.+(a,b) = 0))) == Expr(:error, "invalid function name \"$(nameof(m)).:.+\" around $(@__FILE__):$(@__LINE__)")
    @test Meta.lower(m, :($m.@id28992((.+)(a,b) = 0))) == Expr(:error, "invalid function name \"(.$(nameof(m)).+)\" around $(@__FILE__):$(@__LINE__)")
end
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
@test_parseerror "var\"#\"x" # Reject string macro-like suffix
@test_parseerror "var \"#\""
@test_parseerror "var\"for\" i = 1:10; end"
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
@test_warn "declares type variable C1 but does not use it" @eval function f33135(x::T) where {C1, T}
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
@test_deprecated eval(quote
    # This syntax is deprecated. This test should be removed when the
    # deprecation is.
    f33987(args::(Vararg{Any, N} where N); kwargs...) = args
    @test f33987(1,2,3) === (1,2,3)
end)

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
@test_parseerror("''", "invalid empty character literal")
@test_parseerror("'abc'", "character literal contains multiple characters")

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

const warn28789 = "Assignment to `s28789` in soft scope is ambiguous because a global variable by the same name exists: "*
    "`s28789` will be treated as a new local. Disambiguate by using `local s28789` to suppress this warning or "*
    "`global s28789` to assign to the existing global variable."
@test_logs (:warn, warn28789) @test_throws UndefVarError @eval begin
    s28789 = 0
    for i = 1:10
        s28789 += i
    end
end

# issue #38650, `struct` should always be a hard scope
f38650() = 0
@eval begin
    $(Expr(:softscope, true))
    struct S38650
        f38650() = 1
    end
end
@test f38650() == 0

# issue #37126
@test isempty(Test.collect_test_logs() do
    include_string(@__MODULE__, """
        function foo37126()
            f(lhs::Integer, rhs::Integer) = nothing
            f(lhs::Integer, rhs::AbstractVector{<:Integer}) = nothing
            return f
        end
        struct Bar37126{T<:Real, P<:Real} end
        """)
    end[1])

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
@test_parseerror "[1 :\n2] == [1:2]"

# added ⟂ to operator precedence (#24404)
@test Meta.parse("a ⟂ b ⟂ c") == Expr(:comparison, :a, :⟂, :b, :⟂, :c)
@test Meta.parse("a ⟂ b ∥ c") == Expr(:comparison, :a, :⟂, :b, :∥, :c)

# issue 39350
@testset "binary ⫪ and ⫫" begin
    @test Meta.parse("a ⫪ b") == Expr(:call, :⫪, :a, :b)
    @test Meta.parse("a ⫫ b") == Expr(:call, :⫫, :a, :b)
end

# issue 45962
@testset "binary ⭄, ⥺, ⭃, and ⥷" begin
    @test Meta.parse("a ⭄ b") == Expr(:call, :⭄, :a, :b)
    @test Meta.parse("a ⥺ b") == Expr(:call, :⥺, :a, :b)
    @test Meta.parse("a ⭃ b") == Expr(:call, :⭃, :a, :b)
    @test Meta.parse("a ⥷ b") == Expr(:call, :⥷, :a, :b)
end

# only allow certain characters after interpolated vars (#25231)
@test_parseerror("\"\$x෴  \"",
                 "interpolated variable \$x ends with invalid character \"෴\"; use \"\$(x)\" instead.")
@test Base.incomplete_tag(Meta.parse("\"\$foo", raise=false)) === :string

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
        )
        @test_parseerror("$imprt A, B: x, y",
                         "\":\" in \"$imprt\" syntax can only be used when importing a single module. Split imports into multiple lines.")
        @test_parseerror("$imprt A: x, B: y",
                         "\":\" in \"$imprt\" syntax can only be used when importing a single module. Split imports into multiple lines.")
    end
end

# Syntax desugaring pass errors contain line numbers
@test Meta.lower(@__MODULE__, Expr(:block, LineNumberNode(101, :some_file), :(f(x,x)=1))) ==
    Expr(:error, "function argument name not unique: \"x\" around some_file:101")

@test Meta.lower(@__MODULE__, Expr(:block, LineNumberNode(102, :some_file), :(function f(x) where T where T; x::T; end))) ==
    Expr(:error, "function static parameter name not unique: \"T\" around some_file:102")

@test Meta.lower(@__MODULE__, Expr(:block, LineNumberNode(103, :some_file), :(function f(t) where t; x; end))) ==
    Expr(:error, "function argument and static parameter name not distinct: \"t\" around some_file:103")

# Ensure file names don't leak between `eval`s
eval(LineNumberNode(11, :incorrect_file))
let exc = try eval(:(f(x,x)=1)) catch e ; e ; end
    @test !occursin("incorrect_file", exc.msg)
end

@testset "issue #34967" begin
    @test_parseerror "#\xf5b\nx" "invalid UTF-8 sequence"

    # Test line UTF-8 errors with line numbers
    let ex = Meta.parseall("x\n#\xf5b\ny")
        @test Meta.isexpr(ex, :toplevel, 4) && Meta.isexpr(last(ex.args), :error)
        @test ex.args[3] == LineNumberNode(2,:none)
    end
    let ex = Meta.parseall("x\xf5\n#\xf5b\ny")
        @test Meta.isexpr(ex, :toplevel, 2) && Meta.isexpr(last(ex.args), :error)
        @test ex.args[1] == LineNumberNode(1,:none)
    end
    let ex = Meta.parseall("#line1\n#line2\n#\xf5b\ny")
        @test Meta.isexpr(ex, :toplevel, 2) && Meta.isexpr(last(ex.args), :error)
        @test ex.args[1] == LineNumberNode(3,:none)
    end
end

@test_parseerror "aa\u200b_" "invisible character \\u200b near column 3"
@test_parseerror "aa\UE0080" "invalid character \"\Ue0080\" near column 3"

@testset "unrecognized escapes in string/char literals" begin
    @test_parseerror "\"\\.\""
    @test_parseerror "\'\\.\'"
end

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

# issue #44343
f44343(;kw...) = NamedTuple(kw)
@test f44343(u = (; :a => 1)) === (u = (; :a => 1),)

@testset "issue #34544/35367/35429" begin
    # Test these evals shouldn't segfault
    eval(Expr(:call, :eval, Expr(:quote, Expr(:module, true, :bar1, Expr(:block)))))
    eval(Expr(:module, true, :bar2, Expr(:block)))
    eval(Expr(:quote, Expr(:module, true, :bar3, Expr(:quote))))
    @test_throws ErrorException eval(Expr(:call, :eval, Expr(:quote, Expr(:module, true, :bar4, Expr(:quote)))))
    @test_throws ErrorException eval(Expr(:module, true, :bar5, Expr(:foo)))
    @test_throws ErrorException eval(Expr(:module, true, :bar6, Expr(:quote)))

    #35429
    @test_throws ErrorException eval(Expr(:thunk, x->x+9))
    @test_throws ErrorException eval(Expr(:thunk, Meta.parse("x=17")))
    @test_throws ErrorException eval(Expr(:thunk, Meta.parse("17")))
end

# issue #35391
macro a35391(b)
    :(GC.@preserve ($(esc(b)),) )
end
@test @a35391(0) === (0,)

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

module GlobalContainment
using Test
@testset "scope of global declarations" begin

    # global declarations from the top level are not inherited by functions.
    # don't allow such a declaration to override an outer local, since it's not
    # clear what it should do.
    @test Meta.lower(
        Main,
        :(let
              x = 1
              let
                  global x
              end
          end)) == Expr(:error, "`global x`: x is a local variable in its enclosing scope")

    # a declared global can shadow a local in an outer scope
    @test let
        function f()
            g0 = 2
            let; global g0 = 1; end
            a = () -> (global g0 = 1); a();
            return g0
        end
        (f(), g0);
    end === (2, 1)
    @test let
        function f()
            let; global g2 = 1; end;
            let; try; g2 = 2; catch _; end; end;
        end
        (f(), g2)
    end === (2, 1)

    # an inner global declaration should not interfere with the closure (#57547)
    @test let
        g3 = 1
        function f()
            let; global g3 = 2; end;
            return g3
        end
        f()
    end === 1
    @test_throws UndefVarError let
        function returns_global()
            for i in 1
                global ge = 2
            end
            return ge # local declared below
        end
        ge = returns_global()
    end
    @test let
        function f(x::T) where T
            function g(x)
                let; global T = 1; end
                x::T
            end; g(x)
        end; f(1)
    end === 1

end
end

# lowering of adjoint
@test (1 + im)' == 1 - im
x = let var"'"(x) = 2x
    3'
end
@test x == 6

# issue #36196
@test_parseerror "(for i=1; println())"  "\"for\" at none:1 expected \"end\", got \")\""
@test_parseerror "(try i=1; println())"  "\"try\" at none:1 expected \"end\", got \")\""

# issue #36272
macro m36272()
    :((a, b=1) -> a*b)
end
@test @m36272()(1) == 1

# issue #37134
macro m37134()
    :(x :: Int -> 62)
end
@test @m37134()(1) == 62
@test_throws MethodError @m37134()(1.0) == 62

macro n37134()
    :($(esc(Expr(:tuple, Expr(:..., :x))))->$(esc(:x)))
end
@test @n37134()(2,1) === (2,1)

@testset "unary ± and ∓" begin
    @test Meta.parse("±x") == Expr(:call, :±, :x)
    @test Meta.parse("∓x") == Expr(:call, :∓, :x)
end

@testset "test .<: and .>:" begin
    tmp = [Int, Float64, String, Bool] .<: Union{Int, String}
    @test tmp == Bool[1, 0, 1, 0]

    tmp = [Int, Float64, String, Bool] .>: [Int, Float64, String, Bool]
    @test tmp == Bool[1, 1, 1, 1]

    tmp = @. [Int, Float64, String, Bool] <: Union{Int, String}
    @test tmp == Bool[1, 0,1, 0]

    @test (Int .<: [Integer] .<: [Real]) == [true]
end

@test :(a <-- b <-- c) == Expr(:call, :<--, :a, Expr(:call, :<--, :b, :c))
@test :(a .<-- b.<--c) == Expr(:call, :.<--, :a, Expr(:call, :.<--, :b, :c))
@test :(a<-->b<-->c) == Expr(:call, :<-->, :a, Expr(:call, :<-->, :b, :c))
@test :(a.<-->b .<--> c) == Expr(:call, :.<-->, :a, Expr(:call, :.<-->, :b, :c))
@test :(a --> b --> c) == Expr(:-->, :a, Expr(:-->, :b, :c))
@test :(a --> b.-->c) == Expr(:-->, :a, Expr(:call, :.-->, :b, :c))
let (-->) = (+)
    @test (40 --> 2) == 42
end
@test_parseerror("1<---2", "invalid operator \"<---\"")
@test_parseerror("1 .<--- 2", "invalid operator \".<---\"")
@test_parseerror("a---b", "invalid operator \"--\"")
@test_parseerror("a.---b", "invalid operator \".--\"")

# issue #37228
# NOTE: the `if` needs to be at the top level
if isodd(1) && all(iseven(2) for c in ())
    @test true
else
    @test false
end

@test :(a +ꜝ b) == Expr(:call, :+ꜝ, :a, :b)

function ncalls_in_lowered(ex, fname)
    lowered_exprs = Meta.lower(Main, ex).args[1].code
    return count(lowered_exprs) do ex
        if Meta.isexpr(ex, :call)
            arg = ex.args[1]
            if isa(arg, Core.SSAValue)
                arg = lowered_exprs[arg.id]
            end
            return arg == fname
        end
        return false
    end
end

@testset "standalone .op" begin
    @test :(.+) == Expr(:., :+)
    @test :(map(.-, a)) == Expr(:call, :map, Expr(:., :-), :a)

    @test ncalls_in_lowered(:(.*), GlobalRef(Base, :BroadcastFunction)) == 1
    @test ncalls_in_lowered(:((.^).(a, b)), GlobalRef(Base, :broadcasted)) == 1
    @test ncalls_in_lowered(:((.^).(a, b)), GlobalRef(Base, :BroadcastFunction)) == 1
    @test ncalls_in_lowered(:((.+)(a, b .- (.^)(c, 2))), GlobalRef(Base, :broadcasted)) == 3
    @test ncalls_in_lowered(:((.+)(a, b .- (.^)(c, 2))), GlobalRef(Base, :materialize)) == 1
    @test ncalls_in_lowered(:((.+)(a, b .- (.^)(c, 2))), GlobalRef(Base, :BroadcastFunction)) == 0
end

# issue #37656
@test :(if true 'a' else 1 end) == Expr(:if, true, quote 'a' end, quote 1 end)

# issue #37664
@test_parseerror("a b",     "extra token \"b\" after end of expression")
@test_parseerror("a#==#b",  "extra token \"b\" after end of expression")
@test_parseerror("a #==#b", "extra token \"b\" after end of expression")
@test_parseerror("a#==# b", "extra token \"b\" after end of expression")
@test_parseerror("1 2",     "extra token \"2\" after end of expression")
@test_parseerror("1#==#2",  "extra token \"2\" after end of expression")
@test_parseerror("1 #==#2", "extra token \"2\" after end of expression")
@test_parseerror("1#==# 2", "extra token \"2\" after end of expression")

@test size([1#==#2#==#3]) == size([1 2 3])
@test size([1#==#2#==#3]) == size([1	2	3]) # tabs
@test size([1#==#2#==#3]) == size([1	2 3]) # tabs and spaces
@test size([1#==#2#==#3]) == size([1 2	3]) # tabs and spaces
@test [zeros(Int,2,2)#==#[1;2]
       [3#==#4]#==#5]          == [zeros(Int,2,2) [1; 2]
                                   [3 4]          5     ] == [0 0 1
                                                              0 0 2
                                                              3 4 5]

@test Meta.parse("for x in 1:10 g(x) end") ==
    Meta.parse("for#==#x#==#in#==#1:10#==#g(x)#==#end")
@test Meta.parse("(f->f(1))() do x x+1 end") ==
    Meta.parse("(f->f(1))()#==#do#==#x#==#x+1#==#end")
@test Meta.parse("while i < 10 i += 1 end") ==
    Meta.parse("while#==#i#==#<#==#10#==#i#==#+=#==#1#==#end")
@test Meta.parse("begin x=1 end") == Meta.parse("begin#==#x=1#==#end")
@test Meta.parse("if x<y x+1 elseif y>0 y+1 else z end") ==
    Meta.parse("if#==#x<y#==#x+1#==#elseif#==#y>0#==#y+1#==#else#==#z#==#end")
@test Meta.parse("function(x) x end") == Meta.parse("function(x)#==#x#==#end")
@test Meta.parse("a ? b : c") == Meta.parse("a#==#?#==#b#==#:#==#c")
@test_parseerror("f#==#(x)=x", "space before \"(\" not allowed in \"f (\" at none:1")
@test Meta.parse("try f() catch e g() finally h() end") ==
    Meta.parse("try#==#f()#==#catch#==#e#==#g()#==#finally#==#h()#==#end")
@test Meta.parse("@m a b") == Meta.parse("@m#==#a#==#b")

# issue #37540
macro m37540()
    quote
        x = 1
        :($x)
    end
end
@test @m37540() == 1

# issue #37890
struct A37890{A, B}
    a
    b
    A37890(args::Tuple) = return new{typeof.(args)...}(args...)
end
@test A37890((1, "")) isa A37890{Int, String}
@test_throws ErrorException A37890((1,1,1))
@test_throws TypeError A37890((1,))

struct B37890{A, B}
    a
    b
    B37890(a, b) = new{Int, ()..., Int8}(a, b)
end
@test B37890(1.0, 2.0f0) isa B37890{Int, Int8}

# import ... as
@test_parseerror("using A as B",       "invalid syntax \"using A as ...\"")
@test_parseerror("using A.b as B",     "invalid syntax \"using A.b as ...\"")
@test_parseerror("using X, A.b as B",  "invalid syntax \"using A.b as ...\"")
@test_parseerror("import A as B: c",   "invalid syntax \"import A as B:\"")
@test_parseerror("import A.b as B: c", "invalid syntax \"import A.b as B:\"")

module TestImportAs
using Test

module Mod
const x = 1
global maybe_undef, always_undef
export always_undef
def() = (global maybe_undef = 0)
func(x) = 2x + 1

macro mac(x)
    :($(esc(x)) + 1)
end
end

module Mod2
import ..Mod.x as x_from_mod
import ..Mod.x as x_from_mod2
const y = 2

export x_from_mod2
end

import .Mod: x as x2

@test x2 == 1
@test !@isdefined(x)

module_names = names(@__MODULE__; all=true, imported=true)
@test :x2 ∈ module_names
@test :x ∉ module_names

import .Mod2.y as y2

@test y2 == 2
@test !@isdefined(y)

# Test that eval rejects the invalid syntax `import .Mod.x as (a.b)`
@test_throws ErrorException eval(
    Expr(:import, Expr(:as, Expr(:., :., :Mod, :x), Expr(:., :a, QuoteNode(:b)))))

import .Mod.maybe_undef as mu
@test_throws UndefVarError mu
Mod.def()
@test mu === 0

module Mod3
using ..Mod: func as f
using ..Mod
end
@test Mod3.f(10) == 21
@test !isdefined(Mod3, :func)
@test_throws ErrorException("invalid method definition in Mod3: function Mod.f must be explicitly imported to be extended") Core.eval(Mod3, :(f(x::Int) = x))
@test !isdefined(Mod3, :always_undef) # resolve this binding now in Mod3
@test Core.eval(Mod3, :(always_undef(x::Int) = x)) == invokelatest(getglobal, Mod3, :always_undef)
@test Core.eval(Mod3, :(const always_undef = 3)) == invokelatest(getglobal, Mod3, :always_undef)
@test_throws ErrorException("cannot declare Mod3.f constant; it was already declared as an import") Core.eval(Mod3, :(const f = 3))
@test_throws ErrorException("cannot declare Mod.maybe_undef constant; it was already declared global") Core.eval(Mod, :(const maybe_undef = 3))

z = 42
import .z as also_z
@test also_z == 42

import .Mod.@mac as @m
@test @m(3) == 4

@test_throws ErrorException eval(:(import .Mod.@mac as notmacro))
@test_throws ErrorException eval(:(import .Mod.func as @notmacro))
@test_throws ErrorException eval(:(using .Mod: @mac as notmacro))
@test_throws ErrorException eval(:(using .Mod: func as @notmacro))

import .Mod2.x_from_mod

@test @isdefined(x_from_mod)
@test x_from_mod == Mod.x

using .Mod2

@test_nowarn @eval x_from_mod2
@test @isdefined(x_from_mod2)
@test x_from_mod2 == x_from_mod == Mod.x
end

import .TestImportAs.Mod2 as M2
@test !@isdefined(Mod2)
@test M2 === TestImportAs.Mod2

# 57702: nearby bindings shouldn't cause us to closure-convert in import/using
module OddImports
using Test
module ABC end
x = let; let; import .ABC; end; let; ABC() = (ABC,); end; end
y = let; let; using  .ABC; end; let; ABC() = (ABC,); end; end
z = let; let; import SHA: R; end; let; R(x...) = R(x); end; end
@test x isa Function
@test y isa Function
@test z isa Function
end

@testset "unicode modifiers after '" begin
    @test Meta.parse("a'ᵀ") == Expr(:call, Symbol("'ᵀ"), :a)
    @test Meta.parse("a'⁻¹") == Expr(:call, Symbol("'⁻¹"), :a)
    @test Meta.parse("a'ᵀb") == Expr(:call, :*, Expr(:call, Symbol("'ᵀ"), :a), :b)
    @test Meta.parse("a'⁻¹b") == Expr(:call, :*, Expr(:call, Symbol("'⁻¹"), :a), :b)
end

@testset "issue #37393" begin
    @test remove_linenums!(:(for outer i = 1:3; end)) == Expr(:for, Expr(:(=), Expr(:outer, :i), :(1:3)), :(;;))
    i = :i
    @test remove_linenums!(:(for outer $i = 1:3; end)) == Expr(:for, Expr(:(=), Expr(:outer, :i), :(1:3)), :(;;))
    @test remove_linenums!(:(for outer = 1:3; end)) == Expr(:for, Expr(:(=), :outer, :(1:3)), :(;;))
    # TIL that this is possible
    for outer $ i = 1:3
        @test 1 $ 2 in 1:3
    end

    # 😭
    @test Meta.isexpr(Meta.parse("""
        [i for i
        in 1:3]"""), :comprehension)
    @test Meta.isexpr(Meta.parse("""
        [i for outer
        in 1:3]"""), :comprehension)
    @test Meta.isexpr(Meta.parse("""
        [i for outer
        i in 1:3]"""), :comprehension)
    @test Meta.isexpr(Meta.parse("""
        f(i for i
        in 1:3)""").args[2], :generator)
    @test_parseerror """
        for i
            in 1:3
        end"""
end

# PR #37973
@test Meta.parse("1¦2⌿3") == Expr(:call, :¦, 1, Expr(:call, :⌿, 2, 3))

@testset "slurp in assignments" begin
    res = begin x, y, z... = 1:7 end
    @test res == 1:7
    @test x == 1 && y == 2
    @test z == Vector(3:7)

    res = begin x, y, z... = [1, 2] end
    @test res == [1, 2]
    @test x == 1 && y == 2
    @test z == Int[]

    x = 1
    res = begin x..., = x end
    @test res == 1
    @test x == 1

    x, y, z... = 1:7
    res = begin y, z, x... = z..., x, y end
    @test res == ((3:7)..., 1, 2)
    @test y == 3
    @test z == 4
    @test x == ((5:7)..., 1, 2)

    res = begin x, _, y... = 1, 2 end
    @test res == (1, 2)
    @test x == 1
    @test y == ()

    res = begin x, y... = 1 end
    @test res == 1
    @test x == 1
    @test y == Iterators.rest(1, nothing)

    res = begin x, y, z... = 1, 2, 3:5 end
    @test res == (1, 2, 3:5)
    @test x == 1 && y == 2
    @test z == (3:5,)

    @test Meta.isexpr(Meta.@lower(begin a, b..., c... = 1, 2, 3 end), :error)

    @test_throws BoundsError begin x, y, z... = 1:1 end
    @test_throws BoundsError begin x, y, _, z... = 1, 2 end

    car((a, d...)) = a
    cdr((a, d...)) = d
    @test car(1:3) == 1
    @test cdr(1:3) == [2, 3]

    @test begin a, b = (;c = 3, d = 4) end === (c = 3, d = 4)
    @test begin a, b, c = (x = "", y = 2.0, z = 1) end === (x = "", y = 2.0, z = 1)
    a, b, c = (x = "", y = 2.0, z = 1)
    @test a === ""
    @test b === 2.0
    @test c === 1
    @test begin a, b... = (x = "", y = 2.0, z = 1) end === (x = "", y = 2.0, z = 1)
    a, b... = (x = "", y = 2.0, z = 1)
    @test b === (y = 2.0, z = 1)
    let t = (x = "", y = 1, z = 3.0)
        _, a, b = t
        @test a === 1
        @test b === 3.0
        a, b... = t
        @test a === ""
        @test b === (y = 1, z = 3.0)
    end
end

@testset "issue #33460" begin
    err = Expr(:error, "more than one semicolon in argument list")
    @test Meta.lower(Main, :(f(a; b=1; c=2) = 2))  == err
    @test Meta.lower(Main, :(f( ; b=1; c=2)))      == err
    @test Meta.lower(Main, :(f(a; b=1; c=2)))      == err
    @test Meta.lower(Main, :(f(a; b=1, c=2; d=3))) == err
    @test Meta.lower(Main, :(f(a; b=1; c=2, d=3))) == err
    @test Meta.lower(Main, :(f(a; b=1; c=2; d=3))) == err
end

@test eval(Expr(:if, Expr(:block, Expr(:&&, true, Expr(:call, :(===), 1, 1))), 1, 2)) == 1

# issue #38386
macro m38386()
    fname = :f38386
    :(function $(esc(fname)) end)
end
@m38386
@test isempty(methods(f38386))

@testset "non-lhs all-underscore vars should fail in lowering" begin
    # OK
    @test (_ = 1) === 1
    @test ((_, _) = (1, 2)) == (1, 2)
    @test Meta.isexpr(Meta.lower(Main, :(for _ in 1:2; 1; end)), :thunk)
    @test (try; throw(1); catch _; 2; end) === 2
    @test (let _ = 1; 2; end) === 2
    # ERROR: syntax: all-underscore identifiers are write-only and their values cannot be used in expressions
    @test Meta.isexpr(Meta.lower(Main, :(_ = 1; a = _)), :error)
    @test Meta.isexpr(Meta.lower(Main, :(let; function f(); _; end; end)), :error)
    @test Meta.isexpr(Meta.lower(Main, :(let; function f(); _; 1; end; end)), :error)
    @test Meta.isexpr(Meta.lower(Main, :(begin; _; 1; end)), :error)
end

@testset "all-underscore varargs on the rhs" begin
    @test ncalls_in_lowered(quote _..., = a end, GlobalRef(Base, :rest)) == 0
    @test ncalls_in_lowered(quote ___..., = a end, GlobalRef(Base, :rest)) == 0
    @test ncalls_in_lowered(quote a, _... = b end, GlobalRef(Base, :rest)) == 0
    @test ncalls_in_lowered(quote a, _... = b, c end, GlobalRef(Base, :rest)) == 0
    @test ncalls_in_lowered(quote a, _... = (b...,) end, GlobalRef(Base, :rest)) == 0
end

# issue #38501
@test :"a $b $("str") c" == Expr(:string, "a ", :b, " ", Expr(:string, "str"), " c")

@testset "property destructuring" begin
    res = begin (; num, den) = 1 // 2 end
    @test res == 1 // 2
    @test num == 1
    @test den == 2

    res = begin (; b, a) = (a=1, b=2, c=3) end
    @test res == (a=1, b=2, c=3)
    @test b == 2
    @test a == 1

    # could make this an error instead, but I think this is reasonable
    res = begin (; a, b, a) = (a=5, b=6) end
    @test res == (a=5, b=6)
    @test a == 5
    @test b == 6

    @test_throws FieldError (; a, b) = (x=1,)

    @test Meta.isexpr(Meta.@lower(begin (a, b; c) = x end), :error)
    @test Meta.isexpr(Meta.@lower(begin (a, b; c) = x, y end), :error)
    @test Meta.isexpr(Meta.@lower(begin (; c, a.b) = x end), :error)

    f((; a, b)) = a, b
    @test f((b=3, a=4)) == (4, 3)
    @test f((b=3, c=2, a=4)) == (4, 3)
    @test_throws FieldError f((;))

    # with type annotation
    let num, den, a, b
        res = begin (; num::UInt8, den::Float64) = 1 // 2 end
        @test res === 1 // 2
        @test num === 0x01
        @test den === 2.0

        res = begin (; b, a::Bool) = (a=1.0, b=2, c=0x03) end
        @test res === (a=1.0, b=2, c=0x03)
        @test b === 2
        @test a === true
    end

    @test Meta.isexpr(Meta.@lower(f((; a, b::Int)) = a + b), :error)
end

# #33697
@testset "N-dimensional concatenation" begin
    @test :([1 2 5; 3 4 6;;; 0 9 3; 4 5 4]) ==
        Expr(:ncat, 3, Expr(:nrow, 1, Expr(:row, 1, 2, 5), Expr(:row, 3, 4, 6)),
                        Expr(:nrow, 1, Expr(:row, 0, 9, 3), Expr(:row, 4, 5, 4)))
    @test :([1 ; 2 ;; 3 ; 4]) == Expr(:ncat, 2, Expr(:nrow, 1, 1, 2), Expr(:nrow, 1, 3, 4))

    @test_parseerror "[1 2 ;; 3 4]" # cannot mix spaces and ;; except as line break
    @test :([1 2 ;;
            3 4]) == :([1 2 3 4])
    @test :([1 2 ;;
            3 4 ; 2 3 4 5]) == :([1 2 3 4 ; 2 3 4 5])

    @test Meta.parse("[1;\n]") == :([1;]) # ensure line breaks following semicolons are treated correctly
    @test Meta.parse("[1;\n\n]") == :([1;])
    @test Meta.parse("[1\n;]") == :([1;]) # semicolons following a linebreak are fine
    @test Meta.parse("[1\n;;; 2]") == :([1;;; 2])
    @test_parseerror "[1;\n;2]" # semicolons cannot straddle a line break
    @test_parseerror "[1; ;2]" # semicolons cannot be separated by a space
end

# issue #25652
x25652 = 1
x25652_2 = let (x25652, _) = (x25652, nothing)
    x25652 = x25652 + 1
    x25652
end
@test x25652_2 == 2
@test x25652 == 1

@test let x = x25652
    x25652 = x+3
    x25652
end == 4
@test let (x,) = (x25652,)
    x25652 = x+3
    x25652
end == 4

@testset "issue #39600" begin
    A = 1:.5:2
    @test (!).(1 .< A .< 2) == [true, false, true]
    @test .!(1 .< A .< 2) == [true, false, true]
    @test (.!)(1 .< A .< 2) == [true, false, true]

    @test ncalls_in_lowered(:((!).(1 .< A .< 2)), GlobalRef(Base, :materialize)) == 1
    @test ncalls_in_lowered(:(.!(1 .< A .< 2)), GlobalRef(Base, :materialize)) == 1
    @test ncalls_in_lowered(:((.!)(1 .< A .< 2)), GlobalRef(Base, :materialize)) == 1
end

# issue #39705
@eval f39705(x) = $(Expr(:||)) && x
@test f39705(1) === false


struct A x end
Base.dotgetproperty(::A, ::Symbol) = [0, 0, 0]

@testset "dotgetproperty" begin
    a = (x = [1, 2, 3],)
    @test @inferred((a -> a.x .+= 1)(a)) == [2, 3, 4]

    b = [1, 2, 3]
    @test A(b).x === b
    @test begin A(b).x .= 1 end == [1, 1, 1]
    @test begin A(b).x .+= 1 end == [2, 3, 4]
    @test b == [1, 2, 3]
end

@test Meta.@lower((::T) = x) == Expr(:error, "invalid assignment location \"::T\"")
@test Meta.@lower((::T,) = x) == Expr(:error, "invalid assignment location \"::T\"")
@test Meta.@lower((; ::T) = x) == Expr(:error, "invalid assignment location \"::T\"")

# flisp conversion for quoted SSAValues
@test eval(:(x = $(QuoteNode(Core.SSAValue(1))))) == Core.SSAValue(1)
@test eval(:(x = $(QuoteNode(Core.SlotNumber(1))))) == Core.SlotNumber(1)
@test_throws ErrorException("syntax: SSAValue objects should not occur in an AST") eval(:(x = $(Core.SSAValue(1))))
@test_throws ErrorException("syntax: SlotNumber objects should not occur in an AST") eval(:(x = $(Core.SlotNumber(1))))

# juxtaposition of radical symbols (#40094)
@test Meta.parse("2√3") == Expr(:call, :*, 2, Expr(:call, :√, 3))
@test Meta.parse("2∛3") == Expr(:call, :*, 2, Expr(:call, :∛, 3))
@test Meta.parse("2∜3") == Expr(:call, :*, 2, Expr(:call, :∜, 3))

macro m_underscore_hygiene()
    return :(_ = 1)
end

@test Meta.@lower(@m_underscore_hygiene()) === 1

macro m_begin_hygiene(a)
    return :($(esc(a))[begin])
end

@test @m_begin_hygiene([1, 2, 3]) === 1

# issue 40258
@test "a $("b $("c")")" == "a b c"

@test "$(([[:a, :b], [:c, :d]]...)...)" == "abcd"

@test eval(Expr(:string, "a", Expr(:string, "b", "c"))) == "abc"
@test eval(Expr(:string, "a", Expr(:string, "b", Expr(:string, "c")))) == "abc"

macro m_nospecialize_unnamed_hygiene()
    return :(f(@nospecialize(::Any)) = Any)
end

@test @m_nospecialize_unnamed_hygiene()(1) === Any

# https://github.com/JuliaLang/julia/issues/40574
@testset "no mutation while destructuring" begin
    x = [1, 2]
    x[2], x[1] = x
    @test x == [2, 1]

    x = [1, 2, 3]
    x[3], x[1:2]... = x
    @test x == [2, 3, 1]
end

@testset "escaping newlines inside strings" begin
    c = "c"

    @test "a\
b" == "ab"
    @test "a\
    b" == "ab"
    @test raw"a\
b" == "a\\\nb"
    @test "a$c\
b" == "acb"
    @test "\\
" == "\\\n"


    @test """
          a\
          b""" == "ab"
    @test """
          a\
            b""" == "ab"
    @test """
            a\
          b""" == "ab"
    @test raw"""
          a\
          b""" == "a\\\nb"
    @test """
          a$c\
          b""" == "acb"

    @test """
          \
          """ == ""
    @test """
          \\
          """ == "\\\n"
    @test """
          \\\
          """ == "\\"
    @test """
          \\\\
          """ == "\\\\\n"
    @test """
          \\\\\
          """ == "\\\\"
    @test """
          \
          \
          """ == ""
    @test """
          \\
          \
          """ == "\\\n"
    @test """
          \\\
          \
          """ == "\\"


    @test `a\
b` == `ab`
    @test `a\
    b` == `ab`
    @test `a$c\
b` == `acb`
    @test `"a\
b"` == `ab`
    @test `'a\
b'` == `$("a\\\nb")`
    @test `\\
` == `'\'`


    @test ```
          a\
          b``` == `ab`
    @test ```
          a\
            b``` == `ab`
    @test ```
            a\
          b``` == `  ab`
    @test ```
          a$c\
          b``` == `acb`
    @test ```
          "a\
          b"``` == `ab`
    @test ```
          'a\
          b'``` == `$("a\\\nb")`
    @test ```
          \\
          ``` == `'\'`
end

# issue #41253
@test (function (::Dict{}); end)(Dict()) === nothing

@testset "issue #41330" begin
    @test Meta.parse("\"a\\\r\nb\"") == "ab"
    @test Meta.parse("\"a\\\rb\"") == "ab"
    @test eval(Meta.parse("`a\\\r\nb`")) == `ab`
    @test eval(Meta.parse("`a\\\rb`")) == `ab`
end

@testset "slurping into function def" begin
    x, f1()... = [1, 2, 3]
    @test x == 1
    @test f1() == [2, 3]
    # test that call to `Base.rest` is outside the definition of `f`
    @test f1() === f1()

    x, f2()... = 1, 2, 3
    @test x == 1
    @test f2() == (2, 3)
end

@testset "long function bodies" begin
    ex = Expr(:block)
    ex.args = fill!(Vector{Any}(undef, 700000), 1)
    f = eval(Expr(:function, :(), ex))
    @Core.latestworld
    @test f() == 1
    ex = Expr(:vcat)
    ex.args = fill!(Vector{Any}(undef, 600000), 1)
    @test_throws ErrorException("syntax: expression too large") eval(ex)
end

# issue 25678
@generated g25678(x) = return :x
@test g25678(7) === 7

# issue 25678: module of name `Core`
# https://github.com/JuliaLang/julia/pull/40778/files#r784416018
@test @eval Module() begin
    Core = 1
    @generated f() = 1
    f() == 1
end

# issue 25678: argument of name `tmp`
# https://github.com/JuliaLang/julia/pull/43823#discussion_r785365312
@test @eval Module() begin
    @generated f(tmp) = tmp
    f(1) === Int
end

# issue #19012
@test Meta.parse("\U2200", raise=false) === Symbol("∀")
@test Meta.parse("\U2203", raise=false) === Symbol("∃")
@test Meta.parse("a\U2203", raise=false) === Symbol("a∃")
@test Meta.parse("\U2204", raise=false) === Symbol("∄")

# issue 42220
macro m42220()
    return quote
        function foo(::Type{T}=Float64) where {T}
            return Vector{T}(undef, 10)
        end
    end
end
@test @m42220()() isa Vector{Float64}
@test @m42220()(Bool) isa Vector{Bool}

@testset "try else" begin
    fails(f) = try f() catch; true else false end
    @test fails(error)
    @test !fails(() -> 1 + 2)

    @test_parseerror "try foo() else bar() end"
    @test_parseerror "try foo() else bar() catch; baz() end"
    @test_parseerror "try foo() catch; baz() finally foobar() else bar() end"
    @test_parseerror "try foo() finally foobar() else bar() catch; baz() end"

    err = try
        try
            1 + 2
        catch
        else
            error("foo")
        end
    catch e
        e
    end
    @test err == ErrorException("foo")

    x = 0
    err = try
        try
            1 + 2
        catch
        else
            error("foo")
        finally
            x += 1
        end
    catch e
        e
    end
    @test err == ErrorException("foo")
    @test x == 1

    x = 0
    err = try
        try
            1 + 2
        catch
            5 + 6
        else
            3 + 4
        finally
            x += 1
        end
    catch e
        e
    end
    @test err == 3 + 4
    @test x == 1

    x = 0
    err = try
        try
            error()
        catch
            5 + 6
        else
            3 + 4
        finally
            x += 1
        end
    catch e
        e
    end
    @test err == 5 + 6
    @test x == 1

    x = 0
    try
    catch
    else
        x = 1
    end
    @test x == 1

    try
    catch
    else
        tryelse_in_local_scope = true
    end

    @test !@isdefined(tryelse_in_local_scope)
end

@test_parseerror """
function checkUserAccess(u::User)
	if u.accessLevel != "user\u202e \u2066# users are not allowed\u2069\u2066"
		return true
	end
	return false
end
"""

@test_parseerror """
function checkUserAccess(u::User)
	#=\u202e \u2066if (u.isAdmin)\u2069 \u2066 begin admins only =#
		return true
	#= end admin only \u202e \u2066end\u2069 \u2066=#
	return false
end
"""

@testset "empty nd arrays" begin
    @test :([])    == Expr(:vect)
    @test :([;])   == Expr(:ncat, 1)
    @test :([;;])  == Expr(:ncat, 2)
    @test :([;;;]) == Expr(:ncat, 3)

    @test []    == Array{Any}(undef, 0)
    @test [;]   == Array{Any}(undef, 0)
    @test [;;]  == Array{Any}(undef, 0, 0)
    @test [;;;] == Array{Any}(undef, 0, 0, 0)

    @test :(T[])    == Expr(:ref, :T)
    @test :(T[;])   == Expr(:typed_ncat, :T, 1)
    @test :(T[;;])  == Expr(:typed_ncat, :T, 2)
    @test :(T[;;;]) == Expr(:typed_ncat, :T, 3)

    @test Int[]    == Array{Int}(undef, 0)
    @test Int[;]   == Array{Int}(undef, 0)
    @test Int[;;]  == Array{Int}(undef, 0, 0)
    @test Int[;;;] == Array{Int}(undef, 0, 0, 0)

    @test :([  ]) == Expr(:vect)
    @test :([
            ]) == Expr(:vect)
    @test :([ ;; ]) == Expr(:ncat, 2)
    @test :([
             ;;
            ]) == Expr(:ncat, 2)

    @test_parseerror "[; ;]"
    @test_parseerror "[;; ;]"
    @test_parseerror "[;\n;]"
end

@test Meta.parseatom("@foo", 1; filename="foo", lineno=7) == (Expr(:macrocall, :var"@foo", LineNumberNode(7, :foo)), 5)
@test Meta.parseall("@foo"; filename="foo", lineno=3) == Expr(:toplevel, LineNumberNode(3, :foo), Expr(:macrocall, :var"@foo", LineNumberNode(3, :foo)))

module M43993
function foo43993 end
const typeof = error
end
let ex = :(const $(esc(:x)) = 1; (::typeof($(esc(:foo43993))))() = $(esc(:x)))
    Core.eval(M43993, Expr(:var"hygienic-scope", ex, Core))
    @Core.latestworld
    @test M43993.x === 1
    @test invokelatest(M43993.foo43993) === 1
end

struct Foo44013
    x
    f
end

@testset "issue #44013" begin
    f = Foo44013(1, 2)
    res = begin (; x, f) = f end
    @test res == Foo44013(1, 2)
    @test x == 1
    @test f == 2

    f = Foo44013(1, 2)
    res = begin (; f, x) = f end
    @test res == Foo44013(1, 2)
    @test x == 1
    @test f == 2
end

@testset "typed globals" begin
    m = Module()
    @eval m begin
        x::Int = 1
        f(y) = x + y
    end
    @test Base.return_types(m.f, (Int,)) == [Int]

    m = Module()
    @eval m begin
        global x::Int
        f(y) = x + y
    end
    @test Base.return_types(m.f, (Int,)) == [Int]

    m = Module()
    @test_throws ErrorException @eval m begin
        function f()
            global x
            x::Int = 1
            x = 2.
        end
        g() = x
    end

    m = Module()
    @test_throws ErrorException @eval m function f()
        global x
        x::Int = 1
        x::Float64 = 2.
    end

    m = Module()
    @test_throws ErrorException @eval m begin
        x::Int = 1
        x::Float64 = 2
    end

    m = Module()
    @test_throws ErrorException @eval m begin
        x::Int = 1
        const x = 2
    end

    m = Module()
    @test_throws ErrorException @eval m begin
        const x = 1
        x::Int = 2
    end

    m = Module()
    @test_throws ErrorException @eval m begin
        x = 1
        global x::Float64
    end

    m = Module()
    @test_throws ErrorException @eval m begin
        x = 1
        global x::Int
    end

    m = Module()
    @eval m module Foo
        export bar
        bar = 1
    end
    @eval m begin
        using .Foo
        bar::Float64 = 2
    end
    @test m.bar === 2.0
    @test Core.get_binding_type(m, :bar) == Float64
    @test m.Foo.bar === 1
    @test Core.get_binding_type(m.Foo, :bar) == Any
end

# issue 44723
demo44723()::Any = Base.Experimental.@opaque () -> true ? 1 : 2
@test demo44723()() == 1

@testset "slurping in non-final position" begin
    res = begin x, y..., z = 1:7 end
    @test res == 1:7
    @test x == 1
    @test y == Vector(2:6)
    @test z == 7

    res = begin x, y..., z = [1, 2] end
    @test res == [1, 2]
    @test x == 1
    @test y == Int[]
    @test z == 2

    x, y, z... = 1:7
    res = begin y, z..., x = z..., x, y end
    @test res == ((3:7)..., 1, 2)
    @test y == 3
    @test z == ((4:7)..., 1)
    @test x == 2

    res = begin x, _..., y = 1, 2 end
    @test res == (1, 2)
    @test x == 1
    @test y == 2

    res = begin x, y..., z = 1, 2:4, 5 end
    @test res == (1, 2:4, 5)
    @test x == 1
    @test y == (2:4,)
    @test z == 5

    @test_throws ArgumentError begin x, y..., z = 1:1 end
    @test_throws BoundsError begin x, y, _..., z = 1, 2 end

    last((a..., b)) = b
    front((a..., b)) = a
    @test last(1:3) == 3
    @test front(1:3) == [1, 2]

    res = begin x, y..., z = "abcde" end
    @test res == "abcde"
    @test x == 'a'
    @test y == "bcd"
    @test z == 'e'

    res = begin x, y..., z = (a=1, b=2, c=3, d=4) end
    @test res == (a=1, b=2, c=3, d=4)
    @test x == 1
    @test y == (b=2, c=3)
    @test z == 4

    v = rand(Bool, 7)
    res = begin x, y..., z = v end
    @test res === v
    @test x == v[1]
    @test y == v[2:6]
    @test z == v[end]

    res = begin x, y..., z = Core.svec(1, 2, 3, 4) end
    @test res == Core.svec(1, 2, 3, 4)
    @test x == 1
    @test y == Core.svec(2, 3)
    @test z == 4
end

# rewriting inner constructors with return type decls
struct InnerCtorRT{T}
    InnerCtorRT()::Int = new{Int}()
    InnerCtorRT{T}() where {T} = ()->new()
end
@test_throws MethodError InnerCtorRT()
@test InnerCtorRT{Int}()() isa InnerCtorRT{Int}

# issue #45162
f45162(f) = f(x=1)
@test first(methods(f45162)).called != 0

# issue #45024
@test_parseerror "const x" "expected assignment after \"const\""
@test_parseerror "const x::Int" "expected assignment after \"const\""
# these cases have always been caught during lowering, since (const (global x)) is not
# ambiguous with the lowered form (const x), but that could probably be changed.
@test Meta.lower(@__MODULE__, Expr(:const, Expr(:global, :x))) == Expr(:error, "expected assignment after \"const\"")
@test Meta.lower(@__MODULE__, Expr(:const, Expr(:global, Expr(:(::), :x, :Int)))) == Expr(:error, "expected assignment after \"const\"")

@testset "issue 25072" begin
    @test '\xc0\x80' == reinterpret(Char, 0xc0800000)
    @test '\x80' == reinterpret(Char, 0x80000000)
    @test '\xff' == reinterpret(Char, 0xff000000)
    @test_parseerror "'\\xff\\xff\\xff\\xff'" "character literal contains multiple characters" # == reinterpret(Char, 0xffffffff)
    @test '\uffff' == Char(0xffff)
    @test '\U00002014' == Char(0x2014)
    @test '\100' == reinterpret(Char, UInt32(0o100) << 24)
    @test_parseerror "'\\100\\42'" "character literal contains multiple characters" # == reinterpret(Char, (UInt32(0o100) << 24) | (UInt32(0o42) << 16))
    @test_parseerror "''" "invalid empty character literal"
    @test_parseerror "'\\xff\\xff\\xff\\xff\\xff'" "character literal contains multiple characters"
    @test_parseerror "'abcd'" "character literal contains multiple characters"
    @test_parseerror "'\\uff\\xff'" "character literal contains multiple characters"
    @test_parseerror "'\\xff\\uff'" "character literal contains multiple characters"
    @test_parseerror "'\\xffa'" "character literal contains multiple characters"
    @test_parseerror "'\\uffffa'" "character literal contains multiple characters"
    @test_parseerror "'\\U00002014a'" "character literal contains multiple characters"
    @test_parseerror "'\\1000'" "character literal contains multiple characters"
    @test Meta.isexpr(Meta.parse("'a"), :incomplete)
    @test ''' == "'"[1]
end

# issue #46251
@test begin; global value = 1; (value, value += 1) end == (1, 2)
@test begin; global value = 1; "($(value), $(value += 1))" end == "(1, 2)"

# issue #47410
# note `eval` is needed since this needs to be at the top level
@test eval(:(if false
             elseif false || (()->true)()
                 42
             end)) == 42

macro _macroexpand(x, m=__module__)
    :($__source__; macroexpand($m, Expr(:var"hygienic-scope", $(esc(Expr(:quote, x))), $m)))
end

@testset "unescaping in :global expressions" begin
    m = @__MODULE__
    @test @_macroexpand(global x::T) == :(global x::$(GlobalRef(m, :T)))
    @test @_macroexpand(global (x, $(esc(:y)))) == :(global (x, y))
    @test @_macroexpand(global (x::S, $(esc(:y))::$(esc(:T)))) ==
        :(global (x::$(GlobalRef(m, :S)), y::T))
    @test @_macroexpand(global (; x, $(esc(:y)))) == :(global (; x, y))
    @test @_macroexpand(global (; x::S, $(esc(:y))::$(esc(:T)))) ==
        :(global (; x::$(GlobalRef(m, :S)), y::T))

    @test @_macroexpand(global x::T = a) == :(global x::$(GlobalRef(m, :T)) = $(GlobalRef(m, :a)))
    @test @_macroexpand(global (x, $(esc(:y))) = a) == :(global (x, y) = $(GlobalRef(m, :a)))
    @test @_macroexpand(global (x::S, $(esc(:y))::$(esc(:T))) = a) ==
        :(global (x::$(GlobalRef(m, :S)), y::T) = $(GlobalRef(m, :a)))
    @test @_macroexpand(global (; x, $(esc(:y))) = a) == :(global (; x, y) = $(GlobalRef(m, :a)))
    @test @_macroexpand(global (; x::S, $(esc(:y))::$(esc(:T))) = a) ==
        :(global (; x::$(GlobalRef(m, :S)), y::T) = $(GlobalRef(m, :a)))
end

# issue #49920
let line1 = (quote end).args[1],
    line2 = (quote end).args[1],
    line3 = (quote end).args[1]
    @test 1 === eval(Meta.lower(Main, Expr(:block, line1, 1, line2, line3)))
end

# issue #49984
macro z49984(s); :(let a; $(esc(s)); end); end
@test let a = 1; @z49984(a) === 1; end

# issues #37783, #39929, #42552, #43379, and #48332
let x = 1 => 2
    @test_throws ErrorException @eval a => b = 2
    @test_throws "function Base.=> must be explicitly imported to be extended" @eval a => b = 2
end

# Splatting in non-final default value (Ref #50518)
for expr in (quote
    function g1(a=(1,2)..., b...=3)
        b
    end
end,quote
    function g2(a=(1,2)..., b=3, c=4)
        (b, c)
    end
end,quote
    function g3(a=(1,2)..., b=3, c...=4)
        (b, c)
    end
end)
    let exc = try eval(expr); catch exc; exc end
        @test isa(exc, ErrorException)
        @test startswith(exc.msg, "syntax: invalid \"...\" in non-final positional argument default value")
    end
end

# Test that bad lowering does not segfault (ref #50518)
@test_throws ErrorException("syntax: Attempted to use slot marked unused") @eval function funused50518(::Float64)
    $(Symbol("#unused#"))
end

@testset "public keyword" begin
    p(str) = Base.remove_linenums!(Meta.parse(str))
    # tests ported from JuliaSyntax.jl
    @test p("function f(public)\n    public + 3\nend") == Expr(:function, Expr(:call, :f, :public), Expr(:block, Expr(:call, :+, :public, 3)))
    @test p("public A, B") == Expr(:public, :A, :B)
    @test p("if true \n public *= 4 \n end") == Expr(:if, true, Expr(:block, Expr(:*=, :public, 4)))
    @test p("module Mod\n public A, B \n end") == Expr(:module, true, :Mod, Expr(:block, Expr(:public, :A, :B)))
    @test p("module Mod2\n a = 3; b = 6; public a, b\n end") == Expr(:module, true, :Mod2, Expr(:block, Expr(:(=), :a, 3), Expr(:(=), :b, 6), Expr(:public, :a, :b)))
    @test p("a = 3; b = 6; public a, b") == Expr(:toplevel, Expr(:(=), :a, 3), Expr(:(=), :b, 6), Expr(:public, :a, :b))
    @test_throws Meta.ParseError p("begin \n public A, B \n end")
    @test_throws Meta.ParseError p("if true \n public A, B \n end")
    @test_throws Meta.ParseError p("public export=true foo, bar")
    @test_throws Meta.ParseError p("public experimental=true foo, bar")
    @test p("public(x::String) = false") == Expr(:(=), Expr(:call, :public, Expr(:(::), :x, :String)), Expr(:block, false))
    @test p("module M; export @a; end") == Expr(:module, true, :M, Expr(:block, Expr(:export, :var"@a")))
    @test p("module M; public @a; end") == Expr(:module, true, :M, Expr(:block, Expr(:public, :var"@a")))
    @test p("module M; export ⤈; end") == Expr(:module, true, :M, Expr(:block, Expr(:export, :⤈)))
    @test p("module M; public ⤈; end") == Expr(:module, true, :M, Expr(:block, Expr(:public, :⤈)))
    @test p("public = 4") == Expr(:(=), :public, 4)
    @test p("public[7] = 5") == Expr(:(=), Expr(:ref, :public, 7), 5)
    @test p("public() = 6") == Expr(:(=), Expr(:call, :public), Expr(:block, 6))
end

@testset "removing argument sideeffects" begin
    # Allow let blocks in broadcasted LHSes, but only evaluate them once:
    execs = 0
    array = [1]
    let x = array; execs += 1; x; end .+= 2
    @test array == [3]
    @test execs == 1
    let; execs += 1; array; end .= 4
    @test array == [4]
    @test execs == 2
    let x = array; execs += 1; x; end::Vector{Int} .+= 2
    @test array == [6]
    @test execs == 3
    let; execs += 1; array; end::Vector{Int} .= 7
    @test array == [7]
    @test execs == 4
end

# Allow GlobalRefs in macro definition
module MyMacroModule
    macro mymacro end
end
macro MyMacroModule.mymacro()
    1
end
@eval macro $(GlobalRef(MyMacroModule, :mymacro))(x)
    2
end
@test (@MyMacroModule.mymacro) == 1
@test (@MyMacroModule.mymacro(a)) == 2

# Issue #53673 - missing macro hygiene for for/generator
baremodule MacroHygieneFor
    import ..Base
    using Base: esc, Expr, +
    macro for1()
        :(let a=(for i=10; end; 1); a; end)
    end
    macro for2()
        :(let b=(for j=11, k=12; end; 2); b; end)
    end
    macro for3()
        :(let c=($(Expr(:for, esc(Expr(:block, :(j=11), :(k=12))), :())); 3); c; end)
    end
    macro for4()
        :(begin; local j; let a=(for outer j=10; end; 4); j+a; end; end)
    end
end
let nnames = length(names(MacroHygieneFor; all=true))
    @test (@MacroHygieneFor.for1) == 1
    @test (@MacroHygieneFor.for2) == 2
    @test (@MacroHygieneFor.for3) == 3
    @test (@MacroHygieneFor.for4) == 14
    @test length(names(MacroHygieneFor; all=true)) == nnames
end

baremodule MacroHygieneGenerator
    using ..Base: Any, !
    my!(x) = !x
    macro gen1()
        :(let a=Any[x for x in 1]; a; end)
    end
    macro gen2()
        :(let a=Bool[x for x in (true, false) if my!(x)]; a; end)
    end
    macro gen3()
        :(let a=Bool[x for x in (true, false), y in (true, false) if my!(x) && my!(y)]; a; end)
    end
end
let nnames = length(names(MacroHygieneGenerator; all=true))
    @test (MacroHygieneGenerator.@gen1) == Any[x for x in 1]
    @test (MacroHygieneGenerator.@gen2) == Bool[false]
    @test (MacroHygieneGenerator.@gen3) == Bool[false]
    @test length(names(MacroHygieneGenerator; all=true)) == nnames
end

# Issue #53729 - Lowering recursion into Expr(:toplevel)
@test eval(Expr(:let, Expr(:block), Expr(:block, Expr(:toplevel, :(f53729(x) = x)), :(x=1)))) == 1
@test f53729(2) == 2

# Issue #54701 - Macro hygiene of argument destructuring
macro makef54701()
    quote
        call(f) = f((1, 2))
        function $(esc(:f54701))()
            call() do (a54701, b54701)
                return a54701+b54701
            end
        end
    end
end
@makef54701
@test f54701() == 3
@test !@isdefined(a54701)
@test !@isdefined(b54701)

# Issue #54607 - binding creation in foreign modules should not be permitted
module Foreign54607
    # Syntactic, not dynamic
    try_to_create_binding1() = (Foreign54607.foo = 2)
    # GlobalRef is allowed for same-module assignment and declares the binding
    @eval try_to_create_binding2() = ($(GlobalRef(Foreign54607, :foo2)) = 2)
    function global_create_binding()
        global bar
        bar = 3
    end
    baz = 4
    begin;
        @Base.Experimental.force_compile
        compiled_assign = 5
    end
    @eval $(GlobalRef(Foreign54607, :gr_assign)) = 6
end
@test_throws ErrorException (Foreign54607.foo = 1)
@test_throws ErrorException Foreign54607.try_to_create_binding1()
Foreign54607.try_to_create_binding2()
function assign_in_foreign_module()
    (Foreign54607.foo = 1)
    nothing
end
@test !Core.Compiler.is_nothrow(Base.infer_effects(assign_in_foreign_module))
@test_throws ErrorException begin
    @Base.Experimental.force_compile
    (Foreign54607.foo = 1)
end
@test_throws ErrorException @eval (GlobalRef(Foreign54607, :gr_assign2)) = 7
Foreign54607.global_create_binding()
@test isdefined(Foreign54607, :bar)
@test isdefined(Foreign54607, :baz)
@test isdefined(Foreign54607, :compiled_assign)
@test isdefined(Foreign54607, :gr_assign)
@test isdefined(Foreign54607, :foo2)
Foreign54607.bar = 8
@test Foreign54607.bar == 8
begin
    @Base.Experimental.force_compile
    Foreign54607.bar = 9
end
@test Foreign54607.bar == 9

# Issue #54805 - export mislowering
module Export54805
let
    local b54805=1
    export b54805
end
b54805 = 2
end
using .Export54805
@test b54805 == 2

# F{T} = ... has special syntax semantics, not found anywhere else in the language
# that make `F` `const` iff an assignment to `F` is global in the relevant scope.
# We implicitly test this elsewhere, but there's some tricky interactions with
# explicit declarations that we test here.
module ImplicitCurlies
    using ..Test
    let
        ImplicitCurly1{T} = Ref{T}
    end
    @test !@isdefined(ImplicitCurly1)
    let
        global ImplicitCurly2
        ImplicitCurly2{T} = Ref{T}
    end
    @test @isdefined(ImplicitCurly2) && isconst(@__MODULE__, :ImplicitCurly2)
    begin
        ImplicitCurly3{T} = Ref{T}
    end
    @test @isdefined(ImplicitCurly3) && isconst(@__MODULE__, :ImplicitCurly3)
    begin
        local ImplicitCurly4
        ImplicitCurly4{T} = Ref{T}
    end
    @test !@isdefined(ImplicitCurly4)
    @test_throws "syntax: `global const` declaration not allowed inside function" Core.eval(@__MODULE__, :(function implicit5()
        global ImplicitCurly5
        ImplicitCurly5{T} = Ref{T}
    end))
    @test !@isdefined(ImplicitCurly5)
    function implicit6()
        ImplicitCurly6{T} = Ref{T}
        return ImplicitCurly6
    end
    @test !@isdefined(ImplicitCurly6)
    # Check return value of assignment expr
    @test isa(Core.eval(@__MODULE__, :(const ImplicitCurly7{T} = Ref{T})), UnionAll)
    @test isa(begin; ImplicitCurly8{T} = Ref{T}; end, UnionAll)
end

# `const` does not distribute over assignments
const aconstassign = bconstassign = 2
@test isconst(@__MODULE__, :aconstassign)
@test !isconst(@__MODULE__, :bconstassign)
@test aconstassign == bconstassign

const afunc_constassign() = bfunc_constassign() = 2
@test afunc_constassign()() == 2
@test !@isdefined(bfunc_constassign)

# `const` RHS is regular toplevel scope (not `let`)
const arhs_toplevel = begin
    athis_should_be_a_global = 1
    2
end
@test isconst(@__MODULE__, :arhs_toplevel)
@test !isconst(@__MODULE__, :athis_should_be_a_global)
@test arhs_toplevel == 2
@test athis_should_be_a_global == 1

# `const` is permitted before function assignment for legacy reasons
const fconst_assign() = 1
const (gconst_assign(), hconst_assign()) = (2, 3)
@test (fconst_assign(), gconst_assign(), hconst_assign()) == (1, 2, 3)
@test isconst(@__MODULE__, :fconst_assign)
@test isconst(@__MODULE__, :gconst_assign)
@test isconst(@__MODULE__, :hconst_assign)

# `const` assignment to `_` drops the assignment effect,
# and the conversion, but not the rhs.
struct CantConvert; end
Base.convert(::Type{CantConvert}, x) = error()
# @test splices into a function, where const cannot appear
@test Core.eval(@__MODULE__, :(const _::CantConvert = 1)) == 1
@test !isconst(@__MODULE__, :_)
@test_throws ErrorException("expected") (const _ = error("expected"))

# Issue #54787
const (destruct_const54787...,) = (1,2,3)
@test destruct_const54787 == (1,2,3)
@test isconst(@__MODULE__, :destruct_const54787)
const a54787, b54787, c54787 = destruct_const54787
@test (a54787, b54787, c54787) == (1,2,3)
@test isconst(@__MODULE__, :a54787)
@test isconst(@__MODULE__, :b54787)
@test isconst(@__MODULE__, :c54787)

# Same number of statements on lhs and rhs, but non-atom
const c54787_1,c54787_2 = 1,(2*1)
@test isconst(@__MODULE__, :c54787_1)
@test isconst(@__MODULE__, :c54787_2)
@test c54787_1 == 1
@test c54787_2 == 2

# Methods can be added to any singleton not just generic functions
struct SingletonMaker; end
const no_really_this_is_a_function_i_promise = Val{SingletonMaker()}()
no_really_this_is_a_function_i_promise(a) = 2 + a
@test Val{SingletonMaker()}()(2) == 4

# Test that lowering doesn't accidentally put a `Module` in the Method name slot
let src = @Meta.lower let capture=1
    global foo_lower_block
    foo_lower_block() = capture
end
    code = src.args[1].code
    for i = length(code):-1:1
        expr = code[i]
        Meta.isexpr(expr, :method) || continue
        @test isa(expr.args[1], Union{GlobalRef, Symbol})
    end
end

let src = Meta.@lower let
    try
        try
            return 1
        catch
        end
    finally
        nothing
    end
end
    code = src.args[1].code
    for stmt in code
        if Meta.isexpr(stmt, :leave) && length(stmt.args) > 1
            # Expr(:leave, ...) should list the arguments to pop from
            # inner-most scope to outer-most
            @test issorted(Int[
                (arg::Core.SSAValue).id
                for arg in stmt.args
            ]; rev=true)
        end
    end
end

# Test that globals can be `using`'d even if they are not yet defined
module UndefGlobal54954
    global theglobal54954::Int
end
using .UndefGlobal54954: theglobal54954
@test Core.get_binding_type(@__MODULE__, :theglobal54954) === Int

# Extended isdefined
module ExtendedIsDefined
    using Test
    module Import
        export x2, x3
        x2 = 2
        x3 = 3
        x4 = 4
    end
    const x1 = 1
    using .Import
    import .Import.x4
    @test x2 == 2 # Resolve the binding
    @eval begin
        @test Core.isdefinedglobal(@__MODULE__, :x1)
        @test Core.isdefinedglobal(@__MODULE__, :x2)
        @test Core.isdefinedglobal(@__MODULE__, :x3)
        @test Core.isdefinedglobal(@__MODULE__, :x4)

        @test Core.isdefinedglobal(@__MODULE__, :x1, false)
        @test !Core.isdefinedglobal(@__MODULE__, :x2, false)
        @test !Core.isdefinedglobal(@__MODULE__, :x3, false)
        @test !Core.isdefinedglobal(@__MODULE__, :x4, false)
    end

    @eval begin
        @Base.Experimental.force_compile
        @test Core.isdefinedglobal(@__MODULE__, :x1)
        @test Core.isdefinedglobal(@__MODULE__, :x2)
        @test Core.isdefinedglobal(@__MODULE__, :x3)
        @test Core.isdefinedglobal(@__MODULE__, :x4)

        @test Core.isdefinedglobal(@__MODULE__, :x1, false)
        @test !Core.isdefinedglobal(@__MODULE__, :x2, false)
        @test !Core.isdefinedglobal(@__MODULE__, :x3, false)
        @test !Core.isdefinedglobal(@__MODULE__, :x4, false)
    end
end

# Test importing the same module twice using two different paths
module FooDualImport
end
module BarDualImport
import ..FooDualImport
import ..FooDualImport.FooDualImport
end

# Test trying to define a constant and then importing the same constant
const ImportConstant = 1
module ImportConstantTestModule
    using Test
    const ImportConstant = 1
    import ..ImportConstant
    @test ImportConstant == 1
    @test isconst(@__MODULE__, :ImportConstant)
end

# Test trying to define a constant and then trying to assign to the same value
module AssignConstValueTest
    using Test
    const x = 1
    @test_throws ErrorException @eval x = 1
    @test_throws ErrorException @eval begin
        @Base.Experimental.force_compile
        global x = 1
    end
end
@test isconst(AssignConstValueTest, :x)

# Module Replacement
module ReplacementContainer
    using Test
    module ReplaceMe
        const x = 1
    end
    const Old = ReplaceMe
    @eval module ReplaceMe
        const x = 2
    end
end
@test ReplacementContainer.Old !== ReplacementContainer.ReplaceMe
@test ReplacementContainer.ReplaceMe.x === 2

# Setglobal of previously declared global
module DeclareSetglobal
    using Test
    @test_throws ErrorException setglobal!(@__MODULE__, :DeclareMe, 1)
    global DeclareMe
    setglobal!(@__MODULE__, :DeclareMe, 1)
    @test DeclareMe === 1
end

# Binding type of const (N.B.: This may change in the future)
module ConstBindingType
    using Test
    const x = 1
    @test Core.get_binding_type(@__MODULE__, :x) === Any
end

# Explicit import may resolve using failed
module UsingFailedExplicit
    using Test
    module A; export x; x = 1; end
    module B; export x; x = 2; end
    using .A, .B
    @test_throws UndefVarError x
    using .A: x as x
    @test x === 1
end

# issue #45494
begin
  local b::Tuple{<:Any} = (0,)
  function f45494()
    b = b
    b
  end
end
@test f45494() === (0,)

@test_throws "\"esc(...)\" used outside of macro expansion" eval(esc(:(const x=1)))

# Inner function declaration world age
function create_inner_f_no_methods()
    function inner_f end
end
@test isa(create_inner_f_no_methods(), Function)
@test length(methods(create_inner_f_no_methods())) == 0

function create_inner_f_one_method()
    inner_f() = 1
end
@test isa(create_inner_f_no_methods(), Function)
@test length(methods(create_inner_f_no_methods())) == 0
@test Base.invoke_in_world(first(methods(create_inner_f_one_method)).primary_world, create_inner_f_one_method()) == 1

# Issue 56711 - Scope of signature hoisting
function fs56711()
    f(lhs::Integer) = 1
    f(lhs::Integer, rhs::(local x_should_not_be_defined=Integer; x_should_not_be_defined)) = 2
    return f
end
@test !@isdefined(x_should_not_be_defined)

# Test that importing twice is allowed without warning
@test_nowarn @eval baremodule ImportTwice
    import ..Base
    using .Base: zero, zero
end

# PR# 55040 - Macrocall as function sig
@test :(function @f()() end) == :(function (@f)() end)

function callme end
macro callmemacro(args...)
    Expr(:call, esc(:callme), map(esc, args)...)
end
function @callmemacro(a::Int)
    return 1
end
@callmemacro(b::Float64) = 2
function @callmemacro(a::T, b::T) where T <: Int
    return 3
end
function @callmemacro(a::Int, b::Int, c::Int)::Float64
    return 4
end
function @callmemacro(d::String)
    (a, b, c)
    # ^ Should not be accidentally parsed as an argument list
    return 4
end

@test callme(1) === 1
@test callme(2.0) === 2
@test callme(3, 3) === 3
@test callme(4, 4, 4) === 4.0

# Ambiguous 1-arg anymous vs macrosig
@test_parseerror "function (@foo(a)) end"

# #57267 - Missing `latestworld` after typealias
abstract type A57267{S, T} end
@test_nowarn @eval begin
    B57267{S} = A57267{S, 1}
    const C57267 = B57267
end

# #57404 - Binding ambiguity resolution ignores guard bindings
module Ambig57404
    module A
        export S
    end
    using .A
    module B
        const S = 1
        export S
    end
    using .B
end
@test Ambig57404.S == 1

# Issue #56904 - lambda linearized twice
@test (let; try 3; finally try 1; f(() -> x); catch x; end; end; x = 7; end) === 7
@test (let; try 3; finally try 4; finally try 1; f(() -> x); catch x; end; end; end; x = 7; end) === 7

# Issue #57546 - explicit function declaration should create new global
module FuncDecl57546
    using Test
    @test_nowarn @eval function Any end
    @test isa(Any, Function)
    @test isempty(methods(Any))
end

# #57334
let
    x57334 = Ref(1)
    @test_throws "syntax: cannot declare \"x57334[]\" `const`" Core.eval(@__MODULE__, :(const x57334[] = 1))
end

# #57470
module M57470
using ..Test

@test_throws(
    "syntax: `global const` declaration not allowed inside function",
    Core.eval(@__MODULE__, :(function f57470()
                                 const global x57470 = 1
                             end)))
@test_throws(
    "unsupported `const` declaration on local variable",
    Core.eval(@__MODULE__, :(let
                                 const y57470 = 1
                             end))
)

let
    global a57470
    const a57470 = 1
end
@test a57470 === 1

let
    global const z57470 = 1
    const global w57470 = 1
end

@test z57470 === 1
@test w57470 === 1

const (; field57470_1, field57470_2) = (field57470_1 = 1, field57470_2 = 2)
@test field57470_1 === 1
@test field57470_2 === 2

# TODO: 1.11 allows these, but should we?
const X57470{T}, Y57470{T} = Int, Bool
@test X57470 === Int
@test Y57470 === Bool
const A57470{T}, B57470{T} = [Int, Bool]
@test A57470 === Int
@test B57470 === Bool
const a57470, f57470(x), T57470{U} = [1, 2, Int]
@test a57470 === 1
@test f57470(0) === 2
@test T57470 === Int

module M57470_sub end
@test_throws("syntax: cannot declare \"M57470_sub.x\" `const`",
             Core.eval(@__MODULE__, :(const M57470_sub.x = 1)))

# # `const global` should not trample previously declared `local`
@test_throws(
    "syntax: variable \"v57470\" declared both local and global",
    Core.eval(@__MODULE__, :(let
                                 local v57470
                                 const global v57470 = 1
                             end))
)

# Chain of assignments must happen right-to-left:
let
    x = [0, 0]; i = 1
    i = x[i] = 2
    @test x == [2, 0]
    x = [0, 0]; i = 1
    x[i] = i = 2
    @test x == [0, 2]
end

# Global const decl inside local scope
let
    const global letf_57470(x)::Int = 2+x
    const global letT_57470{T} = Int64
end
@test letf_57470(3) == 5
@test letT_57470 === Int64

end

# #57574
module M57574
struct A{T} end
out = let
    for B in ()
    end
    let
        B{T} = A{T}
        B
    end
end
end
@test M57574.out === M57574.A

# Double import of CONST_IMPORT symbol
module DoubleImport
    import Test: Random
    import Random
end
@test DoubleImport.Random === Test.Random

# Expr(:method) returns the method
let ex = @Meta.lower function return_my_method(); 1; end
    code = ex.args[1].code
    idx = findfirst(ex->Meta.isexpr(ex, :method) && length(ex.args) > 1, code)
    code[end] = Core.ReturnNode(Core.SSAValue(idx))
    @test isa(Core.eval(@__MODULE__, ex), Method)
end
