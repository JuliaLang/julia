using Test

randop() = rand(["-->", "→",
                 "||",
                 "&&",
                 "<", "==", "<:", ">:",
                 "<|", "|>",
                 ":",
                 "+", "-",
                 ">>", "<<",
                 "*", "/",
                 "//",
                 "^", "↑",
                 "::",
                 ".", "->"])

test_expr_broken(str) = test_expr(str, false)

function test_expr(str, show_data = true)
    x, ps = CSTParser.parse(ParseState(str))

    x0 = Expr(x)
    x1 = remlineinfo!(Meta.parse(str))
    if ps.errored || x0 != x1
        if show_data
            println("Mismatch between flisp and CSTParser when parsing string $str")
            println("ParserState:\n $ps\n")
            println("CSTParser Expr:\n $x\n")
            println("Converted CSTParser Expr:\n $x0\n")
            println("Base EXPR:\n $x1\n")
        end
        return false
    end
    return true
end

@testset "All tests" begin
@test Meta.parse("(1,)") == Expr(:tuple, 1)
@testset "Operators" begin
    # @testset "Binary Operators" begin
    #     for iter = 1:25
    #         println(iter)
    #         str = join([["x$(randop())" for i = 1:19];"x"])

    #         @test test_expr(str)
    #     end
    # end
    @testset "Conditional Operator" begin
        strs = ["a ? b : c"
                "a ? b : c : d"
                "a ? b : c : d :e"]
        for str in strs
            @test test_expr(str)
        end
    end


    @testset "Dot Operator" begin
        @test "a.b"  |> test_expr
        @test "a.b.c"  |> test_expr
        @test "(a(b)).c"  |> test_expr
        @test "(a).(b).(c)"  |> test_expr
        @test "(a).b.(c)"  |> test_expr
        @test "(a).b.(c+d)"  |> test_expr
    end

    @testset "Unary Operator" begin
        @test "+" |> test_expr
        @test "-" |> test_expr
        @test "!" |> test_expr
        @test "~" |> test_expr
        @test "&" |> test_expr
        # @test "::" |> test_expr
        @test "<:" |> test_expr
        @test ">:" |> test_expr
        @test "¬" |> test_expr
        @test "√" |> test_expr
        @test "∛" |> test_expr
        @test "∜" |> test_expr
    end

    @testset "Unary Operator" begin
        @test "a=b..." |> test_expr
        @test "a-->b..." |> test_expr
        @test "a&&b..." |> test_expr
        @test "a||b..." |> test_expr
        @test "a<b..." |> test_expr
        @test "a:b..." |> test_expr
        @test "a+b..." |> test_expr
        @test "a<<b..." |> test_expr
        @test "a*b..." |> test_expr
        @test "a//b..." |> test_expr
        @test "a^b..." |> test_expr
        @test "a::b..." |> test_expr
        @test "a where b..." |> test_expr
        @test "a.b..." |> test_expr
    end

    @testset "unary op calls" begin
        @test "+(a,b)" |> test_expr
        @test "-(a,b)" |> test_expr
        @test "!(a,b)" |> test_expr
        @test "¬(a,b)" |> test_expr
        @test "~(a,b)" |> test_expr
        @test_broken "<:(a,b)" |> test_expr_broken
        @test "√(a,b)" |> test_expr
        @test "\$(a,b)" |> test_expr
        @test ":(a,b)" |> test_expr
        @test "&a" |> test_expr
        @test "&(a,b)" |> test_expr
        @test "::a" |> test_expr
        @test "::(a,b)" |> test_expr
    end

    @testset "where precedence" begin
        @test "a = b where c = d" |> test_expr
        @test "a = b where c" |> test_expr
        @test "b where c = d" |> test_expr

        @test "a ? b where c : d" |> test_expr

        @test "a --> b where c --> d" |> test_expr
        @test "a --> b where c" |> test_expr
        @test "b where c --> d" |> test_expr

        @test "a || b where c || d" |> test_expr
        @test "a || b where c" |> test_expr
        @test "b where c || d" |> test_expr

        @test "a && b where c && d" |> test_expr
        @test "a && b where c" |> test_expr
        @test "b where c && d" |> test_expr

        @test "a <: b where c <: d" |> test_expr
        @test "a <: b where c" |> test_expr
        @test "b where c <: d" |> test_expr

        @test "a <| b where c <| d" |> test_expr
        @test "a <| b where c" |> test_expr
        @test "b where c <| d" |> test_expr

        @test "a : b where c : d" |> test_expr
        @test "a : b where c" |> test_expr
        @test "b where c : d" |> test_expr

        @test "a + b where c + d" |> test_expr
        @test "a + b where c" |> test_expr
        @test "b where c + d" |> test_expr

        @test "a << b where c << d" |> test_expr
        @test "a << b where c" |> test_expr
        @test "b where c << d" |> test_expr

        @test "a * b where c * d" |> test_expr
        @test "a * b where c" |> test_expr
        @test "b where c * d" |> test_expr

        @test "a // b where c // d" |> test_expr
        @test "a // b where c" |> test_expr
        @test "b where c // d" |> test_expr

        @test "a ^ b where c ^ d" |> test_expr
        @test "a ^ b where c" |> test_expr
        @test "b where c ^ d" |> test_expr

        @test "a :: b where c :: d" |> test_expr
        @test "a :: b where c" |> test_expr
        @test "b where c :: d" |> test_expr

        @test "a.b where c.d" |> test_expr
        @test "a.b where c" |> test_expr
        @test "b where c.d" |> test_expr

        @test "a where b where c" |> test_expr
    end
end


@testset "Type Annotations" begin
    @testset "Curly" begin
        @test "x{T}" |> test_expr
        @test "x{T,S}" |> test_expr
        @test "a.b{T}" |> test_expr
        @test "a(b){T}" |> test_expr
        @test "(a(b)){T}" |> test_expr
        @test "a{b}{T}" |> test_expr
        @test "a{b}(c){T}" |> test_expr
        @test "a{b}.c{T}" |> test_expr
        @test """x{T,
        S}""" |> test_expr
    end
end

@testset "Tuples" begin
    @static if VERSION > v"1.1-"
        @test CSTParser.parse("1,") isa CSTParser.EXPR{CSTParser.ErrorToken}
    else
        @test "1," |> test_expr
    end
    @test "1,2" |> test_expr
    @test "1,2,3" |> test_expr
    @test "()" |> test_expr
    @test "(==)" |> test_expr
    @test "(1)" |> test_expr
    @test "(1,)" |> test_expr
    @test "(1,2)" |> test_expr
    @test "(a,b,c)" |> test_expr
    @test "(a...)" |> test_expr
    @test "((a,b)...)" |> test_expr
    @test "a,b = c,d" |> test_expr
    @test "(a,b) = (c,d)" |> test_expr
end

@testset "Function Calls" begin
    @testset "Simple Calls" begin
        @test "f(x)" |> test_expr
        @test "f(x,y)" |> test_expr
        @test "f(g(x))" |> test_expr
        @test "f((x,y))" |> test_expr
        @test "f((x,y), z)" |> test_expr
        @test "f(z, (x,y), z)" |> test_expr
        @test "f{a}(x)" |> test_expr
        @test "f{a<:T}(x::T)" |> test_expr
    end

    @testset "Keyword Arguments" begin
        @test "f(x=1)" |> test_expr
        @test "f(x=1,y::Int = 1)" |> test_expr
    end

    @testset "Compact Declaration" begin
        @test "f(x) = x" |> test_expr
        @test "f(x) = g(x)" |> test_expr
        @test "f(x) = (x)" |> test_expr
        @test "f(x) = (x;y)" |> test_expr
        @test "f(g(x)) = x" |> test_expr
        @test "f(g(x)) = h(x)" |> test_expr
    end

    @testset "Standard Declaration" begin
        @test "function f end" |> test_expr
        @test "function f(x) x end" |> test_expr
        @test "function f(x); x; end" |> test_expr
        @test "function f(x) x; end" |> test_expr
        @test "function f(x); x end" |> test_expr
        @test "function f(x) x;y end" |> test_expr
        @test """function f(x)
            x
        end""" |> test_expr
        @test """function f(x,y =1)
            x
        end""" |> test_expr
        @test """function f(x,y =1;z =2)
            x
        end""" |> test_expr
    end
    @testset "Anonymous" begin
        @test "x->y" |> test_expr
        @test "(x,y)->x*y" |> test_expr
        @test """function ()
            return
        end""" |> test_expr
    end
end





@testset "Types" begin
    @testset "Abstract" begin
        @test "abstract type t end" |> test_expr
        @test "abstract type t{T} end" |> test_expr
        @test "abstract type t <: S end" |> test_expr
        @test "abstract type t{T} <: S end" |> test_expr
    end

    @testset "primitive" begin
        @test "primitive type Int 64 end" |> test_expr
        @test "primitive type Int 4*16 end" |> test_expr
    end

    @testset "Structs" begin
        @test "struct a end" |> test_expr
        @test "struct a; end" |> test_expr
        @test "struct a; b;end" |> test_expr
        @test """struct a
            arg1
        end""" |> test_expr
        @test """struct a <: T
            arg1::Int
            arg2::Int
        end""" |> test_expr
        @test """struct a
            arg1::T
        end""" |> test_expr
        @test """struct a{T}
            arg1::T
            a(args) = new(args)
        end""" |> test_expr
        @test """struct a <: Int
            arg1::Vector{Int}
        end""" |> test_expr
        @test """mutable struct a <: Int
            arg1::Vector{Int}
        end""" |> test_expr
    end
end


@testset "Modules" begin
    @testset "Imports " begin
        @test "import ModA" |> test_expr
        @test "import .ModA" |> test_expr
        @test "import ..ModA.a" |> test_expr
        @test "import ModA.subModA" |> test_expr
        @test "import ModA.subModA: a" |> test_expr
        @test "import ModA.subModA: a, b" |> test_expr
        @test "import ModA.subModA: a, b.c" |> test_expr
        @test "import .ModA.subModA: a, b.c" |> test_expr
        @test "import ..ModA.subModA: a, b.c" |> test_expr
    end
    @testset "Export " begin
        @test "export ModA" |> test_expr
        @test "export a, b, c" |> test_expr
    end
end

@testset "Generators" begin
    @test "(y for y in X)" |> test_expr
    @test "((x,y) for x in X, y in Y)" |> test_expr
    @test "(y.x for y in X)" |> test_expr
    @test "((y) for y in X)" |> test_expr
    @test "(y,x for y in X)" |> test_expr
    @test "((y,x) for y in X)" |> test_expr
    @test "[y for y in X]" |> test_expr
    @test "[(y) for y in X]" |> test_expr
    @test "[(y,x) for y in X]" |> test_expr
    @test "Int[y for y in X]" |> test_expr
    @test "Int[(y) for y in X]" |> test_expr
    @test "Int[(y,x) for y in X]" |> test_expr
    @test """
    [a
    for a = 1:2]""" |> test_expr
    @test "[ V[j][i]::T for i=1:length(V[1]), j=1:length(V) ]" |> test_expr
    @test "all(d ≥ 0 for d in B.dims)" |> test_expr
    @test "(arg for x in X)" |> test_expr
    @test "(arg for x in X for y in Y)" |> test_expr
    @test "(arg for x in X for y in Y for z in Z)" |> test_expr
    @test "(arg for x in X if A)" |> test_expr
    @test "(arg for x in X if A for y in Y)" |> test_expr
    @test "(arg for x in X if A for y in Y if B)" |> test_expr
    @test "(arg for x in X if A for y in Y for z in Z)" |> test_expr
    @test "(arg for x in X if A for y in Y if B for z in Z)" |> test_expr
    @test "(arg for x in X if A for y in Y if B for z in Z if C)" |> test_expr
    @test_broken "(arg for x in X, y in Y for z in Z)" |> test_expr_broken
    @test "(arg for x in X, y in Y if A for z in Z)" |> test_expr
end

@testset "Macros " begin
    @test "macro m end" |> test_expr
    @test "macro m() end" |> test_expr
    @test "macro m() a end" |> test_expr
    @test "@mac" |> test_expr
    @test "@mac a b c" |> test_expr
    @test "@mac f(5)" |> test_expr
    @test "(@mac x)" |> test_expr
    @test "Mod.@mac a b c" |> test_expr
    # @test "[@mac a b]" |> test_expr
    @test "@inline get_chunks_id(i::Integer) = _div64(Int(i)-1)+1, _mod64(Int(i) -1)" |> test_expr
    @test "@inline f() = (), ()" |> test_expr
    @test "@sprintf(\"%08d\", id)" |> test_expr
end

@testset "Square " begin
    @testset "vect" begin
        @test "[x]" |> test_expr
        @test "[(1,2)]" |> test_expr
        @test "[x...]" |> test_expr
        @test "[1,2,3,4,5]" |> test_expr
    end

    @testset "ref" begin
        @test "t[i]" |> test_expr
        @test "t[i, j]" |> test_expr
    end

    @testset "vcat" begin
        @test "[x;]" |> test_expr
        @test "[x;y;z]" |> test_expr
        @test """[x
                  y
                  z]""" |> test_expr
        @test """[x
                  y;z]""" |> test_expr
        @test """[x;y
                  z]""" |> test_expr
        @test "[x,y;z]" |> test_expr
    end

    @testset "typed_vcat" begin
        @test "t[x;]" |> test_expr
        @test "t[x;y]" |> test_expr
        @test """t[x
                   y]""" |> test_expr
        @test "t[x;y]" |> test_expr
        @test "t[x y; z]" |> test_expr
        @test "t[x, y; z]" |> test_expr
    end

    @testset "hcat" begin
        @test "[x y]" |> test_expr
    end

    @testset "typed_hcat" begin
        @test "t[x y]" |> test_expr
    end

    @testset "Comprehension" begin
        @test "[i for i = 1:10]" |> test_expr
        @test "Int[i for i = 1:10]" |> test_expr
    end
end


@testset "Keyword Blocks" begin
    @testset "If" begin
        @test "if cond end" |> test_expr
        @test "if cond; a; end" |> test_expr
        @test "if cond a; end" |> test_expr
        @test "if cond; a end" |> test_expr
        @test """if cond
            1
            1
        end""" |> test_expr
        @test """if cond
        else
            2
            2
        end""" |> test_expr
        @test """if cond
            1
            1
        else
            2
            2
        end""" |> test_expr
        @test "if 1<2 end" |> test_expr
        @test """if 1<2
            f(1)
            f(2)
        end""" |> test_expr
        @test """if 1<2
            f(1)
        elseif 1<2
            f(2)
        end""" |> test_expr
        @test """if 1<2
            f(1)
        elseif 1<2
            f(2)
        else
            f(3)
        end""" |> test_expr
        @test "if cond a end" |> test_expr
    end


    @testset "Try" begin
        # @test "try f(1) end" |> test_expr
        # @test "try; f(1) end" |> test_expr
        # @test "try; f(1); end" |> test_expr
        @test "try; f(1); catch e; e; end" |> test_expr
        @test "try; f(1); catch e; e end" |> test_expr
        @test "try; f(1); catch e e; end" |> test_expr
        @test """try
            f(1)
        catch
        end""" |> test_expr
        @test """try
            f(1)
        catch
            error(err)
        end""" |> test_expr
        @test """try
            f(1)
        catch err
            error(err)
        end""" |> test_expr
        @test """try
            f(1)
        catch
            error(err)
        finally
            stop(f)
        end""" |> test_expr
        @test """try
            f(1)
        catch err
            error(err)
        finally
            stop(f)
        end""" |> test_expr
        @test """try
            f(1)
        finally
            stop(f)
        end""" |> test_expr
    end
    @testset "For" begin
        @test """for i = 1:10
            f(i)
        end""" |> test_expr
        @test """for i = 1:10, j = 1:20
            f(i)
        end""" |> test_expr
    end

    @testset "Let" begin
        @test """let x = 1
            f(x)
        end""" |> test_expr
        @test """let x = 1, y = 2
            f(x)
        end""" |> test_expr
        @test """let
            x
        end""" |> test_expr
    end

    @testset "Do" begin
        @test """f(X) do x
            return x
        end""" |> test_expr
        @test """f(X,Y) do x,y
            return x,y
        end""" |> test_expr
    end
end

@testset "Triple-quoted string" begin
    @test CSTParser.parse("\"\"\" \" \"\"\"").val == " \" "
    @test CSTParser.parse("\"\"\"a\"\"\"").val == "a"
    @test CSTParser.parse("\"\"\"\"\"\"").val == ""
    @test CSTParser.parse("\"\"\"\n\t \ta\n\n\t \tb\"\"\"").val == "a\n\nb"
    @test Expr(CSTParser.parse("\"\"\"\ta\n\tb \$c\n\td\n\"\"\"")) == Expr(:string, "\ta\n\tb ", :c, "\n\td\n")
    @test Expr(CSTParser.parse("\"\"\"\n\ta\n\tb \$c\n\td\n\"\"\"")) == Expr(:string, "\ta\n\tb ", :c, "\n\td\n")
    @test Expr(CSTParser.parse("\"\"\"\n\ta\n\tb \$c\n\td\n\t\"\"\"")) == Expr(:string, "a\nb ", :c, "\nd\n")
    @test Expr(CSTParser.parse("\"\"\"\n\t \ta\$(1+\n1)\n\t \tb\"\"\"")) == Expr(:string, "a", :(1+1), "\nb")
    ws = "                         "
    "\"\"\"\n$ws%rv = atomicrmw \$rmw \$lt* %0, \$lt %1 acq_rel\n$(ws)ret \$lt %rv\n$ws\"\"\"" |> test_expr
    ws1 = "        "
    ws2 = "    "
    "\"\"\"\n$(ws1)a\n$(ws1)b\n$(ws2)c\n$(ws2)d\n$(ws2)\"\"\"" |> test_expr
    "\"\"\"\n$(ws1)a\n\n$(ws1)b\n\n$(ws2)c\n\n$(ws2)d\n\n$(ws2)\"\"\"" |> test_expr
    @test "\"\"\"\n$(ws1)α\n$(ws1)β\n$(ws2)γ\n$(ws2)δ\n$(ws2)\"\"\"" |> test_expr
end

@testset "No longer broken things" begin
    @test "[ V[j][i]::T for i=1:length(V[1]), j=1:length(V) ]" |> test_expr
    @test "all(d ≥ 0 for d in B.dims)" |> test_expr
    @test ":(=)" |> test_expr
    @test ":(1)" |> test_expr
    @test ":(a)" |> test_expr
    @test "(@_inline_meta(); f(x))" |> test_expr
    @test "isa(a,b) != c" |> test_expr
    @test "isa(a,a) != isa(a,a)" |> test_expr
    @test "@mac return x" |> test_expr
    @static if VERSION > v"1.1-"
        @test CSTParser.parse("a,b,") isa CSTParser.EXPR{CSTParser.ErrorToken}
    else
        @test "a,b," |> test_expr
    end
    @test "m!=m" |> test_expr
    @test "+(x...)" |> test_expr
    @test "+(promote(x,y)...)" |> test_expr
    @test "\$(x...)" |> test_expr #
    @test "ccall(:gethostname, stdcall, Int32, ())" |> test_expr
    @test "@inbounds @ncall a b c" |> test_expr
    @test "(a+b)``" |> test_expr
    @test "(-, ~)" |> test_expr
    @test """function +(x::Bool, y::T)::promote_type(Bool,T) where T<:AbstractFloat
                return ifelse(x, oneunit(y) + y, y)
            end""" |> test_expr
    @test """finalizer(x,x::GClosure->begin
                    ccall((:g_closure_unref,Gtk.GLib.libgobject),Void,(Ptr{GClosure},),x.handle)
                end)""" |> test_expr
    @test "function \$A end" |> test_expr
    @test "&ctx->exe_ctx_ref" |> test_expr
    @test ":(\$(docstr).\$(TEMP_SYM)[\$(key)])" |> test_expr
    @test "SpecialFunctions.\$(fsym)(n::Dual)" |> test_expr
    @test "(Base.@_pure_meta;)" |> test_expr
    @test "@M a b->(@N c = @O d e f->g)" |> test_expr
    @test "! = f" |> test_expr
    @test "[a=>1, b=>2]" |> test_expr
    @test "a.\$(b)" |> test_expr
    @test "a.\$f()" |> test_expr
    @test "4x/y" |> test_expr
    @test """
    ccall(:jl_finalize_th, Void, (Ptr{Void}, Any,),
                Core.getptls(), o)
    """ |> test_expr
    @test """
    A[if n == d
        i
    else
        (indices(A,n) for n = 1:nd)
    end...]
    """ |> test_expr
    @test """
    @spawnat(p,
        let m = a
            isa(m, Exception) ? m : nothing
        end)
    """ |> test_expr #
    @test "[@spawn f(R, first(c), last(c)) for c in splitrange(length(R), nworkers())]" |> test_expr
    @test "M.:(a)" |> test_expr
    @test """
            begin
                for i in I for j in J
                    if cond
                        a
                    end
                end end
            end""" |> test_expr
    @test "-f.(a.b + c)" |> test_expr
    @test ":(import Base: @doc)" |> test_expr
    @test "[a for a in A for b in B]" |> test_expr
    @test "+(a,b,c...)" |> test_expr
    @test """@testset a for t in T
        t
    end""" |> test_expr
    @test "import Base.==" |> test_expr
    @test "a`text`" |> test_expr
    @test "a``" |> test_expr
    @test "a`text`b" |> test_expr
    @test "[a; a 0]" |> test_expr
    @test "[a, b; c]" |> test_expr
    @test "t{a; b} " |> test_expr
    @test "a ~ b + c -d" |> test_expr
    @test_broken "y[j=1:10,k=3:2:9; isodd(j+k) && k <= 8]" |> test_expr
    @test "(8=>32.0, 12=>33.1, 6=>18.2)" |> test_expr
    @test "(a,b = c,d)" |> test_expr
    @test "[ -1 -2;]" |> test_expr
    @test "-2y" |> test_expr # precedence
    @test "'''" |> test_expr #tokenize
    @test """
        if j+k <= deg +1
        end
        """ |> test_expr
    @test "function f() ::T end" |> test_expr # ws closer
    @test "import Base: +, -, .+, .-" |> test_expr
    @test "[a +   + l]" |> test_expr #ws closer
    @test "@inbounds C[i,j] = - α[i] * αjc" |> test_expr
    @test "@inbounds C[i,j] = - n * p[i] * pj" |> test_expr
    @test """
        if ! a
            b
        end
        """ |> test_expr # ws closer
    @test "[:-\n:+]" |> test_expr
    @test "::a::b" |> test_expr
    @test "-[1:nc]" |> test_expr
    @test "f() where {a} = b" |> test_expr
    @test "@assert .!(isna(res[2]))" |> test_expr # v0.6
    @test "-((attr.rise / PANGO_SCALE)pt).value" |> test_expr
    @test "!(a = b)" |> test_expr
    @test "-(1)a" |> test_expr
    @test "!(a)::T" |> test_expr
    @test "a::b where T<:S" |> test_expr
    @test "+(x::Bool, y::T)::promote_type(Bool,T) where T<:AbstractFloat" |> test_expr
    @test "T where V<:(T where T)" |> test_expr
    @test "function ^(z::Complex{T}, p::Complex{T})::Complex{T} where T<:AbstractFloat end" |> test_expr
    @test "function +(a) where T where S end" |> test_expr
    @test "function -(x::Rational{T}) where T<:Signed end" |> test_expr
    @test "\$(a)(b)" |> test_expr
    @test "if !(a) break end" |> test_expr
    @test "module a() end" |> test_expr
    @test "M.r\"str\" " |> test_expr
    @test "f(a for a in A if cond)" |> test_expr
    @test "\"dimension \$d is not 1 ≤ \$d ≤ \$nd\" " |> test_expr
    @test "-(-x)^1" |> test_expr
    @test """
        "\\\\\$ch"
        """ |> test_expr
    @test "begin\n\"\"\"Float\$(bit)\"\"\"\n\$(Symbol(\"Float\",bit))\nend" |> test_expr
    @test "µs" |> test_expr # normalize unicode
    @test """(x, o; p = 1) -> begin
    return o, p
    end""" |> test_expr # normalize unicode
    @test """(x, o...; p...) -> begin
    return o, p
    end""" |> test_expr # normalize unicode
    @test "function func() where {A where T} x + 1 end" |> test_expr # nested where
    @test "(;x)" |> test_expr # issue 39
    @test """let f = ((; a = 1, b = 2) -> ()),
    m = first(methods(f))
    @test DSE.keywords(f, m) == [:a, :b]
end""" |> test_expr
    @test "-1^a" |> test_expr_broken
    @test "function(f, args...; kw...) end" |> test_expr_broken
    @test "2a * b" |> test_expr
    @test "(g1090(x::T)::T) where {T} = x+1.0" |> test_expr
    @test "(:) = Colon()" |> test_expr
end

@testset "Broken things" begin
    @test_broken "\$(a) * -\$(b)" |> test_expr_broken
end

# test_fsig_decl(str) = (x->x.id).(CSTParser._get_fsig(CSTParser.parse(str)).defs)
# @testset "func-sig variable declarations" begin
#     @test test_fsig_decl("f(x) = x") == [:x]
#     @test test_fsig_decl("""function f(x)
#         x
#     end""") == [:x]

#     @test test_fsig_decl("f{T}(x::T) = x") == [:T, :x]
#     @test test_fsig_decl("""function f{T}(x::T)
#         x
#     end""") == [:T, :x]

#     @test test_fsig_decl("f(x::T) where T = x") == [:T, :x]
#     @test test_fsig_decl("""function f(x::T) where T
#         x
#     end""") == [:T, :x]


#     @test test_fsig_decl("f(x::T{S}) where T where S = x") == [:T, :S, :x]
#     @test test_fsig_decl("""function f(x::T{S}) where T where S
#         x
#     end""") == [:T, :S, :x]
# end

@testset "Spans" begin
    CSTParser.parse(raw"""
    "ABC$(T)"
    """).fullspan >= 9
    CSTParser.parse("\"_\"").fullspan == 3
    CSTParser.parse("T.mutable && print(\"Ok\")").fullspan == 24
    CSTParser.parse("(\"\$T\")").fullspan == 6
    CSTParser.parse("\"\"\"\$T is not supported\"\"\"").fullspan == 25
    CSTParser.parse("using Compat: @compat\n").fullspan == 22
    CSTParser.parse("primitive = 1").fullspan == 13
end

@testset "Command or string with unicode" begin
    @test "```αhelloworldω```" |> test_expr
    @test "\"αhelloworldω\"" |> test_expr
end

@testset "conversion of floats with underscore" begin
    @test "30.424_876_125_859_513" |> test_expr
end

end
