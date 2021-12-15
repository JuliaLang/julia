function test_parse(production, code)
    stream = ParseStream(code)
    production(JuliaSyntax.ParseState(stream))
    t = JuliaSyntax.to_raw_tree(stream, wrap_toplevel_as_kind=K"toplevel")
    # @test Text(sprint(JuliaSyntax.show_diagnostics, stream, code)) == Text("")
    s = SyntaxNode(SourceFile(code), t)
    sprint(show, MIME("text/x.sexpression"), s)
end

# Version of test_parse for interactive exploration
function itest_parse(production, code)
    stream = ParseStream(code)
    production(JuliaSyntax.ParseState(stream))
    t = JuliaSyntax.to_raw_tree(stream, wrap_toplevel_as_kind=K"toplevel")
    s = SyntaxNode(SourceFile(code, filename="none"), t)
    ex = Expr(s)

    println(stdout, "# Code:\n$code\n")

    println(stdout, "# Green tree:")
    show(stdout, MIME"text/plain"(), t, code)
    JuliaSyntax.show_diagnostics(stdout, stream, code)

    println(stdout, "\n# SyntaxNode:")
    show(stdout, MIME"text/x.sexpression"(), s)

    println(stdout, "\n\n# Julia Expr:")
    show(stdout, MIME"text/plain"(), ex)

    f_ex = Base.remove_linenums!(Meta.parse(code, raise=false))
    if ex != f_ex
        printstyled(stdout, "\n\n# flisp Julia Expr:\n", color=:red)
        show(stdout, MIME"text/plain"(), f_ex)
    end
    (code, stream, t, s, ex)
end

# TODO:
# * Extract the following test cases from the source itself.
# * Use only the green tree to generate the S-expressions
#   (add flag annotations to heads)
tests = [
    JuliaSyntax.parse_block => [
        "a;b;c"   => "(block :a :b :c)"
        "a;;;b;;" => "(block :a :b)"
        "a\nb"    => "(block :a :b)"
    ],
    JuliaSyntax.parse_stmts => [
        "a;b;c"   => "(toplevel :a :b :c)"
        "a;;;b;;" => "(toplevel :a :b)"
    ],
    JuliaSyntax.parse_cond => [
        "a ? b : c"   => "(if :a :b :c)"
        "a ?\nb : c"  => "(if :a :b :c)"
        "a ? b :\nc"  => "(if :a :b :c)"
        "a ? b : c:d" =>   "(if :a :b (call :(:) :c :d))"
        # Following are errors but should recover
        "a? b : c"    => "(if :a (error) :b :c)"
        "a ?b : c"    => "(if :a (error) :b :c)"
        "a ? b: c"    => "(if :a :b (error) :c)"
        "a ? b :c"    => "(if :a :b (error) :c)"
        "a ? b c"     => "(if :a :b (error) :c)"
    ],
    JuliaSyntax.parse_arrow => [
        "x → y"     =>  "(call :→ :x :y)"
        "x <--> y"  =>  "(call :<--> :x :y)"
        "x --> y"   =>  "(--> :x :y)"
    ],
    JuliaSyntax.parse_or => [
        "x || y || z" => "(|| :x (|| :y :z))"
    ],
    JuliaSyntax.parse_and => [
        "x && y && z" => "(&& :x (&& :y :z))"
    ],
    JuliaSyntax.parse_comparison => [
        "x > y"       => "(call :> :x :y)"
        "x < y < z"   => "(comparison :x :< :y :< :z)"
        "x == y < z"  => "(comparison :x :(==) :y :< :z)"
        "x <: y"      => "(<: :x :y)"
        "x >: y"      => "(>: :x :y)"
    ],
    JuliaSyntax.parse_pipe_lt => [
        "x <| y <| z" => "(call :<| :x (call :<| :y :z))"
    ],
    JuliaSyntax.parse_pipe_gt => [
        "x |> y |> z" => "(call :|> (call :|> :x :y) :z)"
    ],
    JuliaSyntax.parse_range => [
        "1:2"       => "(call :(:) 1 2)"
        "1:2:3"     => "(call :(:) 1 2 3)"
        "a:b:c:d:e" => "(call :(:) (call :(:) :a :b :c) :d :e)"
        "a :< b"    => "(call (error :(:) :<) :a :b)"
    ],
    JuliaSyntax.parse_range => [
        "a..b"      => "(call :.. :a :b)"
        "a … b"     => "(call :… :a :b)"
        # [1 :a]      ==>  (vcat 1 (quote a))
        # [1 2:3 :a]  ==>  (vcat 1 (call-i 2 : 3) (quote a))
        "x..."     => "(... :x)"
        "x:y..."   => "(... (call :(:) :x :y))"
        "x..y..."  => "(... (call :.. :x :y))"
    ],
    JuliaSyntax.parse_expr => [
        # "[x +y]"  ==>  "(hcat x (call + y))"
        # [x+y +z]   ==>  (hcat (call-i x + y) (call + z))
        # Conversely
        # [x+y+z]    ==>  (hcat (call-i x + y z))
        # [x+y + z]  ==>  (hcat (call-i x + y z))
        "a - b - c"    =>  "(call :- (call :- :a :b) :c)"
        "a + b + c"    =>  "(call :+ :a :b :c)"
        "a +₁ b +₁ c"  =>  "(call :+₁ (call :+₁ :a :b) :c)"
        "a .+ b .+ c"  =>  "(call :.+ (call :.+ :a :b) :c)"
    ],
    JuliaSyntax.parse_term => [
        "a * b * c"  => "(call :* :a :b :c)"
        # For parse_unary
        "-2*x"  =>  "(call :* -2 :x)"
    ],
    JuliaSyntax.parse_juxtapose => [
        "2x"         => "(call :* 2 :x)"
        "2x"         => "(call :* 2 :x)"
        "2(x)"       => "(call :* 2 :x)"
        "(2)(3)x"    => "(call :* 2 3 :x)"
        "(x-1)y"     => "(call :* (call :- :x 1) :y)"
        # errors
        "\"a\"\"b\"" => "(call :* \"a\" (error) \"b\")"
        "\"a\"x"     => "(call :* \"a\" (error) :x)"
    ],
    JuliaSyntax.parse_unary => [
        "+2"    =>  "2"
        "-2^x"  =>  "(call :- (call :^ 2 :x))"
        # -2[1, 3]  ==>  (call - (ref 2 1 3))
    ],
    JuliaSyntax.parse_unary_call => [
        ".+"  =>  "(. :+)"
        "+)"   =>  ":+"
        # Function calls:
        "+(a,b)"   =>  "(call :+ :a :b)"
        "+(a=1,)"  =>  "(call :+ (kw :a 1))"
        # Not function calls:
        "+(a;b)"   =>  "(call :+ (block :a :b))"
        "+(a=1)"   =>  "(call :+ (= :a 1))"
        "+(a;b,c)" =>  "(call :+ :a (parameters :b :c))"
    ],
    JuliaSyntax.parse_decl => [
        #"a::b"    =>   "(:: :a :b)"
        #"a->b"    =>   "(-> :a :b)"
    ],
    JuliaSyntax.parse_unary_prefix => [
        #"&a"   => "(& :a)"
        #"::a"  => "(:: :a)"
        #"\$a"  => "(\$ :a)"
        #"\$\$a"  => "(\$ (\$ :a))"
    ],
    JuliaSyntax.parse_paren => [
        # Parentheses used for grouping
        # NB: The toplevel below is an artificial part of the test setup
        "(a * b)"     =>  "(toplevel (call :* :a :b))"
        "(a=1)"       =>  "(toplevel (= :a 1))"
        "(x)"         =>  "(toplevel :x)"
        # Tuple syntax with commas
        "(x,)"        =>  "(tuple :x)"
        "(x,y)"       =>  "(tuple :x :y)"
        "(x=1, y=2)"  =>  "(tuple (= :x 1) (= :y 2))"
        # Named tuples with initial semicolon
        "(;)"         =>  "(tuple (parameters ))"
        "(; a=1)"     =>  "(tuple (parameters (kw :a 1)))"
        # Extra credit: nested parameters and frankentuples
        "(; a=1; b=2)"    => "(tuple (parameters (kw :a 1) (parameters (kw :b 2))))"
        "(a; b; c,d)"     => "(tuple :a (parameters :b (parameters :c :d)))"
        "(a=1, b=2; c=3)" => "(tuple (= :a 1) (= :b 2) (parameters (kw :c 3)))"
        # Block syntax
        "(;;)"        =>  "(block )"
        "(a=1;)"      =>  "(block (= :a 1))"
        "(a;b;;c)"    =>  "(block :a :b :c)"
        "(a=1; b=2)"  =>  "(block (= :a 1) (= :b 2))"
    ],
    JuliaSyntax.parse_atom => [
        ":foo" => "(quote :foo)"
        # Literal colons
        ":)"   => ":(:)"
        ": end"   => ":(:)"
        # Macros
        "@foo x y" => "(macrocall :foo :x :y)"
        "@foo x\ny" => "(macrocall :foo :x)"
        # Doc macro parsing
        "@doc x\ny" => "(macrocall :doc :x :y)"
        "@doc x\nend" => "(macrocall :doc :x)"
        "@doc x y\nz" => "(macrocall :doc :x :y)"
        # __dot__ macro
        "@. x y" => "(macrocall :__dot__ :x :y)"
        # Errors
        ": foo" => "(quote (error ) :foo)"
    ],
    JuliaSyntax.parse_docstring => [
        "\"doc\" foo" => "(macrocall :(Core.var\"@doc\") \"doc\" :foo)"
    ],
]

@testset "Inline test cases" begin
    @testset "$production" for (production, test_specs) in tests
        for (input,output) in test_specs
            @test test_parse(production, input) == output
        end
    end
end
