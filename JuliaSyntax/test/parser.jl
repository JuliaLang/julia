function test_parse(production, code)
    stream = ParseStream(code)
    production(JuliaSyntax.ParseState(stream))
    t = JuliaSyntax.to_raw_tree(stream, wrap_toplevel_as_kind=K"Nothing")
    source = SourceFile(code)
    s = SyntaxNode(source, t)
    if JuliaSyntax.kind(s) == K"Nothing"
        join([sprint(show, MIME("text/x.sexpression"), c) for c in children(s)], ' ')
    else
        sprint(show, MIME("text/x.sexpression"), s)
    end
end

# Version of test_parse for interactive exploration
function itest_parse(production, code)
    stream = ParseStream(code)
    production(JuliaSyntax.ParseState(stream))
    t = JuliaSyntax.to_raw_tree(stream, wrap_toplevel_as_kind=K"toplevel")

    println(stdout, "# Code:\n$code\n")

    println(stdout, "# Green tree:")
    show(stdout, MIME"text/plain"(), t, code)
    JuliaSyntax.show_diagnostics(stdout, stream, code)

    s = SyntaxNode(SourceFile(code, filename="none"), t)
    println(stdout, "\n# SyntaxNode:")
    show(stdout, MIME"text/x.sexpression"(), s)

    ex = Expr(s)
    println(stdout, "\n\n# Julia Expr:")
    dump(ex)
    #show(stdout, MIME"text/plain"(), ex)

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
        "a::b"     =>   "(:: :a :b)"
        "a->b"     =>   "(-> :a :b)"
        "a::b->c"  =>  "(-> (:: :a :b) :c)"
    ],
    JuliaSyntax.parse_unary_prefix => [
        "&)"   => ":&"
        "\$\n" => ":\$"
        "&a"   => "(& :a)"
        "::a"  => "(:: :a)"
        "\$a"  => "(\$ :a)"
        "\$\$a"  => "(\$ (\$ :a))"
    ],
    JuliaSyntax.parse_call => [
        "f(a,b)" => "(call :f :a :b)"
        "f(a).g(b)" => "(call (. (call :f :a) (quote :g)) :b)"
        # do
        "f() do x, y\n body end"  =>  "(do (call :f) (-> (tuple :x :y) (block :body)))"
        "f() do\nend"         =>  "(do (call :f) (-> (tuple) (block)))"
        "f() do ; body end"   =>  "(do (call :f) (-> (tuple) (block :body)))"
        "f(x) do y,z body end"  =>  "(do (call :f :x) (-> (tuple :y :z) (block :body)))"
        # Keyword arguments depend on call vs macrocall
        "foo(a=1)"  =>  "(call :foo (kw :a 1))"
        "@foo(a=1)" =>  "(macrocall :foo (= :a 1))"
        # f(x) do y body end  ==>  (do (call :f :x) (-> (tuple :y) (block :body)))
        "@foo a b"     =>  "(macrocall :foo :a :b)"
        "A.@foo a b"   =>  "(macrocall (. :A (quote :foo)) :a :b)"
        "@A.foo a b"   =>  "(macrocall (. :A (quote :foo)) :a :b)"
        # Special @doc parsing rules
        "@doc x\ny"    =>  "(macrocall :doc :x :y)"
        "A.@doc x\ny"  =>  "(macrocall (. :A (quote :doc)) :x :y)"
        "@A.doc x\ny"  =>  "(macrocall (. :A (quote :doc)) :x :y)"
        "@doc x y\nz"  =>  "(macrocall :doc :x :y)"
        "@doc x\n\ny"  =>  "(macrocall :doc :x)"
        "@doc x\nend"  =>  "(macrocall :doc :x)"
        # Allow `@` in macrocall only in first and last position
        "A.B.@x"    =>  "(macrocall (. (. :A (quote :B)) (quote :x)))"
        "@A.B.x"    =>  "(macrocall (. (. :A (quote :B)) (quote :x)))"
        "A.@B.x"    =>  "(macrocall (. (. :A (quote :B)) (error) (quote :x)))"
        "a().@x(y)" => "(macrocall (error (. (call :a) (quote :x))) :y)"
        "a().@x y"  => "(macrocall (error (. (call :a) (quote :x))) :y)"
        "a().@x{y}" => "(macrocall (error (. (call :a) (quote :x))) (braces :y))"
        # Keyword params always use kw inside tuple in dot calls
        "f.(a,b)"   =>  "(. :f (tuple :a :b))"
        "f.(a=1)"   =>  "(. :f (tuple (kw :a 1)))"
        # Other dotted syntax
        "A.:+"      =>  "(. :A (quote :+))"
        "f.\$x"     =>  "(. :f (quote (\$ :x)))"
        "f.\$(x+y)" =>  "(. :f (quote (\$ (call :+ :x :y))))"
        # .' discontinued
        "f.'"    =>  ":f (error :. Symbol(\"'\"))"
        # Field/property syntax
        "f.x.y"  =>  "(. (. :f (quote :x)) (quote :y))"
        # Adjoint
        "f'"  => "(' :f)"
        "f'ᵀ" => "(call Symbol(\"'ᵀ\") :f)"
        # Curly calls
        "@S{a,b}" => "(macrocall :S (braces :a :b))"
        "S{a,b}"  => "(curly :S :a :b)"
        # String macros
        """x"str\"""" => """(macrocall :x_str "str")"""
        """x`str`"""  => """(macrocall :x_cmd "str")"""
        # Macro sufficies can include keywords and numbers
        "x\"s\"y"    => """(macrocall :x_str "s" "y")"""
        "x\"s\"end"  => """(macrocall :x_str "s" "end")"""
        "x\"s\"2"    => """(macrocall :x_str "s" 2)"""
        "x\"s\"10.0" => """(macrocall :x_str "s" 10.0)"""
    ],
    JuliaSyntax.parse_paren => [
        # Parentheses used for grouping
        # NB: The toplevel below is an artificial part of the test setup
        "(a * b)"     =>  "(call :* :a :b)"
        "(a=1)"       =>  "(= :a 1)"
        "(x)"         =>  ":x"
        # Tuple syntax with commas
        "(x,)"        =>  "(tuple :x)"
        "(x,y)"       =>  "(tuple :x :y)"
        "(x=1, y=2)"  =>  "(tuple (= :x 1) (= :y 2))"
        # Named tuples with initial semicolon
        "(;)"         =>  "(tuple (parameters))"
        "(; a=1)"     =>  "(tuple (parameters (kw :a 1)))"
        # Extra credit: nested parameters and frankentuples
        "(; a=1; b=2)"    => "(tuple (parameters (kw :a 1) (parameters (kw :b 2))))"
        "(a; b; c,d)"     => "(tuple :a (parameters :b (parameters :c :d)))"
        "(a=1, b=2; c=3)" => "(tuple (= :a 1) (= :b 2) (parameters (kw :c 3)))"
        # Block syntax
        "(;;)"        =>  "(block)"
        "(a=1;)"      =>  "(block (= :a 1))"
        "(a;b;;c)"    =>  "(block :a :b :c)"
        "(a=1; b=2)"  =>  "(block (= :a 1) (= :b 2))"
    ],
    JuliaSyntax.parse_atom => [
        ":foo" => "(quote :foo)"
        # Literal colons
        ":)"   => ":(:)"
        ": end"   => ":(:)"
        # Special symbols quoted
        ":end" => "(quote :end)"
        ":(end)" => "(quote (error :end))"
        ":<:"  => "(quote :<:)"
        # Macro names can be keywords
        "@end x" => "(macrocall :end :x)"
        # __dot__ macro
        "@. x y" => "(macrocall :__dot__ :x :y)"
        # Errors
        ": foo" => "(quote (error) :foo)"
    ],
    JuliaSyntax.parse_docstring => [
        "\"doc\" foo" => "(macrocall :(Core.var\"@doc\") \"doc\" :foo)"
    ],
]

@testset "Inline test cases" begin
    @testset "$production" for (production, test_specs) in tests
        @testset "$input" for (input,output) in test_specs
            @test test_parse(production, input) == output
        end
    end
end
