function test_parse(production, code)
    stream = ParseStream(code)
    production(JuliaSyntax.ParseState(stream))
    t = JuliaSyntax.to_raw_tree(stream, wrap_toplevel_as_kind=K"error")
    # @test Text(sprint(JuliaSyntax.show_diagnostics, stream, code)) == Text("")
    s = SyntaxNode(SourceFile(code), t)
    sprint(show, MIME("text/x.sexpression"), s)
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
        "a ? b : c"    => "(if :a :b :c)"
        # Following are errors but should recover
        "a? b : c"    => "(if :a :b :c)"
        "a ?b : c"    => "(if :a :b :c)"
        "a ? b: c"    => "(if :a :b :c)"
        "a ? b :c"    => "(if :a :b :c)"
        "a ? b c"    => "(if :a :b :c)"
        #"a ?\nb : c"   => "(if :a :b :c)"
        #"a ? b :\nc"   => "(if :a :b :c)"
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
        # a ? b : c:d   ==>   (if a b (call-i c : d))
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
    ],
    JuliaSyntax.parse_decl => [
        #"a::b"    =>   "(:: a b)"
        #"a->b"    =>   "(-> a b)"
    ],
    JuliaSyntax.parse_unary_prefix => [
        #"&a"   => "(& :a)"
        #"::a"  => "(:: :a)"
        #"\$a"  => "(\$ :a)"
        #"\$\$a"  => "(\$ (\$ :a))"
    ],
    JuliaSyntax.parse_docstring => [
        "\"doc\" foo" => "(macrocall :(Core.var\"@doc\") \"doc\" :foo)"
    ],
    JuliaSyntax.parse_atom => [
        ":foo" => "(quote :foo)"
        # Literal colons
        ":)"   => ":(:)"
        ": end"   => ":(:)"
        # Errors
        ": foo" => "(quote :foo)"
    ]
]

@testset "$production" for (production, test_specs) in tests
    for (input,output) in test_specs
        @test test_parse(production, input) == output
    end
end

