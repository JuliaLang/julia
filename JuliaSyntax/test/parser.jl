"""
Parse string to SyntaxNode tree and show as an sexpression
"""
function parse_to_sexpr_str(production, code::AbstractString; v=v"1.6")
    stream = ParseStream(code, version=v)
    production(ParseState(stream))
    JuliaSyntax.validate_tokens(stream)
    t = build_tree(GreenNode, stream)
    source = SourceFile(code)
    s = SyntaxNode(source, t, keep_parens=true)
    return sprint(show, MIME("text/x.sexpression"), s)
end

function test_parse(production, input, output)
    if !(input isa AbstractString)
        opts, input = input
    else
        opts = NamedTuple()
    end
    parsed = parse_to_sexpr_str(production, input; opts...)
    if output isa Regex # Could be AbstractPattern, but that type was added in Julia 1.6.
        @test match(output, parsed) !== nothing
    else
        @test parsed == output
    end
end

function test_parse(inout::Pair)
    test_parse(JuliaSyntax.parse_toplevel, inout...)
end

const PARSE_ERROR = r"\(error-t "

with_version(v::VersionNumber, (i,o)::Pair) = ((;v=v), i) => o

# TODO:
# * Extract the following test cases from the source itself.
# * Use only the green tree to generate the S-expressions
#   (add flag annotations to heads)
tests = [
    JuliaSyntax.parse_toplevel => [
        "a \n b"     =>  "(toplevel a b)"
        "a;b \n c;d" =>  "(toplevel (toplevel-; a b) (toplevel-; c d))"
        "a \n \n"    =>  "(toplevel a)"
        ""           =>  "(toplevel)"
    ],
    JuliaSyntax.parse_block => [
        "a;b;c"   => "(block a b c)"
        "a;;;b;;" => "(block a b)"
        ";a"      => "(block a)"
        "\n a"    => "(block a)"
        "a\nb"    => "(block a b)"
    ],
    JuliaSyntax.parse_stmts => [
        "a;b;c"   => "(toplevel-; a b c)"
        "a;;;b;;" => "(toplevel-; a b)"
        """ "x" a ; "y" b """ =>
            """(toplevel-; (doc (string "x") a) (doc (string "y") b))"""
        "x y"  =>  "(wrapper x (error-t y))"
    ],
    JuliaSyntax.parse_eq => [
        # parse_assignment
        "a = b"       =>  "(= a b)"
        "a .= b"      =>  "(.= a b)"
        "a += b"      =>  "(+= a b)"
        "a .+= b"     =>  "(.+= a b)"
        "a, b = c, d" =>  "(= (tuple a b) (tuple c d))"
        "x, = xs"     =>  "(= (tuple x) xs)"
        "[a ~b]"      =>  "(hcat a (call-pre ~ b))"
        "a ~ b"       =>  "(call-i a ~ b)"
        "a .~ b"      =>  "(dotcall-i a ~ b)"
        "[a ~ b c]"   =>  "(hcat (call-i a ~ b) c)"
        "[a~b]"       =>  "(vect (call-i a ~ b))"
    ],
    JuliaSyntax.parse_pair => [
        "a => b"  =>  "(call-i a => b)"
        "a .=> b" => "(dotcall-i a => b)"
    ],
    JuliaSyntax.parse_cond => [
        "a ? b : c"   => "(? a b c)"
        "a ?\nb : c"  => "(? a b c)"
        "a ? b :\nc"  => "(? a b c)"
        "a ? b : c:d" =>   "(? a b (call-i c : d))"
        # Following are errors but should recover
        "a? b : c"    => "(? a (error-t) b c)"
        "a ?b : c"    => "(? a (error-t) b c)"
        "a ? b: c"    => "(? a b (error-t) c)"
        "a ? b :c"    => "(? a b (error-t) c)"
        "a ? b c"     => "(? a b (error-t) c)"
        "A[x ? y : end]" => "(ref A (? x y end))"
    ],
    JuliaSyntax.parse_arrow => [
        "x → y"     =>  "(call-i x → y)"
        "x <--> y"  =>  "(call-i x <--> y)"
        "x --> y"   =>  "(--> x y)"
        "x .--> y"  =>  "(dotcall-i x --> y)"
        "x -->₁ y"  =>  "(call-i x -->₁ y)"
    ],
    JuliaSyntax.parse_or => [
        "x || y || z" => "(|| x (|| y z))"
        ((v=v"1.6",), "x .|| y") => "(error (.|| x y))"
        ((v=v"1.7",), "x .|| y") => "(.|| x y)"
    ],
    JuliaSyntax.parse_and => [
        "x && y && z" => "(&& x (&& y z))"
        ((v=v"1.6",), "x .&& y") => "(error (.&& x y))"
        ((v=v"1.7",), "x .&& y") => "(.&& x y)"
    ],
    JuliaSyntax.parse_comparison => [
        # Type comparisons are syntactic
        "x <: y"      => "(<: x y)"
        "x >: y"      => "(>: x y)"
        # Normal binary comparisons
        "x < y"       => "(call-i x < y)"
        "x .< y"      => "(dotcall-i x < y)"
        "x .<: y"     => "(dotcall-i x <: y)"
        # Comparison chains
        "x < y < z"   => "(comparison x < y < z)"
        "x == y < z"  => "(comparison x == y < z)"
        "x .< y .< z" => "(comparison x (. <) y (. <) z)"
        "x .< y < z"  => "(comparison x (. <) y < z)"
    ],
    JuliaSyntax.parse_pipe_lt => [
        "x <| y <| z" => "(call-i x <| (call-i y <| z))"
    ],
    JuliaSyntax.parse_pipe_gt => [
        "x |> y |> z" => "(call-i (call-i x |> y) |> z)"
        "x .|> y"     => "(dotcall-i x |> y)"
    ],
    JuliaSyntax.parse_range => [
        "1:2"       => "(call-i 1 : 2)"
        "1:2:3"     => "(call-i 1 : 2 3)"
        "a:b:c:d:e" => "(call-i (call-i a : b c) : d e)"
        "a :< b"    => "(call-i a (error : <) b)"
        "1:\n2"     => "(call-i 1 : (error))"
    ],
    JuliaSyntax.parse_range => [
        "a..b"       => "(call-i a .. b)"
        "a … b"      => "(call-i a … b)"
        "a .… b"     => "(dotcall-i a … b)"
        "[1 :a]"     => "(hcat 1 (quote-: a))"
        "[1 2:3 :a]" =>  "(hcat 1 (call-i 2 : 3) (quote-: a))"
        "x..."     => "(... x)"
        "x:y..."   => "(... (call-i x : y))"
        "x..y..."  => "(... (call-i x .. y))"
    ],
    JuliaSyntax.parse_invalid_ops => [
        "a--b"  =>  "(call-i a (ErrorInvalidOperator) b)"
    ],
    JuliaSyntax.parse_expr => [
        "a - b - c"  => "(call-i (call-i a - b) - c)"
        "a + b + c"  => "(call-i a + b c)"
        "a + b .+ c" => "(dotcall-i (call-i a + b) + c)"
        # parse_with_chains:
        # The following is two elements of a hcat
        "[x +y]"     =>  "(hcat x (call-pre + y))"
        "[x+y +z]"   =>  "(hcat (call-i x + y) (call-pre + z))"
        # Conversely the following are infix calls
        "[x +₁y]"    =>  "(vect (call-i x +₁ y))"
        "[x+y+z]"    =>  "(vect (call-i x + y z))"
        "[x+y + z]"  =>  "(vect (call-i x + y z))"
        # Dotted and normal operators
        "a +₁ b +₁ c" =>  "(call-i (call-i a +₁ b) +₁ c)"
        "a .+ b .+ c" =>  "(dotcall-i (dotcall-i a + b) + c)"
    ],
    JuliaSyntax.parse_term => [
        "a * b * c"  => "(call-i a * b c)"
        "a .* b"     => "(dotcall-i a * b)"
        "-2*x"       => "(call-i -2 * x)"
    ],
    JuliaSyntax.parse_rational => [
        "x // y // z" => "(call-i (call-i x // y) // z)"
    ],
    JuliaSyntax.parse_shift => [
        "x >> y >> z" => "(call-i (call-i x >> y) >> z)"
    ],
    JuliaSyntax.parse_juxtapose => [
        "2x"         => "(juxtapose 2 x)"
        "2x"         => "(juxtapose 2 x)"
        "2(x)"       => "(juxtapose 2 (parens x))"
        "(2)(3)x"    => "(juxtapose (parens 2) (parens 3) x)"
        "(x-1)y"     => "(juxtapose (parens (call-i x - 1)) y)"
        "x'y"        => "(juxtapose (call-post x ') y)"
        "1√x"        =>  "(juxtapose 1 (call-pre √ x))"
        # errors
        "\"a\"\"b\"" => "(juxtapose (string \"a\") (error-t) (string \"b\"))"
        "\"a\"x"     => "(juxtapose (string \"a\") (error-t) x)"
        "\"\$y\"x"   => "(juxtapose (string y) (error-t) x)"
        "\"a\"begin end" => "(juxtapose (string \"a\") (error-t) (block))"
        # Not juxtaposition - parse_juxtapose will consume only the first token.
        "x.3"       =>  "x"
        "f(2)2"     =>  "(call f 2)"
        "x' y"      =>  "(call-post x ')"
        "x 'y"      =>  "x"
        "x@y"       =>  "x"
        "(begin end)x" => "(parens (block))"
    ],
    JuliaSyntax.parse_unary => [
        ":T"       => "(quote-: T)"
        "in::T"    => "(::-i in T)"
        "isa::T"   => "(::-i isa T)"
        "-2^x"     => "(call-pre - (call-i 2 ^ x))"
        "-2[1, 3]" => "(call-pre - (ref 2 1 3))"
        # signed literals
        "-2"       => "-2"
        "+2.0"     => "2.0"
        "-1.0f0"   => "-1.0f0"
        "-0xf.0p0" => "-15.0"
        "+0b10010" => "0x12"
        "+0o22"    => "0x12"
        "+0x12"    => "0x12"
        "-0b10010" => "(call-pre - 0x12)"
        "-0o22"    => "(call-pre - 0x12)"
        "-0x12"    => "(call-pre - 0x12)"
        # Standalone dotted operators are parsed as (|.| op)
        ".+"   =>  "(. +)"
        ".+\n" =>  "(. +)"
        ".+ =" =>  "(. +)"
        ".+)"  =>  "(. +)"
        ".&"   =>  "(. &)"
        # Standalone non-dotted operators
        "+)"   =>  "+"
        # Call with type parameters or non-unary prefix call
        "+{T}(x::T)"  =>  "(call (curly + T) (::-i x T))"
        "*(x)"        =>  "(call * x)"
        ".*(x)"       =>  "(call (. *) x)"
        # Prefix function calls for operators which are both binary and unary
        "+(a,b)"   =>  "(call + a b)"
        ".+(a,)"   =>  "(call (. +) a)"
        "(.+)(a)"  =>  "(call (parens (. +)) a)"
        "+(a=1,)"  =>  "(call + (= a 1))"
        "+(a...)"  =>  "(call + (... a))"
        "+(a;b,c)" =>  "(call + a (parameters b c))"
        "+(;a)"    =>  "(call + (parameters a))"
        "+(;;a)"   =>  "(call + (parameters) (parameters a))"
        "+()"      =>  "(call +)"
        "+(\n;a)"  =>  "(call + (parameters a))"
        "+(;)"     =>  "(call + (parameters))"
        "+(\n;\n)" =>  "(call + (parameters))"
        "+(\n)"    =>  "(call +)"
        # Whitespace not allowed before prefix function call bracket
        "+ (a,b)"  =>  "(call + (error) a b)"
        # Prefix calls have higher precedence than ^
        "+(a,b)^2"  =>  "(call-i (call + a b) ^ 2)"
        "+(a,b)(x)^2"  =>  "(call-i (call (call + a b) x) ^ 2)"
        "<:(a,)"  =>  "(<: a)"
        # Unary function calls with brackets as grouping, not an arglist
        ".+(a)"   =>  "(dotcall-pre + (parens a))"
        "+(a;b)"  =>  "(call-pre + (block-p a b))"
        "+(;;)"   =>  "(call-pre + (block-p))"
        "+(;;)"   =>  "(call-pre + (block-p))"
        "+(a;)"   =>  "(call-pre + (block-p a))"
        "+(a;;)"  =>  "(call-pre + (block-p a))"
        "+(\n;\n;\n)" =>  "(call-pre + (block-p))"
        "+(a=1)"  =>  "(call-pre + (parens (= a 1)))"
        # Unary operators have lower precedence than ^
        "+(a)^2"  =>  "(call-pre + (call-i (parens a) ^ 2))"
        ".+(a)^2" =>  "(dotcall-pre + (call-i (parens a) ^ 2))"
        "+(a)(x,y)^2"  =>  "(call-pre + (call-i (call (parens a) x y) ^ 2))"
        "<:(a)"   =>  "(<:-pre (parens a))"
        # Normal unary calls
        "+x" => "(call-pre + x)"
        "√x" => "(call-pre √ x)"
        ".~x" => "(dotcall-pre ~ x)"
        # Things which are not quite negative literals
        "-0x1"=> "(call-pre - 0x01)"
        "- 2" => "(call-pre - 2)"
        ".-2" => "(dotcall-pre - 2)"
        # Not a unary operator
        "/x"     => "(call-pre (error /) x)"
        "+₁ x"   => "(call-pre (error +₁) x)"
        ".<: x"  => "(dotcall-pre (error (. <:)) x)"
        "?\"str\"" => """(call-pre (error ?) (string "str"))"""
    ],
    JuliaSyntax.parse_factor => [
        "x^y"      =>  "(call-i x ^ y)"
        "x^y^z"    =>  "(call-i x ^ (call-i y ^ z))"
        "x .^ y"   =>  "(dotcall-i x ^ y)"
        "begin x end::T"  =>  "(::-i (block x) T)"
        # parse_decl_with_initial_ex
        "a::b"     =>  "(::-i a b)"
        "a->b"     =>  "(-> a b)"
        "a::b::c"  =>  "(::-i (::-i a b) c)"
        "a::b->c"  =>  "(-> (::-i a b) c)"
    ],
    JuliaSyntax.parse_unary_subtype => [
        "<: )"    =>  "<:"
        "<: \n"   =>  "<:"
        "<: ="    =>  "<:"
        "<:{T}(x::T)"   =>  "(call (curly <: T) (::-i x T))"
        "<:(x::T)"      =>  "(<:-pre (parens (::-i x T)))"
        "<: x"          =>  "(<:-pre x)"
        "<: <: x"       =>  "(<:-pre (<:-pre x))"
        "<: A where B"  =>  "(<:-pre (where A B))"
        # FIXME: The following bizarre precedence seems broken, but is
        # compatible with the reference parser (see #248)
        "+ <: A where B"  =>  "(where (call-pre + (<:-pre A)) B)"
        # Really for parse_where
        "x where \n {T}"  =>  "(where x (braces T))"
        "x where {T,S}"  =>  "(where x (braces T S))"
        "x where {T S}"  =>  "(where x (bracescat (row T S)))"
        "x where {y for y in ys}"  =>  "(where x (braces (generator y (= y ys))))"
        "x where T"  =>  "(where x T)"
        "x where \n T"  =>  "(where x T)"
        "x where T<:S"  =>  "(where x (<: T S))"
        # nested unary and unary-syntactic ops
        "<: + <: + A" => "(<:-pre (call-pre + (<:-pre (call-pre + A))))"
        "* <: A"      => "(call-pre (error *) (<:-pre A))"
    ],
    JuliaSyntax.parse_unary_prefix => [
        "&)"   => "&"
        "\$\n" => "\$"
        "&a"   => "(& a)"
        "::a"  => "(::-pre a)"
        "\$a"  => "(\$ a)"
        "\$\$a"  => "(\$ (\$ a))"
    ],
    JuliaSyntax.parse_call => [
        # parse_call
        "f(x)"    =>  "(call f x)"
        "\$f(x)"  =>  "(call (\$ f) x)"
        ".&(x,y)" =>  "(call (. &) x y)"
        # parse_call_chain
        "f(a).g(b)" => "(call (. (call f a) g) b)"
        "\$A.@x"    =>  "(macrocall (. (\$ A) @x))"

        # non-errors in space sensitive contexts
        "[f (x)]"    =>  "(hcat f (parens x))"
        "[f x]"      =>  "(hcat f x)"
        # space separated macro calls
        "@foo a b"     =>  "(macrocall @foo a b)"
        "@foo (x)"     =>  "(macrocall @foo (parens x))"
        "@foo (x,y)"   =>  "(macrocall @foo (tuple-p x y))"
        "A.@foo a b"   =>  "(macrocall (. A @foo) a b)"
        "@A.foo a b"   =>  "(macrocall (. A @foo) a b)"
        "[@foo x]"     =>  "(vect (macrocall @foo x))"
        "[@foo]"       =>  "(vect (macrocall @foo))"
        "@var\"#\" a"  =>  "(macrocall (var @#) a)"
        "@(A) x"       =>  "(macrocall (parens @A) x)"
        "A.@x y"       =>  "(macrocall (. A @x) y)"
        "A.@var\"#\" a"=>  "(macrocall (. A (var @#)) a)"
        "@+x y"        =>  "(macrocall @+ x y)"
        "A.@.x"        =>  "(macrocall (. A @.) x)"
        # Macro names
        "@! x"  => "(macrocall @! x)"
        "@.. x" => "(macrocall @.. x)"
        "@\$ y"  => "(macrocall @\$ y)"
        "@[x] y z" => "(macrocall (error (vect x)) y z)"
        # Special @doc parsing rules
        "@doc x\ny"    =>  "(macrocall @doc x y)"
        "A.@doc x\ny"  =>  "(macrocall (. A @doc) x y)"
        "@A.doc x\ny"  =>  "(macrocall (. A @doc) x y)"
        "@doc x y\nz"  =>  "(macrocall @doc x y)"
        "@doc x\n\ny"  =>  "(macrocall @doc x)"
        "@doc x\nend"  =>  "(macrocall @doc x)"

        # calls with brackets
        "f(a,b)"  => "(call f a b)"
        "f(a=1; b=2)" => "(call f (= a 1) (parameters (= b 2)))"
        "f(a; b; c)" => "(call f a (parameters b) (parameters c))"
        "(a=1)()" =>  "(call (parens (= a 1)))"
        "f (a)" => "(call f (error-t) a)"
        "@x(a, b)"   =>  "(macrocall-p @x a b)"
        "A.@x(y)"    =>  "(macrocall-p (. A @x) y)"
        "A.@x(y).z"  =>  "(. (macrocall-p (. A @x) y) z)"
        # do
        "f() do\nend"         =>  "(call f (do (tuple) (block)))"
        "f() do ; body end"   =>  "(call f (do (tuple) (block body)))"
        "f() do x, y\n body end"  =>  "(call f (do (tuple x y) (block body)))"
        "f(x) do y body end"  =>  "(call f x (do (tuple y) (block body)))"
        "@f(x) do y body end" =>  "(macrocall-p @f x (do (tuple y) (block body)))"

        # square brackets
        "@S[a,b]"  => "(macrocall @S (vect a b))"
        "@S[a b]"  => "(macrocall @S (hcat a b))"
        "@S[a; b]" => "(macrocall @S (vcat a b))"
        "A.@S[a]"  =>  "(macrocall (. A @S) (vect a))"
        "@S[a].b"  =>  "(. (macrocall @S (vect a)) b)"
        ((v=v"1.7",), "@S[a ;; b]")  =>  "(macrocall @S (ncat-2 a b))"
        ((v=v"1.6",), "@S[a ;; b]")  =>  "(macrocall @S (error (ncat-2 a b)))"
        "a[i]"  =>  "(ref a i)"
        "a [i]"  =>  "(ref a (error-t) i)"
        "a[i,j]"  =>  "(ref a i j)"
        "(a=1)[]" =>  "(ref (parens (= a 1)))"
        "a[end]"  =>  "(ref a end)"
        "a[begin]"  =>  "(ref a begin)"
        "a[:(end)]" => "(typed_hcat a (quote-: (parens (error-t))) (error-t))"
        "T[x   y]"  =>  "(typed_hcat T x y)"
        "T[x ; y]"  =>  "(typed_vcat T x y)"
        "T[a b; c d]"  =>  "(typed_vcat T (row a b) (row c d))"
        "T[x for x in xs]"  =>  "(typed_comprehension T (generator x (= x xs)))"
        ((v=v"1.8",), "T[a ; b ;; c ; d]") => "(typed_ncat-2 T (nrow-1 a b) (nrow-1 c d))"

        # Dotted forms
        # Allow `@` in macrocall only in first and last position
        "A.B.@x"    =>  "(macrocall (. (. A B) @x))"
        "@A.B.x"    =>  "(macrocall (. (. A B) @x))"
        "A.@B.x"    =>  "(macrocall (. (. A B) (error-t) @x))"
        "@M.(x)"    =>  "(macrocall (dotcall @M (error-t) x))"
        "f.(a,b)"   =>  "(dotcall f a b)"
        "f.(a=1; b=2)" => "(dotcall f (= a 1) (parameters (= b 2)))"
        "(a=1).()" =>  "(dotcall (parens (= a 1)))"
        "f. (x)"    =>  "(dotcall f (error-t) x)"
        # Other dotted syntax
        "A.:+"      =>  "(. A (quote-: +))"
        "A.:.+"     =>  "(. A (quote-: (. +)))"
        "A.: +"     =>  "(. A (quote-: (error-t) +))"
        "f.\$x"     =>  "(. f (\$ x))"
        "f.\$(x+y)" =>  "(. f (\$ (parens (call-i x + y))))"
        "A.\$B.@x"  =>  "(macrocall (. (. A (\$ B)) @x))"
        "@A.\$x a"  =>  "(macrocall (. A (error x)) a)"
        "A.@x"      =>  "(macrocall (. A @x))"
        "A.@x a"    =>  "(macrocall (. A @x) a)"
        "@A.B.@x a" =>  "(macrocall (. (. A B) (error-t) @x) a)"
        # .' discontinued
        "f.'"    =>  "(wrapper f (error-t '))"
        # Field/property syntax
        "f.x.y"  =>  "(. (. f x) y)"
        "x .y"   =>  "(. x (error-t) y)"
        # Adjoint
        "f'"  => "(call-post f ')"
        "f'ᵀ" => "(call-post f 'ᵀ)"
        # Curly calls
        "S {a}"   => "(curly S (error-t) a)"
        "A.@S{a}" => "(macrocall (. A @S) (braces a))"
        "@S{a,b}" => "(macrocall @S (braces a b))"
        "A.@S{a}" => "(macrocall (. A @S) (braces a))"
        "@S{a}.b" => "(. (macrocall @S (braces a)) b)"
        "S{a,b}"  => "(curly S a b)"
        # String macros
        "x\"str\""   => """(macrocall @x_str (string-r "str"))"""
        "x`str`"     => """(macrocall @x_cmd (cmdstring-r "str"))"""
        "x\"\""      => """(macrocall @x_str (string-r ""))"""
        "x``"        => """(macrocall @x_cmd (cmdstring-r ""))"""
        "in\"str\""  => """(macrocall @in_str (string-r "str"))"""
        "outer\"str\"" => """(macrocall @outer_str (string-r "str"))"""
        # Triple quoted procesing for custom strings
        "r\"\"\"\nx\"\"\""        => raw"""(macrocall @r_str (string-s-r "x"))"""
        "r\"\"\"\n x\n y\"\"\""   => raw"""(macrocall @r_str (string-s-r "x\n" "y"))"""
        "r\"\"\"\n x\\\n y\"\"\"" => raw"""(macrocall @r_str (string-s-r "x\\\n" "y"))"""
        # Macro sufficies can include keywords and numbers
        "x\"s\"y"    => """(macrocall @x_str (string-r "s") "y")"""
        "x\"s\"end"  => """(macrocall @x_str (string-r "s") "end")"""
        "x\"s\"in"   => """(macrocall @x_str (string-r "s") "in")"""
        "x\"s\"2"    => """(macrocall @x_str (string-r "s") 2)"""
        "x\"s\"10.0" => """(macrocall @x_str (string-r "s") 10.0)"""
        #
    ],
    JuliaSyntax.parse_resword => [
        # In normal_context
        "begin f() where T = x end" => "(block (= (where (call f) T) x))"
        # block
        "begin end"         =>  "(block)"
        "begin a ; b end"   =>  "(block a b)"
        "begin\na\nb\nend"  =>  "(block a b)"
        # quote
        "quote end"         =>  "(quote (block))"
        "quote body end"    =>  "(quote (block body))"
        # while
        "while cond body end"  =>  "(while cond (block body))"
        "while x < y \n a \n b \n end"  =>  "(while (call-i x < y) (block a b))"
        # for
        "for x in xs end" => "(for (= x xs) (block))"
        "for x in xs, y in ys \n a \n end" => "(for (cartesian_iterator (= x xs) (= y ys)) (block a))"
        # let
        "let x=1\n end"    =>  "(let (block (= x 1)) (block))"
        "let x=1 ; end"    =>  "(let (block (= x 1)) (block))"
        "let x ; end"      =>  "(let (block x) (block))"
        "let x::1 ; end"   =>  "(let (block (::-i x 1)) (block))"
        "let x=1,y=2 end"  =>  "(let (block (= x 1) (= y 2)) (block))"
        "let x+=1 ; end"   =>  "(let (block (+= x 1)) (block))"
        "let ; end"        =>  "(let (block) (block))"
        "let ; body end"   =>  "(let (block) (block body))"
        "let\na\nb\nend"   =>  "(let (block) (block a b))"
        # abstract type
        "abstract type A end"            =>  "(abstract A)"
        "abstract type A ; end"          =>  "(abstract A)"
        "abstract type \n\n A \n\n end"  =>  "(abstract A)"
        "abstract type A <: B end"       =>  "(abstract (<: A B))"
        "abstract type A <: B{T,S} end"  =>  "(abstract (<: A (curly B T S)))"
        "abstract type A < B end"        =>  "(abstract (call-i A < B))"
        # primitive type
        "primitive type A 32 end"   =>  "(primitive A 32)"
        "primitive type A 32 ; end" =>  "(primitive A 32)"
        "primitive type A \$N end"  =>  "(primitive A (\$ N))"
        "primitive type A <: B \n 8 \n end"  =>  "(primitive (<: A B) 8)"
        # struct
        "struct A <: B \n a::X \n end" =>  "(struct (<: A B) (block (::-i a X)))"
        "struct A \n a \n b \n end"    =>  "(struct A (block a b))"
        "mutable struct A end"         =>  "(struct-mut A (block))"
        ((v=v"1.8",), "struct A const a end") => "(struct A (block (const a)))"
        ((v=v"1.7",), "struct A const a end") => "(struct A (block (error (const a))))"
        "struct A end"    =>  "(struct A (block))"
        "struct try end"  =>  "(struct (error try) (block))"
        # return
        "return\nx"   =>  "(return)"
        "return)"     =>  "(return)"
        "return x"    =>  "(return x)"
        "return x,y"  =>  "(return (tuple x y))"
        # break/continue
        "break"    => "(break)"
        "continue" => "(continue)"
        # module/baremodule
        "module A end"      =>  "(module A (block))"
        "baremodule A end"  =>  "(module-bare A (block))"
        "module do \n end"  =>  "(module (error do) (block))"
        "module \$A end"    =>  "(module (\$ A) (block))"
        "module A \n a \n b \n end"  =>  "(module A (block a b))"
        """module A \n "x"\na\n end""" => """(module A (block (doc (string "x") a)))"""
        # export
        "export a"   =>  "(export a)"
        "export @a"  =>  "(export @a)"
        "export @var\"'\"" =>  "(export (var @'))"
        "export a, \n @b"  =>  "(export a @b)"
        "export +, =="     =>  "(export + ==)"
        "export \n a"      =>  "(export a)"
        "export \$a, \$(a*b)"  =>  "(export (\$ a) (\$ (parens (call-i a * b))))"
        "export (x::T)"  =>  "(export (error (parens (::-i x T))))"
        "export outer"  =>  "(export outer)"
        "export (\$f)"  =>  "(export (parens (\$ f)))"
    ],
    JuliaSyntax.parse_if_elseif => [
        "if a xx elseif b yy else zz end" => "(if a (block xx) (elseif b (block yy) (block zz)))"
        "if end"        =>  "(if (error) (block))"
        "if \n end"     =>  "(if (error) (block))"
        "if a end"      =>  "(if a (block))"
        "if a xx end"   =>  "(if a (block xx))"
        "if a \n\n xx \n\n end"   =>  "(if a (block xx))"
        "if a xx elseif b yy end"   =>  "(if a (block xx) (elseif b (block yy)))"
        "if a xx else if b yy end"  =>  "(if a (block xx) (error-t) (elseif b (block yy)))"
        "if a xx else yy end"   =>  "(if a (block xx) (block yy))"
        "if true; x ? true elseif true end"  => "(if true (block (if x true (error-t) (error-t))) (elseif true (block)))"
        "if true; x ? true end"  => "(if true (block (if x true (error-t) (error-t))))"
        "if true; x ? true\nend"  => "(if true (block (if x true (error-t) (error-t))))"
        "if true; x ? true : elseif true end"  => "(if true (block (if x true (error-t))) (elseif true (block)))"
    ],
    JuliaSyntax.parse_resword => [
        "global x"    =>  "(global x)"
        "local x"     =>  "(local x)"
        "global x,y"  =>  "(global x y)"
        "global const x = 1" => "(global (const (= x 1)))"
        "local const x = 1"  => "(local (const (= x 1)))"
        "const global x = 1" => "(const (global (= x 1)))"
        "const local x = 1"  => "(const (local (= x 1)))"
        "const x,y = 1,2"    => "(const (= (tuple x y) (tuple 1 2)))"
        "const x = 1"    =>  "(const (= x 1))"
        "const x .= 1"   => "(error (const (.= x 1)))"
        "global x ~ 1"   =>  "(global (call-i x ~ 1))"
        "global x += 1"  => "(global (+= x 1))"
        "const x"        => "(error (const x))"
        "global const x" => "(global (error (const x)))"
        "const global x" => "(error (const (global x)))"
    ],
    JuliaSyntax.parse_resword => [
        # Macros and functions
        "macro while(ex) end"  =>  "(macro (call (error while) ex) (block))"
        "macro f()     end"    =>  "(macro (call f) (block))"
        "macro (:)(ex) end"    =>  "(macro (call (parens :) ex) (block))"
        "macro (type)(ex) end" =>  "(macro (call (parens type) ex) (block))"
        "macro \$f()    end"   =>  "(macro (call (\$ f)) (block))"
        "macro (\$f)()  end"   =>  "(macro (call (parens (\$ f))) (block))"
        "function (x) body end"=>  "(function (tuple-p x) (block body))"
        "function (x,y) end"   =>  "(function (tuple-p x y) (block))"
        "function (x=1) end"   =>  "(function (tuple-p (= x 1)) (block))"
        "function (;x=1) end"  =>  "(function (tuple-p (parameters (= x 1))) (block))"
        "function ()(x) end"   =>  "(function (call (tuple-p) x) (block))"
        "function (A).f() end" =>  "(function (call (. (parens A) f)) (block))"
        "function (:)() end"   =>  "(function (call (parens :)) (block))"
        "function (x::T)() end"=>  "(function (call (parens (::-i x T))) (block))"
        "function (::g(x))() end" => "(function (call (parens (::-pre (call g x)))) (block))"
        "function (f::T{g(i)})() end" => "(function (call (parens (::-i f (curly T (call g i))))) (block))"
        "function (::T)() end" =>  "(function (call (parens (::-pre T))) (block))"
        "function (:*=(f))() end" => "(function (call (parens (call (quote-: *=) f))) (block))"
        "function begin() end" =>  "(function (call (error begin)) (block))"
        "function f() end"     =>  "(function (call f) (block))"
        "function type() end"  =>  "(function (call type) (block))"
        "function \n f() end"  =>  "(function (call f) (block))"
        "function \$f() end"   =>  "(function (call (\$ f)) (block))"
        "function (::Type{T})(x) end"  =>  "(function (call (parens (::-pre (curly Type T))) x) (block))"
        # Function/macro definition with no methods
        "function f end"      =>  "(function f)"
        "function f \n\n end" =>  "(function f)"
        "function \$f end"    =>  "(function (\$ f))"
        "macro f end"         =>  "(macro f)"
        # Function argument list
        "function f(x,y) end"    =>  "(function (call f x y) (block))"
        "function f{T}() end"    =>  "(function (call (curly f T)) (block))"
        "function A.f()   end"   =>  "(function (call (. A f)) (block))"
        "function f body end"    =>  "(function (error f) (block body))"
        "function f()::T    end" =>  "(function (::-i (call f) T) (block))"
        "function f()::g(T) end" =>  "(function (::-i (call f) (call g T)) (block))"
        "function f() where {T} end"  => "(function (where (call f) (braces T)) (block))"
        "function f() where T   end"  => "(function (where (call f) T) (block))"
        "function f()::S where T end" => "(function (where (::-i (call f) S) T) (block))"
        # Ugly cases for compat where extra parentheses existed and we've
        # already parsed at least the call part of the signature
        "function (f() where T) end" => "(function (parens (where (call f) T)) (block))"
        "function (f()) where T end" => "(function (where (parens (call f)) T) (block))"
        "function (f() where T) where U end" => "(function (where (parens (where (call f) T)) U) (block))"
        "function (f()::S) end"=>  "(function (parens (::-i (call f) S)) (block))"
        "function ((f()::S) where T) end" => "(function (parens (where (parens (::-i (call f) S)) T)) (block))"
        "function (x*y ) end" => "(function (parens (call-i x * y)) (block))"
        # body
        "function f() \n a \n b end"  =>  "(function (call f) (block a b))"
        "function f() end"       =>  "(function (call f) (block))"
        # Errors
        "function"            => "(function (error (error)) (block (error)) (error-t))"
    ],
    JuliaSyntax.parse_try => [
        "try \n x \n catch e \n y \n finally \n z end" =>
            "(try (block x) (catch e (block y)) (finally (block z)))"
        ((v=v"1.8",), "try \n x \n catch e \n y \n else z finally \n w end") =>
            "(try (block x) (catch e (block y)) (else (block z)) (finally (block w)))"
        "try x catch end"       =>  "(try (block x) (catch false (block)))"
        "try x catch ; y end"   =>  "(try (block x) (catch false (block y)))"
        "try x catch \n y end"  =>  "(try (block x) (catch false (block y)))"
        "try x catch e y end"   =>  "(try (block x) (catch e (block y)))"
        "try x catch \$e y end" =>  "(try (block x) (catch (\$ e) (block y)))"
        "try x catch var\"#\" y end" => "(try (block x) (catch (var #) (block y)))"
        "try x catch e+3 y end" =>  "(try (block x) (catch (error (call-i e + 3)) (block y)))"
        "try x finally y end"   =>  "(try (block x) (finally (block y)))"
        # v1.8 only
        ((v=v"1.8",), "try catch ; else end") => "(try (block) (catch false (block)) (else (block)))"
        ((v=v"1.8",), "try else x finally y end") => "(try (block) (else (error (block x))) (finally (block y)))"
        ((v=v"1.7",), "try catch ; else end")  =>  "(try (block) (catch false (block)) (else (error (block))))"
        # finally before catch :-(
        "try x finally y catch e z end"  =>  "(try (block x) (finally (block y)) (catch e (block z)))"
        "try x end" => "(try (block x) (error-t))"
    ],
    JuliaSyntax.parse_imports => [
        "import A as B: x" => "(import (: (error (as (importpath A) B)) (importpath x)))"
        "import A, y"      => "(import (importpath A) (importpath y))"
        "import A: +, =="  => "(import (: (importpath A) (importpath +) (importpath ==)))"
        "import A: x, y"   => "(import (: (importpath A) (importpath x) (importpath y)))"
        "import A: x, B: y" => "(import (: (importpath A) (importpath x) (importpath B) (error-t (importpath y))))"
        "import A: x"      => "(import (: (importpath A) (importpath x)))"
        "using  A"         => "(using (importpath A))"
        "import A"         => "(import (importpath A))"
        # parse_import
        "import A: x, y"   =>  "(import (: (importpath A) (importpath x) (importpath y)))"
        "import A as B"    =>  "(import (as (importpath A) B))"
        "import A: x as y" =>  "(import (: (importpath A) (as (importpath x) y)))"
        "using  A: x as y" =>  "(using (: (importpath A) (as (importpath x) y)))"
        ((v=v"1.5",), "import A as B") =>  "(import (error (as (importpath A) B)))"
        "using A as B"     =>  "(using (error (as (importpath A) B)))"
        "using A, B as C"  =>  "(using (importpath A) (error (as (importpath B) C)))"
        # parse_import_path
        # When parsing import we must split initial dots into nontrivial
        # leading dots for relative paths
        "import .A"     =>  "(import (importpath . A))"
        "import ..A"    =>  "(import (importpath . . A))"
        "import ...A"   =>  "(import (importpath . . . A))"
        "import ....A"  =>  "(import (importpath . . . . A))"
        # Dots with spaces are allowed (a misfeature?)
        "import . .A"   =>  "(import (importpath . . A))"
        # Modules with operator symbol names
        "import .⋆"     =>  "(import (importpath . ⋆))"
        # Expressions allowed in import paths
        "import @x"     =>  "(import (importpath @x))"
        "import \$A"    =>  "(import (importpath (\$ A)))"
        "import \$A.@x" =>  "(import (importpath (\$ A) @x))"
        "import A.B"    =>  "(import (importpath A B))"
        "import A.B.C"  =>  "(import (importpath A B C))"
        "import A.:+"   =>  "(import (importpath A (quote-: +)))"
        "import A.(:+)" =>  "(import (importpath A (parens (quote-: +))))"
        "import A.:(+)" =>  "(import (importpath A (quote-: (parens +))))"
        "import A.=="   =>  "(import (importpath A ==))"
        "import A.⋆.f"  =>  "(import (importpath A ⋆ f))"
        "import A..."   =>  "(import (importpath A ..))"
        "import A; B"   =>  "(import (importpath A))"
    ],
    JuliaSyntax.parse_iteration_spec => [
        "i = rhs"        =>  "(= i rhs)"
        "i in rhs"       =>  "(= i rhs)"
        "i ∈ rhs"        =>  "(= i rhs)"
        "i = 1:10"       =>  "(= i (call-i 1 : 10))"
        "(i,j) in iter"  =>  "(= (tuple-p i j) iter)"
        "outer = rhs"       =>  "(= outer rhs)"
        "outer <| x = rhs"  =>  "(= (call-i outer <| x) rhs)"
        "outer i = rhs"     =>  "(= (outer i) rhs)"
        "outer (x,y) = rhs" =>  "(= (outer (tuple-p x y)) rhs)"
    ],
    JuliaSyntax.parse_paren => [
        # Tuple syntax with commas
        "()"          =>  "(tuple-p)"
        "(x,)"        =>  "(tuple-p x)"
        "(x,y)"       =>  "(tuple-p x y)"
        "(x=1, y=2)"  =>  "(tuple-p (= x 1) (= y 2))"
        # Named tuples with initial semicolon
        "(;)"         =>  "(tuple-p (parameters))"
        "(; a=1)"     =>  "(tuple-p (parameters (= a 1)))"
        # Extra credit: nested parameters and frankentuples
        "(x...; y)"       => "(tuple-p (... x) (parameters y))"
        "(x...;)"         => "(tuple-p (... x) (parameters))"
        "(; a=1; b=2)"    => "(tuple-p (parameters (= a 1)) (parameters (= b 2)))"
        "(a; b; c,d)"     => "(tuple-p a (parameters b) (parameters c d))"
        "(a=1, b=2; c=3)" => "(tuple-p (= a 1) (= b 2) (parameters (= c 3)))"
        # Block syntax
        "(;;)"        =>  "(block-p)"
        "(a=1;)"      =>  "(block-p (= a 1))"
        "(a;b;;c)"    =>  "(block-p a b c)"
        "(a=1; b=2)"  =>  "(block-p (= a 1) (= b 2))"
        # Parentheses used for grouping
        "(a * b)"     =>  "(parens (call-i a * b))"
        "(a=1)"       =>  "(parens (= a 1))"
        "(x)"         =>  "(parens x)"
        "(a...)"      =>  "(parens (... a))"
        # Generators
        "(x for a in as)"       =>  "(parens (generator x (= a as)))"
        "(x \n\n for a in as)"  =>  "(parens (generator x (= a as)))"
        # Range parsing in parens
        "(1:\n2)" => "(parens (call-i 1 : 2))"
        "(1:2)" => "(parens (call-i 1 : 2))"
    ],
    JuliaSyntax.parse_atom => [
        # char literal
        "'a'"           =>  "(char 'a')"
        "'α'"           =>  "(char 'α')"
        "'\\xce\\xb1'"  =>  "(char 'α')"
        "'\\u03b1'"     =>  "(char 'α')"
        "'\\U1D7DA'"    =>  "(char '𝟚')"
        "'a"            =>  "(char 'a' (error-t))"
        "''"            =>  "(char (error))"
        "'"             =>  "(char (error))"
        # symbol/expression quote
        ":foo"   => "(quote-: foo)"
        # Literal colons
        ":)"     => ":"
        ": end"  => ":"
        # Whitespace after quoting colon
        ": foo"  => "(quote-: (error-t) foo)"
        ":\nfoo" => "(quote-: (error-t) foo)"
        # plain equals
        "="      => "(error =)"
        # Identifiers
        "xx"     => "xx"
        "x₁"     => "x₁"
        # var syntax
        """var"x" """   =>  "(var x)"
        # var syntax raw string unescaping
        "var\"\""          =>  "(var )"
        "var\"\\\"\""      =>  "(var \")"
        "var\"\\\\\\\"\""  =>  "(var \\\")"
        "var\"\\\\x\""     =>  "(var \\\\x)"
        # trailing syntax after var
        """var"x"+"""   =>  "(var x)"
        """var"x")"""   =>  "(var x)"
        """var"x"("""   =>  "(var x)"
        """var"x"end""" =>  "(var x (error-t))"
        """var"x"1"""   =>  "(var x (error-t))"
        """var"x"y"""   =>  "(var x (error-t))"
        # Standalone syntactic operators are errors
        "+="  =>  "(error +=)"
        "?"   =>  "(error ?)"
        ".+=" =>  "(error (. +=))"
        # Normal operators
        "+"  =>  "+"
        "~"  =>  "~"
        # Quoted syntactic operators allowed
        ":+="  =>  "(quote-: +=)"
        ":.="  =>  "(quote-: (. =))"
        ":.&&" =>  "(quote-: (. &&))"
        # Special symbols quoted
        ":end" => "(quote-: end)"
        ":(end)" => "(quote-: (parens (error-t)))"
        ":<:"  => "(quote-: <:)"
        # unexpect =
        "="    => "(error =)"
        # parse_cat
        "[]"        =>  "(vect)"
        "[x,]"      =>  "(vect x)"
        "[x\n,,]"   =>  "(vect x (error-t ✘))"
        "[x]"       =>  "(vect x)"
        "[x \n ]"   =>  "(vect x)"
        "[x \n, ]"  =>  "(vect x)"
        "[x"        =>  "(vect x (error-t))"
        "[x \n\n ]" =>  "(vect x)"
        "[x for a in as]"  =>  "(comprehension (generator x (= a as)))"
        "[x \n\n for a in as]"  =>  "(comprehension (generator x (= a as)))"
        # parse_generator
        "(x for a in as for b in bs)" => "(parens (generator x (= a as) (= b bs)))"
        "(x for a in as, b in bs)" => "(parens (generator x (cartesian_iterator (= a as) (= b bs))))"
        "(x for a in as, b in bs if z)" => "(parens (generator x (filter (cartesian_iterator (= a as) (= b bs)) z)))"
        "(x for a in as, b in bs for c in cs, d in ds)" => "(parens (generator x (cartesian_iterator (= a as) (= b bs)) (cartesian_iterator (= c cs) (= d ds))))"
        "(x for a in as for b in bs if z)" => "(parens (generator x (= a as) (filter (= b bs) z)))"
        "(x for a in as if z for b in bs)" => "(parens (generator x (filter (= a as) z) (= b bs)))"
        "[x for a = as for b = bs if cond1 for c = cs if cond2]"  =>  "(comprehension (generator x (= a as) (filter (= b bs) cond1) (filter (= c cs) cond2)))"
        "[x for a = as if begin cond2 end]"  =>  "(comprehension (generator x (filter (= a as) (block cond2))))"
        "[(x)for x in xs]"  =>  "(comprehension (generator (parens x) (error-t) (= x xs)))"
        "(x for a in as if z)" => "(parens (generator x (filter (= a as) z)))"
        # parse_vect
        "[x, y]"        =>  "(vect x y)"
        "[x, y]"        =>  "(vect x y)"
        "[x,\n y]"      =>  "(vect x y)"
        "[x\n, y]"      =>  "(vect x y)"
        "[x\n,, y]"     =>  "(vect x (error-t ✘ y))"
        "[x,y ; z]"     =>  "(vect x y (parameters z))"
        "[x=1, y=2]"    =>  "(vect (= x 1) (= y 2))"
        "[x=1, ; y=2]"  =>  "(vect (= x 1) (parameters (= y 2)))"
        # parse_paren
        ":(=)"  =>  "(quote-: (parens =))"
        ":(::)"  =>  "(quote-: (parens ::))"
        ":(::\n)" => "(quote-: (parens ::))"
        "(function f \n end)" => "(parens (function f))"
        # braces
        "{x y}"      =>  "(bracescat (row x y))"
        ((v=v"1.7",), "{x ;;; y}") =>  "(bracescat (nrow-3 x y))"
        # Macro names can be keywords
        "@end x" => "(macrocall @end x)"
        # __dot__ macro
        "@. x" => "(macrocall @. x)"
        # cmd strings
        "``"         =>  "(macrocall core_@cmd (cmdstring-r \"\"))"
        "`cmd`"      =>  "(macrocall core_@cmd (cmdstring-r \"cmd\"))"
        "```cmd```"  =>  "(macrocall core_@cmd (cmdstring-s-r \"cmd\"))"
        # literals
        "true" => "true"
        "42"   => "42"
        "1.0e-1000"   => "0.0"
        "0x123456789abcdefp+0" => "8.19855292164869e16"
        # closing tokens
        ")"    => "(error)"
    ],
    JuliaSyntax.parse_atom => [
        # Actually parse_array
        # Normal matrix construction syntax
        "[x y ; z w]"  =>  "(vcat (row x y) (row z w))"
        "[x y ; z w ; a b]"  =>  "(vcat (row x y) (row z w) (row a b))"
        "[x ; y ; z]"  =>  "(vcat x y z)"
        "[x;]"  =>  "(vcat x)"
        "[x y]"  =>  "(hcat x y)"
        # Early abort in array parsing
        "[x@y"   =>  "(hcat x (error-t ✘ y))"
        "[x@y]"  =>  "(hcat x (error-t ✘ y))"
        # Mismatched rows
        "[x y ; z]"  =>  "(vcat (row x y) z)"
        # Single elements in rows
        ((v=v"1.7",), "[x ; y ;; z ]")  =>  "(ncat-2 (nrow-1 x y) z)"
        ((v=v"1.7",), "[x  y ;;; z ]")  =>  "(ncat-3 (row x y) z)"
        # Higher dimensional ncat
        # Row major
        ((v=v"1.7",), "[x y ; z w ;;; a b ; c d]")  =>
            "(ncat-3 (nrow-1 (row x y) (row z w)) (nrow-1 (row a b) (row c d)))"
        # Column major
        ((v=v"1.7",), "[x ; y ;; z ; w ;;; a ; b ;; c ; d]")  =>
            "(ncat-3 (nrow-2 (nrow-1 x y) (nrow-1 z w)) (nrow-2 (nrow-1 a b) (nrow-1 c d)))"
        # Array separators
        # Newlines before semicolons are not significant
        "[a \n ;]"  =>  "(vcat a)"
        # Newlines after semicolons are not significant
        "[a ; \n]"  =>  "(vcat a)"
        "[a ; \n\n b]"  =>  "(vcat a b)"
        ((v=v"1.7",), "[a ;; \n b]")  =>  "(ncat-2 a b)"
        # In hcat with spaces as separators, `;;` is a line
        # continuation character
        ((v=v"1.7",), "[a b ;; \n c]")  =>  "(hcat a b c)"
        ((v=v"1.7",), "[a b \n ;; c]")  =>  "(ncat-2 (row a b (error-t)) c)"
        # Can't mix spaces and multiple ;'s
        ((v=v"1.7",), "[a b ;; c]")  =>  "(ncat-2 (row a b (error-t)) c)"
        # Linebreaks not significant before closing `]`
        "[a b\n\n]" =>  "(hcat a b)"
        # Treat a linebreak prior to a value as a semicolon (ie, separator for
        # the first dimension) if no previous semicolons observed
        "[a \n b]"  =>  "(vcat a b)"
        # Can't mix multiple ;'s and spaces
        ((v=v"1.7",), "[a ;; b c]")  =>  "(ncat-2 a (row b (error-t) c))"
        # Empty nd arrays
        ((v=v"1.8",), "[;]")   =>  "(ncat-1)"
        ((v=v"1.8",), "[;;]")  =>  "(ncat-2)"
        ((v=v"1.8",), "[\n  ;; \n ]")  =>  "(ncat-2)"
        ((v=v"1.7",), "[;;]")  =>  "(ncat-2 (error))"
        # parse_string
        "\"\"\"\n\$x\n a\"\"\"" => "(string-s x \"\\n\" \" a\")"
        "\"a \$(x + y) b\""  =>  "(string \"a \" (parens (call-i x + y)) \" b\")"
        "\"hi\$(\"ho\")\""   =>  "(string \"hi\" (parens (string \"ho\")))"
        "\"\$(x,y)\""        =>  "(string (parens (error x y)))"
        "\"\$(x;y)\""        =>  "(string (parens (error x y)))"
        "\"\$(x for y in z)\"" => "(string (parens (error (generator x (= y z)))))"
        "\"\$((x for y in z))\"" => "(string (parens (parens (generator x (= y z)))))"
        "\"\$(xs...)\""  =>  "(string (parens (... xs)))"
        "\"a \$foo b\""  =>  "(string \"a \" foo \" b\")"
        "\"\$var\""      =>  "(string var)"
        "\"\$outer\""    =>  "(string outer)"
        "\"\$in\""       =>  "(string in)"
        # Triple-quoted dedenting:
        "\"\"\"\nx\"\"\""   =>  raw"""(string-s "x")"""
        "\"\"\"\n\nx\"\"\"" =>  raw"""(string-s "\n" "x")"""
        "```\n x\n y```"    =>  raw"""(macrocall core_@cmd (cmdstring-s-r "x\n" "y"))"""
        # Various newlines (\n \r \r\n) and whitespace (' ' \t)
        "\"\"\"\n x\n y\"\"\""  =>  raw"""(string-s "x\n" "y")"""
        "\"\"\"\r x\r y\"\"\""  =>  raw"""(string-s "x\n" "y")"""
        "\"\"\"\r\n x\r\n y\"\"\""  =>  raw"""(string-s "x\n" "y")"""
        # Spaces or tabs or mixtures acceptable
        "\"\"\"\n\tx\n\ty\"\"\""  =>  raw"""(string-s "x\n" "y")"""
        "\"\"\"\n \tx\n \ty\"\"\""  =>  raw"""(string-s "x\n" "y")"""
        # Mismatched tab vs space not deindented
        # Find minimum common prefix in mismatched whitespace
        "\"\"\"\n\tx\n y\"\"\""  =>  raw"""(string-s "\tx\n" " y")"""
        "\"\"\"\n x\n  y\"\"\""  =>  raw"""(string-s "x\n" " y")"""
        "\"\"\"\n  x\n y\"\"\""  =>  raw"""(string-s " x\n" "y")"""
        "\"\"\"\n \tx\n  y\"\"\""  =>  raw"""(string-s "\tx\n" " y")"""
        "\"\"\"\n  x\n \ty\"\"\""  =>  raw"""(string-s " x\n" "\ty")"""
        # Empty lines don't affect dedenting
        "\"\"\"\n x\n\n y\"\"\""  =>  raw"""(string-s "x\n" "\n" "y")"""
        # Non-empty first line doesn't participate in deindentation
        "\"\"\" x\n y\"\"\""  =>  raw"""(string-s " x\n" "y")"""
        # Dedenting and interpolations
        "\"\"\"\n  \$a\n  \$b\"\"\""  =>  raw"""(string-s a "\n" b)"""
        "\"\"\"\n  \$a \n  \$b\"\"\""  =>  raw"""(string-s a " \n" b)"""
        "\"\"\"\n  \$a\n  \$b\n\"\"\""  =>  raw"""(string-s "  " a "\n" "  " b "\n")"""
        # Empty chunks after dedent are removed
        "\"\"\"\n \n \"\"\""  =>  "(string-s \"\\n\")"
        # Newline at end of string
        "\"\"\"\n x\n y\n\"\"\""  =>  raw"""(string-s " x\n" " y\n")"""
        # Empty strings, or empty after triple quoted processing
        "\"\""              =>  "(string \"\")"
        "\"\"\"\n  \"\"\""  =>  "(string-s \"\")"
        # Missing delimiter
        "\"str"  =>  "(string \"str\" (error-t))"
        # String interpolations
        "\"\$x\$y\$z\""  =>  "(string x y z)"
        "\"\$(x)\""  =>  "(string (parens x))"
        "\"\$x\""  =>  "(string x)"
        # Strings with embedded whitespace trivia
        "\"a\\\nb\""     =>  raw"""(string "a" "b")"""
        "\"a\\\rb\""     =>  raw"""(string "a" "b")"""
        "\"a\\\r\nb\""   =>  raw"""(string "a" "b")"""
        "\"a\\\n \tb\""  =>  raw"""(string "a" "b")"""
        # Strings with only a single valid string chunk
        "\"str\""     => "(string \"str\")"
        "\"a\\\n\""   => "(string \"a\")"
        "\"a\\\r\""   => "(string \"a\")"
        "\"a\\\r\n\"" => "(string \"a\")"
    ],
    JuliaSyntax.parse_atom => [
        # errors in literals
        "\"\\xqqq\""  =>  "(string (ErrorInvalidEscapeSequence))"
        "'\\xq'"      =>  "(char (ErrorInvalidEscapeSequence))"
        "'ab'"        =>  "(char (ErrorOverLongCharacter))"
        "\"\xf5\""    =>  "(string (ErrorInvalidUTF8))"
        "'\xf5'"      =>  "(char (ErrorInvalidUTF8))"
        "`\xf5`"      =>  "(macrocall core_@cmd (cmdstring-r (ErrorInvalidUTF8)))"
        "10.0e1000'"  =>  "(ErrorNumericOverflow)"
        "10.0f100'"   =>  "(ErrorNumericOverflow)"
    ],
    JuliaSyntax.parse_stmts => with_version.(v"1.11", [
        "function f(public)\n    public + 3\nend"       => "(function (call f public) (block (call-i public + 3)))"
        "public A, B"                                   => "(public A B)"
        "if true \n public *= 4 \n end"                 => "(if true (block (*= public 4)))"
        "module Mod\n public A, B \n end"               => "(module Mod (block (public A B)))"
        "module Mod2\n a = 3; b = 6; public a, b\n end" => "(module Mod2 (block (= a 3) (= b 6) (public a b)))"
        "a = 3; b = 6; public a, b"                     => "(toplevel-; (= a 3) (= b 6) (public a b))"
        "begin \n public A, B \n end"                   => PARSE_ERROR
        "if true \n public A, B \n end"                 => PARSE_ERROR
        "public export=true foo, bar"                   => PARSE_ERROR # but these may be
        "public experimental=true foo, bar"             => PARSE_ERROR # supported soon ;)
        "public(x::String) = false"                     => "(= (call public (::-i x String)) false)"
        "module M; export @a; end"                      => "(module M (block (export @a)))"
        "module M; public @a; end"                      => "(module M (block (public @a)))"
        "module M; export ⤈; end"                       => "(module M (block (export ⤈)))"
        "module M; public ⤈; end"                       => "(module M (block (public ⤈)))"
        "public = 4"                                    => "(= public 4)"
        "public[7] = 5"                                 => "(= (ref public 7) 5)"
        "public() = 6"                                  => "(= (call public) 6)"
    ]),
    JuliaSyntax.parse_docstring => [
        """ "notdoc" ]        """ => "(string \"notdoc\")"
        """ "notdoc" \n]      """ => "(string \"notdoc\")"
        """ "notdoc" \n\n foo """ => "(string \"notdoc\")"
        """ "doc" \n foo      """ => """(doc (string "doc") foo)"""
        """ "doc" foo         """ => """(doc (string "doc") foo)"""
        """ "doc \$x" foo     """ => """(doc (string "doc " x) foo)"""
        # Allow docstrings with embedded trailing whitespace trivia
        "\"\"\"\n doc\n \"\"\" foo"  => """(doc (string-s "doc\\n") foo)"""
    ],
]

@testset "Inline test cases" begin
    @testset "$production" for (production, test_specs) in tests
        @testset "$(repr(input))" for (input, output) in test_specs
            test_parse(production, input, output)
        end
    end
end

parsestmt_test_specs = [
    # whitespace before keywords in space-insensitive mode
    "(y::\nif x z end)" => "(parens (::-i y (if x (block z))))"
    # Contextual keyword pairs inside parentheses
    "(abstract type X end)" => "(parens (abstract X))"
    "(mutable struct X end)" => "(parens (struct-mut X (block)))"
    # parsing of tricky primes
    "x in'c'"   => "(call-i x in (char 'c'))"
    "1where'c'" => "(where 1 (char 'c'))"
    ":+'y'"     => "(juxtapose (call-post (quote-: +) ') (call-post y '))"
    # unary subtype ops and newlines
    "a +\n\n<:" => "(call-i a + <:)"
    "for\n\n<:" => "(for (= <: (error (error-t))) (block (error)) (error-t))"
    # Empty character consumes trailing ' delimiter (ideally this could be
    # tested above but we don't require the input stream to be consumed in the
    # unit tests there.
    "''" => "(char (error))"

    # The following may not be ideal error recovery! But at least the parser
    # shouldn't crash
    "@(x y)" => "(macrocall (parens @x (error-t y)))"
    "|(&\nfunction" => "(call | (& (function (error (error)) (block (error)) (error-t))) (error-t))"
    "@(" => "(macrocall (parens (error-t)))"
    "x = @(" => "(= x (macrocall (parens (error-t))))"
    "function(where" => "(function (tuple-p where (error-t)) (block (error)) (error-t))"
    # Contextual keyword pairs must not be separated by newlines even within parens
    "(abstract\ntype X end)" => "(wrapper (parens abstract (error-t type X)) (error-t end ✘))"
    "(mutable\nstruct X end)" => "(wrapper (parens mutable (error-t struct X)) (error-t end ✘))"

    # Lexer vs parser: issues detecting which tokens are string delimiters and
    # detecting raw vs non-raw strings. The old parser was tightly coupled to
    # the lexer and the parser state was used to disambiguate these cases.
    "x in' '" => "(call-i x in (char (error)))"
    "x in'``\$" => "(call-i x in (call-i (juxtapose (char '`' (error-t)) (macrocall core_@cmd (cmdstring-r (error-t)))) \$ (error)))"
    "var\"#\"`str`" => "(juxtapose (var # (error-t)) (macrocall core_@cmd (cmdstring-r \"str\")))"
    "var\"#\"\"str\"" => "(juxtapose (var # (error-t)) (error-t) (string \"str\"))"
]

@testset "Parser does not crash on broken code" begin
    @testset "$(repr(input))" for (input, output) in parsestmt_test_specs
        test_parse(JuliaSyntax.parse_stmts, input, output)
    end
end

@testset "Trivia attachment" begin
    # TODO: Need to expand this greatly to cover as many forms as possible!
    @test show_green_tree("f(a;b)") == """
         1:6      │[toplevel]
         1:6      │  [call]
         1:1      │    Identifier           ✔   "f"
         2:2      │    (                        "("
         3:3      │    Identifier           ✔   "a"
         4:5      │    [parameters]
         4:4      │      ;                      ";"
         5:5      │      Identifier         ✔   "b"
         6:6      │    )                        ")"
    """
end

@testset "Unicode normalization in tree conversion" begin
    # ɛµ normalizes to εμ
    @test parse_to_sexpr_str(JuliaSyntax.parse_eq, "\u025B\u00B5()") == "(call \u03B5\u03BC)"
    @test parse_to_sexpr_str(JuliaSyntax.parse_eq, "@\u025B\u00B5") == "(macrocall @\u03B5\u03BC)"
    @test parse_to_sexpr_str(JuliaSyntax.parse_eq, "\u025B\u00B5\"\"") == "(macrocall @\u03B5\u03BC_str (string-r \"\"))"
    @test parse_to_sexpr_str(JuliaSyntax.parse_eq, "\u025B\u00B5``") == "(macrocall @\u03B5\u03BC_cmd (cmdstring-r \"\"))"
    # · and · normalize to ⋅
    @test parse_to_sexpr_str(JuliaSyntax.parse_eq, "a \u00B7 b") == "(call-i a \u22C5 b)"
    @test parse_to_sexpr_str(JuliaSyntax.parse_eq, "a \u0387 b") == "(call-i a \u22C5 b)"
    # − normalizes to -
    @test parse_to_sexpr_str(JuliaSyntax.parse_expr, "a \u2212 b")  == "(call-i a - b)"
    @test parse_to_sexpr_str(JuliaSyntax.parse_eq, "a \u2212= b") == "(-= a b)"
    @test parse_to_sexpr_str(JuliaSyntax.parse_eq, "a .\u2212= b") == "(.-= a b)"
end

@testset "Unbalanced bidirectional unicode" begin
    # https://trojansource.codes
    @test_throws JuliaSyntax.ParseError parsestmt(GreenNode, """
    function checkUserAccess(u::User)
        if u.accessLevel != "user\u202e \u2066# users are not allowed\u2069\u2066"
            return true
        end
        return false
    end
    """)

    @test_throws JuliaSyntax.ParseError parsestmt(GreenNode, """
    function checkUserAccess(u::User)
        #=\u202e \u2066if (u.isAdmin)\u2069 \u2066 begin admins only =#
            return true
        #= end admin only \u202e \u2066end\u2069 \u2066=#
        return false
    end
    """)
end
