function _unwrap_parse_error(core_hook_result)
    @test Meta.isexpr(core_hook_result[1], :error, 1)
    err = core_hook_result[1].args[1]
    if JuliaSyntax._has_v1_10_hooks
        @test err isa Meta.ParseError
        return err.detail
    else
        @test err isa JuliaSyntax.ParseError
        return err
    end
end

@testset "Hooks for Core integration" begin
    @testset "whitespace and comment parsing" begin
        @test JuliaSyntax.core_parser_hook("", "somefile", 1, 0, :statement) == Core.svec(nothing, 0)
        @test JuliaSyntax.core_parser_hook("", "somefile", 1, 0, :statement) == Core.svec(nothing, 0)

        @test JuliaSyntax.core_parser_hook("  ", "somefile", 1, 2, :statement) == Core.svec(nothing,2)
        @test JuliaSyntax.core_parser_hook(" #==# ", "somefile", 1, 6, :statement) == Core.svec(nothing,6)

        @test JuliaSyntax.core_parser_hook(" x \n", "somefile", 1, 0, :statement) == Core.svec(:x,4)
        @test JuliaSyntax.core_parser_hook(" x \n", "somefile", 1, 0, :atom)      == Core.svec(:x,2)

        # https://github.com/JuliaLang/JuliaSyntax.jl/issues/316#issuecomment-1870294857
        stmtstr =
            """
            plus(a, b) = a + b

            # Issue #81
            f() = nothing
            """
        @test JuliaSyntax.core_parser_hook(stmtstr, "somefile", 1, 0, :statement)[2] == 19
    end

    @testset "filename and lineno" begin
        ex = JuliaSyntax.core_parser_hook("@a", "somefile", 1, 0, :statement)[1]
        @test Meta.isexpr(ex, :macrocall)
        @test ex.args[2] == LineNumberNode(1, "somefile")

        ex = JuliaSyntax.core_parser_hook("@a", "otherfile", 2, 0, :statement)[1]
        @test ex.args[2] == LineNumberNode(2, "otherfile")

        # Errors also propagate file & lineno
        err = _unwrap_parse_error(
            JuliaSyntax.core_parser_hook("[x)", "f1", 1, 0, :statement)
        )
        @test err isa JuliaSyntax.ParseError
        @test filename(err) == "f1"
        @test err.source.first_line == 1
        err = _unwrap_parse_error(
            JuliaSyntax.core_parser_hook("[x)", "f2", 2, 0, :statement)
        )
        @test err isa JuliaSyntax.ParseError
        @test filename(err) == "f2"
        @test err.source.first_line == 2

        # Errors including nontrivial offset indices
        err = _unwrap_parse_error(
            JuliaSyntax.core_parser_hook("a\nh{x)\nb", "test.jl", 1, 2, :statement)
        )
        @test err isa JuliaSyntax.ParseError
        @test err.source.first_line == 1
        @test err.diagnostics[1].first_byte == 6
        @test err.diagnostics[1].last_byte == 5
        @test err.diagnostics[1].message == "Expected `}` or `,`"
    end

    @testset "toplevel errors" begin
        ex = JuliaSyntax.core_parser_hook("a\nb\n[x,\ny)", "somefile", 1, 0, :all)[1]
        @test ex.head == :toplevel
        @test ex.args[1:5] == [
            LineNumberNode(1, "somefile"),
            :a,
            LineNumberNode(2, "somefile"),
            :b,
            LineNumberNode(4, "somefile"),
        ]
        @test Meta.isexpr(ex.args[6], :error)

        ex = JuliaSyntax.core_parser_hook("x.", "somefile", 0, 0, :all)[1]
        @test ex.head == :toplevel
        @test ex.args[2].head == :incomplete
    end

    @testset "enable_in_core!" begin
        JuliaSyntax.enable_in_core!()

        @test Meta.parse("x + 1") == :(x + 1)
        @test Meta.parse("x + 1", 1) == (:(x + 1), 6)

        # Test that parsing statements incrementally works and stops after
        # whitespace / comment trivia
        @test Meta.parse("x + 1\n(y)\n", 1) == (:(x + 1), 7)
        @test Meta.parse("x + 1\n(y)\n", 7) == (:y, 11)
        @test Meta.parse(" x#==#", 1) == (:x, 7)
        @test Meta.parse(" #==# ", 1) == (nothing, 7)

        # Check the exception type that Meta.parse throws
        if JuliaSyntax._has_v1_10_hooks
            @test_throws Meta.ParseError Meta.parse("[x)")
            @test_throws Meta.ParseError eval(Meta.parse("[x)", raise=false))
            @test_throws Meta.ParseError eval(Meta.parse("(x")) # Expr(:incomplete)
        else
            @test_throws JuliaSyntax.ParseError Meta.parse("[x)")
        end

        # Check custom string types defined in a world age later than
        # enable_in_core!() can be passed to Meta.parse()
        mystr = @eval begin
            struct MyString <: AbstractString
                x::String
            end
            Base.String(s::MyString) = s.x
            Base.ncodeunits(s::MyString) = ncodeunits(s.x)

            MyString("hi")
        end
        @test Meta.parse(mystr) == :hi

        err = Meta.parse("\"")
        @test Meta.isexpr(err, :incomplete)
        if JuliaSyntax._has_v1_10_hooks
            @test err.args[1] isa Meta.ParseError
            exc = err.args[1]
            @test exc.msg == "ParseError:\n# Error @ none:1:2\n\"\n#└ ── unterminated string literal"
            @test exc.detail isa JuliaSyntax.ParseError
            @test exc.detail.incomplete_tag === :string
        else
            @test err.args[1] isa String
        end

        JuliaSyntax.enable_in_core!(false)
    end

    @testset "Expr(:incomplete)" begin
        for (str, tag) in [
                "\""           => :string
                "\"\$foo"      => :string
                "#="           => :comment
                "'"            => :char
                "'a"           => :char
                "`"            => :cmd
                "("            => :other
                "["            => :other
                "begin"        => :block
                "quote"        => :block
                "let"          => :block
                "let;"         => :block
                "for"          => :other
                "for x=xs"     => :block
                "function"     => :other
                "function f()" => :block
                "macro"        => :other
                "macro f()"    => :block
                "f() do"       => :other
                "f() do x"     => :block
                "module"       => :other
                "module X"     => :block
                "baremodule"   => :other
                "baremodule X" => :block
                "mutable struct"    => :other
                "mutable struct X"  => :block
                "struct"       => :other
                "struct X"     => :block
                "if"           => :other
                "if x"         => :block
                "while"        => :other
                "while x"      => :block
                "try"          => :block
                # could be `try x catch exc body end` or `try x catch ; body end`
                "try x catch"  => :block
                "using"        => :other
                "import"       => :other
                "local"        => :other
                "global"       => :other

                "1 == 2 ?"     => :other
                "1 == 2 ? 3 :" => :other
                "1,"           => :other
                "1, "          => :other
                "1,\n"         => :other
                "1, \n"        => :other
                "f(1, "        => :other
                "[x "          => :other
                "( "           => :other

                # Reference parser fails to detect incomplete exprs in this case
                "(x for y"     => :other

                # Syntax which may be an error but is not incomplete
                ""             => :none
                ")"            => :none
                "1))"          => :none
                "a b"          => :none
                "()x"          => :none
                "."            => :none

                # Some error tokens which cannot be made complete by appending more characters
                "1.e1."        => :none
                "\u200b"       => :none
                "x #=\xf5b\n=#" => :none
                "₁"            => :none
                "0x1.0\n"      => :none
                "\"\$x෴\""     => :none
                "10e1000"      => :none

                # Multiline input with comments (#519)
                "function f()\nbody #comment" => :block
                "a = [\n1,\n2, #comment"      => :other

                # Extended set of cases extracted from the REPL stdlib tests.
                # There is some redundancy here, but we've mostly left these
                # here because incomplete-detection is partly heuristic and
                # it's good to have a wide variety of incomplete expressions.
                #
                # The "desired" incomplete tag here was generated from the
                # flisp parser.
                "Main.CompletionFoo." => :other
                "Base.return_types(getin" => :other
                "test7()." => :other
                "(3,2)." => :other
                "Base.print(\"lol" => :string
                "run(`lol" => :cmd
                "copy(A')." => :other
                "cd(\"path_to_an_empty_folder_should_not_complete_latex\\\\\\alpha" => :string
                "\"C:\\\\ \\alpha" => :string
                "cd(\"C:\\U" => :string
                "max(" => :other
                "!(" => :other
                "!isnothing(" => :other
                "!!isnothing(" => :other
                "CompletionFoo.test(1, 1, " => :other
                "CompletionFoo.test(CompletionFoo.array," => :other
                "CompletionFoo.test(1,1,1," => :other
                "CompletionFoo.test1(Int," => :other
                "CompletionFoo.test1(Float64," => :other
                "prevind(\"θ\",1," => :other
                "(1, CompletionFoo.test2(\")\"," => :other
                "(1, CompletionFoo.test2(')'," => :other
                "(1, CompletionFoo.test2(`')'`," => :other
                "CompletionFoo.test3([1, 2] .+ CompletionFoo.varfloat," => :other
                "CompletionFoo.test3([1.,2.], 1.," => :other
                "CompletionFoo.test4(\"e\",r\" \"," => :other
                "CompletionFoo.test5(broadcast((x,y)->x==y, push!(Base.split(\"\",' '),\"\",\"\"), \"\")," => :other
                "CompletionFoo.test5(Bool[x==1 for x=1:4]," => :other
                "CompletionFoo.test4(CompletionFoo.test_y_array[1]()[1], CompletionFoo.test_y_array[1]()[2], " => :other
                "CompletionFoo.test4(\"\\\"\"," => :other
                "convert(" => :other
                "convert(" => :other
                "CompletionFoo.test5(AbstractArray[Bool[]][1]," => :other
                "CompletionFoo.test3(@time([1, 2] .+ CompletionFoo.varfloat)," => :other
                "CompletionFoo.kwtest( " => :other
                "CompletionFoo.kwtest(;" => :other
                "CompletionFoo.kwtest(; x=1, " => :other
                "CompletionFoo.kwtest(; kw=1, " => :other
                "CompletionFoo.kwtest(x=1, " => :other
                "CompletionFoo.kwtest(x=1; " => :other
                "CompletionFoo.kwtest(x=kw=1, " => :other
                "CompletionFoo.kwtest(; x=kw=1, " => :other
                "CompletionFoo.kwtest2(1, x=1," => :other
                "CompletionFoo.kwtest2(1; x=1, " => :other
                "CompletionFoo.kwtest2(1, x=1; " => :other
                "CompletionFoo.kwtest2(1, kw=1, " => :other
                "CompletionFoo.kwtest2(1; kw=1, " => :other
                "CompletionFoo.kwtest2(1, kw=1; " => :other
                "CompletionFoo.kwtest2(y=3, 1, " => :other
                "CompletionFoo.kwtest2(y=3, 1; " => :other
                "CompletionFoo.kwtest2(kw=3, 1, " => :other
                "CompletionFoo.kwtest2(kw=3, 1; " => :other
                "CompletionFoo.kwtest2(1; " => :other
                "CompletionFoo.kwtest2(1, " => :other
                "CompletionFoo.kwtest4(x23=18, x; " => :other
                "CompletionFoo.kwtest4(x23=18, x, " => :other
                "CompletionFoo.kwtest4(x23=18, " => :other
                "CompletionFoo.kwtest5(3, somekwarg=6," => :other
                "CompletionFoo.kwtest5(3, somekwarg=6, anything, " => :other
                "CompletionFoo.?([1,2,3], 2.0" => :other
                "CompletionFoo.?('c'" => :other
                "CompletionFoo.?(false, \"a\", 3, " => :other
                "CompletionFoo.?(false, \"a\", 3, " => :other
                "CompletionFoo.?(\"a\", 3, " => :other
                "CompletionFoo.?(; " => :other
                "CompletionFoo.?(" => :other
                "CompletionFoo.test10(z, Integer[]...," => :other
                "CompletionFoo.test10(3, Integer[]...," => :other
                "CompletionFoo.test10(3, 4," => :other
                "CompletionFoo.test10(3, 4, 5," => :other
                "CompletionFoo.test10(z, z, 0, " => :other
                "CompletionFoo.test10(\"a\", Union{Signed,Bool,String}[3][1], " => :other
                "CompletionFoo.test11(Integer[false][1], Integer[14][1], " => :other
                "CompletionFoo.test11(Integer[-7][1], Integer[0x6][1], 6," => :other
                "CompletionFoo.test11(3, 4," => :other
                "CompletionFoo.test11(0x8, 5," => :other
                "CompletionFoo.test11(0x8, 'c'," => :other
                "CompletionFoo.test11('d', 3," => :other
                "CompletionFoo.test!12(" => :other
                "CompletionFoo.kwtest(; x=2, y=4; kw=3, " => :other
                "CompletionFoo.kwtest(x=2; y=4; " => :other
                "CompletionFoo.kwtest((x=y)=4, " => :other
                "CompletionFoo.kwtest(; (x=y)=4, " => :other
                "CompletionFoo.kwtest(; w...=16, " => :other
                "CompletionFoo.kwtest(; 2, " => :other
                "CompletionFoo.kwtest(; 2=3, " => :other
                "CompletionFoo.kwtest3(im; (true ? length : length), " => :other
                "CompletionFoo.kwtest.(x=2; y=4; " => :other
                "CompletionFoo.kwtest.(; w...=16, " => :other
                "(1+2im)." => :other
                "((1+2im))." => :other
                "CompletionFoo.test_y_array[1]." => :other
                "CompletionFoo.named." => :other
                "#=\n\\alpha" => :comment
                "#=\nmax" => :comment
                "using " => :other
                "(max" => :other
                "@show \"/dev/nul" => :string
                "@show \"/tm" => :string
                "@show \"/dev/nul" => :string
                "(Iter" => :other
                "\"/tmp/jl_4sjOtz/tmpfoob" => :string
                "\"~" => :string
                "\"~user" => :string
                "\"/tmp/jl_Mn9Rbz/selfsym" => :string
                "\"~/ka8w5rsz" => :string
                "\"foo~bar" => :string
                "\"~/Zx6Wa0GkC" => :string
                "\"~/Zx6Wa0GkC0" => :string
                "\"~/Zx6Wa0GkC0/my_" => :string
                "\"~/Zx6Wa0GkC0/my_file" => :string
                "cd(\"folder_do_not_exist_77/file" => :string
                "CompletionFoo.tuple." => :other
                "CompletionFoo.test_dict[\"ab" => :string
                "CompletionFoo.test_dict[\"abcd" => :string
                "CompletionFoo.test_dict[ \"abcd" => :string
                "CompletionFoo.test_dict[\"abcd" => :string
                "CompletionFoo.test_dict[:b" => :other
                "CompletionFoo.test_dict[:bar2" => :other
                "CompletionFoo.test_dict[Ba" => :other
                "CompletionFoo.test_dict[occ" => :other
                "CompletionFoo.test_dict[`l" => :cmd
                "CompletionFoo.test_dict[6" => :other
                "CompletionFoo.test_dict[66" => :other
                "CompletionFoo.test_dict[(" => :other
                "CompletionFoo.test_dict[\"\\alp" => :string
                "CompletionFoo.test_dict[\"\\alpha" => :string
                "CompletionFoo.test_dict[\"α" => :string
                "CompletionFoo.test_dict[:α" => :other
                "CompletionFoo.test_dict[" => :other
                "CompletionFoo.test_customdict[\"ab" => :string
                "CompletionFoo.test_customdict[\"abcd" => :string
                "CompletionFoo.test_customdict[ \"abcd" => :string
                "CompletionFoo.test_customdict[\"abcd" => :string
                "CompletionFoo.test_customdict[:b" => :other
                "CompletionFoo.test_customdict[:bar2" => :other
                "CompletionFoo.test_customdict[Ba" => :other
                "CompletionFoo.test_customdict[occ" => :other
                "CompletionFoo.test_customdict[`l" => :cmd
                "CompletionFoo.test_customdict[6" => :other
                "CompletionFoo.test_customdict[66" => :other
                "CompletionFoo.test_customdict[(" => :other
                "CompletionFoo.test_customdict[\"\\alp" => :string
                "CompletionFoo.test_customdict[\"\\alpha" => :string
                "CompletionFoo.test_customdict[\"α" => :string
                "CompletionFoo.test_customdict[:α" => :other
                "CompletionFoo.test_customdict[" => :other
                "test_repl_comp_dict[\"ab" => :string
                "test_repl_comp_dict[\"abcd" => :string
                "test_repl_comp_dict[ \"abcd" => :string
                "test_repl_comp_dict[\"abcd" => :string
                "test_repl_comp_dict[:b" => :other
                "test_repl_comp_dict[:bar2" => :other
                "test_repl_comp_dict[Ba" => :other
                "test_repl_comp_dict[occ" => :other
                "test_repl_comp_dict[`l" => :cmd
                "test_repl_comp_dict[6" => :other
                "test_repl_comp_dict[66" => :other
                "test_repl_comp_dict[(" => :other
                "test_repl_comp_dict[\"\\alp" => :string
                "test_repl_comp_dict[\"\\alpha" => :string
                "test_repl_comp_dict[\"α" => :string
                "test_repl_comp_dict[:α" => :other
                "test_repl_comp_dict[" => :other
                "test_repl_comp_customdict[\"ab" => :string
                "test_repl_comp_customdict[\"abcd" => :string
                "test_repl_comp_customdict[ \"abcd" => :string
                "test_repl_comp_customdict[\"abcd" => :string
                "test_repl_comp_customdict[:b" => :other
                "test_repl_comp_customdict[:bar2" => :other
                "test_repl_comp_customdict[Ba" => :other
                "test_repl_comp_customdict[occ" => :other
                "test_repl_comp_customdict[`l" => :cmd
                "test_repl_comp_customdict[6" => :other
                "test_repl_comp_customdict[66" => :other
                "test_repl_comp_customdict[(" => :other
                "test_repl_comp_customdict[\"\\alp" => :string
                "test_repl_comp_customdict[\"\\alpha" => :string
                "test_repl_comp_customdict[\"α" => :string
                "test_repl_comp_customdict[:α" => :other
                "test_repl_comp_customdict[" => :other
                "CompletionFoo.kwtest3(a;foob" => :other
                "CompletionFoo.kwtest3(a; le" => :other
                "CompletionFoo.kwtest3.(a;\nlength" => :other
                "CompletionFoo.kwtest3(a, length=4, l" => :other
                "CompletionFoo.kwtest3(a; kwargs..., fo" => :other
                "CompletionFoo.kwtest3(a; another!kwarg=0, le" => :other
                "CompletionFoo.kwtest3(a; another!" => :other
                "CompletionFoo.kwtest3(a; another!kwarg=0, foob" => :other
                "CompletionFoo.kwtest3(a; namedarg=0, foob" => :other
                "kwtest3(blabla; unknown=4, namedar" => :other
                "kwtest3(blabla; named" => :other
                "kwtest3(blabla; named." => :other
                "kwtest3(blabla; named..., another!" => :other
                "kwtest3(blabla; named..., len" => :other
                "kwtest3(1+3im; named" => :other
                "kwtest3(1+3im; named." => :other
                "CompletionFoo.kwtest4(a; x23=0, _" => :other
                "CompletionFoo.kwtest4(a; xαβγ=1, _" => :other
                "CompletionFoo.kwtest4.(a; xαβγ=1, _" => :other
                "CompletionFoo.kwtest4(a; x23=0, x" => :other
                "CompletionFoo.kwtest4.(a; x23=0, x" => :other
                "CompletionFoo.kwtest4(a; _a1b=1, x" => :other
                "CompletionFoo.kwtest5(3, 5; somek" => :other
                "CompletionFoo.kwtest5(3, 5, somekwarg=4, somek" => :other
                "CompletionFoo.kwtest5(3, 5, 7; somekw" => :other
                "CompletionFoo.kwtest5(3, 5, 7, 9; somekw" => :other
                "CompletionFoo.kwtest5(3, 5, 7, 9, Any[]...; somek" => :other
                "CompletionFoo.kwtest5(unknownsplat...; somekw" => :other
                "CompletionFoo.kwtest5(3, 5, 7, 9, somekwarg=4, somek" => :other
                "CompletionFoo.kwtest5(String[]..., unknownsplat...; xy" => :other
                "CompletionFoo.kwtest5('a', unknownsplat...; xy" => :other
                "CompletionFoo.kwtest5('a', 3, String[]...; xy" => :other
                "CompletionFoo.kwtest3(" => :other
                "CompletionFoo.kwtest3(a;" => :other
                "CompletionFoo.kwtest3(a; len2=" => :other
                "CompletionFoo.kwtest3(a; len2=le" => :other
                "CompletionFoo.kwtest3(a; len2=3 " => :other
                "CompletionFoo.kwtest3(a; [le" => :other
                "CompletionFoo.kwtest3([length; le" => :other
                "CompletionFoo.kwtest3(a; (le" => :other
                "CompletionFoo.kwtest3(a; foo(le" => :other
                "CompletionFoo.kwtest3(a; (; le" => :other
                "CompletionFoo.kwtest3(a; length, " => :other
                "CompletionFoo.kwtest3(a; kwargs..., " => :other
                ":(function foo(::Int) end).args[1].args[2]." => :other
                "log(log.(varfloat)," => :other
                "Base.return_types(getin" => :other
                "test(1,1, " => :other
                "test.(1,1, " => :other
                "prevind(\"θ\",1," => :other
                "typeof(+)." => :other
                "test_dict[\"ab" => :string
                "CompletionFoo.x." => :other
                "@noexist." => :other
                "Main.@noexist." => :none # <- Invalid syntax which adding a suffix can't fix
                "@Main.noexist." => :other
                "@show." => :other
                "@macroexpand." => :other
                "CompletionFoo.@foobar()." => :other
                "CompletionFoo.@foobar(4)." => :other
                "foo(#=#==#=##==#).rs[1]." => :other
                "foo().r." => :other
                "foo(#=#=# =#= =#).r." => :other
                "test_47594." => :other
                "Issue36437(42)." => :other
                "Some(Issue36437(42)).value." => :other
                "some_issue36437.value." => :other
                "some_issue36437.value.a, some_issue36437.value." => :other
                "@show some_issue36437.value.a; some_issue36437.value." => :other
                "()." => :other
                "Ref(Issue36437(42))[]." => :other
                "global_dict[:r]." => :other
                "global_dict_nested[:g][:r]." => :other
                "global_dict_nested[" => :other
                "global_dict_nested[:g][" => :other
                "pop!(global_xs)." => :other
                "tcd1." => :other
                "tcd1.x." => :other
                "tcd1.x.v." => :other
                "getkeyelem(mutable_const_prop)." => :other
                "getkeyelem(mutable_const_prop).value." => :other
                "var\"complicated " => :string
                "WeirdNames().var\"oh " => :string
                "WeirdNames().var\"" => :string
                "\"abc\"." => :other
                "(rand(Bool) ? issue51499_2_1 : issue51499_2_2)." => :other
                "union_somes(1, 1.0)." => :other
                "union_some_ref(1, 1.0)." => :other
                "Issue49892(fal" => :other
                "-CompletionFoo.Test_y(3)." => :other
                "99 ⨷⁻ᵨ⁷ CompletionFoo.type_test." => :other
                "CompletionFoo.type_test + CompletionFoo.Test_y(2)." => :other
                "(CompletionFoo.type_test + CompletionFoo.Test_y(2))." => :other
                "CompletionFoo.type_test + CompletionFoo.unicode_αβγ." => :other
                "(CompletionFoo.type_test + CompletionFoo.unicode_αβγ)." => :other
                "using Base." => :other
                "@time(using .Iss" => :other
                "using .Issue52922.Inner1." => :other
                "Issue53126()." => :other
                "using " => :other
                "global xxx::Number = Base." => :other
                "let x = 1 # comment" => :other
            ]
            @testset "$(repr(str))" begin
                # Test :statement parsing
                ex = JuliaSyntax.core_parser_hook(str, "somefile", 1, 0, :statement)[1]
                @test Base.incomplete_tag(ex) == tag
                # Test :all parsing - this is what the REPL uses to parse user input.
                ex = JuliaSyntax.core_parser_hook(str, "somefile", 1, 0, :all)[1]
                @test ex.head == :toplevel
                @test Base.incomplete_tag(ex.args[end]) == tag
            end
        end

        # Should not throw
        @test JuliaSyntax.core_parser_hook("+=", "somefile", 1, 0, :statement)[1] isa Expr
    end
end
