
# Make sure paired brackets parse
pairs = Any[
    ("[|","|]", symbol("@enclose_Brack"), symbol("@call_Brack")),
    ("⟦" , "⟧", symbol("@enclose_Brack"), symbol("@call_Brack")),
    ("{|","|}", symbol("@enclose_Brace"), symbol("@call_Brace")),
    ("⦃" , "⦄", symbol("@enclose_Brace"), symbol("@call_Brace")),
    ("⟪" , "⟫", symbol("@enclose_Angle"), symbol("@call_Angle")),
    ("❮" , "❯", symbol("@enclose_Angle"), symbol("@call_Angle")),
    ("❰" , "❱", symbol("@enclose_Angle"), symbol("@call_Angle"))
    ]

args = Any[
( "", 0 ),
( " ", 0 ),
( "1", 1 ),
( "a", 1 ),
( "a,", 1 ), # test trailing commas
( "a,b,", 2 ), # tst trailing commas
( "a,1", 2 ),
( "2,a", 2 ),
( "2,a,", 2 ),
]

for t in pairs
    left = t[1]
    right = t[2]
    expecthead = t[3]
    expectcallhead = t[4]
    for arg in args
        try
            str = left * arg[1] * right
            #println( str )
            ex = parse( str )
        catch er
            println( er )
            println( left * arg[1] * right )
            @test false
        end
        @test ex.head == :macrocall
        @test ex.args[1]== expecthead
        @test length( ex.args ) == arg[2]+1

        try
            str = "foo" * left * arg[1] * right
            #println( str )
            ex = parse( str )
        catch er
            println( er )
            println( "foo" * left * arg[1] * right )
            @test false
        end
        @test ex.head == :macrocall
        @test ex.args[1]== expectcallhead
        @test ex.args[2] == :foo
        @test length( ex.args ) == arg[2] + 2
    end

    # this is not a normal function call so kwargs with semi-colons do not work
    @test_throws( ParseError, parse( "foo" * left * "a,1;b=2" * right ))

    # however, commas separated assignment works
    ex = parse( "foo" * left * "a,1,b=2" * right )
    @test Base.Meta.isexpr( ex.args[5], :(=) )
end

# make sure similar but unmatched brackets do NOT parse
@test_throws( ParseError, parse( "⟪1⟩" ) )
@test_throws( ParseError, parse( "⟪1❯" ) )
@test_throws( ParseError, parse( "[|1⟧" ) )

# make sure ill-formed nesting throws
@test_throws( ParseError, parse( "[⟪1]⟫" ) )
@test_throws( ParseError, parse( "[| ⟪ 1 |] ⟫" ) )

# test deep nesting
ex = parse( "⟪ a * [| D{ c }(T) = {| foobar |} |] ⟫" )
@test ex.args[1] == symbol( "@enclose_Angle" )
@test ex.args[2].args[3].args[1] == symbol( "@enclose_Brack" )
@test ex.args[2].args[3].args[2].args[2].args[1]== symbol( "@enclose_Brace" )

# we may bestow meaning to such expressions later but for now they throw
@test_throws( ArgumentError, eval( parse( "{| 1 |}")) )
@test_throws( ArgumentError, eval( parse( "b{| 1 |}")) )
@test_throws( ArgumentError, eval( parse( "[| 1 |]")) )
@test_throws( UndefVarError, eval( parse( "non_existent_value[| 1 |]")) )
@test_throws( ArgumentError, eval( parse( "⟪1⟫")) )
@test_throws( UndefVarError, eval( parse( "non_existent_value⟪1⟫")) )

# However, this should just throw a standard exception
@test_throws( UndefVarError, eval( parse( "{| non_existent_value |}")) )
