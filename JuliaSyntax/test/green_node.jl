@testset "GreenNode" begin
    t = parsestmt(GreenNode, "aa + b")

    @test span(t) == 6
    @test haschildren(t)
    @test head(t) == SyntaxHead(K"call", 0x0008)
    @test span.(children(t)) == [2,1,1,1,1]
    @test head.(children(t)) == [
         SyntaxHead(K"Identifier", 0x0000)
         SyntaxHead(K"Whitespace", 0x0001)
         SyntaxHead(K"+", 0x0000)
         SyntaxHead(K"Whitespace", 0x0001)
         SyntaxHead(K"Identifier", 0x0000)
    ]

    t2 = parsestmt(GreenNode, "aa + b")
    @test t == t2
    @test t !== t2

    text = "f(@x(y), z)"
    @test sprint(show, MIME("text/plain"), parsestmt(GreenNode, text)) ==
    """
         1:11     │[call]
         1:1      │  Identifier             ✔
         2:2      │  (
         3:7      │  [macrocall]
         3:3      │    @
         4:4      │    MacroName            ✔
         5:5      │    (
         6:6      │    Identifier           ✔
         7:7      │    )
         8:8      │  ,
         9:9      │  Whitespace
        10:10     │  Identifier             ✔
        11:11     │  )
    """

    @test sprint(show, MIME("text/plain"), parsestmt(GreenNode, text), text) ==
    """
         1:11     │[call]
         1:1      │  Identifier             ✔   "f"
         2:2      │  (                          "("
         3:7      │  [macrocall]
         3:3      │    @                        "@"
         4:4      │    MacroName            ✔   "x"
         5:5      │    (                        "("
         6:6      │    Identifier           ✔   "y"
         7:7      │    )                        ")"
         8:8      │  ,                          ","
         9:9      │  Whitespace                 " "
        10:10     │  Identifier             ✔   "z"
        11:11     │  )                          ")"
    """
end
