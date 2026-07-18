@testset "GreenNode" begin
    t = parsestmt(GreenNode, "aa + b")

    @test span(t) == 6
    @test !is_leaf(t)
    @test head(t) == SyntaxHead(K"call", 0x0088)
    @test span.(children(t)) == [2,1,1,1,1]
    @test head.(children(t)) == [
         SyntaxHead(K"Identifier", 0x0000)
         SyntaxHead(K"Whitespace", 0x0001)
         SyntaxHead(K"Identifier", 0x0000)
         SyntaxHead(K"Whitespace", 0x0001)
         SyntaxHead(K"Identifier", 0x0000)
    ]

    @test numchildren(t) == 5
    @test !is_leaf(t)
    @test is_leaf(t[1])

    @test t[1] === children(t)[1]
    @test t[2:4] == [t[2],t[3],t[4]]
    @test firstindex(t) == 1
    @test lastindex(t) == 5

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
         3:4      │    [macro_name]
         3:3      │      @
         4:4      │      Identifier         ✔
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
         3:4      │    [macro_name]
         3:3      │      @                      "@"
         4:4      │      Identifier         ✔   "x"
         5:5      │    (                        "("
         6:6      │    Identifier           ✔   "y"
         7:7      │    )                        ")"
         8:8      │  ,                          ","
         9:9      │  Whitespace                 " "
        10:10     │  Identifier             ✔   "z"
        11:11     │  )                          ")"
    """

    @test sprint(show, parsestmt(GreenNode, "a + bb - f(ccc)")) ==
        "(call-i (call-i 1-1::Identifier 2-2::Whitespace-t 3-3::Identifier 4-4::Whitespace-t 5-6::Identifier) 7-7::Whitespace-t 8-8::Identifier 9-9::Whitespace-t (call 10-10::Identifier 11-11::(-t 12-14::Identifier 15-15::)-t))"
end
