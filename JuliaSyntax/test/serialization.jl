using Serialization

@testset "Equality $T" for T in [Expr, SyntaxNode, JuliaSyntax.GreenNode]
    x = JuliaSyntax.parsestmt(T, "f(x) = x + 2")
    y = JuliaSyntax.parsestmt(T, "f(x) = x + 2")
    z = JuliaSyntax.parsestmt(T, "f(x) = 2 + x")
    @test x == y
    @test x != z
    @test y != z
end

@testset "Hashing $T" for T in [Expr, SyntaxNode, JuliaSyntax.GreenNode]
    x = hash(JuliaSyntax.parsestmt(T, "f(x) = x + 2"))::UInt
    y = hash(JuliaSyntax.parsestmt(T, "f(x) = x + 2"))::UInt
    z = hash(JuliaSyntax.parsestmt(T, "f(x) = 2 + x"))::UInt
    @test x == y # Correctness
    @test x != z # Collision
    @test y != z # Collision
end

@testset "Serialization $T" for T in [Expr, SyntaxNode, JuliaSyntax.GreenNode]
    x = JuliaSyntax.parsestmt(T, "f(x) = x + 2")
    f = tempname()
    open(f, "w") do io
        serialize(io, x)
    end
    y = open(deserialize, f, "r")
    @test x == y
end
