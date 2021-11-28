using JuliaSyntax
using Test

#@testset "JuliaSyntax.jl" begin
    # Write your tests here.
#end


code = """
[1,2, 3]
"""

using JuliaSyntax: RawSyntaxNode
using JuliaSyntax: Kind, @K_str

const N = RawSyntaxNode

code = """
for i = 1:10
    a + 2
    # hi
    c
end
"""

t =
N(K"for",
    N(K"for", 3),
    N(K" ", 1),
    N(K"=",
        N(K"Identifier", 1),
        N(K" ", 1),
        N(K"=", 1),
        N(K" ", 1),
        N(K"call",
            N(K"Integer", 1),
            N(K":", 1),
            N(K"Integer", 2))),
    N(K"\n", 5),
    N(K"call",
        N(K"Identifier", 1),
        N(K" ", 1),
        N(K"+", 1),
        N(K" ", 1),
        N(K"Integer", 1)),
    N(K"\n", 5),
    N(K"Comment", 4),
    N(K"\n", 5),
    N(K"Identifier", 1),
    N(K"\n", 1),
    N(K"end", 3))

show(stdout, MIME"text/plain"(), t, code)
