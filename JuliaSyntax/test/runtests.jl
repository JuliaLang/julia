using JuliaSyntax
using Test

#@testset "JuliaSyntax.jl" begin
    # Write your tests here.
#end


using JuliaSyntax: SourceFile
using JuliaSyntax: RawSyntaxNode, SyntaxNode, raw_flags
using JuliaSyntax: Kind, @K_str, children, child
using JuliaSyntax: highlight

# Trivia nodes
T(k, s) = RawSyntaxNode(k, s, raw_flags(trivia=true))
# Non-trivia nodes
N(k, s) = RawSyntaxNode(k, s)
N(k, args::RawSyntaxNode...) = RawSyntaxNode(k, args...)
# Non-trivia, infix form
NI(k, args::RawSyntaxNode...) = RawSyntaxNode(k, raw_flags(infix=true), args...)

# For this code:
code = """
for i = 1:10
    a + 2
    # hi
    c
end
"""

source = SourceFile(code)

# We'd like to produce something the following raw tree
t =
N(K"for",
  T(K"for", 3),
  T(K" ", 1),
  N(K"=",
    N(K"Identifier", 1),
    T(K" ", 1),
    T(K"=", 1),
    T(K" ", 1),
    NI(K"call",
      N(K"Integer", 1),
      N(K":", 1),
      N(K"Integer", 2))),
  N(K"block", 
    T(K"\n", 5),
    NI(K"call",
      N(K"Identifier", 1),
      T(K" ", 1),
      N(K"+", 1),
      T(K" ", 1),
      N(K"Integer", 1)),
    T(K"\n", 5),
    T(K"Comment", 4),
    T(K"\n", 5),
    N(K"Identifier", 1),
    T(K"\n", 1)),
  T(K"end", 3))

println("\nRawSyntaxNode")
show(stdout, MIME"text/plain"(), t, code, show_trivia=true)

println("\nSyntaxNode")

# And the following AST
s = SyntaxNode(source, t, 1)

#code = "42"
#SyntaxNode(N(K"Integer", 2), 1, code)

# Simulate the following Undescores.jl - like macro:

# @U f(x+_, y)
#   ↦
# f(_1 -> x+_1, y)

# macro U(ex)
# end

