#-------------------------------------------------------------------------------
# Raw syntax tree and AST layering

# For this code:
code = """
for i = 1:10
    a + 2
    # hi
    c
end
"""

source = SourceFile(code, filename="none.jl")

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

# And the following AST
s = SyntaxNode(source, t)

println("\nRawSyntaxNode")
show(stdout, MIME"text/plain"(), t, code, show_trivia=true)

println("\nSyntaxNode")
show(stdout, MIME"text/plain"(), s)

#code = "42"
#SyntaxNode(N(K"Integer", 2), 1, code)

