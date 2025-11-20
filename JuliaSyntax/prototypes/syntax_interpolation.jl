# # Macros and expression interpolation

using JuliaSyntax: SourceFile, SyntaxNode, parseall, child, setchild!

# The following shows that SyntaxNode works nicely for simple macros which
# just interpolate expressions into one another. In particular it shows how
# precise source information from multiple files can coexist within the same
# syntax tree.

# First, here's the functionality that we're going to implement as a normal
# Julia macro. It's similar to the standard @show macro.
macro show2(ex)
    name = sprint(Base.show_unquoted, ex)
    quote
        value = $(esc(ex))
        println($name, " = ", value)
        value
    end
end

# Now, let's implement the same expression interpolation but using SyntaxNode
# (and with a normal Julia function which we need to use, absent any deeper
# integration with the Julia runtime)
function at_show2(ex::SyntaxNode)
    name = sprint(show, MIME"text/x.sexpression"(), ex)
    quote
        value = $(esc(ex))
        println($name, " = ", value)
        value
    end
    # The following emulates the expression interpolation lowering which is
    # usually done by the compiler.
    # 1. Extract the expression literal as `block`
    tree = parseall(SyntaxNode, String(read(@__FILE__)), filename=@__FILE__)
    block = child(tree, 3, 2, 2, 1)
    # 2. Interpolate local variables into the block at positions of $'s
    # Interpolating a SyntaxNode `ex` is simple:
    setchild!(block, (1, 2), ex)
    # The interpolation of a Julia *value* should inherit the source location
    # of the $ interpolation expression. This is different to when substituting
    # in a SyntaxNode which should just be inserted as-is.
    setchild!(block, (2, 2),
              JuliaSyntax.interpolate_literal(child(block, 2, 2), name))
    block
end

# Usage of at_show2()

# Let's have some simple expression to pass to at_show2. This will be
# attributed to a different file foo.jl
s2 = parseall(SyntaxNode, "foo +\n42", filename="foo.jl", rule=:statement)

# Calling at_show2, we see that the precise source information is preserved for
# both the surrounding expression and the interpolated fragments.
println("\nInterpolation example")
s3 = at_show2(s2)
