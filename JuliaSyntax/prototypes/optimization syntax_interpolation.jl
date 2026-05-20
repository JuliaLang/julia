using JuliaSyntax: SourceFile, SyntaxNode, parseall, child, setchild!

# ---------------------------------------------------
# Pre-build interpolation template ONCE (big speedup)
# ---------------------------------------------------

const SHOW2_TEMPLATE = let
    src = """
    quote
        value = \$(EXPR)
        println(NAME, " = ", value)
        value
    end
    """
    tree = parseall(SyntaxNode, src, filename="template.jl")
    child(tree, 1)   # extract the quote block
end


# ---------------------------------------------------
# Optimized macro-like function using SyntaxNode
# ---------------------------------------------------

function at_show2(ex::SyntaxNode)

    # Get expression string representation
    name = sprint(show, MIME"text/x.sexpression"(), ex)

    # Copy template (never mutate global tree!)
    block = copy(SHOW2_TEMPLATE)

    # Interpolate SyntaxNode expression
    setchild!(block, (1, 2), ex)

    # Interpolate string literal with correct source mapping
    literal_node = child(block, 2, 2)
    setchild!(
        block,
        (2, 2),
        JuliaSyntax.interpolate_literal(literal_node, name)
    )

    return block
end


# ---------------------------------------------------
# Example usage
# ---------------------------------------------------

s2 = parseall(SyntaxNode, "foo +\n42", filename="foo.jl", rule=:statement)

println("\nInterpolation example")
s3 = at_show2(s2)
