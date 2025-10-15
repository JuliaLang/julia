# This file is a part of Julia. License is MIT: https://julialang.org/license

# Each pass takes the input string and returns an AnnotatedString with styling annotations

module StylingPasses

using StyledStrings
using StyledStrings: Face
using JuliaSyntaxHighlighting
import Base: AnnotatedString, annotate!, annotations, JuliaSyntax

export StylingPass, StylingContext, SyntaxHighlightPass, RegionHighlightPass,
       EnclosingParenHighlightPass, apply_styling_passes, merge_annotations

# Context information passed to all styling passes
struct StylingContext
    cursor_pos::Int
    region_start::Int
    region_stop::Int
end

StylingContext(cursor_pos::Int) = StylingContext(cursor_pos, 0, 0)

abstract type StylingPass end

function merge_annotations(annotated_strings::Vector{<:AnnotatedString})
    isempty(annotated_strings) && return AnnotatedString("")

    result = AnnotatedString(annotated_strings[1])

    for source in annotated_strings
        for ann in annotations(source)
            annotate!(result, ann.region, ann.label, ann.value)
        end
    end

    return result
end

function apply_style(pass::StylingPass, input::String, context::StylingContext)
    return pass(input, context)::AnnotatedString{String}
end

function apply_styling_passes(input::String, passes::Vector{StylingPass}, context::StylingContext)
    if isempty(passes)
        return AnnotatedString(input)
    end

    results = [apply_style(pass, input, context) for pass in passes]
    return merge_annotations(results)
end

# Applies Julia syntax highlighting
struct SyntaxHighlightPass <: StylingPass end

function (::SyntaxHighlightPass)(input::String, ::StylingContext)
    try
        return JuliaSyntaxHighlighting.highlight(input)
    catch e
        e isa InterruptException && rethrow()
        @error "Error in SyntaxHighlightPass" exception=(e, catch_backtrace()) maxlog=1
        return AnnotatedString(input)
    end
end

# Applies inverse video styling to the selected region
struct RegionHighlightPass <: StylingPass end

function (::RegionHighlightPass)(input::String, context::StylingContext)
    result = AnnotatedString(input)

    if context.region_start > 0 && context.region_stop >= context.region_start
        # Add inverse face to the region
        # Region positions are 1-based byte positions
        region_range = context.region_start:context.region_stop
        annotate!(result, region_range, :face, Face(inverse=true))
    end

    return result
end

# Applies bold styling to parentheses that enclose the cursor position
struct EnclosingParenHighlightPass <: StylingPass
    face::Face
end

EnclosingParenHighlightPass() = EnclosingParenHighlightPass(Face(weight=:bold, underline=true))

function (pass::EnclosingParenHighlightPass)(input::String, context::StylingContext)
    result = AnnotatedString(input)

    if isempty(input) || context.cursor_pos < 1
        return result
    end

    try
        ast = JuliaSyntax.parseall(JuliaSyntax.GreenNode, input; ignore_errors=true)
        paren_pairs = find_enclosing_parens(input, ast, context.cursor_pos)

        for (open_pos, close_pos) in paren_pairs
            annotate!(result, open_pos:open_pos, :face, pass.face)
            annotate!(result, close_pos:close_pos, :face, pass.face)
        end
    catch e
        e isa InterruptException && rethrow()
        @error "Error in EnclosingParenHighlightPass" exception=(e, catch_backtrace()) maxlog=1
    end

    return result
end

function paren_type(k)
    if     k == JuliaSyntax.K"(";  1, :paren
    elseif k == JuliaSyntax.K")"; -1, :paren
    elseif k == JuliaSyntax.K"[";  1, :bracket
    elseif k == JuliaSyntax.K"]"; -1, :bracket
    elseif k == JuliaSyntax.K"{";  1, :curly
    elseif k == JuliaSyntax.K"}"; -1, :curly
    else                           0, :none
    end
end

function find_enclosing_parens(content::String, ast, cursor_pos::Int)
    innermost_pairs = Dict{Symbol,Tuple{Int,Int}}()
    paren_stack = Tuple{Int,Int,Symbol}[]  # (open_pos, depth, type)

    walk_tree(ast, content, 0) do node, offset
        nkind = JuliaSyntax.kind(node)
        pos = firstindex(content) + offset

        depthchange, ptype = paren_type(nkind)

        if ptype != :none
            if depthchange > 0
                # Opening paren - push to stack
                push!(paren_stack, (pos, length(paren_stack) + 1, ptype))
            elseif depthchange < 0 && !isempty(paren_stack)
                # Closing paren - pop from stack and check if cursor is inside
                open_pos, depth, open_ptype = pop!(paren_stack)
                if open_ptype == ptype && open_pos <= cursor_pos < pos
                    # Cursor is inside this paren pair - keep only innermost per type
                    # Only update if this is the first pair or if it's smaller (more inner) than existing
                    if !haskey(innermost_pairs, ptype) || (pos - open_pos) < (innermost_pairs[ptype][2] - innermost_pairs[ptype][1])
                        innermost_pairs[ptype] = (open_pos, pos)
                    end
                end
            end
        end
    end

    return collect(values(innermost_pairs))
end

function walk_tree(f::Function, node, content::String, offset::Int)
    f(node, offset)

    if JuliaSyntax.numchildren(node) > 0
        for child in JuliaSyntax.children(node)
            walk_tree(f, child, content, offset)
            offset += JuliaSyntax.span(child)
        end
    end
end

end # module StylingPasses
