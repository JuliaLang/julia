# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    LogicalTreeTests

For showing logical test failures informatively.
"""
module LogicalTreeTests

import ..Test: _should_escape_call, _escape_call, eval_test_comparison, eval_test_function,
               comparison_prec, Returned, Threw

export LogicalNode, LeafNode, LogicalOpNode, NegationNode, LogicalResult
export build_logical_tree, evaluate_logical_tree

abstract type LogicalNode end

"""
    LeafNode <: LogicalNode

Represents a leaf node in a logical expression tree - contains a single
expression that is not a logical operator (&& or ||).
"""
struct LeafNode <: LogicalNode
    expr::Any
    source::LineNumberNode
end

"""
    LogicalOpNode <: LogicalNode

Represents an internal node in a logical expression tree - contains a logical
operator (&& or ||) with left and right child nodes.
"""
struct LogicalOpNode <: LogicalNode
    op::Symbol             # :&& or :||
    left::LogicalNode
    right::LogicalNode
    source::LineNumberNode
end

"""
    NegationNode <: LogicalNode

Represents a negation (!) applied to any logical node.
Can wrap any other type of LogicalNode.
"""
struct NegationNode <: LogicalNode
    child::LogicalNode
    source::LineNumberNode
end

"""
    LogicalResult

Contains the result of evaluating a logical expression tree node,
including the boolean result, string representation, and metadata.
"""
struct LogicalResult
    value::Bool            # The boolean result
    display::String        # String representation for display
    was_short_circuited::Bool # Whether evaluation was short-circuited
    source::LineNumberNode # Source location
end

"""
    build_logical_tree(ex, source::LineNumberNode) -> LogicalNode

Recursively builds a tree representation of a logical expression.
Handles negation (!), logical operators (&& and ||), and leaf expressions.
Applies the same normalization logic as the original get_test_result function.
"""
function build_logical_tree(ex, source::LineNumberNode)
    # Handle negation
    if isa(ex, Expr) && ex.head === :call && length(ex.args) == 2 && ex.args[1] === :!
        child_tree = build_logical_tree(ex.args[2], source)
        return NegationNode(child_tree, source)
    end

    # Handle logical operators
    if isa(ex, Expr) && (ex.head === :|| || ex.head === :&&) && length(ex.args) == 2
        left_tree = build_logical_tree(ex.args[1], source)
        right_tree = build_logical_tree(ex.args[2], source)
        return LogicalOpNode(ex.head, left_tree, right_tree, source)
    end

    # For leaf expressions, apply the same normalization as get_test_result
    normalized_ex = normalize_expression(ex)

    # Handle leaf expressions (everything else)
    return LeafNode(normalized_ex, source)
end

"""
    normalize_expression(ex) -> Any

Applies the same expression normalization as get_test_result function.
Converts comparison operators to :comparison expressions.
"""
function normalize_expression(ex)
    # Normalize non-dot comparison operator calls to :comparison expressions
    is_splat = x -> isa(x, Expr) && x.head === :...
    if isa(ex, Expr) && ex.head === :call && length(ex.args) == 3 &&
        first(string(ex.args[1])) != '.' && !is_splat(ex.args[2]) && !is_splat(ex.args[3]) &&
        (ex.args[1] === :(==) || Base.operator_precedence(ex.args[1]) == comparison_prec)
        return Expr(:comparison, ex.args[2], ex.args[1], ex.args[3])

    # Mark <: and >: as :comparison expressions
    elseif isa(ex, Expr) && length(ex.args) == 2 &&
        !is_splat(ex.args[1]) && !is_splat(ex.args[2]) &&
        Base.operator_precedence(ex.head) == comparison_prec
        return Expr(:comparison, ex.args[1], ex.head, ex.args[2])
    end

    return ex
end

"""
    evaluate_logical_tree(node::LogicalNode, negate_root::Bool=false) -> Expr

Generates code to evaluate a logical expression tree, returning a `Returned`
object with the result. The `negate_root` parameter applies an additional
negation to the final result.
"""
function evaluate_logical_tree(node::LogicalNode, negate_root::Bool=false)
    eval_expr = generate_eval_expr(node)

    if negate_root
        return quote
            result = $eval_expr
            if isa(result, LogicalResult)
                final_value = !result.value
                final_display = result.display === "" ? nothing : "!(" * result.display * ")"
                Returned(final_value, final_display, result.source)
            else
                # Handle error case - result should be a LogicalResult but might be an exception
                result
            end
        end
    else
        return quote
            result = $eval_expr
            if isa(result, LogicalResult)
                final_display = result.display === "" ? nothing : result.display
                Returned(result.value, final_display, result.source)
            else
                # Handle error case
                result
            end
        end
    end
end



"""
    generate_eval_expr(node::LogicalNode) -> Expr

Internal function that generates the evaluation expression for a logical tree node.
"""
function generate_eval_expr(node::LeafNode)
    # Handle comparison expressions specially
    if isa(node.expr, Expr) && node.expr.head === :comparison
        escaped_terms = [esc(arg) for arg in node.expr.args]
        quoted_terms = [QuoteNode(arg) for arg in node.expr.args]
        return quote
            result = eval_test_comparison(
                Expr(:comparison, $(escaped_terms...)),
                Expr(:comparison, $(quoted_terms...)),
                $(QuoteNode(node.source)),
                false
            )

            display_str = if isa(result, Returned) && result.data === nothing
                # For logical expressions, preserve comparison form even for literals
                LogicalTreeTests.format_comparison_for_logical($(QuoteNode(node.expr)))
            elseif isa(result, Returned)
                string(result.data)
            else
                ""
            end

            # Inline the _process_test_result logic
            if isa(result, Returned) && isa(result.value, Bool)
                LogicalResult(result.value, display_str, false, $(QuoteNode(node.source)))
            else
                result  # Return error result as-is
            end
        end

    elseif _should_escape_call(node.expr)
        # Handle function calls with the existing infrastructure
        call = _escape_call(node.expr)
        return quote
            result = eval_test_function(
                $(call.func),
                ($(call.args...),),
                ($(call.kwargs...),),
                $(call.quoted_func),
                $(QuoteNode(node.source)),
                false
            )

            display_str = isa(result, Returned) && result.data !== nothing ? string(result.data) : ""

            # Inline the _process_test_result logic
            if isa(result, Returned) && isa(result.value, Bool)
                LogicalResult(result.value, display_str, false, $(QuoteNode(node.source)))
            else
                result  # Return error result as-is
            end
        end

    else
        # Handle simple expressions
        return quote
            try
                val = $(esc(node.expr))
                if isa(val, Bool)
                    display_str = LogicalTreeTests.should_show_expansion($(QuoteNode(node.expr))) ?
                        string($(QuoteNode(node.expr)), " = ", val) : ""
                    LogicalResult(val, display_str, false, $(QuoteNode(node.source)))
                else
                    # Non-boolean result - pass through as error
                    Returned(val, nothing, $(QuoteNode(node.source)))
                end
            catch e
                e isa InterruptException && rethrow()
                Threw(e, Base.current_exceptions(), $(QuoteNode(node.source)))
            end
        end
    end
end

function generate_eval_expr(node::LogicalOpNode)
    left_eval = generate_eval_expr(node.left)
    right_eval = generate_eval_expr(node.right)

    op_str = string(node.op)
    short_circuit_val = node.op === :||

    # Use unique variable names to avoid conflicts in nested expressions
    left_var = gensym("left_result")
    right_var = gensym("right_result")

    return quote
        $left_var = $left_eval

        # Handle error cases from left evaluation
        if !(isa($left_var, LogicalResult))
            $left_var  # Return error result
        elseif $left_var.value === $short_circuit_val
            # Short-circuit: don't evaluate right side
            left_display = $left_var.display === "" ? string($left_var.value) : $left_var.display
            display_str = left_display * " " * $op_str * " ..."
            LogicalResult($short_circuit_val, display_str, true, $left_var.source)
        else
            # Need to evaluate right side
            $right_var = $right_eval

            if !(isa($right_var, LogicalResult))
                $right_var  # Return error result
            else
                # Combine results
                combined_val = $(node.op === :|| ?
                    :($left_var.value || $right_var.value) :
                    :($left_var.value && $right_var.value))

                # Build display string
                left_str = $left_var.display === "" ? string($left_var.value) : $left_var.display
                right_str = $right_var.display === "" ? string($right_var.value) : $right_var.display

                display_str = if $right_var.was_short_circuited
                    # Right side was short-circuited
                    left_str * " " * $op_str * " " * right_str
                else
                    LogicalTreeTests.flatten_display_string(left_str, $op_str, right_str, $(QuoteNode(node.op)))
                end

                # Only show expanded form if at least one side has meaningful expansion
                has_expansion = $left_var.display !== "" || $right_var.display !== ""
                final_display = has_expansion ? display_str : ""

                LogicalResult(combined_val, final_display, false, $(QuoteNode(node.source)))
            end
        end
    end
end

function generate_eval_expr(node::NegationNode)
    child_eval = generate_eval_expr(node.child)

    return quote
        child_result = $child_eval

        if !(isa(child_result, LogicalResult))
            child_result  # Return error result
        else
            negated_val = !child_result.value
            display_str = child_result.display === "" ? "" : "!(" * child_result.display * ")"
            LogicalResult(negated_val, display_str, child_result.was_short_circuited, child_result.source)
        end
    end
end

"""
    should_show_expansion(expr) -> Bool

Determines whether an expression should show expanded form in test output.
Returns true for variables and complex expressions, false for simple literals.
"""
function should_show_expansion(expr)
    # Show expansion for symbols (variables) and function calls/property access
    isa(expr, Symbol) && return true
    isa(expr, Expr) && expr.head in (:call, :ref, :.) && return true
    return false
end

"""
    flatten_display_string(left_str, op_str, right_str, op) -> String

Helper function to properly flatten display strings for logical expressions.
"""
function flatten_display_string(left_str, op_str, right_str, op)
    return left_str * " " * op_str * " " * right_str
end

"""
    format_comparison_for_logical(expr) -> String

Formats a comparison expression for display in logical expressions.
"""
function format_comparison_for_logical(expr)
    if isa(expr, Expr) && expr.head === :comparison && length(expr.args) >= 3
        return string(expr.args[1], " ", expr.args[2], " ", expr.args[3])
    else
        return string(expr)
    end
end

end  # module LogicalTreeTests
