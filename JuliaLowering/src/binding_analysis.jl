#-------------------------------------------------------------------------------
# Syntactic-block-local dominance analysis to optimize Box allocations.
#
# This analysis identifies captured variables that don't need Core.Box by
# checking if assignments "dominate" closure captures within syntactic blocks.
# Similar to flisp's `lambda-optimize-vars!` in julia-syntax.scm.
#
# Key insight: Within a syntactic block, a statement dominates all statements
# in any "inner" syntactic blocks that follow. This means:
#   `x = 1; if c; f = ()->x; end`  - x=1 dominates the capture → no Box
#   `if c; x = 1; end; f = ()->x`  - x=1 inside inner block, uncertain → Box
#
# The save/restore pattern implements this by:
# - Saving `live` set before entering control flow constructs
# - Restoring after, so only outer-block assignments remain "live"
#
# Two conditions require Box allocation:
# 1. Dominance broken by labels (@label): Jumping into a block bypasses assignments
#    `@goto L; x = 1; @label L; f = ()->x` - x=1 can be skipped → Box
# 2. Dynamic single-assignment broken by loops: Dominance holds statically, but
#    outer-scope variables get multiple assignments at runtime (once per iteration)
#    `let x; for i in 1:3; x = i; f = ()->x; end; end` - x from outer scope → Box
#    `for i in 1:3; x = i; f = ()->x; end` - x is loop-local, fresh each iteration → no Box
#
# State variables:
# - `unused`: candidates not yet read in current block
# - `live`: variables assigned in current block (cleared at control flow)
# - `seen`: all variables ever assigned (for optimization marking)
# - `decl`: variables declared before loop (via `local` or outer scope)

"""
    analyze_def_and_use!(ctx, ex)

Perform tree-based def-use analysis to find captured variables that are
assigned before any closure captures them (never-undef). For such variables,
we can mark them as `is_always_defined=true` to avoid unnecessary `Core.Box`
allocations during closure conversion.

This is called on the outermost lambda, and recursively processes nested lambdas.
"""
function analyze_def_and_use!(ctx, ex)
    k = kind(ex)
    if k != K"lambda"
        return
    end

    # First, recursively analyze nested lambdas (depth-first)
    if numchildren(ex) >= 3
        _analyze_nested_lambdas!(ctx, ex[3])
    end

    # Now analyze this lambda
    _analyze_lambda_vars!(ctx, ex)
end

function _analyze_nested_lambdas!(ctx, ex)
    k = kind(ex)
    if k == K"lambda"
        analyze_def_and_use!(ctx, ex)
    elseif !is_leaf(ex) && !is_quoted(ex)
        for child in children(ex)
            _analyze_nested_lambdas!(ctx, child)
        end
    end
end

function _analyze_lambda_vars!(ctx, ex)
    lambda_bindings = ex.lambda_bindings

    # Collect candidate variables: captured and single-assigned
    # We check binfo.is_captured instead of lbinfo (is_capt) because for variables
    # defined in this lambda and captured by inner lambdas, lbinfo may be false
    # but binfo.is_captured will be true.
    candidates = Set{IdTag}()
    for (id, _) in lambda_bindings.locals_capt
        binfo = get_binding(ctx, id)
        if (binfo.is_captured && binfo.is_assigned_once &&
            (binfo.kind == :local || binfo.kind == :argument))
            push!(candidates, id)
            # For arguments, reset is_always_defined so we can determine if the
            # outer-scope assignment dominates the capture. Arguments start with
            # is_always_defined=true, but if they're reassigned inside a closure
            # (not in outer scope), we need the def-use analysis to decide.
            if binfo.kind == :argument
                binfo.is_always_defined = false
            end
        end
    end
    isempty(candidates) && return

    # flisp-compatible tables for tracking variable def and use:
    # - unused: candidate variables not yet used (read) in current block
    # - live: variables that have been assigned in current block
    # - seen: all variables we've seen assigned
    # - decl: variables declared before loop (via `local` or outer scope)
    # - args: argument variables (never undefined, special handling in mark_used!)
    unused = candidates
    live = Set{IdTag}()
    seen = Set{IdTag}()
    decl = Set{IdTag}()
    decl_outside_loop = Set{IdTag}()
    args = Set{IdTag}()
    # Initialize decl and args with arguments since they're implicitly declared outside any loop
    for id in candidates
        binfo = get_binding(ctx, id)
        if binfo.kind == :argument
            push!(decl, id)
            push!(args, id)
        end
    end

    # At CFG merge points, we lose certainty about which path was taken,
    # so variables assigned in one branch may not have been assigned.
    # Move live variables back to unused to require re-assignment.
    # NOTE: This is NOT needed at branch points (return/break/goto) because
    # code after them is unreachable - only at merge points (if/while/label).
    function kill!()
        union!(unused, live)
        empty!(live)
    end

    # Restore live to a previous state, moving new additions back to unused
    function restore!(prev)
        for id in decl_outside_loop
            if (id in prev) && !(id in unused)
                # This variable was 'used' inside this branch, but it's declared
                # outside of a loop so it may see the dominating assignment execute
                # multiple times. Invalidate it here for soundness.
                delete!(prev, id)
            end
        end
        for id in live
            if !(id in prev)
                push!(unused, id)
            end
        end
        empty!(live)
        union!(live, prev)
    end

    # At the beginning of a loop, move all active decls into the "decl_outside_loop" set.
    function enter_loop!()
        prev_decl_outside_loop = decl_outside_loop
        decl_outside_loop = decl
        decl = copy(decl)
        return prev_decl_outside_loop
    end

    # At the end of a loop, restore the previous set of "declared" variables.
    function leave_loop!(prev_decl_outside_loop)
        decl = decl_outside_loop
        decl_outside_loop = prev_decl_outside_loop
    end

    # When a variable is used (read), remove from unused.
    # Note: arguments are only "used" for purposes of this analysis when
    # they are captured, since they are never undefined.
    function mark_used!(var_id)
        if var_id in unused && !(var_id in args)
            delete!(unused, var_id)
        end
    end

    # When a variable is captured by a nested lambda before being assigned
    function mark_captured!(var_id)
        if var_id in unused
            delete!(unused, var_id)
        end
    end

    # When a variable is assigned, move from unused to live
    function assign!(var_id)
        if var_id in unused
            push!(live, var_id)
            push!(seen, var_id)
            delete!(unused, var_id)
        end
    end

    # Track local declarations for loop handling
    function declare!(var_id)
        if var_id in unused
            push!(decl, var_id)
        end
    end

    # Returns whether e contained a symbolic_label
    function visit(e)
        k = kind(e)

        if k == K"BindingId"
            mark_used!(e.var_id)
            return false

        elseif k == K"symbolic_label"
            # Must check BEFORE is_leaf since symbolic_label is a leaf node
            kill!()
            return true

        elseif k == K"label"
            kill!()
            return false

        elseif k in KSet"break symbolic_goto"
            # this kill!() is not required for soundness since these are branch points
            # not merge points, but it's here for parity with flisp
            kill!()
            return false

        elseif k == K"="
            # Visit RHS first, then record assignment
            has_label = visit(e[2])
            lhs = e[1]
            if kind(lhs) == K"BindingId"
                assign!(lhs.var_id)
            end
            return has_label

        elseif k == K"lambda"
            # Check captures from nested lambda
            nested_lb = e.lambda_bindings
            for (id, is_capt) in nested_lb.locals_capt
                if is_capt
                    mark_captured!(id)
                end
            end
            # Don't recurse into nested lambdas - they have their own analysis
            return false

        elseif k == K"local"
            # Track local declarations for loop handling
            # Note: For typed locals like `local x::T`, the K"local" node only
            # contains the BindingId after desugaring. The type info is in
            # a separate K"decl" node. So we only need to handle K"BindingId" here.
            for child in children(e)
                if kind(child) == K"BindingId"
                    declare!(child.var_id)
                end
            end
            return false

        elseif k == K"decl"
            # Don't recurse into decl nodes - the BindingId is just a declaration,
            # not a use. We only need to visit the type expression.
            if numchildren(e) >= 2
                return visit(e[2])
            end
            return false

        elseif k == K"method_defs" || k == K"function_decl"
            # Process nested lambdas within
            has_label = false
            for child in children(e)
                has_label |= visit(child)
            end
            return has_label

        elseif k == K"return"
            has_label = numchildren(e) >= 1 ? visit(e[1]) : false
            kill!() # not necessary, but included for flisp parity
            return has_label

        elseif k in KSet"if elseif trycatchelse tryfinally"
            prev = copy(live)
            has_label = false
            for child in children(e)
                has_label |= visit(child)
                kill!()
            end
            if has_label
                # If there's a label inside, we could have skipped a prior
                # variable initialization
                return true
            else
                restore!(prev)
                return false
            end

        elseif k in KSet"_while _do_while"
            prev = copy(live)
            old_decl = enter_loop!()
            has_label = false
            for child in children(e)
                has_label |= visit(child)
            end
            leave_loop!(old_decl)
            if has_label
                kill!()
                return true
            else
                restore!(prev)
                return false
            end

        elseif k == K"break_block"
            # Skip the first child (break target label) - it's not a @goto target
            # No save/restore needed: the body always executes (break just exits early)
            has_label = false
            for child in children(e)[2:end]
                has_label |= visit(child)
            end
            return has_label

        elseif is_leaf(e) || is_quoted(e) ||
            k in KSet"local meta inbounds boundscheck noinline loopinfo decl
                with_static_parameters toplevel_butfirst global globalref
                extension constdecl atomic isdefined toplevel module error
                gc_preserve_begin gc_preserve_end export public inline"

            # Forms that don't interact with locals or affect control flow (likely more than is necessary).
            # flisp: `lambda-opt-ignored-exprs`
            return false

        else
            has_label = false
            for child in children(e)
                has_label |= visit(child)
            end
            return has_label
        end
    end

    # Visit the lambda body
    if numchildren(ex) >= 3
        body = ex[3]
        if kind(body) == K"block"
            for stmt in children(body)
                visit(stmt)
            end
        else
            visit(body)
        end
    end

    # Variables in live or unused (that were seen assigned) are never-undef
    for id in union(live, unused)
        if id in seen
            get_binding(ctx, id).is_always_defined = true
        end
    end
end
