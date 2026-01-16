#-------------------------------------------------------------------------------
# Syntactic-block-local dominance analysis to optimize Box allocations.
# Nearly identical (by design) to flisp's `lambda-optimize-vars!` in julia-syntax.scm.
#
# This pass attempts to prove
# (for the special case of assigned-once variables):
#   1. A variable is always defined at every use / capture
#   2. A variable will not be modified anywhere after any capture
#
# Within inner syntactic blocks, an outer assignment effectively "guards" the
# variable so that uses / captures do not taint the "always-defined" status.
#
# This "guard" behavior is disabled when a variable was introduced outside of
# an enclosing loop, since that would allow condition (2) to be violated. In
# contrast, straight-line captures (i.e. an assignment followed by a capture
# in the same block) is allowed even in loops, since it's unconditional.
#
# In the implementation, any variables in `unused` / `live` are considered
# "always-defined- when-used-or-captured-and-only-modified-once-dynamically".
# These variables may temporarily lose their status when considering uses /
# captures in inner blocks, but this is restored later if dominated by an
# outer assignment.
#
# XXX: This pass under-approximates the "is_always_defined" flag to mean something
#      closer to "is_always_defined_and_not_modified_after_any_capture" (which is
#      the real condition needed to apply unboxing safely)

"""
    analyze_def_and_use!(ctx, ex)

Perform tree-based def-use analysis to find captured variables that are
assigned before any closure captures them (never-undef) and not modified
afterward. For such variables, as an abuse of binding flags we can mark them
as `is_always_defined=true` to avoid unnecessary `Core.Box` allocations during
closure conversion.

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

"""
    DefUseState

State for def-use analysis (flisp-compatible tables for tracking variable def and use).

Fields:
- `unused`: candidate variables not yet used (read) in current block
- `live`: variables that have been assigned in current block
- `seen`: all variables we've seen assigned
- `decl`: variables scoped in current scope (via `local` or an argument)
- `decl_outside_loop`: variables scoped in scope outside loop (via `local` or an argument)
- `args`: argument variables (never undefined, special handling in mark_used!)
"""
mutable struct DefUseState
    const unused::Set{IdTag}
    const live::Set{IdTag}
    const seen::Set{IdTag}
    decl::Set{IdTag}
    decl_outside_loop::Set{IdTag}
    const args::Set{IdTag}

    function DefUseState(ctx, candidates)
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
        return new(unused, live, seen, decl, decl_outside_loop, args)
    end
end

# At CFG merge points, we lose certainty about which path was taken,
# so variables assigned in one branch may not have been assigned.
# Move live variables back to unused to require re-assignment.
# NOTE: This is NOT needed at branch points (return/break/goto) because
# code after them is unreachable - only at merge points (if/while/label).
function du_kill!(state::DefUseState)
    union!(state.unused, state.live)
    empty!(state.live)
end

# Restore live to a previous state, moving new additions back to unused
function du_restore!(state::DefUseState, prev)
    for id in state.decl_outside_loop
        if (id in prev) && !(id in state.unused)
            # This variable was 'used' inside this branch, but it's declared
            # outside of a loop so it may see the dominating assignment execute
            # multiple times. Invalidate it here for soundness.
            delete!(prev, id)
        end
    end
    for id in state.live
        if !(id in prev)
            push!(state.unused, id)
        end
    end
    empty!(state.live)
    union!(state.live, prev)
end

# At the beginning of a loop, move all active decls into the "decl_outside_loop" set.
function du_enter_loop!(state::DefUseState)
    prev_decl_outside_loop = state.decl_outside_loop
    state.decl_outside_loop = state.decl
    state.decl = copy(state.decl)
    return prev_decl_outside_loop
end

# At the end of a loop, restore the previous set of "declared" variables.
function du_leave_loop!(state::DefUseState, prev_decl_outside_loop)
    state.decl = state.decl_outside_loop
    state.decl_outside_loop = prev_decl_outside_loop
end

# When a variable is used (read), remove from unused.
# Note: arguments are only "used" for purposes of this analysis when
# they are captured, since they are never undefined.
function du_mark_used!(state::DefUseState, var_id)
    if var_id in state.unused && !(var_id in state.args)
        delete!(state.unused, var_id)
    end
end

# When a variable is captured by a nested lambda before being assigned
function du_mark_captured!(state::DefUseState, var_id)
    if var_id in state.unused
        delete!(state.unused, var_id)
    end
end

# When a variable is assigned, move from unused to live
function du_assign!(state::DefUseState, var_id)
    if var_id in state.unused
        push!(state.live, var_id)
        push!(state.seen, var_id)
        delete!(state.unused, var_id)
    end
end

# Track local declarations for loop handling
function du_declare!(state::DefUseState, var_id)
    if var_id in state.unused
        push!(state.decl, var_id)
    end
end

# Returns whether e contained a symbolic_label
function du_visit!(ctx, state::DefUseState, e)
    k = kind(e)

    if k == K"BindingId"
        du_mark_used!(state, e.var_id)
        return false

    elseif k == K"symbolic_label"
        # Must check BEFORE is_leaf since symbolic_label is a leaf node
        du_kill!(state)
        return true

    elseif k == K"label"
        du_kill!(state)
        return false

    elseif k in KSet"break symbolic_goto"
        # this kill!() is not required for soundness since these are branch points
        # not merge points, but it's here for parity with flisp
        du_kill!(state)
        return false

    elseif k == K"="
        # Visit RHS first, then record assignment
        has_label = du_visit!(ctx, state, e[2])
        lhs = e[1]
        if kind(lhs) == K"BindingId"
            du_assign!(state, lhs.var_id)
        end
        return has_label

    elseif k == K"lambda"
        # Check captures from nested lambda
        nested_lb = e.lambda_bindings
        for (id, is_capt) in nested_lb.locals_capt
            if is_capt
                du_mark_captured!(state, id)
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
                du_declare!(state, child.var_id)
            end
        end
        return false

    elseif k == K"decl"
        # Don't recurse into decl nodes - the BindingId is just a declaration,
        # not a use. We only need to visit the type expression.
        if numchildren(e) >= 2
            return du_visit!(ctx, state, e[2])
        end
        return false

    elseif k == K"method_defs" || k == K"function_decl"
        # Process nested lambdas within
        has_label = false
        for child in children(e)
            has_label |= du_visit!(ctx, state, child)
        end
        return has_label

    elseif k == K"return"
        has_label = numchildren(e) >= 1 ? du_visit!(ctx, state, e[1]) : false
        du_kill!(state) # not necessary, but included for flisp parity
        return has_label

    elseif k in KSet"if elseif trycatchelse tryfinally"
        prev = copy(state.live)
        has_label = false
        for child in children(e)
            has_label |= du_visit!(ctx, state, child)
            du_kill!(state)
        end
        if has_label
            # If there's a label inside, we could have skipped a prior
            # variable initialization
            return true
        else
            du_restore!(state, prev)
            return false
        end

    elseif k in KSet"_while _do_while"
        prev = copy(state.live)
        old_decl = du_enter_loop!(state)
        has_label = false
        for child in children(e)
            has_label |= du_visit!(ctx, state, child)
        end
        du_leave_loop!(state, old_decl)
        if has_label
            du_kill!(state)
            return true
        else
            du_restore!(state, prev)
            return false
        end

    elseif k == K"break_block"
        # Skip the first child (break target label) - it's not a @goto target
        # No save/restore needed: the body always executes (break just exits early)
        has_label = false
        for child in children(e)[2:end]
            has_label |= du_visit!(ctx, state, child)
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
            has_label |= du_visit!(ctx, state, child)
        end
        return has_label
    end
end

function _analyze_lambda_vars!(ctx, ex)
    lambda_bindings = ex.lambda_bindings

    # Collect candidate variables: captured and single-assigned
    candidates = Set{IdTag}()
    for (id, from_outer_lambda) in lambda_bindings.locals_capt
        binfo = get_binding(ctx, id)
        maybe_boxed = binfo.is_captured && binfo.kind in (:local, :argument)
        safe_to_analyze = binfo.is_assigned_once
        if !from_outer_lambda && maybe_boxed && safe_to_analyze
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

    state = DefUseState(ctx, candidates)

    # Visit the lambda body
    if numchildren(ex) >= 3
        body = ex[3]
        if kind(body) == K"block"
            for stmt in children(body)
                du_visit!(ctx, state, stmt)
            end
        else
            du_visit!(ctx, state, body)
        end
    end

    # Variables in live or unused (that were seen assigned) are never-undef
    for id in union(state.live, state.unused)
        if id in state.seen
            get_binding(ctx, id).is_always_defined = true
        end
    end
end
