_PROFILE_LINES = 1
_PROFILE_DESCEND = 2
_PROFILE_STATE = _PROFILE_LINES | _PROFILE_DESCEND    # state is a bitfield

# Record of expressions to parse according to the current _PROFILE_STATE
_PROFILE_EXPR = {}

# To avoid the performance penalty of global variables, we design the
# macro to create local variables through a "let" block that shares
# the timing and count data with "reporting" and "clearing" functions.
_PROFILE_REPORTS = {}  # list of reporting functions
_PROFILE_CLEARS = {}   # list of clearing functions
_PROFILE_TAGS = {}     # line #s for all timing variables

# Profile calibration, to compensate for the overhead of calling time()
_PROFILE_CALIB = 0
# Do it inside a let block, just like in real profiling, in case of
# extra overhead
let # tlast::Uint64 = 0x0, tnow::Uint64 = 0x0
global profile_calib
function profile_calib(n_iter)
    trec = Array(Uint64, n_iter)
    for i = 1:n_iter
        tlast = time_ns()
        tnow = time_ns()
        trec[i] = tnow - tlast
    end
    return trec
end
end
_PROFILE_CALIB = min(profile_calib(100))

# Utilities
# Generic expression type testing
is_expr_head(ex::Expr, s::Symbol) = ex.head == s
is_expr_head(nonex, s::Symbol) = false

# Test whether an expression is a function declaration
function isfuncexpr(ex::LineNumberNode)
    return false
end
function isfuncexpr(ex::Expr)
    return ex.head == :function || (ex.head == :(=) && ex.args[1].head == :call)
end

# Get the "full syntax" of the function call
function funcsyntax(ex::Expr)
    return ex.args[1]
end

# Get the symbol associated with the function call
function funcsym(ex::Expr)
    tmp = funcsyntax(ex)
    tmp = tmp.args[1]
    if is_expr_head(tmp, :curly)
        tmp = tmp.args[1]
    end
    return tmp
end

# Test for control-flow statements
is_cf_expr(ex::Expr) = contains([:for, :while, :if, :try], ex.head)
is_cf_expr(ex) = false

# General switchyard function
function insert_profile(ex::Expr, tlast, tnow, timers, counters, tags, indx::Int, retsym, rettest)
    if ex.head == :block
        insert_profile_block(ex, tlast, tnow, timers, counters, tags, indx, retsym, rettest)
    elseif is_cf_expr(ex)
        insert_profile_cf(ex, tlast, tnow, timers, counters, tags, indx, retsym)
    else
        error("Don't know what to do")
    end
end

# Insert profiling statements into a code block
# rettest is a function with the following syntax:
#    rettest(Expr, Int)
# and evaluates to true if the return value of Expr needs to be saved
# before inserting profiling statements.
function insert_profile_block(fblock::Expr, tlast, tnow, timers, counters, tags, indx::Int, retsym, rettest)
    global _PROFILE_STATE, _PROFILE_DESCEND
    if fblock.head != :block
        println(fblock)
        error("expression is not a block")
    end
    descend = _PROFILE_STATE & _PROFILE_DESCEND > 0
    fblocknewargs = {}
    for i = 1:length(fblock.args)
        if isa(fblock.args[i],LineNumberNode) || is_expr_head(fblock.args[i], :line)
            # This is a line expression, so no counters/timers required
            push(fblocknewargs,fblock.args[i])
             # ...but keep track of the line # for use during reporting
            lasttag = fblock.args[i]
        elseif descend && is_cf_expr(fblock.args[i])
            # This is a control-flow statement, it requires special
            # handling (recursive)
            cfnew, indx = insert_profile_cf(fblock.args[i], tlast, tnow, timers, counters, tags, indx, retsym)
            push(fblocknewargs, cfnew)
        else
            # This is an "ordinary" statement
            saveret = rettest(fblock, i)
            push(tags,lasttag)
            if saveret
                if is_expr_head(fblock.args[i], :return)
                    push(fblocknewargs, :($retsym = $(fblock.args[i].args[1])))
                else
                    push(fblocknewargs, :($retsym = $(fblock.args[i])))
                end
            else
                push(fblocknewargs, fblock.args[i])
            end
            # This next line inserts timing statements between two
            # lines of code, equivalent to:
            #   timehr(_PROFILE_USE_CLOCK, tnow)  # end time of prev
            #   timers[indx] += timehr_diff(tlast, tnow)
            #   counters[indx] += 1
            #   timehr(_PROFILE_USE_CLOCK, tlast) # start time for next
            append!(fblocknewargs,{:($tnow = time_ns()), :(($timers)[($indx)] += $tnow - $tlast), :(($counters)[($indx)] += 1), :($tlast = time_ns())})
            indx += 1
            if saveret
                push(fblocknewargs, :(return $retsym))
            end
        end
    end
    return expr(:block, fblocknewargs), indx
end

# Handling control-flow statements
function insert_profile_cf(ex::Expr, tlast, tnow, timers, counters, tags, indx::Int, retsym)
    rettest = (ex, i) -> is_expr_head(ex, :return)
    if length(ex.args) == 2
        # This is a for, while, or 2-argument if or try block
        block1, indx = insert_profile(ex.args[2], tlast, tnow, timers, counters, tags, indx, retsym, rettest)
        return expr(ex.head, {ex.args[1], block1}), indx
    elseif length(ex.args) == 3
        # This is for a 3-argument if or try block
        block1, indx = insert_profile(ex.args[2], tlast, tnow, timers, counters, tags, indx, retsym, rettest)
        block2, indx = insert_profile(ex.args[3], tlast, tnow, timers, counters, tags, indx, retsym, rettest)
        return expr(ex.head, {ex.args[1], block1, block2}), indx
    else
        error("Wrong number of arguments")
    end
end

# Insert timing and counters into function body.
# Function bodies differ from blocks in these respects:
#   - We need to initialize tlast
#   - The final statement of a function should be returned, even if there
#     is no explicit return statement
#   - Functions can be defined in "short-form" (e.g.,
#     "isempty(x) = numel(x)==0"), and the return value for these
#     needs to be managed, too
function insert_profile_function(ex::Expr, tlast, tnow, timers, counters, tags, indx::Int, retsym)
    fblock = ex.args[2]
    if fblock.head != :block
        error("Can't parse func expression")
    end
    # Prepare the test for whether we need to save the return value of
    # a given line of code.  We may need to store the return value
    # because we need to run timing operations after computing the
    # output.
    # For a function, this will be true in three cases:
    #   - For a "f1(x) = x+1" type of function declaration
    #   - For an explicit return statement
    #   - For the last line of a function that does not have
    #     an explicit return statement in it.
    if ex.head == :(=)
        # This is a short-form function declaration
        savefunc = (ex, i) -> true
    else
        # Long form, check to see if it's a return or the last line
        savefunc = (ex, i) -> i == length(fblock.args) || is_expr_head(ex, :return)
    end
    # Insert the profiling statements in the function
    fblocknewargs, indx = insert_profile_block(fblock, tlast, tnow, timers, counters, tags, indx, retsym, savefunc)
    # Prepend the initialization of tlast
    fblocknewargs = vcat({:($tlast = time_ns())}, fblocknewargs.args)
    return expr(:function,{funcsyntax(ex),expr(:block,fblocknewargs)}), indx
end

function profile_parse(ex::Expr)
    if _PROFILE_STATE & _PROFILE_LINES > 0
        # Create the "let" variables for timing and counting
        tlast = gensym()
        tnow = gensym()
        timers = gensym()
        counters = gensym()
        # Keep track of line numbers
        tags = {}
        # Preserve return values
        retsym = gensym()
        # Create the symbols used for reporting and clearing the data
        # for this block
        funcreport = gensym()
        funcclear = gensym()
        # Parse the block and insert instructions
        indx = 1
        coreargs = {}
        if ex.head == :block
            # This is a block which may contain many function declarations
            for i = 1:length(ex.args)
                if isfuncexpr(ex.args[i])
                    # Insert "global" statement for each function
                    push(coreargs,expr(:global,funcsym(ex.args[i])))
                    # Insert function-call counters
                    newfuncexpr, indx = insert_profile_function(ex.args[i], tlast, tnow, timers, counters, tags, indx, retsym)
                    push(coreargs, newfuncexpr)
                else
                    push(coreargs,ex.args[i])
                end
            end
        elseif isfuncexpr(ex)
            # This is a single function declaration
            push(coreargs,expr(:global,funcsym(ex)))
            newfuncexpr, indx = insert_profile_function(ex, tlast, tnow, timers, counters, tags, indx, retsym)
            push(coreargs, newfuncexpr)
        else
            error("Could not parse expression")
        end
        n_lines = indx-1
        # Insert reporting function
        # Because we're using a gensym for the function name, we can't
        # quote the whole thing
        push(coreargs, expr(:global, funcreport))
        push(coreargs, expr(:function, {expr(:call, {funcreport}), expr(:block,{:(return $timers, $counters)})}))
        # Insert clearing function
        push(coreargs, expr(:global, funcclear))
        push(coreargs, expr(:function, {expr(:call, {funcclear}), expr(:block,{:(fill!($timers,0)), :(fill!($counters,0))})}))
        # Put all this inside a let block
        excore = expr(:block,coreargs)
        exlet = expr(:let,{expr(:block,excore), :($timers = zeros(Uint64, $n_lines)), :($counters = zeros(Uint64, $n_lines))})
        return exlet, tags, funcreport, funcclear
    else
        return ex ,{}, :funcnoop, :funcnoop
    end
end

function funcnoop()
end

function profile_parse_all()
    del_all(_PROFILE_REPORTS)
    del_all(_PROFILE_CLEARS)
    del_all(_PROFILE_TAGS)
    retargs = {}
    for i = 1:length(_PROFILE_EXPR)
        newblock, tags, funcreport, funcclear = profile_parse(_PROFILE_EXPR[i])
        retargs = vcat(retargs, newblock.args)
        if !isempty(tags)
            push(_PROFILE_TAGS, tags)
            push(_PROFILE_REPORTS, funcreport)
            push(_PROFILE_CLEARS, funcclear)
        end
    end
    push(retargs,:(return nothing))
    return expr(:block,retargs)
end

function profile_report()
    exret = cell(length(_PROFILE_REPORTS)+2)
    ret = gensym()
    exret[1] = :($ret = {})
    for i = 1:length(_PROFILE_REPORTS)
        exret[i+1] = :(push($ret,$(expr(:call,{_PROFILE_REPORTS[i]}))))
    end
    exret[end] = :(profile_print($ret))
    return expr(:block,exret)
end

function profile_print(tc)
    # Compute total elapsed time
    ttotal = 0.0
    for i = 1:length(tc)
        timers = tc[i][1]
        counters = tc[i][2]
        for j = 1:length(counters)
            calib_time = timers[j] - counters[j]*_PROFILE_CALIB
            ttotal += calib_time
        end
    end
    # Display output
    for i = 1:length(tc)
        timers = tc[i][1]
        counters = tc[i][2]
        println("   count  time(%)  time(s)")
        for j = 1:length(counters)
            if counters[j] != 0
                calib_time = timers[j] - counters[j]*_PROFILE_CALIB
                @printf("%8d    %5.2f  %f %s\n", counters[j],
                        100*(calib_time/ttotal),
                        calib_time*1e-9,
                        _PROFILE_TAGS[i][j])
            end
        end
    end
end

function profile_clear()
    exret = cell(length(_PROFILE_CLEARS)+1)
    for i = 1:length(_PROFILE_CLEARS)
        exret[i] = expr(:call,{_PROFILE_CLEARS[i]})
    end
    exret[end] = :(return nothing)
    return expr(:block,exret)
end

macro profile(ex)
    global _PROFILE_STATE
    if isa(ex,Symbol)
        # State changes
        if ex == :off
            _PROFILE_STATE = 0
        elseif ex == :on
            _PROFILE_STATE = _PROFILE_LINES
        elseif ex == :reset
        elseif ex == :report
            return profile_report()
        elseif ex == :clear
            return profile_clear()
        else
            error("Profile mode not recognized")
        end
        return profile_parse_all()
    elseif isa(ex,Expr)
        push(_PROFILE_EXPR,ex)
        exret, tags, funcreport, funcclear = profile_parse(ex)
        if !isempty(tags)
            push(_PROFILE_TAGS, tags)
            push(_PROFILE_REPORTS, funcreport)
            push(_PROFILE_CLEARS, funcclear)
        end
        return exret
    end
end

