# test suite functions and macros

# runtests
function runtests(onestr::String) 
    # if onestr is an existing readable file, pass it to the primary testing function 
    if (is_file_readable(onestr))
        runtests([onestr], test_printer_simple)
    else
    # if it's a directory name, find all test_*.jl in that and subdirectories, and pass
    # that list
    
    # otherwise, throw an error
        error("not a file or directory: $onestr")
    end
end

function runtests(filenames, outputter::Function)
    # run these files as a task
    hdl = Task(() -> _runtests_task(filenames))
    outputter(hdl)
end

function _runtests_task(filenames)
    for fn = filenames
        load(fn)
    end
end

# the default printer
function test_printer_simple(hdl::Task)
    for t = hdl
        if (t.result)
            print(".")
        else
            println("")
            show(t)
            println("")
        end
    end
end

# things to set state
function test_context(context::String)
    tls(:context, context)
end
function test_group(group::String)
    tls(:group, group)
end

# data structures
type NoException <: Exception
end

type TestResult
    context
    group
    expr_str::String
    result::Bool
    elapsed::Float
    exception_thrown::Exception
    operation
    arg1
    arg2
    arg3
end
TestResult() = TestResult("", "", "", false, NaN, NoException(), Nothing, Nothing, Nothing, Nothing)

# the macro captures the expression, then calls a function that does the actual work
macro test(ex)
    res, elapsed, exc, op, arg1, arg2, arg3 = gensym(7)
    quote
        local $elapsed = NaN
        local $res = false
        local $exc = NoException()
        local $op = Nothing
        local $arg1 = Nothing
        local $arg2 = Nothing
        
        try
            $elapsed = @elapsed $res = assert_test($ex)
        catch except
            $exc = except
        end
        
        # if we failed without an exception, pull apart the expression and see if we can't evaluate its
        # parts
        if ($res == false && $exc == NoException())
            if ($string(ex.head) == "comparison")
                $op = $string(ex.head)
                $arg1 = eval($ex.args[1])
                $arg2 = eval($ex.args[3])
            end
        
        end
        
        produce(TestResult(tls(:context), tls(:group), $string(ex), $res, $elapsed,
            $exc, $op, $arg1, $arg2, Nothing))
        #produce(_dotest(:(ex), $string(ex)))
    end
end
