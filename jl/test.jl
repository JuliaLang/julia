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
    println("a")
    hdl = Task(() -> _runtests_task(filenames))
    println("b")
    outputter(hdl)
    println("c")
end

function _runtests_task(filenames)
    println("C")
    for fn = filenames
        println("d")
        load(fn)
        println("e")
    end
end

# the default printer
function test_printer_simple(hdl::Task)
    for t = hdl
        show(t)
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
    quote
        produce(_dotest(:(ex), $string(ex)))
    end
end

function _dotest(ex::Expr, ex_str::String)
    tr = TestResult()
    
    # store things we already know
    tr.expr_str = ex_str
    tr.context = tls(:context)
    tr.group = tls(:group)
    
    # evaluate the expression while timing it and catching exceptions
    #try
        #$(tr).elapsed = @elapsed (
            tr.result = assert_test(eval(ex))
    # catch except
    #      $tr.result = False
    #      $tr.exception_thrown = except
    #  end
         
    
    # figure out the parts of the expression and evaluate separately
    
    tr
end