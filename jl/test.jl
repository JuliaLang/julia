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
    hdl = Task(Nothing -> _runtests_task(filenames))
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

# the primary macros
macro test(ex)
    tr = gensym()
    quote
        local $tr = TestResult()
        
        # store things we know already
        $tr.expr_str = $string(ex)
        $tr.context = tls(:context)
        $tr.group = tls(:group)
        
        # evaluate the expression while timing it and catching exceptions
        try
            $tr.elapsed = @elapsed ($tr.result = assert_test($ex))
        catch except
            $tr.result = False
            $tr.exception_thrown = except
        end
            
        
        # figure out the parts
        
        # don't return anything, just produce it
        produce($tr)
    end
end