# test suite functions and macros

# tests
function tests(onestr::String, outputter::Function) 
    # if onestr is an existing file, pass it to the primary testing function 
    stat = strip(readall(`stat -f "%HT" $onestr`))
    if (stat == "Regular File")
        tests([onestr], outputter)
    elseif (stat == "Directory")
        # if it's a directory name, find all test_*.jl in that and subdirectories, and pass
        # that list
        files_str = strip(readall(`find $onestr -name test_*.jl -print`))
        if (length(files_str) > 0)
            tests(split(files_str, "\n"), outputter)
        else
            # otherwise, throw an error
            error("no test_*.jl files in directory: $onestr")
        end
    end
end
tests(onestr::String) = tests(onestr, test_printer_simple)
tests(fn::Function) = tests(".", fn)
tests() = tests(".")

function tests(filenames, outputter::Function)
    # run these files as a task
    hdl = Task(() -> _tests_task(filenames))
    outputter(hdl)
end
tests(filenames) = tests(filenames, test_printer_simple)

function _tests_task(filenames)
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
            show(t) # TODO spiff up
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

# had some issues with struct access inside the macro, so don't build the TestResult until the end,
# which is ugly and should be fixed (TODO)
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
            $elapsed = @elapsed $res = eval($ex)
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
            elseif ($string(ex.head) == "call")
                if ($string(ex.args[1]) == "approx_eq")
                    $op = "approx_eq"
                    $arg1 = eval($ex.args[2])
                    $arg2 = eval($ex.args[3])
                elseif ($string(ex.args[1]) == "prints")
                    $op = "prints"
                    $arg1 = print_to_string(eval($ex.args[2]), eval($ex.args[3])...)
                    #$arg2 = eval($ex.args[4]) # TODO fails?!
                end
            end
        end
        
        # if we're running takes_less_than, see how we did
        if ($string(ex.args[1]) == "takes_less_than")
            $res = $elapsed < eval($ex.args[3])
            $op = "takes_less_than"
            $arg1 = $elapsed
            $arg2 = eval($ex.args[3])
        end
        
        produce(TestResult(tls(:context), tls(:group), $string(ex), $res, $elapsed,
            $exc, $op, $arg1, $arg2, Nothing))
    end
end

# helpful utility tests, supported by the macro
# 

function approx_eq(a, b, tol)
    abs(a - b) < tol
end
approx_eq(a, b) = approx_eq(a, b, 1e-6)

function prints(fn::Function, args, expected::String) 
    print_to_string(fn, args...) == expected
end

function takes_less_than(anything, expected)
    # the magic happens in @test
    true
end

