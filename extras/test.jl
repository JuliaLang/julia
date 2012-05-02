# test suite functions and macros

# tests
# TODO: re-enable when filesystem tests become available!
# function tests(onestr::String, outputter::Function) 
#     # if onestr is an existing file, pass it to the primary testing function 
#     stat = strip(readall(`stat -f "%HT" $onestr`))
#     if (stat == "Regular File")
#         tests([onestr], outputter)
#     elseif (stat == "Directory")
#         # if it's a directory name, find all test_*.jl in that and subdirectories, and pass
#         # that list
#         files_str = strip(readall(`find $onestr -name test_*.jl -print`))
#         if (length(files_str) > 0)
#             tests(split(files_str, "\n"), outputter)
#         else
#             # otherwise, throw an error
#             error("no test_*.jl files in directory: $onestr")
#         end
#     end
# end
# tests(onestr::String) = tests(onestr, test_printer_raw)
# tests(fn::Function) = tests(".", fn)
# tests() = tests(".")

tests(onestr::String, outputter::Function) = tests([onestr], outputter)
tests(onestr::String) = tests([onestr], test_printer_raw)
    
function tests(filenames, outputter::Function)
    # run these files as a task
    hdl = Task(() -> _tests_task(filenames))
    outputter(hdl)
end
tests(filenames) = tests(filenames, test_printer_raw)

function _tests_task(filenames)
    for fn = filenames
        load(fn)
    end
end

# the default printer
function test_printer_raw(hdl::Task)
    for t = hdl
        if (t.succeed)
            print(".")
        else
            println("")
            println("In $(t.context) / $(t.group)")
            println("$(t.expr_str) FAILED")
            println("$(t.operation) with args:")
            println("1: $(t.arg1)\n2: $(t.arg2)\n3: $(t.arg3)")
            println("Exception: $(t.exception_thrown)")
            println(sprintf("%0.3f seconds\n", t.elapsed))
            println("")
        end
    end
    println("")
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
    succeed::Bool # good outcome == true
    elapsed::Float
    exception_thrown::Exception
    operation
    arg1
    arg2
    arg3
end
TestResult() = TestResult("", "", "", false, NaN, NoException(), Nothing, Nothing, Nothing, Nothing)

# the macro just wraps the expression in a couple layers of quotes, then passes it to a function
# that does the real work
macro test(ex)
    quote
        $_test(expr(:quote, ex), true)
    end
end

macro testfails(ex)
    quote
        $_test(expr(:quote, ex), false)
    end
end

function _test(ex::Expr, expect_succeed::Bool)
    local tr = TestResult()
    tr.context = tls(:context)
    tr.group = tls(:group)
    
    # unwrap once
    ex = eval(ex)
    
    # save the string
    tr.expr_str = string(ex)
    
    # eval the whole thing, capturing exceptions and times
    try
        tr.elapsed = @elapsed tr.succeed = eval(ex)
    catch except
        tr.exception_thrown = except
    end
    
    # if we failed without an exception, pull apart the expression and see about evaluating
    # the parts
    if (!tr.succeed && tr.exception_thrown == NoException())
        if (ex.head == :comparison)
            tr.operation = ex.head
            tr.arg1 = eval(ex.args[1])
            tr.arg2 = eval(ex.args[3])
        elseif (ex.head == :call) # is it a helper we know about?
            if (ex.args[1] == :isapprox)
                tr.operation = ex.args[1]
                tr.arg1 = eval(ex.args[2])
                tr.arg2 = eval(ex.args[3])
            elseif (ex.args[1] == :prints)
                tr.operation = ex.args[1]
                tr.arg1 = sprint(eval(ex.args[2]), eval(ex.args[3])...)
                tr.arg2 = eval(ex.args[4]) 
            end
        end
    end
    
    # if we failed with an exception, handle throws_exception
    if tr.exception_thrown != NoException() && ex.args[1] == :throws_exception
        if isa(tr.exception_thrown, eval(ex.args[3])) # we got the right one
            tr.succeed = true
        end
    end
    
    # if we're running takes_less_than, see how we did
    if (ex.args[1] == :takes_less_than)
        tr.succeed = tr.elapsed < eval(ex.args[3])
        tr.operation = ex.args[1]
        tr.arg1 = tr.elapsed
        tr.arg2 = eval(ex.args[3])
    end
    
    # flip the result if we expect to fail
    if !expect_succeed
        tr.succeed = !tr.succeed
    end
    
    produce(tr)
end

# helpful utility tests, supported by the macro
# 

function approx_eq(a, b, tol)
    abs(a - b) < tol
end
approx_eq(a, b) = approx_eq(a, b, 1e-6)

function prints(fn::Function, args, expected::String) 
    sprint(fn, args...) == expected
end

function takes_less_than(anything, expected)
    # the magic happens in _test
    true
end

function throws_exception(anything, expected_exception)
    # the magic happens in _test
    true
end

