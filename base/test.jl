module Test

export @test, @test_fails, @test_throws, @test_approx_eq, @test_approx_eq_eps

abstract Result
type Success <: Result
    expr
end
type Failure <: Result
    expr
end
type Error <: Result
    expr
    err
    backtrace
end

default_handler(r::Success) = nothing
default_handler(r::Failure) = error("test failed: $(r.expr)")
default_handler(r::Error)   = rethrow(r)

handler() = get(task_local_storage(), :TEST_HANDLER, default_handler)

with_handler(f::Function, handler) =
    task_local_storage(f, :TEST_HANDLER, handler)

import Base.showerror

showerror(io::IO, r::Error) = showerror(io, r, {})
function showerror(io::IO, r::Error, bt)
    println(io, "test error during $(r.expr)")
    showerror(io, r.err, r.backtrace)
end

function do_test(body,qex)
    handler()(try
        body() ? Success(qex) : Failure(qex)
    catch err
        Error(qex,err,catch_backtrace())
    end)
end

function do_test_throws(body,qex)
    handler()(try
        body()
        Failure(qex)
    catch
        Success(qex)
    end)
end

macro test(ex)
    :(do_test(()->($(esc(ex))),$(Expr(:quote,ex))))
end

macro test_throws(ex)
    :(do_test_throws(()->($(esc(ex))),$(Expr(:quote,ex))))
end
macro test_fails(ex)
    Base.warn_once("@test_fails is deprecated, use @test_throws instead.")
    :(@test_throws $ex)
end

function test_approx_eq(va, vb, Eps, astr, bstr)
    diff = maximum(abs(va - vb))
    if diff > Eps
        sdiff = string("|", astr, " - ", bstr, "| <= ", Eps)
        error("assertion failed: ", sdiff,
	      "\n  ", astr, " = ", va,
	      "\n  ", bstr, " = ", vb,
	      "\n  difference = ", diff, " > ", Eps)
    end
end

test_approx_eq(va, vb, astr, bstr) =
    test_approx_eq(va, vb, 10^4*length(va)*max(eps(float(maximum(abs(va)))), eps(float(maximum(abs(vb))))), astr, bstr)

macro test_approx_eq_eps(a, b, c)
    :(test_approx_eq($(esc(a)), $(esc(b)), $(esc(c)), $(string(a)), $(string(b))))
end

macro test_approx_eq(a, b)
    :(test_approx_eq($(esc(a)), $(esc(b)), $(string(a)), $(string(b))))
end

end # module
