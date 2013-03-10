module Test

export @test, @test_fails, @test_approx_eq, @test_approx_eq_eps

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

import Base.error_show

error_show(io::IO, r::Error) = error_show(io, r, {})
function error_show(io::IO, r::Error, bt)
    println(io, "test error during $(r.expr)")
    error_show(io, r.err, r.backtrace)
end

const handlers = [default_handler]

function do_test(thk, qex)
    handlers[end](try
        thk() ? Success(qex) : Failure(qex)
    catch err
        Error(qex,err,backtrace())
    end)
end

function do_test_fails(thk, qex)
    handlers[end](try
        thk()
        Failure(qex)
    catch err
        Success(qex)
    end)
end

macro test(ex)
    :(do_test(()->($(esc(ex))),$(Expr(:quote,ex))))
end

macro test_fails(ex)
    :(do_test_fails(()->($(esc(ex))),$(Expr(:quote,ex))))
end

function test_approx_eq(va, vb, Eps, astr, bstr)
    diff = max(abs(va - vb))
    sdiff = string("|", astr, " - ", bstr, "| < ", Eps)
    if diff < Eps
        nothing
    else
        error("assertion failed: ", sdiff, "\n  ", astr, " = ", va, "\n  ",
              bstr, " = ", vb)
    end
end

test_approx_eq(va, vb, astr, bstr) = test_approx_eq(va, vb, 10^4*length(va)*eps(max(max(abs(va)), max(abs(vb)))) * max(1, max(abs(va)), max(abs(vb))), astr, bstr)

macro test_approx_eq_eps(a, b, c)
    :(test_approx_eq($(esc(a)), $(esc(b)), $(esc(c)), $(string(a)), $(string(b))))
end

macro test_approx_eq(a, b)
    :(test_approx_eq($(esc(a)), $(esc(b)), $(string(a)), $(string(b))))
end

end # module
