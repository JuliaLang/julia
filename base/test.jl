module Test

export @test, @test_fails, @test_throws, @test_approx_eq, @test_approx_eq_eps, @inferred

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

approx_full(x::StoredArray) = x
approx_full(x::Number) = x
approx_full(x) = full(x)

function test_approx_eq(va, vb, Eps, astr, bstr)
    va = approx_full(va)
    vb = approx_full(vb)
    diff = real(zero(eltype(va)))
    ok = true
    for i = 1:length(va)
        xa = va[i]; xb = vb[i]
        if isfinite(xa) && isfinite(xb)
            diff = max(diff, abs(xa-xb))
        elseif !isequal(xa,xb)
            ok = false; break
        end
    end

    if !ok || (!isnan(Eps) && !(diff <= Eps))
        sdiff = string("|", astr, " - ", bstr, "| <= ", Eps)
        error("assertion failed: ", sdiff,
	      "\n  ", astr, " = ", va,
	      "\n  ", bstr, " = ", vb,
	      "\n  difference = ", diff, " > ", Eps)
    end
end

array_eps(a) = eps(float(maximum(x->(isfinite(x) ? abs(x) : nan(x)), a)))

test_approx_eq(va, vb, astr, bstr) =
    test_approx_eq(va, vb, 1E4*length(va)*max(array_eps(va), array_eps(vb)), astr, bstr)

macro test_approx_eq_eps(a, b, c)
    :(test_approx_eq($(esc(a)), $(esc(b)), $(esc(c)), $(string(a)), $(string(b))))
end

macro test_approx_eq(a, b)
    :(test_approx_eq($(esc(a)), $(esc(b)), $(string(a)), $(string(b))))
end

macro inferred(ex)
    ex.head == :call || error("@inferred requires a call expression")
    quote
        vals = ($([esc(ex.args[i]) for i = 2:length(ex.args)]...),)
        inftypes = Base.return_types($(ex.args[1]), ($([:(typeof(vals[$i])) for i = 1:length(ex.args)-1]...),))
        @assert length(inftypes) == 1
        result = $(esc(ex.args[1]))(vals...)
        rettype = typeof(result)
        is(rettype, inftypes[1]) || error("return type $rettype does not match inferred return type $(inftypes[1])")
        result
    end
end

end # module
