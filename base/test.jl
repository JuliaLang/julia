module Test

export @test, @test_fails, @test_throws, @test_approx_eq, @test_approx_eq_eps, @inferred

abstract Result
type Success <: Result
    expr
    resultexpr
end
Success(expr) = Success(expr, nothing)
type Failure <: Result
    expr
    resultexpr
end
Failure(expr) = Failure(expr, nothing)
type Error <: Result
    expr
    err
    backtrace
end

default_handler(r::Success) = nothing
function default_handler(r::Failure)
    if r.resultexpr != nothing
        error("test failed: $(r.resultexpr)\n in expression: $(r.expr)")
    else
        error("test failed in expression: $(r.expr)")
    end
end
default_handler(r::Error) = rethrow(r)

handler() = get(task_local_storage(), :TEST_HANDLER, default_handler)

with_handler(f::Function, handler) =
    task_local_storage(f, :TEST_HANDLER, handler)

import Base.showerror

showerror(io::IO, r::Error) = showerror(io, r, [])
function showerror(io::IO, r::Error, bt)
    println(io, "test error in expression: $(r.expr)")
    showerror(io, r.err, r.backtrace)
end

function do_test(body,qex)
    handler()(try
        rex, val = body()
        val ? Success(qex, rex) : Failure(qex,rex)
    catch err
        Error(qex,err,catch_backtrace())
    end)
end

function do_test_throws(body, qex, bt, extype)
    handler()(try
        body()
        Failure(qex, "$qex did not throw $(extype == nothing ? "anything" : extype)")
    catch err
        if extype == nothing
            Base.warn("""
            @test_throws without an exception type is deprecated;
            Use `@test_throws $(typeof(err)) $(qex)` instead.
            """, bt = bt)
            Success(qex)
        else
            if isa(err, extype)
                Success(qex)
            else
                Failure(qex, "$err was thrown instead of $extype")
            end
        end
    end)
end

macro test(ex)
    if typeof(ex) == Expr && ex.head == :comparison
        syms = [gensym() for i = 1:length(ex.args)]
        func_block = Expr(:block)
        # insert assignment into a block
        func_block.args = [:($(syms[i]) = $(esc(ex.args[i]))) for i = 1:length(ex.args)]
        # finish the block with a return
        push!(func_block.args, Expr(:return, :(Expr(:comparison, $(syms...)), $(Expr(:comparison, syms...)))))
        :(do_test(()->($func_block), $(Expr(:quote,ex))))
    else
        :(do_test(()->($(Expr(:quote,ex)), $(esc(ex))), $(Expr(:quote,ex))))
    end
end

macro test_throws(args...)
    ex = nothing
    extype = nothing
    # Users should pass (ExceptionType, Expression) but we give a warning to users that only pass (Expression)
    if length(args) == 1
        ex = args[1]
    elseif length(args) == 2
        ex = args[2]
        extype = args[1]
    end
    :(do_test_throws(()->($(esc(ex))),$(Expr(:quote,ex)),backtrace(),$(esc(extype))))
end

macro test_fails(ex)
    Base.warn_once("@test_fails is deprecated, use @test_throws instead.")
    :(@test_throws $ex Exception)
end

approx_full(x::AbstractArray) = x
approx_full(x::Number) = x
approx_full(x) = full(x)

function test_approx_eq(va, vb, Eps, astr, bstr)
    va = approx_full(va)
    vb = approx_full(vb)
    if length(va) != length(vb)
        error("lengths of ", astr, " and ", bstr, " do not match: ",
              "\n  ", astr, " (length $(length(va))) = ", va,
              "\n  ", bstr, " (length $(length(vb))) = ", vb)
    end
    diff = real(zero(eltype(va)))
    for i = 1:length(va)
        xa = va[i]; xb = vb[i]
        if isfinite(xa) && isfinite(xb)
            diff = max(diff, abs(xa-xb))
        elseif !isequal(xa,xb)
            error("mismatch of non-finite elements: ",
                  "\n  ", astr, " = ", va,
                  "\n  ", bstr, " = ", vb)
        end
    end

    if !isnan(Eps) && !(diff <= Eps)
        sdiff = string("|", astr, " - ", bstr, "| <= ", Eps)
        error("assertion failed: ", sdiff,
              "\n  ", astr, " = ", va,
              "\n  ", bstr, " = ", vb,
              "\n  difference = ", diff, " > ", Eps)
    end
end

array_eps{T}(a::AbstractArray{Complex{T}}) = eps(float(maximum(x->(isfinite(x) ? abs(x) : T(NaN)), a)))
array_eps(a) = eps(float(maximum(x->(isfinite(x) ? abs(x) : oftype(x,NaN)), a)))

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
        inftypes = Base.return_types($(esc(ex.args[1])), ($([:(typeof(vals[$i])) for i = 1:length(ex.args)-1]...),))
        @assert length(inftypes) == 1
        result = $(esc(ex.args[1]))(vals...)
        rettype = typeof(result)
        is(rettype, inftypes[1]) || error("return type $rettype does not match inferred return type $(inftypes[1])")
        result
    end
end

end # module
