module BaseTest

export @test, @test_fails, @test_throws, 
       @test_approx_eq, @test_approx_eq_eps, @inferred,
       set_options, clear_results

abstract Result

#=
Global state:
Base.Test maintains tests results and some simple options in
global variables. Accessor methods are provided so that
packages may use the test results however they want, e.g. to
display them in some different way.
=#

# By default, do not fail instantly.
_fail_immediate = false

# By default, store results of tests. The alternative is to 
# store only the test outcomes, important if the test results
# consume a significant amount of memory, or we're just in a
# low memory environment.
_store_results = true

function set_options(; fail_immediate::Bool=nothing, 
                       store_results::Bool=nothing)
    global _fail_immediate
    if fail_immediate != nothing
        _fail_immediate = fail_immediate
    end
    global _store_results
    if store_results != nothing
        _store_results = store_results
    end
    nothing
end

_results_count = Dict(:success=>0,:failure=>0,:error=>0)
_results       = Result[]

function clear_results()
    global _results_count, _results
    _results_count = Dict(:success=>0,:failure=>0,:error=>0)
    _results       = Result[]
end

#=
Results:
Store information about the tests that failed, and the
way they failed.
=#
type Success <: Result
    expr
    resultexpr
end
Success(expr) = Success(expr, nothing)
function do_result(r::Success)
    _results_count[:success] += 1
    _store_results && push!(_results, r)
    return r
end

type Failure <: Result
    expr
    resultexpr
end
Failure(expr) = Failure(expr, nothing)
function do_result(r::Failure)
    if _fail_immediate
        if r.resultexpr != nothing
            error("test failed: $(r.resultexpr)\n in expression: $(r.expr)")
        else
            error("test failed in expression: $(r.expr)")
        end
    end
    _results_count[:failure] += 1
    _store_results && push!(_results, r)
    return r
end

type Error <: Result
    expr
    err
    backtrace
end
function do_result(r::Error)
    _fail_immediate && rethrow(r)
    _results_count[:error] += 1
    _store_results && push!(_results, r)
    return r
end
Base.showerror(io::IO, r::Error) = showerror(io, r, [])
function Base.showerror(io::IO, r::Error, bt)
    println(io, "test error in expression: $(r.expr)")
    showerror(io, r.err, r.backtrace)
end

function do_test(body,qex)
    try
        rex, val = body()
        do_result(val ? Success(qex, rex) : Failure(qex,rex))
    catch err
        do_result(Error(qex,err,catch_backtrace()))
    end
end

function do_test_throws(body, qex, bt, extype)
    do_result()(try
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
                if isa(err,Type)
                    Failure(qex, "the type $err was thrown instead of an instance of $extype")
                else
                    Failure(qex, "$err was thrown instead of $extype")
                end
            end
        end
    end)
end

macro test(ex)
    if typeof(ex) == Expr && ex.head == :comparison
        # If the test is a comparison, we store the values of all
        # terms in the comparison
        syms = [gensym() for i = 1:length(ex.args)]
        func_block = Expr(:block)
        # Insert assignment into a block
        func_block.args = [:($(syms[i]) = $(esc(ex.args[i]))) for i = 1:length(ex.args)]
        # Finish the block with a return
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
        inftypes = Base.return_types($(esc(ex.args[1])), Base.typesof(vals...))
        @assert length(inftypes) == 1
        result = $(esc(ex.args[1]))(vals...)
        rettype = isa(result, Type) ? Type{result} : typeof(result)
        rettype == inftypes[1] || error("return type $rettype does not match inferred return type $(inftypes[1])")
        result
    end
end

end # module
