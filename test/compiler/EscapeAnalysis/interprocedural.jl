# IPO EA Test
# ===========
# EA works on pre-inlining IR

include(normpath(@__DIR__, "setup.jl"))

# callsites
# ---------

noescape(a) = nothing
noescape(a, b) = nothing
function global_escape!(x)
    GR[] = x
    return nothing
end
union_escape!(x) = global_escape!(x)
union_escape!(x::SafeRef) = nothing
union_escape!(x::SafeRefs) = nothing
Base.@constprop :aggressive function conditional_escape!(cnd, x)
    cnd && global_escape!(x)
    return nothing
end

# MethodMatchInfo -- global cache
let result = code_escapes((SafeRef{String},); optimize=false) do x
        return noescape(x)
    end
    @test has_no_escape(ignore_argescape(result.state[Argument(2)]))
end
let result = code_escapes((SafeRef{String},); optimize=false) do x
        identity(x)
        return nothing
    end
    @test has_no_escape(ignore_argescape(result.state[Argument(2)]))
end
let result = code_escapes((SafeRef{String},); optimize=false) do x
        return identity(x)
    end
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test has_return_escape(result.state[Argument(2)], r)
end
let result = code_escapes((SafeRef{String},); optimize=false) do x
        return Ref(x)
    end
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test has_return_escape(result.state[Argument(2)], r)
end
let result = code_escapes((SafeRef{String},); optimize=false) do x
        r = Ref{SafeRef{String}}()
        r[] = x
        return r
    end
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test has_return_escape(result.state[Argument(2)], r)
end
let result = code_escapes((SafeRef{String},); optimize=false) do x
        global_escape!(x)
    end
    @test has_all_escape(result.state[Argument(2)])
end
# UnionSplitInfo
let result = code_escapes((Bool,Vector{Any}); optimize=false) do c, s
        x = c ? s : SafeRef(s)
        union_escape!(x)
    end
    @test has_all_escape(result.state[Argument(3)]) # s
end
let result = code_escapes((Bool,Vector{Any}); optimize=false) do c, s
        x = c ? SafeRef(s) : SafeRefs(s, s)
        union_escape!(x)
    end
    @test has_no_escape(ignore_argescape(result.state[Argument(2)]))
end
# ConstCallInfo -- local cache
let result = code_escapes((SafeRef{String},); optimize=false) do x
        return conditional_escape!(false, x)
    end
    @test has_no_escape(ignore_argescape(result.state[Argument(2)]))
end
# InvokeCallInfo
let result = code_escapes((SafeRef{String},); optimize=false) do x
        return @invoke noescape(x::Any)
    end
    @test has_no_escape(ignore_argescape(result.state[Argument(2)]))
end
let result = code_escapes((SafeRef{String},); optimize=false) do x
        return @invoke conditional_escape!(false::Any, x::Any)
    end
    @test has_no_escape(ignore_argescape(result.state[Argument(2)]))
end

# MethodError
# -----------
# accounts for ThrownEscape via potential MethodError

# no method error
identity_if_string(x::SafeRef) = nothing
let result = code_escapes((SafeRef{String},); optimize=false) do x
        identity_if_string(x)
    end
    i = only(findall(iscall((result.ir, identity_if_string)), result.ir.stmts.inst))
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test !has_thrown_escape(result.state[Argument(2)], i)
    @test !has_return_escape(result.state[Argument(2)], r)
end
let result = code_escapes((Union{SafeRef{String},Vector{String}},); optimize=false) do x
        identity_if_string(x)
    end
    i = only(findall(iscall((result.ir, identity_if_string)), result.ir.stmts.inst))
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test has_thrown_escape(result.state[Argument(2)], i)
    @test !has_return_escape(result.state[Argument(2)], r)
end
let result = code_escapes((SafeRef{String},); optimize=false) do x
        try
            identity_if_string(x)
        catch err
            global GV = err
        end
        return nothing
    end
    @test !has_all_escape(result.state[Argument(2)])
end
let result = code_escapes((Union{SafeRef{String},Vector{String}},); optimize=false) do x
        try
            identity_if_string(x)
        catch err
            global GV = err
        end
        return nothing
    end
    @test has_all_escape(result.state[Argument(2)])
end
# method ambiguity error
ambig_error_test(a::SafeRef, b) = nothing
ambig_error_test(a, b::SafeRef) = nothing
ambig_error_test(a, b) = nothing
let result = code_escapes((SafeRef{String},Any); optimize=false) do x, y
        ambig_error_test(x, y)
    end
    i = only(findall(iscall((result.ir, ambig_error_test)), result.ir.stmts.inst))
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test has_thrown_escape(result.state[Argument(2)], i)  # x
    @test has_thrown_escape(result.state[Argument(3)], i)  # y
    @test !has_return_escape(result.state[Argument(2)], r)  # x
    @test !has_return_escape(result.state[Argument(3)], r)  # y
end
let result = code_escapes((SafeRef{String},Any); optimize=false) do x, y
        try
            ambig_error_test(x, y)
        catch err
            global GV = err
        end
    end
    @test has_all_escape(result.state[Argument(2)])  # x
    @test has_all_escape(result.state[Argument(3)])  # y
end

# Local EA integration
# --------------------

# propagate escapes imposed on call arguments

# FIXME handle _apply_iterate
# FIXME currently we can't prove the effect-freeness of `getfield(RefValue{String}, :x)`
# because of this check https://github.com/JuliaLang/julia/blob/94b9d66b10e8e3ebdb268e4be5f7e1f43079ad4e/base/compiler/tfuncs.jl#L745
# and thus it leads to the following two broken tests

@noinline broadcast_noescape1(a) = (broadcast(identity, a); nothing)
let result = code_escapes() do
        broadcast_noescape1(Ref("Hi"))
    end
    i = only(findall(isnew, result.ir.stmts.inst))
    @test_broken !has_return_escape(result.state[SSAValue(i)])
    @test_broken !has_thrown_escape(result.state[SSAValue(i)])
end
@noinline broadcast_noescape2(b) = broadcast(identity, b)
let result = code_escapes() do
        broadcast_noescape2(Ref("Hi"))
    end
    i = only(findall(isnew, result.ir.stmts.inst))
    @test_broken !has_return_escape(result.state[SSAValue(i)])
    @test_broken !has_thrown_escape(result.state[SSAValue(i)])
end
@noinline allescape_argument(a) = (global GV = a) # obvious escape
let result = code_escapes() do
        allescape_argument(Ref("Hi"))
    end
    i = only(findall(isnew, result.ir.stmts.inst))
    @test has_all_escape(result.state[SSAValue(i)])
end
# if we can't determine the matching method statically, we should be conservative
let result = code_escapes((Ref{Any},)) do a
        may_exist(a)
    end
    @test has_all_escape(result.state[Argument(2)])
end
let result = code_escapes((Ref{Any},)) do a
        Base.@invokelatest broadcast_noescape1(a)
    end
    @test has_all_escape(result.state[Argument(2)])
end

# handling of simple union-split (just exploit the inliner's effort)
@noinline unionsplit_noescape(a)      = string(nothing)
@noinline unionsplit_noescape(a::Int) = a + 10
let result = code_escapes((Union{Int,Nothing},)) do x
        s = SafeRef{Union{Int,Nothing}}(x)
        unionsplit_noescape(s[])
        return nothing
    end
    inds = findall(isnew, result.ir.stmts.inst) # find allocation statement
    @assert !isempty(inds)
    for i in inds
        @test has_no_escape(result.state[SSAValue(i)])
    end
end

@noinline function unused_argument(a)
    println("prevent inlining")
    return Base.inferencebarrier(nothing)
end
let result = code_escapes() do
        a = Ref("foo") # shouldn't be "return escape"
        b = unused_argument(a)
        nothing
    end
    i = only(findall(isnew, result.ir.stmts.inst))
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test !has_return_escape(result.state[SSAValue(i)], r)

    result = code_escapes() do
        a = Ref("foo") # still should be "return escape"
        b = unused_argument(a)
        return a
    end
    i = only(findall(isnew, result.ir.stmts.inst))
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test has_return_escape(result.state[SSAValue(i)], r)
end

# should propagate escape information imposed on return value to the aliased call argument
@noinline returnescape_argument(a) = (println("prevent inlining"); a)
let result = code_escapes() do
        obj = Ref("foo")           # should be "return escape"
        ret = returnescape_argument(obj)
        return ret                 # alias of `obj`
    end
    i = only(findall(isnew, result.ir.stmts.inst))
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test has_return_escape(result.state[SSAValue(i)], r)
end
@noinline noreturnescape_argument(a) = (println("prevent inlining"); identity("hi"))
let result = code_escapes() do
        obj = Ref("foo")              # better to not be "return escape"
        ret = noreturnescape_argument(obj)
        return ret                    # must not alias to `obj`
    end
    i = only(findall(isnew, result.ir.stmts.inst))
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test !has_return_escape(result.state[SSAValue(i)], r)
end
