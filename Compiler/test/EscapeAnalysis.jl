module test_EA

include("setup_Compiler.jl")
include("irutils.jl")

const EscapeAnalysis = Compiler.EscapeAnalysis

include("EAUtils.jl")

using Test, .EscapeAnalysis, .EAUtils
using .EscapeAnalysis: ignore_argescape

let utils_ex = quote
        mutable struct SafeRef{T}
            x::T
        end
        Base.getindex(s::SafeRef) = getfield(s, 1)
        Base.setindex!(s::SafeRef, x) = setfield!(s, 1, x)

        mutable struct SafeRefs{S,T}
            x1::S
            x2::T
        end
        Base.getindex(s::SafeRefs, idx::Int) = getfield(s, idx)
        Base.setindex!(s::SafeRefs, x, idx::Int) = setfield!(s, idx, x)

        global GV::Any
        const global GR = Ref{Any}()
    end
    global function EATModule(utils_ex = utils_ex)
        M = Module()
        Core.eval(M, utils_ex)
        return M
    end
    Core.eval(@__MODULE__, utils_ex)
end

using .EscapeAnalysis: EscapeInfo, IndexableFields

isϕ(@nospecialize x) = isa(x, Core.PhiNode)
"""
    is_load_forwardable(x::EscapeInfo) -> Bool

Queries if `x` is elibigle for store-to-load forwarding optimization.
"""
function is_load_forwardable(x::EscapeInfo)
    AliasInfo = x.AliasInfo
    # NOTE technically we also need to check `!has_thrown_escape(x)` here as well,
    # but we can also do equivalent check during forwarding
    return isa(AliasInfo, IndexableFields)
end

@testset "EAUtils" begin
    @test_throws "everything has been constant folded" code_escapes() do; sin(42); end
    @test code_escapes(sin, (Int,)) isa EAUtils.EscapeResult
    @test code_escapes(sin, (Int,)) isa EAUtils.EscapeResult
end

@testset "basics" begin
    let # arg return
        result = code_escapes((Any,)) do a # return to caller
            println("prevent ConstABI")
            return nothing
        end
        @test has_arg_escape(result.state[Argument(2)])
        # return
        result = code_escapes((Any,)) do a
            println("prevent ConstABI")
            return a
        end
        i = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_arg_escape(result.state[Argument(1)]) # self
        @test !has_return_escape(result.state[Argument(1)], i) # self
        @test has_arg_escape(result.state[Argument(2)]) # a
        @test has_return_escape(result.state[Argument(2)], i) # a
    end
    let # global store
        result = code_escapes((Any,)) do a
            global GV = a
            nothing
        end
        @test has_all_escape(result.state[Argument(2)])
    end
    let # global load
        result = code_escapes() do
            global GV
            return GV
        end
        i = only(findall(has_return_escape, map(i->result.state[SSAValue(i)], 1:length(result.ir.stmts))))
        @test has_all_escape(result.state[SSAValue(i)])
    end
    let # global store / load (https://github.com/aviatesk/EscapeAnalysis.jl/issues/56)
        result = code_escapes((Any,)) do s
            global GV
            GV = s
            return GV
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
    end
    let # :gc_preserve_begin / :gc_preserve_end
        result = code_escapes((String,)) do s
            m = SafeRef(s)
            GC.@preserve m begin
                println(s)
                return nothing
            end
        end
        i = findfirst(==(SafeRef{String}), result.ir.stmts.type) # find allocation statement
        @test !isnothing(i)
        @test has_no_escape(result.state[SSAValue(i)])
    end
    let # :isdefined
        result = code_escapes((String, Bool,)) do a, b
            if b
                s = Ref(a)
            end
            return @isdefined(s)
        end
        i = findfirst(==(Base.RefValue{String}), result.ir.stmts.type) # find allocation statement
        @test isnothing(i) || has_no_escape(result.state[SSAValue(i)])
    end
    let # ϕ-node
        result = code_escapes((Bool,Any,Any)) do cond, a, b
            c = cond ? a : b # ϕ(a, b)
            return c
        end
        @assert any(@nospecialize(x)->isa(x, Core.PhiNode), result.ir.stmts.stmt)
        i = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(3)], i) # a
        @test has_return_escape(result.state[Argument(4)], i) # b
    end
    let # π-node
        result = code_escapes((Any,)) do a
            if isa(a, Regex) # a::π(Regex)
                return a
            end
            return nothing
        end
        @assert any(@nospecialize(x)->isa(x, Core.PiNode), result.ir.stmts.stmt)
        @test any(findall(isreturn, result.ir.stmts.stmt)) do i
            has_return_escape(result.state[Argument(2)], i)
        end
    end
    let # φᶜ-node / ϒ-node
        result = code_escapes((Any,String)) do a, b
            local x::String
            try
                x = a
            catch err
                x = b
            end
            return x
        end
        @assert any(@nospecialize(x)->isa(x, Core.PhiCNode), result.ir.stmts.stmt)
        @assert any(@nospecialize(x)->isa(x, Core.UpsilonNode), result.ir.stmts.stmt)
        i = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], i)
        @test has_return_escape(result.state[Argument(3)], i)
    end
    let # branching
        result = code_escapes((Any,Bool,)) do a, c
            if c
                return nothing # a doesn't escape in this branch
            else
                return a # a escapes to a caller
            end
        end
        @test has_return_escape(result.state[Argument(2)])
    end
    let # loop
        result = code_escapes((Int,)) do n
            c = SafeRef{Bool}(false)
            while n > 0
                rand(Bool) && return c
            end
            nothing
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_return_escape(result.state[SSAValue(i)])
    end
    let # try/catch
        result = code_escapes((Any,)) do a
            try
                println("prevent ConstABI")
                nothing
            catch err
                return a # return escape
            end
        end
        @test has_return_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,)) do a
            try
                println("prevent ConstABI")
                nothing
            finally
                return a # return escape
            end
        end
        @test has_return_escape(result.state[Argument(2)])
    end
    let # :foreigncall
        result = code_escapes((Any,)) do x
            ccall(:some_ccall, Any, (Any,), x)
        end
        @test has_all_escape(result.state[Argument(2)])
    end
end

@testset "builtins" begin
    let # throw
        r = code_escapes((Any,)) do a
            throw(a)
        end
        @test has_thrown_escape(r.state[Argument(2)])
    end

    let # implicit throws
        r = code_escapes((Any,)) do a
            getfield(a, :may_not_field)
        end
        @test has_thrown_escape(r.state[Argument(2)])

        r = code_escapes((Any,)) do a
            sizeof(a)
        end
        @test has_thrown_escape(r.state[Argument(2)])
    end

    let # :===
        result = code_escapes((Bool, SafeRef{String})) do cond, s
            m = cond ? s : nothing
            c = m === nothing
            return c
        end
        @test has_no_escape(ignore_argescape(result.state[Argument(2)]))
    end

    let # sizeof
        result = code_escapes((Vector{Any},)) do xs
            sizeof(xs)
        end
        @test has_no_escape(ignore_argescape(result.state[Argument(2)]))
    end

    let # ifelse
        result = code_escapes((Bool,)) do c
            r = ifelse(c, Ref("yes"), Ref("no"))
            return r
        end
        inds = findall(isnew, result.ir.stmts.stmt)
        @assert !isempty(inds)
        for i in inds
            @test has_return_escape(result.state[SSAValue(i)])
        end
    end
    let # ifelse (with constant condition)
        result = code_escapes() do
            r = ifelse(true, Ref("yes"), Ref(nothing))
            return r
        end
        for i in 1:length(result.ir.stmts)
            if isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == Base.RefValue{String}
                @test has_return_escape(result.state[SSAValue(i)])
            elseif isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == Base.RefValue{Nothing}
                @test has_no_escape(result.state[SSAValue(i)])
            end
        end
    end

    let # typeassert
        result = code_escapes((Any,)) do x
            y = x::Base.RefValue{Any}
            return y
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        @test !has_all_escape(result.state[Argument(2)])
    end

    let # isdefined
        result = code_escapes((Any,)) do x
            isdefined(x, :foo) ? x : throw("undefined")
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        @test !has_all_escape(result.state[Argument(2)])
    end
end

@testset "flow-sensitivity" begin
    # ReturnEscape
    let result = code_escapes((Bool,)) do cond
            r = Ref("foo")
            if cond
                return cond
            end
            return r
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        rts = findall(isreturn, result.ir.stmts.stmt)
        @assert length(rts) == 2
        @test count(rt->has_return_escape(result.state[SSAValue(i)], rt), rts) == 1
    end
    let result = code_escapes((Bool,)) do cond
            r = Ref("foo")
            cnt = 0
            while rand(Bool)
                cnt += 1
                rand(Bool) && return r
            end
            rand(Bool) && return r
            return cnt
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        rts = findall(isreturn, result.ir.stmts.stmt) # return statement
        @assert length(rts) == 3
        @test count(rt->has_return_escape(result.state[SSAValue(i)], rt), rts) == 2
    end
end

@testset "escape through exceptions" begin
    M = @eval Module() begin
        unsafeget(x) = isassigned(x) ? x[] : throw(x)
        @noinline function escape_rethrow!()
            try
                rethrow()
            catch err
                GR[] = err
            end
        end
        @noinline function escape_current_exceptions!()
            excs = Base.current_exceptions()
            GR[] = excs
        end
        const GR = Ref{Any}()
        @__MODULE__
    end

    let # simple: return escape
        result = @eval M $code_escapes() do
            r = Ref{String}()
            local ret
            try
                s = unsafeget(r)
                ret = sizeof(s)
            catch err
                ret = err
            end
            return ret
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_return_escape(result.state[SSAValue(i)])
    end

    let # simple: global escape
        result = @eval M $code_escapes() do
            r = Ref{String}()
            local ret # prevent DCE
            try
                s = unsafeget(r)
                ret = sizeof(s)
            catch err
                global GV = err
            end
            nothing
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
    end

    let # account for possible escapes via nested throws
        result = @eval M $code_escapes() do
            r = Ref{String}()
            try
                try
                    unsafeget(r)
                catch err1
                    throw(err1)
                end
            catch err2
                GR[] = err2
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
    end
    let # account for possible escapes via `rethrow`
        result = @eval M $code_escapes() do
            r = Ref{String}()
            try
                try
                    unsafeget(r)
                catch err1
                    rethrow(err1)
                end
            catch err2
                GR[] = err2
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
    end
    let # account for possible escapes via `rethrow`
        result = @eval M $code_escapes() do
            try
                r = Ref{String}()
                unsafeget(r)
            catch
                escape_rethrow!()
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
    end
    let # account for possible escapes via `rethrow`
        result = @eval M $code_escapes() do
            local t
            try
                r = Ref{String}()
                t = unsafeget(r)
            catch err
                t = typeof(err)
                escape_rethrow!()
            end
            return t
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
    end
    let # account for possible escapes via `Base.current_exceptions`
        result = @eval M $code_escapes() do
            try
                r = Ref{String}()
                unsafeget(r)
            catch
                GR[] = Base.current_exceptions()
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
    end
    let # account for possible escapes via `Base.current_exceptions`
        result = @eval M $code_escapes() do
            try
                r = Ref{String}()
                unsafeget(r)
            catch
                escape_current_exceptions!()
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
    end

    let # contextual: escape information imposed on `err` shouldn't propagate to `r2`, but only to `r1`
        result = @eval M $code_escapes() do
            r1 = Ref{String}()
            r2 = Ref{String}()
            local ret
            try
                s1 = unsafeget(r1)
                ret = sizeof(s1)
            catch err
                global GV = err
            end
            s2 = unsafeget(r2)
            return s2, r2
        end
        is = findall(isnew, result.ir.stmts.stmt)
        @test length(is) == 2
        i1, i2 = is
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i1)])
        @test !has_all_escape(result.state[SSAValue(i2)])
        @test has_return_escape(result.state[SSAValue(i2)], r)
    end

    # XXX test cases below are currently broken because of the technical reason described in `escape_exception!`

    let # limited propagation: exception is caught within a frame => doesn't escape to a caller
        result = @eval M $code_escapes() do
            r = Ref{String}()
            local ret
            try
                s = unsafeget(r)
                ret = sizeof(s)
            catch
                ret = nothing
            end
            return ret
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test_broken !has_return_escape(result.state[SSAValue(i)], r) # TODO? see `escape_exception!`
    end
    let # sequential: escape information imposed on `err1` and `err2 should propagate separately
        result = @eval M $code_escapes() do
            r1 = Ref{String}()
            r2 = Ref{String}()
            local ret
            try
                s1 = unsafeget(r1)
                ret = sizeof(s1)
            catch err1
                global GV = err1
            end
            try
                s2 = unsafeget(r2)
                ret = sizeof(s2)
            catch err2
                ret = err2
            end
            return ret
        end
        is = findall(isnew, result.ir.stmts.stmt)
        @test length(is) == 2
        i1, i2 = is
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i1)])
        @test has_return_escape(result.state[SSAValue(i2)], r)
        @test_broken !has_all_escape(result.state[SSAValue(i2)]) # TODO? see `escape_exception!`
    end
    let # nested: escape information imposed on `inner` shouldn't propagate to `s`
        result = @eval M $code_escapes() do
            r = Ref{String}()
            local ret
            try
                s = unsafeget(r)
                try
                    ret = sizeof(s)
                catch inner
                    return inner
                end
            catch outer
                ret = nothing
            end
            return ret
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test_broken !has_return_escape(result.state[SSAValue(i)])
    end
    let # merge: escape information imposed on `err1` and `err2 should be merged
        result = @eval M $code_escapes() do
            r = Ref{String}()
            local ret
            try
                s = unsafeget(r)
                ret = sizeof(s)
            catch err1
                return err1
            end
            try
                s = unsafeget(r)
                ret = sizeof(s)
            catch err2
                return err2
            end
            nothing
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        rs = findall(isreturn, result.ir.stmts.stmt)
        @test_broken !has_all_escape(result.state[SSAValue(i)])
        for r in rs
            @test has_return_escape(result.state[SSAValue(i)], r)
        end
    end
    let # no exception handling: should keep propagating the escape
        result = @eval M $code_escapes() do
            r = Ref{String}()
            local ret
            try
                s = unsafeget(r)
                ret = sizeof(s)
            finally
                if !@isdefined(ret)
                    ret = 42
                end
            end
            return ret
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test_broken !has_return_escape(result.state[SSAValue(i)], r)
    end
end

@testset "field analysis / alias analysis" begin
    # escaped allocations
    # -------------------

    # escaped object should escape its fields as well
    let result = code_escapes((Any,)) do a
            global GV = SafeRef{Any}(a)
            nothing
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
        @test has_all_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,)) do a
            global GV = (a,)
            nothing
        end
        i = only(findall(iscall((result.ir, tuple)), result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
        @test has_all_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,)) do a
            o0 = SafeRef{Any}(a)
            global GV = SafeRef(o0)
            nothing
        end
        is = findall(isnew, result.ir.stmts.stmt)
        @test length(is) == 2
        i0, i1 = is
        @test has_all_escape(result.state[SSAValue(i0)])
        @test has_all_escape(result.state[SSAValue(i1)])
        @test has_all_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,)) do a
            t0 = (a,)
            global GV = (t0,)
            nothing
        end
        inds = findall(iscall((result.ir, tuple)), result.ir.stmts.stmt)
        @assert length(inds) == 2
        for i in inds; @test has_all_escape(result.state[SSAValue(i)]); end
        @test has_all_escape(result.state[Argument(2)])
    end
    # global escape through `setfield!`
    let result = code_escapes((Any,)) do a
            r = SafeRef{Any}(:init)
            global GV = r
            r[] = a
            nothing
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
        @test has_all_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,Any)) do a, b
            r = SafeRef{Any}(a)
            global GV = r
            r[] = b
            nothing
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[SSAValue(i)])
        @test has_all_escape(result.state[Argument(2)]) # a
        @test has_all_escape(result.state[Argument(3)]) # b
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef(Ref(""))
            $code_escapes((Base.RefValue{String},)) do s
                Rx[] = s
                Core.sizeof(Rx[])
            end
        end
        @test has_all_escape(result.state[Argument(2)])
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef{Any}(nothing)
            $code_escapes((Base.RefValue{String},)) do s
                setfield!(Rx, :x, s)
                Core.sizeof(Rx[])
            end
        end
        @test has_all_escape(result.state[Argument(2)])
    end
    let M = EATModule()
        @eval M module ___xxx___
            import ..SafeRef
            const Rx = SafeRef("Rx")
        end
        result = @eval M begin
            $code_escapes((String,)) do s
                rx = getfield(___xxx___, :Rx)
                rx[] = s
                nothing
            end
        end
        @test has_all_escape(result.state[Argument(2)])
    end

    # field escape
    # ------------

    # field escape should propagate to :new arguments
    let result = code_escapes((Base.RefValue{String},)) do a
            o = SafeRef(a)
            Core.donotdelete(o)
            return o[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    let result = code_escapes((Base.RefValue{String},)) do a
            t = SafeRef((a,))
            f = t[][1]
            return f
        end
        i = only(findall(iscall((result.ir, tuple)), result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    let result = code_escapes((Base.RefValue{String}, Base.RefValue{String})) do a, b
            obj = SafeRefs(a, b)
            Core.donotdelete(obj)
            fld1 = obj[1]
            fld2 = obj[2]
            return (fld1, fld2)
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test has_return_escape(result.state[Argument(3)], r) # b
        @test is_load_forwardable(result.state[SSAValue(i)])
    end

    # field escape should propagate to `setfield!` argument
    let result = code_escapes((Base.RefValue{String},)) do a
            o = SafeRef(Ref("foo"))
            Core.donotdelete(o)
            o[] = a
            return o[]
        end
        i = last(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    # propagate escape information imposed on return value of `setfield!` call
    let result = code_escapes((Base.RefValue{String},)) do a
            obj = SafeRef(Ref("foo"))
            Core.donotdelete(obj)
            return (obj[] = a)
        end
        i = last(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
    end

    # nested allocations
    let result = code_escapes((Base.RefValue{String},)) do a
            o1 = SafeRef(a)
            o2 = SafeRef(o1)
            return o2[]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        for i in 1:length(result.ir.stmts)
            if isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == SafeRef{String}
                @test has_return_escape(result.state[SSAValue(i)], r)
            elseif isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == SafeRef{SafeRef{String}}
                @test is_load_forwardable(result.state[SSAValue(i)])
            end
        end
    end
    let result = code_escapes((Base.RefValue{String},)) do a
            o1 = (a,)
            o2 = (o1,)
            return o2[1]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        for i in 1:length(result.ir.stmts)
            if isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == Tuple{String}
                @test has_return_escape(result.state[SSAValue(i)], r)
            elseif isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == Tuple{Tuple{String}}
                @test is_load_forwardable(result.state[SSAValue(i)])
            end
        end
    end
    let result = code_escapes((Base.RefValue{String},)) do a
            o1  = SafeRef(a)
            o2  = SafeRef(o1)
            o1′ = o2[]
            a′  = o1′[]
            return a′
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        for i in findall(isnew, result.ir.stmts.stmt)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
    end
    let result = code_escapes() do
            o1 = SafeRef("foo")
            o2 = SafeRef(o1)
            return o2
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        for i in findall(isnew, result.ir.stmts.stmt)
            @test has_return_escape(result.state[SSAValue(i)], r)
        end
    end
    let result = code_escapes() do
            o1   = SafeRef("foo")
            o2′  = SafeRef(nothing)
            o2   = SafeRef{SafeRef}(o2′)
            o2[] = o1
            return o2
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        findall(1:length(result.ir.stmts)) do i
            if isnew(result.ir.stmts[i][:stmt])
                t = result.ir.stmts[i][:type]
                return t === SafeRef{String}  || # o1
                       t === SafeRef{SafeRef}    # o2
            end
            return false
        end |> x->foreach(x) do i
            @test has_return_escape(result.state[SSAValue(i)], r)
        end
    end
    let result = code_escapes((Base.RefValue{String},)) do x
            o = Ref(x)
            Core.donotdelete(o)
            broadcast(identity, o)
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
    end

    # ϕ-node allocations
    let result = code_escapes((Bool,Any,Any)) do cond, x, y
            if cond
                ϕ = SafeRef{Any}(x)
            else
                ϕ = SafeRef{Any}(y)
            end
            return ϕ[]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(3)], r) # x
        @test has_return_escape(result.state[Argument(4)], r) # y
        i = only(findall(isϕ, result.ir.stmts.stmt))
        @test is_load_forwardable(result.state[SSAValue(i)])
        for i in findall(isnew, result.ir.stmts.stmt)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
    end
    let result = code_escapes((Bool,Any,Any)) do cond, x, y
            if cond
                ϕ2 = ϕ1 = SafeRef{Any}(x)
            else
                ϕ2 = ϕ1 = SafeRef{Any}(y)
            end
            return ϕ1[], ϕ2[]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(3)], r) # x
        @test has_return_escape(result.state[Argument(4)], r) # y
        for i in findall(isϕ, result.ir.stmts.stmt)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
        for i in findall(isnew, result.ir.stmts.stmt)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
    end
    # when ϕ-node merges values with different types
    let result = code_escapes((Bool,Base.RefValue{String},Base.RefValue{String},Base.RefValue{String})) do cond, x, y, z
            local out
            if cond
                ϕ = SafeRef(x)
                out = ϕ[]
            else
                ϕ = SafeRefs(z, y)
            end
            return @isdefined(out) ? out : throw(ϕ)
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        t = only(findall(iscall((result.ir, throw)), result.ir.stmts.stmt))
        ϕ = only(findall(==(Union{SafeRef{Base.RefValue{String}},SafeRefs{Base.RefValue{String},Base.RefValue{String}}}), result.ir.stmts.type))
        @test has_return_escape(result.state[Argument(3)], r) # x
        @test !has_return_escape(result.state[Argument(4)], r) # y
        @test has_return_escape(result.state[Argument(5)], r) # z
        @test has_thrown_escape(result.state[SSAValue(ϕ)], t)
    end

    # alias analysis
    # --------------

    # alias via getfield & Expr(:new)
    let result = code_escapes((String,)) do s
            r = SafeRef(s)
            Core.donotdelete(r)
            return r[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test isaliased(Argument(2), val, result.state)
        @test !isaliased(Argument(2), SSAValue(i), result.state)
    end
    let result = code_escapes((String,)) do s
            r1 = SafeRef(s)
            r2 = SafeRef(r1)
            Core.donotdelete(r1, r2)
            return r2[]
        end
        i1, i2 = findall(isnew, result.ir.stmts.stmt)
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test !isaliased(SSAValue(i1), SSAValue(i2), result.state)
        @test isaliased(SSAValue(i1), val, result.state)
        @test !isaliased(SSAValue(i2), val, result.state)
    end
    let result = code_escapes((String,)) do s
            r1 = SafeRef(s)
            r2 = SafeRef(r1)
            Core.donotdelete(r1, r2)
            return r2[][]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test isaliased(Argument(2), val, result.state)
        for i in findall(isnew, result.ir.stmts.stmt)
            @test !isaliased(SSAValue(i), val, result.state)
        end
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef("Rx")
            $code_escapes((String,)) do s
                r = SafeRef(Rx)
                Core.donotdelete(r)
                rx = r[] # rx aliased to Rx
                rx[] = s
                nothing
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result.state[Argument(2)])
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    # alias via getfield & setfield!
    let result = code_escapes((String,)) do s
            r = Ref{String}()
            Core.donotdelete(r)
            r[] = s
            return r[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test isaliased(Argument(2), val, result.state)
        @test !isaliased(Argument(2), SSAValue(i), result.state)
    end
    let result = code_escapes((String,)) do s
            r1 = Ref(s)
            r2 = Ref{Base.RefValue{String}}()
            Core.donotdelete(r1, r2)
            r2[] = r1
            return r2[]
        end
        i1, i2 = findall(isnew, result.ir.stmts.stmt)
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test !isaliased(SSAValue(i1), SSAValue(i2), result.state)
        @test isaliased(SSAValue(i1), val, result.state)
        @test !isaliased(SSAValue(i2), val, result.state)
    end
    let result = code_escapes((String,)) do s
            r1 = Ref{String}()
            r2 = Ref{Base.RefValue{String}}()
            Core.donotdelete(r1, r2)
            r2[] = r1
            r1[] = s
            return r2[][]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test isaliased(Argument(2), val, result.state)
        for i in findall(isnew, result.ir.stmts.stmt)
            @test !isaliased(SSAValue(i), val, result.state)
        end
        result = code_escapes((String,)) do s
            r1 = Ref{String}()
            r2 = Ref{Base.RefValue{String}}()
            r1[] = s
            r2[] = r1
            return r2[][]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test isaliased(Argument(2), val, result.state)
        for i in findall(isnew, result.ir.stmts.stmt)
            @test !isaliased(SSAValue(i), val, result.state)
        end
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef("Rx")
            $code_escapes((SafeRef{String}, String,)) do _rx, s
                r = SafeRef(_rx)
                Core.donotdelete(r)
                r[] = Rx
                rx = r[] # rx aliased to Rx
                rx[] = s
                nothing
            end
        end
        i = findfirst(isnew, result.ir.stmts.stmt)
        @test has_all_escape(result.state[Argument(3)])
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    # alias via typeassert
    let result = code_escapes((Any,)) do a
            r = a::Base.RefValue{String}
            return r
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test isaliased(Argument(2), val, result.state)       # a <-> r
    end
    let result = code_escapes((Any,)) do a
            global GV
            (g::SafeRef{Any})[] = a
            nothing
        end
        @test has_all_escape(result.state[Argument(2)])
    end
    # alias via ifelse
    let result = code_escapes((Bool,Any,Any)) do c, a, b
            r = ifelse(c, a, b)
            return r
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test has_return_escape(result.state[Argument(3)], r) # a
        @test has_return_escape(result.state[Argument(4)], r) # b
        @test !isaliased(Argument(2), val, result.state)      # c <!-> r
        @test isaliased(Argument(3), val, result.state)       # a <-> r
        @test isaliased(Argument(4), val, result.state)       # b <-> r
    end
    let result = @eval EATModule() begin
            const Lx, Rx = SafeRef("Lx"), SafeRef("Rx")
            $code_escapes((Bool,String,)) do c, a
                r = ifelse(c, Lx, Rx)
                r[] = a
                nothing
            end
        end
        @test has_all_escape(result.state[Argument(3)]) # a
    end
    # alias via ϕ-node
    let result = code_escapes((Bool,Base.RefValue{String})) do cond, x
            if cond
                ϕ2 = ϕ1 = SafeRef(Ref("foo"))
            else
                ϕ2 = ϕ1 = SafeRef(Ref("bar"))
            end
            ϕ2[] = x
            return ϕ1[]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test has_return_escape(result.state[Argument(3)], r) # x
        @test isaliased(Argument(3), val, result.state) # x
        for i in findall(isϕ, result.ir.stmts.stmt)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
        for i in findall(isnew, result.ir.stmts.stmt)
            if result.ir[SSAValue(i)][:type] <: SafeRef
                @test is_load_forwardable(result.state[SSAValue(i)])
            end
        end
    end
    let result = code_escapes((Bool,Bool,Base.RefValue{String})) do cond1, cond2, x
            if cond1
                ϕ2 = ϕ1 = SafeRef(Ref("foo"))
            else
                ϕ2 = ϕ1 = SafeRef(Ref("bar"))
            end
            cond2 && (ϕ2[] = x)
            return ϕ1[]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test has_return_escape(result.state[Argument(4)], r) # x
        @test isaliased(Argument(4), val, result.state) # x
        for i in findall(isϕ, result.ir.stmts.stmt)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
        for i in findall(isnew, result.ir.stmts.stmt)
            if result.ir[SSAValue(i)][:type] <: SafeRef
                @test is_load_forwardable(result.state[SSAValue(i)])
            end
        end
    end
    # alias via π-node
    let result = code_escapes((Any,)) do x
            if isa(x, Base.RefValue{String})
                return x
            end
            throw("error!")
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        rval = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test has_return_escape(result.state[Argument(2)], r) # x
        @test isaliased(Argument(2), rval, result.state)
    end
    let result = code_escapes((String,)) do x
            global GV
            l = g
            if isa(l, SafeRef{String})
                l[] = x
            end
            nothing
        end
        @test has_all_escape(result.state[Argument(2)]) # x
    end
    # circular reference
    let result = code_escapes() do
            x = Ref{Any}()
            x[] = x
            return x[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[SSAValue(i)], r)
    end
    let result = @eval Module() begin
            const Rx = Ref{Any}()
            Rx[] = Rx
            $code_escapes() do
                r = Rx[]::Base.RefValue{Any}
                return r[]
            end
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        for i in findall(iscall((result.ir, getfield)), result.ir.stmts.stmt)
            @test has_return_escape(result.state[SSAValue(i)], r)
        end
    end
    let result = @eval Module() begin
            @noinline function genr()
                r = Ref{Any}()
                r[] = r
                return r
            end
            $code_escapes() do
                x = genr()
                return x[]
            end
        end
        i = only(findall(isinvoke(:genr), result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[SSAValue(i)], r)
    end

    # dynamic semantics
    # -----------------

    # conservatively handle untyped objects
    let result = @eval code_escapes((Any,Any,)) do T, x
            obj = $(Expr(:new, :T, :x))
        end
        t = only(findall(isnew, result.ir.stmts.stmt))
        @test #=T=# has_thrown_escape(result.state[Argument(2)], t) # T
        @test #=x=# has_thrown_escape(result.state[Argument(3)], t) # x
    end
    let result = @eval code_escapes((Any,Any,Any,Any)) do T, x, y, z
            obj = $(Expr(:new, :T, :x, :y))
            return getfield(obj, :x)
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test #=x=# has_return_escape(result.state[Argument(3)], r)
        @test #=y=# has_return_escape(result.state[Argument(4)], r)
        @test #=z=# !has_return_escape(result.state[Argument(5)], r)
    end
    let result = @eval code_escapes((Any,Any,Any,Any)) do T, x, y, z
            obj = $(Expr(:new, :T, :x))
            setfield!(obj, :x, y)
            return getfield(obj, :x)
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test #=x=# has_return_escape(result.state[Argument(3)], r)
        @test #=y=# has_return_escape(result.state[Argument(4)], r)
        @test #=z=# !has_return_escape(result.state[Argument(5)], r)
    end

    # conservatively handle unknown field:
    # all fields should be escaped, but the allocation itself doesn't need to be escaped
    let result = code_escapes((Base.RefValue{String}, Symbol)) do a, fld
            obj = SafeRef(a)
            return getfield(obj, fld)
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end
    let result = code_escapes((Base.RefValue{String}, Base.RefValue{String}, Symbol)) do a, b, fld
            obj = SafeRefs(a, b)
            return getfield(obj, fld) # should escape both `a` and `b`
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test has_return_escape(result.state[Argument(3)], r) # b
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end
    let result = code_escapes((Base.RefValue{String}, Base.RefValue{String}, Int)) do a, b, idx
            obj = SafeRefs(a, b)
            return obj[idx] # should escape both `a` and `b`
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test has_return_escape(result.state[Argument(3)], r) # b
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end
    let result = code_escapes((Base.RefValue{String}, Base.RefValue{String}, Symbol)) do a, b, fld
            obj = SafeRefs(Ref("a"), Ref("b"))
            setfield!(obj, fld, a)
            return obj[2] # should escape `a`
        end
        i = last(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test !has_return_escape(result.state[Argument(3)], r) # b
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end
    let result = code_escapes((Base.RefValue{String}, Symbol)) do a, fld
            obj = SafeRefs(Ref("a"), Ref("b"))
            setfield!(obj, fld, a)
            return obj[1] # this should escape `a`
        end
        i = last(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end
    let result = code_escapes((Base.RefValue{String}, Base.RefValue{String}, Int)) do a, b, idx
        obj = SafeRefs(Ref("a"), Ref("b"))
            obj[idx] = a
            return obj[2] # should escape `a`
        end
        i = last(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test !has_return_escape(result.state[Argument(3)], r) # b
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end

    # interprocedural
    # ---------------

    let result = @eval EATModule() begin
            @noinline getx(obj) = obj[]
            $code_escapes((Base.RefValue{String},)) do a
                obj = SafeRef(a)
                fld = getx(obj)
                return fld
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(2)], r)
        # NOTE we can't scalar replace `obj`, but still we may want to stack allocate it
        @test_broken is_load_forwardable(result.state[SSAValue(i)])
    end

    # TODO interprocedural alias analysis
    let result = code_escapes((SafeRef{Base.RefValue{String}},)) do s
            s[] = Ref("bar")
            global GV = s[]
            nothing
        end
        @test_broken !has_all_escape(result.state[Argument(2)])
    end

    # aliasing between arguments
    let result = @eval EATModule() begin
            @noinline setxy!(x, y) = x[] = y
            $code_escapes((String,)) do y
                x = SafeRef("init")
                setxy!(x, y)
                return x
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[SSAValue(i)], r)
        @test has_return_escape(result.state[Argument(2)], r) # y
    end
    let result = @eval EATModule() begin
            @noinline setxy!(x, y) = x[] = y
            $code_escapes((String,)) do y
                x1 = SafeRef("init")
                x2 = SafeRef(y)
                Core.donotdelete(x1, x2)
                setxy!(x1, x2[])
                return x1
            end
        end
        i1, i2 = findall(isnew, result.ir.stmts.stmt)
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[SSAValue(i1)], r)
        @test !has_return_escape(result.state[SSAValue(i2)], r)
        @test has_return_escape(result.state[Argument(2)], r) # y
    end
    let result = @eval EATModule() begin
            @noinline mysetindex!(x, a) = x[1] = a
            const Ax = Vector{Any}(undef, 1)
            $code_escapes((Base.RefValue{String},)) do s
                mysetindex!(Ax, s)
            end
        end
        @test has_all_escape(result.state[Argument(2)]) # s
    end

    # TODO flow-sensitivity?
    # ----------------------

    let result = code_escapes((Any,Any)) do a, b
            r = SafeRef{Any}(a)
            Core.donotdelete(r)
            r[] = b
            return r[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test_broken !has_return_escape(result.state[Argument(2)], r) # a
        @test has_return_escape(result.state[Argument(3)], r) # b
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    let result = code_escapes((Any,Any)) do a, b
            r = SafeRef{Any}(:init)
            Core.donotdelete(r)
            r[] = a
            r[] = b
            return r[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test_broken !has_return_escape(result.state[Argument(2)], r) # a
        @test has_return_escape(result.state[Argument(3)], r) # b
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    let result = code_escapes((Any,Any,Bool)) do a, b, cond
            r = SafeRef{Any}(:init)
            Core.donotdelete(r)
            if cond
                r[] = a
                return r[]
            else
                r[] = b
                return nothing
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test is_load_forwardable(result.state[SSAValue(i)])
        r = only(findall(result.ir.stmts.stmt) do @nospecialize x
            isreturn(x) && isa(x.val, Core.SSAValue)
        end)
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test_broken !has_return_escape(result.state[Argument(3)], r) # b
    end

    # handle conflicting field information correctly
    let result = code_escapes((Bool,Base.RefValue{String},Base.RefValue{String},)) do cnd, baz, qux
            if cnd
                o = SafeRef(Ref("foo"))
            else
                o = SafeRefs(Ref("bar"), baz)
                r = getfield(o, 2)
            end
            if cnd
                o = o::SafeRef
                setfield!(o, 1, qux)
                r = getfield(o, 1)
            end
            r
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(3)], r) # baz
        @test has_return_escape(result.state[Argument(4)], r) # qux
        for new in findall(isnew, result.ir.stmts.stmt)
            if !(result.ir[SSAValue(new)][:type] <: Base.RefValue)
                @test is_load_forwardable(result.state[SSAValue(new)])
            end
        end
    end
    let result = code_escapes((Bool,Base.RefValue{String},Base.RefValue{String},)) do cnd, baz, qux
            if cnd
                o = SafeRefs(Ref("foo"), Ref("bar"))
                r = setfield!(o, 2, baz)
            else
                o = SafeRef(qux)
            end
            if !cnd
                o = o::SafeRef
                r = getfield(o, 1)
            end
            r
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result.state[Argument(3)], r) # baz
        @test has_return_escape(result.state[Argument(4)], r) # qux
    end

    # foreigncall should disable field analysis
    let result = code_escapes((Any,Nothing,Int,UInt)) do t, mt, lim, world
            ambig = false
            min = Ref{UInt}(typemin(UInt))
            max = Ref{UInt}(typemax(UInt))
            has_ambig = Ref{Int32}(0)
            mt = ccall(:jl_matching_methods, Any,
                (Any, Any, Cint, Cint, UInt, Ptr{UInt}, Ptr{UInt}, Ref{Int32}),
                t, mt, lim, ambig, world, min, max, has_ambig)::Union{Array{Any,1}, Bool}
            return mt, has_ambig[]
        end
        for i in findall(isnew, result.ir.stmts.stmt)
            @test !is_load_forwardable(result.state[SSAValue(i)])
        end
    end
end

# demonstrate the power of our field / alias analysis with a realistic end to end example
abstract type AbstractPoint{T} end
mutable struct MPoint{T} <: AbstractPoint{T}
    x::T
    y::T
end
add(a::P, b::P) where P<:AbstractPoint = P(a.x + b.x, a.y + b.y)
function compute(T, ax, ay, bx, by)
    a = T(ax, ay)
    b = T(bx, by)
    for i in 0:(100000000-1)
        c = add(a, b) # replaceable
        a = add(c, b) # replaceable
    end
    a.x, a.y
end
let result = @code_escapes compute(MPoint, 1+.5im, 2+.5im, 2+.25im, 4+.75im)
    for i in findall(1:length(result.ir.stmts)) do idx
                 inst = result.ir[SSAValue(idx)]
                 stmt = inst[:stmt]
                 return (isnew(stmt) || isϕ(stmt)) && inst[:type] <: MPoint
             end
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
end
function compute(a, b)
    for i in 0:(100000000-1)
        c = add(a, b) # replaceable
        a = add(c, b) # unreplaceable (aliased to the call argument `a`)
    end
    a.x, a.y
end
# let result = @code_escapes compute(MPoint(1+.5im, 2+.5im), MPoint(2+.25im, 4+.75im))
#     idxs = findall(1:length(result.ir.stmts)) do idx
#         inst = result.ir[SSAValue(idx)]
#         stmt = inst[:stmt]
#         return isnew(stmt) && inst[:type] <: MPoint
#     end
#     @assert length(idxs) == 2
#     @test count(i->is_load_forwardable(result.state[SSAValue(i)]), idxs) == 1
# end
function compute!(a, b)
    for i in 0:(100000000-1)
        c = add(a, b)  # replaceable
        a′ = add(c, b) # replaceable
        a.x = a′.x
        a.y = a′.y
    end
end
let result = @code_escapes compute!(MPoint(1+.5im, 2+.5im), MPoint(2+.25im, 4+.75im))
    for i in findall(1:length(result.ir.stmts)) do idx
                 inst = result.ir[SSAValue(idx)]
                 stmt = inst[:stmt]
                 return isnew(stmt) && inst[:type] <: MPoint
             end
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
end

# demonstrate a simple type level analysis can sometimes improve the analysis accuracy
# by compensating the lack of yet unimplemented analyses
@testset "special-casing bitstype" begin
    let result = code_escapes((Nothing,)) do a
            global GV = a
        end
        @test !(has_all_escape(result.state[Argument(2)]))
    end

    let result = code_escapes((Int,)) do a
            o = SafeRef(a)
            Core.donotdelete(o)
            return o[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test !has_return_escape(result.state[SSAValue(i)], r)
    end

    # an escaped tuple stmt will not propagate to its Int argument (since `Int` is of bitstype)
    let result = code_escapes((Int,Any,)) do a, b
            t = tuple(a, b)
            return t
        end
        i = only(findall(iscall((result.ir, tuple)), result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test !has_return_escape(result.state[Argument(2)], r)
        @test has_return_escape(result.state[Argument(3)], r)
    end
end

# interprocedural analysis
# ========================

# propagate escapes imposed on call arguments
@noinline broadcast_noescape2(b) = broadcast(identity, b)
let result = code_escapes() do
        broadcast_noescape2(Ref(Ref("Hi")))
    end
    i = last(findall(isnew, result.ir.stmts.stmt))
    @test_broken !has_return_escape(result.state[SSAValue(i)]) # TODO interprocedural alias analysis
    @test_broken !has_thrown_escape(result.state[SSAValue(i)]) # IDEA embed const-prop'ed `CodeInstance` for `:invoke`?
end
let result = code_escapes((Base.RefValue{Base.RefValue{String}},)) do x
        out1 = broadcast_noescape2(Ref(Ref("Hi")))
        out2 = broadcast_noescape2(x)
        return out1, out2
    end
    i = last(findall(isnew, result.ir.stmts.stmt))
    @test_broken !has_return_escape(result.state[SSAValue(i)]) # TODO interprocedural alias analysis
    @test_broken !has_thrown_escape(result.state[SSAValue(i)]) # IDEA embed const-prop'ed `CodeInstance` for `:invoke`?
    @test has_thrown_escape(result.state[Argument(2)])
end
@noinline allescape_argument(a) = (global GV = a) # obvious escape
let result = code_escapes() do
        allescape_argument(Ref("Hi"))
    end
    i = only(findall(isnew, result.ir.stmts.stmt))
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
    inds = findall(isnew, result.ir.stmts.stmt) # find allocation statement
    @assert !isempty(inds)
    for i in inds
        @test has_no_escape(result.state[SSAValue(i)])
    end
end

@noinline unused_argument(a) = (println("prevent inlining"); nothing)
let result = code_escapes() do
        a = Ref("foo") # shouldn't be "return escape"
        b = unused_argument(a)
        nothing
    end
    i = only(findall(isnew, result.ir.stmts.stmt))
    @test has_no_escape(result.state[SSAValue(i)])

    result = code_escapes() do
        a = Ref("foo") # still should be "return escape"
        b = unused_argument(a)
        return a
    end
    i = only(findall(isnew, result.ir.stmts.stmt))
    r = only(findall(isreturn, result.ir.stmts.stmt))
    @test has_return_escape(result.state[SSAValue(i)], r)
end

# should propagate escape information imposed on return value to the aliased call argument
@noinline returnescape_argument(a) = (println("prevent inlining"); a)
let result = code_escapes() do
        obj = Ref("foo")           # should be "return escape"
        ret = returnescape_argument(obj)
        return ret                 # alias of `obj`
    end
    i = only(findall(isnew, result.ir.stmts.stmt))
    r = only(findall(isreturn, result.ir.stmts.stmt))
    @test has_return_escape(result.state[SSAValue(i)], r)
end
@noinline noreturnescape_argument(a) = (println("prevent inlining"); identity("hi"))
let result = code_escapes() do
        obj = Ref("foo")              # better to not be "return escape"
        ret = noreturnescape_argument(obj)
        return ret                    # must not alias to `obj`
    end
    i = only(findall(isnew, result.ir.stmts.stmt))
    @test has_no_escape(result.state[SSAValue(i)])
end

function with_self_aliased(from_bb::Int, succs::Vector{Int})
    worklist = Int[from_bb]
    visited = BitSet(from_bb)
    function visit!(bb::Int)
        if bb ∉ visited
            push!(visited, bb)
            push!(worklist, bb)
        end
    end
    while !isempty(worklist)
        foreach(visit!, succs)
    end
    return visited
end
@test code_escapes(with_self_aliased) isa EAUtils.EscapeResult

# accounts for ThrownEscape via potential MethodError

# no method error
@noinline identity_if_string(x::SafeRef{<:AbstractString}) = (println("preventing inlining"); nothing)
let result = code_escapes((SafeRef{String},)) do x
        identity_if_string(x)
    end
    @test has_no_escape(ignore_argescape(result.state[Argument(2)]))
end
let result = code_escapes((SafeRef,)) do x
        identity_if_string(x)
    end
    i = only(findall(iscall((result.ir, identity_if_string)), result.ir.stmts.stmt))
    r = only(findall(isreturn, result.ir.stmts.stmt))
    @test has_thrown_escape(result.state[Argument(2)], i)
    @test_broken !has_return_escape(result.state[Argument(2)], r)
end
let result = code_escapes((SafeRef{String},)) do x
        try
            identity_if_string(x)
        catch err
            global GV = err
        end
        return nothing
    end
    @test !has_all_escape(result.state[Argument(2)])
end
let result = code_escapes((Union{SafeRef{String},Vector{String}},)) do x
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
@noinline ambig_error_test(a::SafeRef, b) = (println("preventing inlining"); nothing)
@noinline ambig_error_test(a, b::SafeRef) = (println("preventing inlining"); nothing)
@noinline ambig_error_test(a, b) = (println("preventing inlining"); nothing)
let result = code_escapes((SafeRef{String},Any)) do x, y
        ambig_error_test(x, y)
    end
    i = only(findall(iscall((result.ir, ambig_error_test)), result.ir.stmts.stmt))
    r = only(findall(isreturn, result.ir.stmts.stmt))
    @test has_thrown_escape(result.state[Argument(2)], i)  # x
    @test has_thrown_escape(result.state[Argument(3)], i)  # y
    @test_broken !has_return_escape(result.state[Argument(2)], r)  # x
    @test_broken !has_return_escape(result.state[Argument(3)], r)  # y
end
let result = code_escapes((SafeRef{String},Any)) do x, y
        try
            ambig_error_test(x, y)
        catch err
            global GV = err
        end
    end
    @test has_all_escape(result.state[Argument(2)])  # x
    @test has_all_escape(result.state[Argument(3)])  # y
end

@eval function scope_folding()
    $(Expr(:tryfinally,
        Expr(:block,
            Expr(:tryfinally, :(), :(), 2),
            :(return Core.current_scope())),
    :(), 1))
end
@eval function scope_folding_opt()
    $(Expr(:tryfinally,
        Expr(:block,
            Expr(:tryfinally, :(), :(), :(Base.inferencebarrier(2))),
            :(return Core.current_scope())),
    :(), :(Base.inferencebarrier(1))))
end
@test (@code_escapes scope_folding()) isa EAUtils.EscapeResult
@test (@code_escapes scope_folding_opt()) isa EAUtils.EscapeResult

end # module test_EA
