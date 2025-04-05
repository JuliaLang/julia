module test_EA

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

        mutable struct Object
            s::String
        end
    end
    global function EATModule(utils_ex = utils_ex)
        M = Module()
        Core.eval(M, utils_ex)
        return M
    end
    Core.eval(@__MODULE__, utils_ex)
end

isϕ(@nospecialize x) = isa(x, Core.PhiNode)
is_load_forwardable_old(x::EscapeAnalysis.EscapeInfo) = # TODO use new `is_load_forwardable` instead
    isa(x.ObjectInfo, EscapeAnalysis.HasIndexableFields)

@testset "EAUtils" begin
    @test_throws "everything has been constant folded" code_escapes() do; sin(42); end
    @test code_escapes(sin, (Int,)) isa EAUtils.EscapeAnalysisResult
    @test code_escapes(sin, (Int,)) isa EAUtils.EscapeAnalysisResult
    let ir = first(only(Base.code_ircode(sin, (Int,))))
        @test code_escapes(ir, 2) isa EAUtils.EscapeAnalysisResult
    end
end

@testset "basics" begin
    let # arg return
        result = code_escapes((Object,)) do a # return to caller
            println("prevent ConstABI")
            return nothing
        end
        @test has_arg_escape(result[Argument(2)])
        # return
        result = code_escapes((Object,)) do a
            println("prevent ConstABI")
            return a
        end
        i = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_arg_escape(result[Argument(1)]) # self
        @test !has_return_escape(result[Argument(1)], i) # self
        @test has_arg_escape(result[Argument(2)]) # a
        @test has_return_escape(result[Argument(2)], i) # a
    end
    let # global store
        result = code_escapes((Object,)) do a
            global GV = a
            nothing
        end
        @test has_all_escape(result[Argument(2)])
    end
    let # global load
        result = code_escapes() do
            global GV
            return GV
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        rval = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test has_all_escape(result[rval])
    end
    let # global store / load (https://github.com/aviatesk/EscapeAnalysis.jl/issues/56)
        result = code_escapes((Object,)) do s
            global GV
            GV = s
            return GV
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r)
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
        @test has_no_escape(result[SSAValue(i)])
    end
    let # :isdefined
        result = code_escapes((String, Bool,)) do a, b
            if b
                s = Object(a)
            end
            return @isdefined(s)
        end
        i = findfirst(==(Object), result.ir.stmts.type) # find allocation statement
        @test isnothing(i) || has_no_escape(result[SSAValue(i)])
    end
    let # ϕ-node
        result = code_escapes((Bool,Object,Object)) do cond, a, b
            c = cond ? a : b # ϕ(a, b)
            return c
        end
        @assert any(@nospecialize(x)->isa(x, Core.PhiNode), result.ir.stmts.stmt)
        i = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(3)], i) # a
        @test has_return_escape(result[Argument(4)], i) # b
    end
    let # π-node
        result = code_escapes((Any,)) do a
            if isa(a, Object) # a::π(Object)
                return a
            end
            return nothing
        end
        @assert any(@nospecialize(x)->isa(x, Core.PiNode), result.ir.stmts.stmt)
        @test any(findall(isreturn, result.ir.stmts.stmt)) do i
            has_return_escape(result[Argument(2)], i)
        end
    end
    let # φᶜ-node / ϒ-node
        result = code_escapes((Any,Object)) do a, b
            local x::Object
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
        @test has_return_escape(result[Argument(2)], i)
        @test has_return_escape(result[Argument(3)], i)
    end
    let # branching
        result = code_escapes((Any,Bool,)) do a, c
            if c
                return nothing # a doesn't escape in this branch
            else
                return a # a escapes to a caller
            end
        end
        @test has_return_escape(result[Argument(2)])
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
        @test has_return_escape(result[SSAValue(i)])
    end
    let # try/catch
        result = code_escapes((Any,)) do a
            try
                @noinline(rand(Bool)) && error("")
                nothing
            catch
                return a # return escape
            end
        end
        @test has_return_escape(result[Argument(2)])
    end
    let result = code_escapes((Any,)) do a
            try
                @noinline(rand(Bool)) && error("")
                nothing
            finally
                return a # return escape
            end
        end
        @test has_return_escape(result[Argument(2)])
    end
    let # :foreigncall
        result = code_escapes((Any,)) do x
            ccall(:some_ccall, Any, (Any,), x)
        end
        @test has_all_escape(result[Argument(2)])
    end
end

@testset "builtins" begin
    let # throw
        r = code_escapes((Any,)) do a
            throw(a)
        end
        @test has_thrown_escape(r[Argument(2)])
    end

    let # implicit throws
        r = code_escapes((Any,)) do a
            getfield(a, :may_not_field)
        end
        @test has_thrown_escape(r[Argument(2)])

        r = code_escapes((Any,)) do a
            sizeof(a)
        end
        @test has_thrown_escape(r[Argument(2)])
    end

    let # :===
        result = code_escapes((Bool, SafeRef{String})) do cond, s
            m = cond ? s : nothing
            c = m === nothing
            return c
        end
        @test !has_thrown_escape(result[Argument(2)])
        @test !has_return_escape(result[Argument(2)])
        @test_broken has_no_escape(result[Argument(2)])
    end

    let # sizeof
        result = code_escapes((Vector{Any},)) do xs
            sizeof(xs)
        end
        @test !has_thrown_escape(result[Argument(2)])
        @test !has_return_escape(result[Argument(2)])
        @test_broken has_no_escape(result[Argument(2)])
    end

    let # ifelse
        result = code_escapes((Bool,)) do c
            r = ifelse(c, Ref("yes"), Ref("no"))
            return r
        end
        inds = findall(isnew, result.ir.stmts.stmt)
        @assert !isempty(inds)
        for i in inds
            @test has_return_escape(result[SSAValue(i)])
        end
    end
    let # ifelse (with constant condition)
        result = code_escapes() do
            r = ifelse(true, Ref("yes"), Ref(nothing))
            return r
        end
        for i in 1:length(result.ir.stmts)
            if isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == Base.RefValue{String}
                @test has_return_escape(result[SSAValue(i)])
            elseif isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == Base.RefValue{Nothing}
                @test has_no_escape(result[SSAValue(i)])
            end
        end
    end

    let # typeassert
        result = code_escapes((Any,)) do x
            y = x::Base.RefValue{Any}
            return y
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r)
        @test !has_all_escape(result[Argument(2)])
    end

    let # isdefined
        result = code_escapes((Any,)) do x
            isdefined(x, :foo) ? x : throw("undefined")
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r)
        @test !has_all_escape(result[Argument(2)])
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
        @test count(rt->has_return_escape(result[SSAValue(i)], rt), rts) == 1
    end
    let result = code_escapes((Bool,)) do cond
            r = Ref("foo")
            cnt = 0
            while @noinline(rand(Bool))
                cnt += 1
                @noinline(rand(Bool)) && return r
            end
            @noinline(rand(Bool)) && return r
            return cnt
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        rts = findall(isreturn, result.ir.stmts.stmt) # return statement
        @assert length(rts) == 3
        @test count(rt->has_return_escape(result[SSAValue(i)], rt), rts) == 2
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
        @test has_return_escape(result[SSAValue(i)])
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
        @test has_all_escape(result[SSAValue(i)])
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
        @test has_all_escape(result[SSAValue(i)])
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
        @test has_all_escape(result[SSAValue(i)])
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
        @test has_all_escape(result[SSAValue(i)])
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
        @test has_all_escape(result[SSAValue(i)])
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
        @test has_all_escape(result[SSAValue(i)])
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
        @test has_all_escape(result[SSAValue(i)])
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
        @test has_all_escape(result[SSAValue(i1)])
        @test !has_all_escape(result[SSAValue(i2)])
        @test has_return_escape(result[SSAValue(i2)], r)
    end

    # XXX test cases below are currently broken because of the technical reason described in `propagate_exct_state`

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
        @test_broken !has_return_escape(result[SSAValue(i)], r) # TODO? see `escape_exception!`
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
        @test has_all_escape(result[SSAValue(i1)])
        @test has_return_escape(result[SSAValue(i2)], r)
        @test_broken !has_all_escape(result[SSAValue(i2)]) # TODO? see `escape_exception!`
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
        @test_broken !has_return_escape(result[SSAValue(i)])
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
        @test_broken !has_all_escape(result[SSAValue(i)])
        for r in rs
            @test has_return_escape(result[SSAValue(i)], r)
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
        @test_broken !has_return_escape(result[SSAValue(i)], r)
    end
end

const g_exception_escape = Ref{Any}()
@noinline function get_exception()
    try
        rethrow()
    catch err
        err
    end
end
let result = code_escapes((Bool,String,)) do b, s
        x = Ref{String}()
        b && (x[] = s)
        try
            return x[] # may throw
        catch
            err = get_exception()
            g_exception_escape[] = err
            return nothing
        end
    end
    i = only(findall(iscall((result.ir, getfield)), result.ir.stmts.stmt))
    @test_broken is_load_forwardable(result[SSAValue(i)]) # TODO CFG-aware `MemoryInfo`
    i = only(findall(isnew, result.ir.stmts.stmt))
    @test has_all_escape(result[SSAValue(i)])
end

@testset "field analysis / alias analysis" begin
    # escaped allocations
    # -------------------

    # escaped object should escape its fields as well
    let result = code_escapes((Object,)) do a
            global GV = SafeRef(a)
            nothing
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result[SSAValue(i)])
        @test has_all_escape(result[Argument(2)])
    end
    let result = code_escapes((Object,)) do a
            global GV = (a,)
            nothing
        end
        i = only(findall(iscall((result.ir, tuple)), result.ir.stmts.stmt))
        @test has_all_escape(result[SSAValue(i)])
        @test has_all_escape(result[Argument(2)])
    end
    let result = code_escapes((Object,)) do a
            o0 = SafeRef(a)
            global GV = SafeRef(o0)
            nothing
        end
        is = findall(isnew, result.ir.stmts.stmt)
        @test length(is) == 2
        i0, i1 = is
        @test has_all_escape(result[SSAValue(i0)])
        @test has_all_escape(result[SSAValue(i1)])
        @test has_all_escape(result[Argument(2)])
    end
    let result = code_escapes((Object,)) do a
            t0 = (a,)
            global GV = (t0,)
            nothing
        end
        inds = findall(iscall((result.ir, tuple)), result.ir.stmts.stmt)
        @assert length(inds) == 2
        for i in inds; @test has_all_escape(result[SSAValue(i)]); end
        @test has_all_escape(result[Argument(2)])
    end
    # global escape through `setfield!`
    let result = code_escapes((Object,)) do a
            r = SafeRef{Any}(nothing)
            global GV = r
            r[] = a
            nothing
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result[SSAValue(i)])
        @test has_all_escape(result[Argument(2)])
    end
    let result = code_escapes((Object,Object)) do a, b
            r = SafeRef{Object}(a)
            global GV = r
            nothing
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result[SSAValue(i)])
        @test has_all_escape(result[Argument(2)]) # a
        @test !has_all_escape(result[Argument(3)]) # b
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef(Ref(""))
            $code_escapes((Base.RefValue{String},)) do s
                Rx[] = s
                Core.sizeof(Rx[])
            end
        end
        @test has_all_escape(result[Argument(2)])
    end
    let M = EATModule()
        @eval M module ___xxx___
            import ..SafeRef
            const Rx = SafeRef(SafeRef("Rx"))
        end
        let result = @eval M begin
                $code_escapes((SafeRef{String},)) do s
                    rx = getfield(___xxx___, :Rx)
                    rx[] = s
                    nothing
                end
            end
            @test has_all_escape(result[Argument(2)])
        end
        let result = @eval M begin
                $code_escapes((SafeRef{String},)) do s
                    rx = getglobal(___xxx___, :Rx)
                    rx[] = s
                    nothing
                end
            end
            @test has_all_escape(result[Argument(2)])
        end
    end

    # field escape
    # ------------

    # field escape should propagate to :new arguments
    let result = code_escapes((Object,)) do a
            o = SafeRef(a)
            Core.donotdelete(o)
            return o[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r)
        @test is_load_forwardable_old(result[SSAValue(i)])
    end
    let result = code_escapes((Object,)) do a
            t = SafeRef((a,))
            f = t[][1]
            return f
        end
        i = only(findall(iscall((result.ir, tuple)), result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r)
        @test is_load_forwardable_old(result[SSAValue(i)])
    end
    let result = code_escapes((Object,Object)) do a, b
            obj = SafeRefs(a, b)
            Core.donotdelete(obj)
            fld1 = obj[1]
            fld2 = obj[2]
            return (fld1, fld2)
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r) # a
        @test has_return_escape(result[Argument(3)], r) # b
        @test is_load_forwardable_old(result[SSAValue(i)])
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
        @test has_return_escape(result[Argument(2)], r)
        @test is_load_forwardable_old(result[SSAValue(i)])
    end
    # propagate escape information imposed on return value of `setfield!` call
    let result = code_escapes((Base.RefValue{String},)) do a
            obj = SafeRef(Ref("foo"))
            Core.donotdelete(obj)
            return (obj[] = a)
        end
        i = last(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r)
        @test is_load_forwardable_old(result[SSAValue(i)])
    end

    # nested allocations
    let result = code_escapes((Object,)) do a
            o1 = SafeRef(a)
            o2 = SafeRef(o1)
            return o2[]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r)
        for i in 1:length(result.ir.stmts)
            if isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == SafeRef{String}
                @test has_return_escape(result[SSAValue(i)], r)
            elseif isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == SafeRef{SafeRef{String}}
                @test is_load_forwardable_old(result[SSAValue(i)])
            end
        end
    end
    let result = code_escapes((Object,)) do a
            o1 = (a,)
            o2 = (o1,)
            return o2[1]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r)
        for i in 1:length(result.ir.stmts)
            if isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == Tuple{String}
                @test has_return_escape(result[SSAValue(i)], r)
            elseif isnew(result.ir.stmts.stmt[i]) && result.ir.stmts.type[i] == Tuple{Tuple{String}}
                @test is_load_forwardable_old(result[SSAValue(i)])
            end
        end
    end
    let result = code_escapes((Object,)) do a
            o1  = SafeRef(a)
            o2  = SafeRef(o1)
            o1′ = o2[]
            a′  = o1′[]
            return a′
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r)
        for i in findall(isnew, result.ir.stmts.stmt)
            @test is_load_forwardable_old(result[SSAValue(i)])
        end
    end
    let result = code_escapes() do
            o1 = SafeRef("foo")
            o2 = SafeRef(o1)
            return o2
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        for i in findall(isnew, result.ir.stmts.stmt)
            @test has_return_escape(result[SSAValue(i)], r)
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
            @test has_return_escape(result[SSAValue(i)], r)
        end
    end
    let result = code_escapes((Base.RefValue{String},)) do x
            o = Ref(x)
            Core.donotdelete(o)
            broadcast(identity, o)
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r)
        @test is_load_forwardable_old(result[SSAValue(i)])
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
        @test has_return_escape(result[Argument(3)], r) # x
        @test has_return_escape(result[Argument(4)], r) # y
        i = only(findall(isϕ, result.ir.stmts.stmt))
        @test is_load_forwardable_old(result[SSAValue(i)])
        for i in findall(isnew, result.ir.stmts.stmt)
            @test is_load_forwardable_old(result[SSAValue(i)])
        end
    end
    let result = code_escapes((Bool,Object,Object)) do cond, x, y
            if cond
                ϕ2 = ϕ1 = SafeRef(x)
            else
                ϕ2 = ϕ1 = SafeRef(y)
            end
            return ϕ1[], ϕ2[]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(3)], r) # x
        @test has_return_escape(result[Argument(4)], r) # y
        for i in findall(isϕ, result.ir.stmts.stmt)
            @test is_load_forwardable_old(result[SSAValue(i)])
        end
        for i in findall(isnew, result.ir.stmts.stmt)
            @test is_load_forwardable_old(result[SSAValue(i)])
        end
    end
    # when ϕ-node merges values with different types
    let result = code_escapes((Bool,Object,Object,Object)) do cond, x, y, z
            local out
            if cond
                ϕ = SafeRef(x)
                out = ϕ[]
            else
                ϕ = SafeRefs(y, z)
            end
            return @isdefined(out) ? out : throw(ϕ)
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        ϕ = only(findall(==(Union{SafeRef{Object},SafeRefs{Object,Object}}), result.ir.stmts.type))
        @test has_return_escape(result[Argument(3)], r) # x
        @test !has_return_escape(result[Argument(4)], r) # y
        @test !has_return_escape(result[Argument(5)], r) # z
        @test has_thrown_escape(result[SSAValue(ϕ)])
    end

    # alias analysis
    # --------------

    # alias via getfield & Expr(:new)
    let result = code_escapes((Object,)) do s
            r = SafeRef(s)
            Core.donotdelete(r)
            return r[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        retval = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test isaliased(result.eresult, Argument(2), retval)
        @test !isaliased(result.eresult, Argument(2), SSAValue(i))
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
        @test !isaliased(result.eresult, SSAValue(i1), SSAValue(i2))
        @test isaliased(result.eresult, SSAValue(i1), val)
        @test !isaliased(result.eresult, SSAValue(i2), val)
    end
    let result = code_escapes((Object,)) do s
            r1 = SafeRef(s)
            r2 = SafeRef(r1)
            Core.donotdelete(r1, r2)
            return r2[][]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test isaliased(result.eresult, Argument(2), val)
        for i in findall(isnew, result.ir.stmts.stmt)
            @test !isaliased(result.eresult, SSAValue(i), val)
        end
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef(SafeRef("Rx"))
            $code_escapes((SafeRef{String},)) do s
                r = SafeRef(Rx)
                Core.donotdelete(r)
                rx = r[] # rx aliased to Rx
                rx[] = s
                nothing
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        @test has_all_escape(result[Argument(2)])
        @test is_load_forwardable_old(result[SSAValue(i)])
    end
    # alias via getfield & setfield!
    let result = code_escapes((Object,)) do s
            r = Ref{Object}()
            Core.donotdelete(r)
            r[] = s
            return r[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test isaliased(result.eresult, Argument(2), val)
        @test !isaliased(result.eresult, Argument(2), SSAValue(i))
    end
    let result = code_escapes((Object,)) do s
            r1 = Ref(s)
            r2 = Ref{Base.RefValue{Object}}()
            Core.donotdelete(r1, r2)
            r2[] = r1
            return r2[]
        end
        i1, i2 = findall(isnew, result.ir.stmts.stmt)
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test !isaliased(result.eresult, SSAValue(i1), SSAValue(i2))
        @test isaliased(result.eresult, SSAValue(i1), val)
        @test !isaliased(result.eresult, SSAValue(i2), val)
    end
    let result = code_escapes((Object,)) do s
            r1 = Ref{Object}()
            r2 = Ref{Base.RefValue{Object}}()
            Core.donotdelete(r1, r2)
            r2[] = r1
            r1[] = s
            return r2[][]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        retval = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test isaliased(result.eresult, Argument(2), retval)
        for i in findall(isnew, result.ir.stmts.stmt)
            @test !isaliased(result.eresult, SSAValue(i), retval)
        end
        result = code_escapes((Object,)) do s
            r1 = Ref{Object}()
            r2 = Ref{Base.RefValue{Object}}()
            r1[] = s
            r2[] = r1
            return r2[][]
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        retval = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test isaliased(result.eresult, Argument(2), retval)
        for i in findall(isnew, result.ir.stmts.stmt)
            @test !isaliased(result.eresult, SSAValue(i), retval)
        end
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef(Object("Rx"))
            $code_escapes((SafeRef{Object}, Object,)) do _rx, s
                r = SafeRef(_rx)
                Core.donotdelete(r)
                r[] = Rx
                rx = r[] # rx aliased to Rx
                rx[] = s
                nothing
            end
        end
        i = findfirst(isnew, result.ir.stmts.stmt)
        @test has_all_escape(result[Argument(3)])
        @test is_load_forwardable_old(result[SSAValue(i)])
    end
    # alias via typeassert
    let result = code_escapes((Any,)) do a
            r = a::Base.RefValue{String}
            return r
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test has_return_escape(result[Argument(2)], r)   # a
        @test isaliased(result.eresult, Argument(2), val) # a <-> r
    end
    let result = code_escapes((Any,)) do a
            global GV
            (g::SafeRef{Any})[] = a
            nothing
        end
        @test has_all_escape(result[Argument(2)])
    end
    # alias via ifelse
    let result = code_escapes((Bool,Any,Any)) do c, a, b
            r = ifelse(c, a, b)
            return r
        end
        r = only(findall(isreturn, result.ir.stmts.stmt))
        val = (result.ir.stmts.stmt[r]::ReturnNode).val::SSAValue
        @test has_return_escape(result[Argument(3)], r) # a
        @test has_return_escape(result[Argument(4)], r) # b
        @test !isaliased(result.eresult, Argument(2), val)      # c <!-> r
        @test isaliased(result.eresult, Argument(3), val)       # a <-> r
        @test isaliased(result.eresult, Argument(4), val)       # b <-> r
    end
    let result = @eval EATModule() begin
            const Lx, Rx = SafeRef(Object("Lx")), SafeRef(Object("Rx"))
            $code_escapes((Bool,Object,)) do c, a
                r = ifelse(c, Lx, Rx)
                r[] = a
                nothing
            end
        end
        @test has_all_escape(result[Argument(3)]) # a
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
        @test has_return_escape(result[Argument(3)], r) # x
        @test isaliased(result.eresult, Argument(3), val) # x
        for i in findall(isϕ, result.ir.stmts.stmt)
            @test is_load_forwardable_old(result[SSAValue(i)])
        end
        for i in findall(isnew, result.ir.stmts.stmt)
            if result.ir[SSAValue(i)][:type] <: SafeRef
                @test is_load_forwardable_old(result[SSAValue(i)])
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
        @test has_return_escape(result[Argument(4)], r) # x
        @test isaliased(result.eresult, Argument(4), val) # x
        for i in findall(isϕ, result.ir.stmts.stmt)
            @test is_load_forwardable_old(result[SSAValue(i)])
        end
        for i in findall(isnew, result.ir.stmts.stmt)
            if result.ir[SSAValue(i)][:type] <: SafeRef
                @test is_load_forwardable_old(result[SSAValue(i)])
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
        @test has_return_escape(result[Argument(2)], r) # x
        @test isaliased(result.eresult, Argument(2), rval)
    end
    let result = code_escapes((Object,)) do x
            global typed_global_object
            tgo = typed_global_object
            if isa(tgo, SafeRef{Object})
                l[] = x
            end
            nothing
        end
        @test has_all_escape(result[Argument(2)]) # x
    end
    # circular reference
    let result = code_escapes() do
            x = Ref{Any}()
            x[] = x
            return x[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[SSAValue(i)], r)
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
            @test has_return_escape(result[SSAValue(i)], r)
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
        @test has_return_escape(result[SSAValue(i)], r)
    end

    # dynamic semantics
    # -----------------

    # conservatively handle untyped objects
    let result = @eval code_escapes((Any,Any,)) do T, x
            obj = $(Expr(:new, :T, :x))
        end
        @test #=x=# has_thrown_escape(result[Argument(3)]) # x
    end
    let result = @eval code_escapes((Any,Any,Any,Any)) do T, x, y, z
            obj = $(Expr(:new, :T, :x, :y))
            return getfield(obj, :x)
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test #=x=# has_return_escape(result[Argument(3)], r)
        @test #=y=# has_return_escape(result[Argument(4)], r)
        @test #=z=# !has_return_escape(result[Argument(5)], r)
    end
    let result = @eval code_escapes((Any,Any,Any,Any)) do T, x, y, z
            obj = $(Expr(:new, :T, :x))
            setfield!(obj, :x, y)
            return getfield(obj, :x)
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test #=x=# has_return_escape(result[Argument(3)], r)
        @test #=y=# has_return_escape(result[Argument(4)], r)
        @test #=z=# !has_return_escape(result[Argument(5)], r)
    end

    # conservatively handle unknown field:
    # all fields should be escaped, but the allocation itself doesn't need to be escaped
    let result = code_escapes((Base.RefValue{String}, Symbol)) do a, fld
            obj = SafeRef(a)
            return getfield(obj, fld)
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r) # a
        @test !is_load_forwardable_old(result[SSAValue(i)]) # obj
    end
    let result = code_escapes((Base.RefValue{String}, Base.RefValue{String}, Symbol)) do a, b, fld
            obj = SafeRefs(a, b)
            return getfield(obj, fld) # should escape both `a` and `b`
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r) # a
        @test has_return_escape(result[Argument(3)], r) # b
        @test !is_load_forwardable_old(result[SSAValue(i)]) # obj
    end
    let result = code_escapes((Base.RefValue{String}, Base.RefValue{String}, Int)) do a, b, idx
            obj = SafeRefs(a, b)
            return obj[idx] # should escape both `a` and `b`
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r) # a
        @test has_return_escape(result[Argument(3)], r) # b
        @test !is_load_forwardable_old(result[SSAValue(i)]) # obj
    end
    let result = code_escapes((Base.RefValue{String}, Base.RefValue{String}, Symbol)) do a, b, fld
            obj = SafeRefs(Ref("a"), Ref("b"))
            setfield!(obj, fld, a)
            return obj[2] # should escape `a`
        end
        i = last(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r) # a
        @test !has_return_escape(result[Argument(3)], r) # b
        @test !is_load_forwardable_old(result[SSAValue(i)]) # obj
    end
    let result = code_escapes((Base.RefValue{String}, Symbol)) do a, fld
            obj = SafeRefs(Ref("a"), Ref("b"))
            setfield!(obj, fld, a)
            return obj[1] # this should escape `a`
        end
        i = last(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r) # a
        @test !is_load_forwardable_old(result[SSAValue(i)]) # obj
    end
    let result = code_escapes((Base.RefValue{String}, Base.RefValue{String}, Int)) do a, b, idx
        obj = SafeRefs(Ref("a"), Ref("b"))
            obj[idx] = a
            return obj[2] # should escape `a`
        end
        i = last(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[Argument(2)], r) # a
        @test !has_return_escape(result[Argument(3)], r) # b
        @test !is_load_forwardable_old(result[SSAValue(i)]) # obj
    end

    # inter-procedural
    # ----------------

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
        @test has_return_escape(result[Argument(2)], r)
        # NOTE we can't scalar replace `obj`, but still we may want to stack allocate it
        @test_broken is_load_forwardable_old(result[SSAValue(i)])
    end

    # TODO inter-procedural alias analysis
    let result = code_escapes((SafeRef{Base.RefValue{String}},)) do s
            s[] = Ref("bar")
            global GV = s[]
            nothing
        end
        @test_broken !has_all_escape(result[Argument(2)])
    end

    # aliasing between arguments
    let result = @eval EATModule() begin
            @noinline setxy!(x, y) = x[] = y
            $code_escapes((SafeRef{Any},)) do y
                x = SafeRef{Any}(nothing)
                setxy!(x, y)
                return x
            end
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[SSAValue(i)], r)
        @test has_return_escape(result[Argument(2)], r) # y
    end
    let result = @eval EATModule() begin
            @noinline setxy!(x, y) = x[] = y
            $code_escapes((SafeRef{Any},)) do y
                x1 = SafeRef{Any}(nothing)
                x2 = SafeRef(y)
                Core.donotdelete(x1, x2)
                setxy!(x1, x2[])
                return x1
            end
        end
        i1, i2 = findall(isnew, result.ir.stmts.stmt)
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test has_return_escape(result[SSAValue(i1)], r)
        @test !has_return_escape(result[SSAValue(i2)], r)
        @test has_return_escape(result[Argument(2)], r) # y
    end
    let result = @eval EATModule() begin
            @noinline mysetindex!(x, a) = x[1] = a
            const Ax = Vector{Any}(undef, 1)
            $code_escapes((Base.RefValue{String},)) do s
                mysetindex!(Ax, s)
            end
        end
        @test has_all_escape(result[Argument(2)]) # s
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
        @test !has_return_escape(result[Argument(2)], r) # a
        @test has_return_escape(result[Argument(3)], r) # b
        @test is_load_forwardable_old(result[SSAValue(i)])
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
        @test !has_return_escape(result[Argument(2)], r) # a
        @test has_return_escape(result[Argument(3)], r) # b
        @test is_load_forwardable_old(result[SSAValue(i)])
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
        @test is_load_forwardable_old(result[SSAValue(i)])
        r = only(findall(result.ir.stmts.stmt) do @nospecialize x
            isreturn(x) && isa(x.val, Core.SSAValue)
        end)
        @test has_return_escape(result[Argument(2)], r) # a
        @test !has_return_escape(result[Argument(3)], r) # b
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
        @test has_return_escape(result[Argument(3)], r) # baz
        @test has_return_escape(result[Argument(4)], r) # qux
        for new in findall(isnew, result.ir.stmts.stmt)
            if !(result.ir[SSAValue(new)][:type] <: Base.RefValue)
                @test is_load_forwardable_old(result[SSAValue(new)])
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
        @test has_return_escape(result[Argument(3)], r) # baz
        @test has_return_escape(result[Argument(4)], r) # qux
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
            @test !is_load_forwardable_old(result[SSAValue(i)])
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
        @test is_load_forwardable_old(result[SSAValue(i)])
    end
end
function compute(a, b)
    for i in 0:(100000000-1)
        c = add(a, b) # replaceable
        a = add(c, b) # unreplaceable (aliased to the call argument `a`)
    end
    a.x, a.y
end
let result = @code_escapes compute(MPoint(1+.5im, 2+.5im), MPoint(2+.25im, 4+.75im))
    idxs = findall(1:length(result.ir.stmts)) do idx
        inst = result.ir[SSAValue(idx)]
        stmt = inst[:stmt]
        return isnew(stmt) && inst[:type] <: MPoint
    end
    @assert length(idxs) == 1
    @test_broken is_load_forwardable_old(result[SSAValue(only(idxs))])
end
function compute!(a, b)
    for i in 0:(100000000-1)
        c = add(a, b)  # replaceable
        a′ = add(c, b) # replaceable
        a.x = a′.x
        a.y = a′.y
    end
end
let result = @code_escapes compute!(MPoint(1+.5im, 2+.5im), MPoint(2+.25im, 4+.75im))
    for i in findall(1:length(result.ir.stmts)) do idx::Int
                 inst = result.ir[SSAValue(idx)]
                 stmt = inst[:stmt]
                 return isnew(stmt) && inst[:type] <: MPoint
             end
        @test is_load_forwardable_old(result[SSAValue(i)])
    end
end

# demonstrate a simple type level analysis can sometimes improve the analysis accuracy
# by compensating the lack of yet unimplemented analyses
@testset "special-casing bitstype" begin
    let result = code_escapes((Nothing,)) do a
            global GV = a
        end
        @test !(has_all_escape(result[Argument(2)]))
    end

    let result = code_escapes((Int,)) do a
            o = SafeRef(a)
            Core.donotdelete(o)
            return o[]
        end
        i = only(findall(isnew, result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test !has_return_escape(result[SSAValue(i)], r)
    end

    # an escaped tuple stmt will not propagate to its Int argument (since `Int` is of bitstype)
    let result = code_escapes((Int,Any,)) do a, b
            t = tuple(a, b)
            return t
        end
        i = only(findall(iscall((result.ir, tuple)), result.ir.stmts.stmt))
        r = only(findall(isreturn, result.ir.stmts.stmt))
        @test !has_return_escape(result[Argument(2)], r)
        @test has_return_escape(result[Argument(3)], r)
    end
end

# inter-procedural analysis
# =========================

# propagate escapes imposed on call arguments
@noinline broadcast_noescape2(b) = broadcast(identity, b)
let result = code_escapes() do
        broadcast_noescape2(Ref(Ref("Hi")))
    end
    i = last(findall(isnew, result.ir.stmts.stmt))
    @test_broken !has_return_escape(result[SSAValue(i)]) # TODO inter-procedural alias analysis
    @test_broken !has_thrown_escape(result[SSAValue(i)]) # TODO inter-procedural alias analysis
end
let result = code_escapes((Base.RefValue{Base.RefValue{String}},)) do x
        out1 = broadcast_noescape2(Ref(Ref("Hi")))
        out2 = broadcast_noescape2(x)
        return out1, out2
    end
    i = last(findall(isnew, result.ir.stmts.stmt))
    @test_broken !has_return_escape(result[SSAValue(i)]) # TODO inter-procedural alias analysis
    @test_broken !has_thrown_escape(result[SSAValue(i)]) # TODO inter-procedural alias analysis
    @test has_thrown_escape(result[Argument(2)])
end
@noinline allescape_argument(a) = (global GV = a) # obvious escape
let result = code_escapes() do
        allescape_argument(Ref("Hi"))
    end
    i = only(findall(isnew, result.ir.stmts.stmt))
    @test has_all_escape(result[SSAValue(i)])
end
# if we can't determine the matching method statically, we should be conservative
let result = code_escapes((Ref{Any},)) do a
        may_exist(a)
    end
    @test has_all_escape(result[Argument(2)])
end
let result = code_escapes((Ref{Any},)) do a
        Base.@invokelatest broadcast_noescape1(a)
    end
    @test has_all_escape(result[Argument(2)])
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
        @test has_no_escape(result[SSAValue(i)])
    end
end

@noinline unused_argument(a) = (println("prevent inlining"); nothing)
let result = code_escapes() do
        a = Ref("foo") # shouldn't be "return escape"
        b = unused_argument(a)
        nothing
    end
    i = only(findall(isnew, result.ir.stmts.stmt))
    @test has_no_escape(result[SSAValue(i)])

    result = code_escapes() do
        a = Ref("foo") # still should be "return escape"
        b = unused_argument(a)
        return a
    end
    i = only(findall(isnew, result.ir.stmts.stmt))
    r = only(findall(isreturn, result.ir.stmts.stmt))
    @test has_return_escape(result[SSAValue(i)], r)
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
    @test has_return_escape(result[SSAValue(i)], r)
end
@noinline noreturnescape_argument(a) = (println("prevent inlining"); identity("hi"))
let result = code_escapes() do
        obj = Ref("foo")              # better to not be "return escape"
        ret = noreturnescape_argument(obj)
        return ret                    # must not alias to `obj`
    end
    i = only(findall(isnew, result.ir.stmts.stmt))
    @test has_no_escape(result[SSAValue(i)])
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
@test code_escapes(with_self_aliased) isa EAUtils.EscapeAnalysisResult

# accounts for ThrownEscape via potential MethodError

# no method error
@noinline identity_if_string(x::SafeRef{<:AbstractString}) = (println("preventing inlining"); nothing)
let result = code_escapes((SafeRef{String},)) do x
        identity_if_string(x)
    end
    @test has_no_escape(ignore_argescape(result[Argument(2)]))
end
let result = code_escapes((SafeRef,)) do x
        identity_if_string(x)
    end
    r = only(findall(isreturn, result.ir.stmts.stmt))
    @test has_thrown_escape(result[Argument(2)])
    @test_broken !has_return_escape(result[Argument(2)], r)
end
let result = code_escapes((SafeRef{String},)) do x
        try
            identity_if_string(x)
        catch err
            global GV = err
        end
        return nothing
    end
    @test !has_all_escape(result[Argument(2)])
end
let result = code_escapes((Union{SafeRef{String},Vector{String}},)) do x
        try
            identity_if_string(x)
        catch err
            global GV = err
        end
        return nothing
    end
    @test has_all_escape(result[Argument(2)])
end
# method ambiguity error
@noinline ambig_error_test(a::SafeRef, b) = (println("preventing inlining"); nothing)
@noinline ambig_error_test(a, b::SafeRef) = (println("preventing inlining"); nothing)
@noinline ambig_error_test(a, b) = (println("preventing inlining"); nothing)
let result = code_escapes((SafeRef{String},Any)) do x, y
        ambig_error_test(x, y)
    end
    r = only(findall(isreturn, result.ir.stmts.stmt))
    @test has_thrown_escape(result[Argument(2)])  # x
    @test has_thrown_escape(result[Argument(3)])  # y
    @test_broken !has_return_escape(result[Argument(2)], r)  # x
    @test_broken !has_return_escape(result[Argument(3)], r)  # y
end
let result = code_escapes((SafeRef{String},Any)) do x, y
        try
            ambig_error_test(x, y)
        catch err
            global GV = err
        end
    end
    @test has_all_escape(result[Argument(2)])  # x
    @test has_all_escape(result[Argument(3)])  # y
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
@test (@code_escapes scope_folding()) isa EAUtils.EscapeAnalysisResult
@test (@code_escapes scope_folding_opt()) isa EAUtils.EscapeAnalysisResult

# flow-sensitivity
# ================

let result = code_escapes() do
        x = Ref{String}()
        x[] = "foo"
        out1 = x[]
        x[] = "bar"
        out2 = x[]
        return x, out1, out2
    end
    idxs = findall(iscall((result.ir, getfield)), result.ir.stmts.stmt)
    @test length(idxs) == 2
    for idx = idxs
        @test is_load_forwardable(result, idx)
    end
end

let result = code_escapes((Bool,String,String)) do c, s1, s2
        if c
            x = Ref(s1)
        else
            x = Ref(s2)
        end
        return x[]
    end
    idx = only(findall(iscall((result.ir, getfield)), result.ir.stmts.stmt))
    @test_broken is_load_forwardable(result, idx) # TODO CFG-aware `MemoryInfo`
end

const g_flowsensitive_1 = Ref{Any}()
let result = code_escapes((Bool,String)) do c, s
        x = Ref(s)
        if c
            g_flowsensitive_1[] = x
            return nothing
        end
        return x[]
    end
    idx = only(findall(iscall((result.ir, getfield)), result.ir.stmts.stmt))
    @test is_load_forwardable(result, idx)
    idx = only(findall(isnew, result.ir.stmts.stmt))
    @test has_all_escape(result[SSAValue(idx)])
end

function func_g_flowsensitive_1()
    g_flowsensitive_1[][] = join(rand(Char, 5))
end
let result = code_escapes((Int,Bool,String,)) do n, x, s
        x = Ref(s)
        local out = nothing
        for i = 1:n
            out = x[]
            if n ≥ length(s)
                g_flowsensitive_1[] = x
            end
            @noinline rand(Bool) && func_g_flowsensitive_1()
        end
        return x, out
    end
    idx = only(findall(iscall((result.ir, getfield)), result.ir.stmts.stmt))
    @test !is_load_forwardable(result, idx)
    idx = only(findall(isnew, result.ir.stmts.stmt))
    @test has_all_escape(result[SSAValue(idx)])
end

let result = code_escapes((Int,Bool,String,)) do n, x, s
        x = Ref(s)
        local out = nothing
        for i = 1:n
            out = x[]
            if n ≥ length(s)
                x[] = ""
            end
        end
        return x, out
    end
    idx = only(findall(iscall((result.ir, getfield)), result.ir.stmts.stmt))
    @test_broken is_load_forwardable(result, idx) # TODO CFG-aware `MemoryInfo`
    idx = only(findall(isnew, result.ir.stmts.stmt))
    @test !has_all_escape(result[SSAValue(idx)])
end

let result = code_escapes((String,)) do s
        x = Ref(s)
        xx = Ref(x)
        g_flowsensitive_1[] = xx
        some_unsafe_call()
        x[]
    end
    idx = only(findall(iscall((result.ir, getfield)), result.ir.stmts.stmt))
    @test !is_load_forwardable(result, idx)
    idxs = findall(isnew, result.ir.stmts.stmt)
    @test length(idxs) == 2
    for idx = idxs
        @test has_all_escape(result[SSAValue(idx)])
    end
end

let result = code_escapes((Bool,Bool,String,String,)) do c1, c2, x, y
        local w
        if c1
            z = w = Ref(x)
        else
            z = Ref(y)
        end
        if c2
            z[] = "foo"
        end
        if @isdefined w
            return w[]
        end
        nothing
    end
    idx = only(findall(iscall((result.ir, getfield)), result.ir.stmts.stmt))
    @test !is_load_forwardable(result, idx) # COMBAK maybe CFG-aware `MemoryInfo` would allow this?
    idxs = findall(isnew, result.ir.stmts.stmt)
    @test length(idxs) == 2
    for idx = idxs
        @test has_no_escape(result[SSAValue(idx)])
    end
end

# Flow-sensitive escape analysis example from the paper:
# Lukas Stadler, Thomas Würthinger, and Hanspeter Mössenböck. 2018.
# Partial Escape Analysis and Scalar Replacement for Java.
# In Proceedings of Annual IEEE/ACM International Symposium on Code Generation and Optimization (CGO '14).
# Association for Computing Machinery, New York, NY, USA, 165–174. https://doi.org/10.1145/2544137.2544157
mutable struct Key
    idx::Int
    ref
    Key(idx::Int, @nospecialize(ref)) = new(idx, ref)
end
import Base: ==
key1::Key == key2::Key =
    key1.idx == key2.idx && key1.ref === key2.ref

global cache_key::Key
global cache_value

function get_value(idx::Int, ref)
    global cache_key, cache_value
    key = Key(idx, ref)
    if key == cache_key
        return cache_value
    else
        cache_key = key
        cache_value = create_value(key)
        return cache_value
    end
end

let result = code_escapes(get_value, (Int,Any))
    idx = only(findall(isnew, result.ir.stmts.stmt))
    idxs = findall(result.ir.stmts.stmt) do @nospecialize x
        return iscall((result.ir, getfield))(x) &&
               x.args[1] == SSAValue(idx)
    end
    for idx = idxs
        @test is_load_forwardable(result, idx)
    end
    @test has_all_escape(result[SSAValue(idx)])
    # TODO Have a better way to represent "flow-sensitive" information for allocating sinking
    rets = findall(isreturn, result.ir.stmts.stmt)
    @test count(rets) do retidx::Int
        bb = Compiler.block_for_inst(result.ir, retidx)
        has_no_escape(result.eresult.bbescapes[bb][SSAValue(idx)])
    end == 1
end

let result = code_escapes((String,Bool)) do s, c
        xs = Ref(s)
        if c
            xs[] = "foo"
        else
            @goto block4
        end
        @label block4
        return xs[], xs
    end
    idx = only(findall(iscall((result.ir, getfield)), result.ir.stmts.stmt))
    @test !is_load_forwardable(result, idx)
end

# `analyze_escapes` should be able to handle `IRCode` with new nodes
let code = Any[
        # block 1
        #=1=# Expr(:new, Base.RefValue{Bool}, Argument(2))
        #=2=# Expr(:call, GlobalRef(Core, :getfield), SSAValue(1), 1)
        #=3=# GotoIfNot(SSAValue(2), 5)
        # block 2
        #=4=# nothing
        # block 3
        #=5=# Expr(:call, GlobalRef(Core, :setfield!), SSAValue(1), 1, false)
        #=6=# ReturnNode(nothing)
    ]
    ir = make_ircode(code; slottypes=Any[Any,Bool])
    ir.stmts[1][:type] = Base.RefValue{Bool}
    Compiler.insert_node!(ir, SSAValue(4), Compiler.NewInstruction(Expr(:call, GlobalRef(Core, :setfield!), SSAValue(1), 1, false), Any), #=attach_after=#false)
    Compiler.insert_node!(ir, SSAValue(4), Compiler.NewInstruction(GotoNode(3), Any), #=attach_after=#true)
    ir[SSAValue(6)] = nothing # eliminate the ReturnNode
    s = Compiler.insert_node!(ir, SSAValue(6), Compiler.NewInstruction(Expr(:call, GlobalRef(Core, :getfield), SSAValue(1), 1), Any), #=attach_after=#false)
    Compiler.insert_node!(ir, SSAValue(6), Compiler.NewInstruction(ReturnNode(s), Any), #=attach_after=#true)
    # now this `ir` would look like:
    # 1 ─ %1 = %new(Base.RefValue{Bool}, _2)::Base.RefValue{Bool}                    │
    # │   %2 =   builtin Core.getfield(%1, 1)::Any                                   │
    # └──      goto #3 if not %2                                                     │
    # 2 ─        builtin Core.setfield!(%1, 1, false)::Any                           │
    # │        nothing::Any
    # └──      goto #3
    # 3 ┄        builtin Core.setfield!(%1, 1, false)::Any                           │
    # │   %9 =   builtin Core.getfield(%1, 1)::Any                                   │
    # │        nothing::Any
    # └──      return %9
    result = code_escapes(ir, 2)
    idxs = findall(iscall((result.ir, getfield)), result.ir.stmts.stmt)
    for idx = idxs
        @test is_load_forwardable(result, idx)
    end
end

mutable struct ObjectC
    c::Char
end
const g_xoc = Ref{ObjectC}()
const g_xxoc = Ref{Ref{ObjectC}}()

@noinline func_inter_return() = g_xoc
let result = code_escapes(func_inter_return)
    @test has_all_escape(result[0])
end
let result = code_escapes((Char,)) do c
        oc = ObjectC(c)
        xoc = func_inter_return()
        xoc[] = oc
        nothing
    end
    idx = only(findall(isnew_with_type((result.ir, ObjectC)), result.ir.stmts.stmt))
    @test has_all_escape(result[SSAValue(idx)])
end

@noinline func_raiser() = throw(g_xoc)
@noinline function func_inter_throw(xoc)
    try
        xoc[] = ObjectC('a')
        func_raiser()
    catch e
        e = e::Base.RefValue{ObjectC}
        g_xxoc[] = e
        e[] = ObjectC('b')
    end
    xoc[]
end
code_escapes(func_inter_throw, (typeof(g_xoc),))

# local alias analysis
# ====================

# refine `:nothrow` information using `MemoryInfo` analysis
function func_refine_nothrow(s::String)
    x = Ref{String}()
    x[] = s
    return x, x[]
end
let result = code_escapes(func_refine_nothrow, (String,))
    idx = only(findall(isnew, result.ir.stmts.stmt))
    @test !has_thrown_escape(result[SSAValue(idx)])
    effects = Base.infer_effects(func_refine_nothrow, (String,))
    @test_broken Compiler.is_nothrow(effects)
end

# inter-procedural alias analysis
# ===============================

mutable struct ObjectS
    s::String
end

@noinline update_os(os::ObjectS) = (os.s = ""; nothing)
@noinline update_os(os::ObjectS, s::String) = (os.s = s; nothing)
@noinline update_os(os::ObjectS, s::String, c::Bool) = (c && (os.s = s); nothing)
code_escapes((String,)) do s
    os = ObjectS(s)
    update_os(os)
    os.s
end

@noinline function swap_os(os1::ObjectS, os2::ObjectS)
    s1 = os1.s
    s2 = os2.s
    os1.s = s1
    os2.s = s2
end

function set_xos_yos(xos::ObjectS, yos::ObjectS)
    xos.s = "a"
    yos.s = "b"
    return xos.s # might be "b"
end

# needs to account for possibility of aliasing between arguments (and their memories)
# > alias analysis is inherently inter-procedural
let result = code_escapes(set_xos_yos, (ObjectS,ObjectS))
    idx = only(findall(iscall((result.ir, getfield)), result.ir.stmts.stmt))
    @test !is_load_forwardable(result, idx)
end

let result = code_escapes((String,)) do s
        os = ObjectS(s)
        set_xos_yos(os, os)
        os
    end
end

@noinline make_os(s::String) = ObjectS(s)
code_escapes((String,)) do s
    os = make_os(s)
    os.s
end

const g_xos = Ref(ObjectS(""))
@noinline function some_unsafe_func()
    println("some_unsafe_func is called")
    g_xos[] = ObjectS("julia2")
end

function callerfunc(@specialize(calleefunc), s::String)
    xxos = Ref(Ref{ObjectS}())
    calleefunc(xxos)
    xxos[][] = ObjectS(s) # may escape here (depending on `calleefunc`)
    some_unsafe_func()
    return xxos[][].s # returns `s`
end
@noinline function calleefunc1(xxos)
    yy = Ref(ObjectS("julia"))
    xxos[] = yy
    nothing
end
@noinline function calleefunc2(xxos)
    xxos[] = g_xos
    nothing
end

let result = code_escapes(callerfunc, (typeof(calleefunc1), String,))
    idx = only(findall(isnew_with_type((result.ir, ObjectS)), result.ir.stmts.stmt))
    @test_broken !has_all_escape(result[SSAValue(idx)])
end

let result = code_escapes(callerfunc, (typeof(calleefunc2), String,))
    idx = only(findall(isnew_with_type((result.ir, ObjectS)), result.ir.stmts.stmt))
    @test has_all_escape(result[SSAValue(idx)])
end

end # module test_EA
