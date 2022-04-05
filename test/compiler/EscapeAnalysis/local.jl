# Local EA Test
# =============
# EA works on post-inlining IR

include(normpath(@__DIR__, "setup.jl"))

@testset "basics" begin
    let # arg return
        result = code_escapes((Any,)) do a # return to caller
            return nothing
        end
        @test has_arg_escape(result.state[Argument(2)])
        # return
        result = code_escapes((Any,)) do a
            return a
        end
        i = only(findall(isreturn, result.ir.stmts.inst))
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
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
    end
    let # :gc_preserve_begin / :gc_preserve_end
        result = code_escapes((String,)) do s
            m = SafeRef(s)
            GC.@preserve m begin
                return nothing
            end
        end
        i = findfirst(isT(SafeRef{String}), result.ir.stmts.type) # find allocation statement
        @test !isnothing(i)
        @test has_no_escape(result.state[SSAValue(i)])
    end
    let # :isdefined
        result = code_escapes((String, Bool, )) do a, b
            if b
                s = Ref(a)
            end
            return @isdefined(s)
        end
        i = findfirst(isT(Base.RefValue{String}), result.ir.stmts.type) # find allocation statement
        @test !isnothing(i)
        @test has_no_escape(result.state[SSAValue(i)])
    end
    let # ϕ-node
        result = code_escapes((Bool,Any,Any)) do cond, a, b
            c = cond ? a : b # ϕ(a, b)
            return c
        end
        @assert any(@nospecialize(x)->isa(x, Core.PhiNode), result.ir.stmts.inst)
        i = only(findall(isreturn, result.ir.stmts.inst))
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
        @assert any(@nospecialize(x)->isa(x, Core.PiNode), result.ir.stmts.inst)
        @test any(findall(isreturn, result.ir.stmts.inst)) do i
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
        @assert any(@nospecialize(x)->isa(x, Core.PhiCNode), result.ir.stmts.inst)
        @assert any(@nospecialize(x)->isa(x, Core.UpsilonNode), result.ir.stmts.inst)
        i = only(findall(isreturn, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
        @test has_return_escape(result.state[SSAValue(i)])
    end
    let # try/catch
        result = code_escapes((Any,)) do a
            try
                nothing
            catch err
                return a # return escape
            end
        end
        @test has_return_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,)) do a
            try
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

let # simple allocation
    result = code_escapes((Bool,)) do c
        mm = SafeRef{Bool}(c) # just allocated, never escapes
        return mm[] ? nothing : 1
    end
    i = only(findall(isnew, result.ir.stmts.inst))
    @test has_no_escape(result.state[SSAValue(i)])
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
        inds = findall(isnew, result.ir.stmts.inst)
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
            if isnew(result.ir.stmts.inst[i]) && isT(Base.RefValue{String})(result.ir.stmts.type[i])
                @test has_return_escape(result.state[SSAValue(i)])
            elseif isnew(result.ir.stmts.inst[i]) && isT(Base.RefValue{Nothing})(result.ir.stmts.type[i])
                @test has_no_escape(result.state[SSAValue(i)])
            end
        end
    end

    let # typeassert
        result = code_escapes((Any,)) do x
            y = x::String
            return y
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
        @test !has_all_escape(result.state[Argument(2)])
    end

    let # isdefined
        result = code_escapes((Any,)) do x
            isdefined(x, :foo) ? x : throw("undefined")
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
        @test !has_all_escape(result.state[Argument(2)])

        result = code_escapes((Module,)) do m
            isdefined(m, 10) # throws
        end
        @test has_thrown_escape(result.state[Argument(2)])
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
        i = only(findall(isnew, result.ir.stmts.inst))
        rts = findall(isreturn, result.ir.stmts.inst)
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
        i = only(findall(isnew, result.ir.stmts.inst))
        rts = findall(isreturn, result.ir.stmts.inst) # return statement
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
        i = only(findall(isnew, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
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
        is = findall(isnew, result.ir.stmts.inst)
        @test length(is) == 2
        i1, i2 = is
        r = only(findall(isreturn, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test_broken !has_return_escape(result.state[SSAValue(i)], r)
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
        is = findall(isnew, result.ir.stmts.inst)
        @test length(is) == 2
        i1, i2 = is
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_all_escape(result.state[SSAValue(i1)])
        @test has_return_escape(result.state[SSAValue(i2)], r)
        @test_broken !has_all_escape(result.state[SSAValue(i2)])
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
        i = only(findall(isnew, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
        rs = findall(isreturn, result.ir.stmts.inst)
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
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
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
        i = only(findall(isnew, result.ir.stmts.inst))
        @test has_all_escape(result.state[SSAValue(i)])
        @test has_all_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,)) do a
            global GV = (a,)
            nothing
        end
        i = only(findall(iscall((result.ir, tuple)), result.ir.stmts.inst))
        @test has_all_escape(result.state[SSAValue(i)])
        @test has_all_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,)) do a
            o0 = SafeRef{Any}(a)
            global GV = SafeRef(o0)
            nothing
        end
        is = findall(isnew, result.ir.stmts.inst)
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
        inds = findall(iscall((result.ir, tuple)), result.ir.stmts.inst)
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
        i = only(findall(isnew, result.ir.stmts.inst))
        @test has_all_escape(result.state[SSAValue(i)])
        @test has_all_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,Any)) do a, b
            r = SafeRef{Any}(a)
            global GV = r
            r[] = b
            nothing
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        @test has_all_escape(result.state[SSAValue(i)])
        @test has_all_escape(result.state[Argument(2)]) # a
        @test has_all_escape(result.state[Argument(3)]) # b
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef{String}("Rx")
            $code_escapes((String,)) do s
                Rx[] = s
                Core.sizeof(Rx[])
            end
        end
        @test has_all_escape(result.state[Argument(2)])
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef{String}("Rx")
            $code_escapes((String,)) do s
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
    let result = code_escapes((String,)) do a
            o = SafeRef(a)
            f = o[]
            return f
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    let result = code_escapes((String,)) do a
            t = SafeRef((a,))
            f = t[][1]
            return f
        end
        i = only(findall(iscall((result.ir, tuple)), result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
        result.state[SSAValue(i)].AliasInfo
    end
    let result = code_escapes((String, String)) do a, b
            obj = SafeRefs(a, b)
            fld1 = obj[1]
            fld2 = obj[2]
            return (fld1, fld2)
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test has_return_escape(result.state[Argument(3)], r) # b
        @test is_load_forwardable(result.state[SSAValue(i)])
    end

    # field escape should propagate to `setfield!` argument
    let result = code_escapes((String,)) do a
            o = SafeRef("foo")
            o[] = a
            f = o[]
            return f
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    # propagate escape information imposed on return value of `setfield!` call
    let result = code_escapes((String,)) do a
            obj = SafeRef("foo")
            return (obj[] = a)
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
    end

    # nested allocations
    let result = code_escapes((String,)) do a
            o1 = SafeRef(a)
            o2 = SafeRef(o1)
            return o2[]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
        for i in 1:length(result.ir.stmts)
            if isnew(result.ir.stmts.inst[i]) && isT(SafeRef{String})(result.ir.stmts.type[i])
                @test has_return_escape(result.state[SSAValue(i)], r)
            elseif isnew(result.ir.stmts.inst[i]) && isT(SafeRef{SafeRef{String}})(result.ir.stmts.type[i])
                @test is_load_forwardable(result.state[SSAValue(i)])
            end
        end
    end
    let result = code_escapes((String,)) do a
            o1 = (a,)
            o2 = (o1,)
            return o2[1]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
        for i in 1:length(result.ir.stmts)
            if isnew(result.ir.stmts.inst[i]) && isT(Tuple{String})(result.ir.stmts.type[i])
                @test has_return_escape(result.state[SSAValue(i)], r)
            elseif isnew(result.ir.stmts.inst[i]) && isT(Tuple{Tuple{String}})(result.ir.stmts.type[i])
                @test is_load_forwardable(result.state[SSAValue(i)])
            end
        end
    end
    let result = code_escapes((String,)) do a
            o1  = SafeRef(a)
            o2  = SafeRef(o1)
            o1′ = o2[]
            a′  = o1′[]
            return a′
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
        for i in findall(isnew, result.ir.stmts.inst)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
    end
    let result = code_escapes() do
            o1 = SafeRef("foo")
            o2 = SafeRef(o1)
            return o2
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        for i in findall(isnew, result.ir.stmts.inst)
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
        r = only(findall(isreturn, result.ir.stmts.inst))
        findall(1:length(result.ir.stmts)) do i
            if isnew(result.ir.stmts[i][:inst])
                t = result.ir.stmts[i][:type]
                return t === SafeRef{String}  || # o1
                       t === SafeRef{SafeRef}    # o2
            end
            return false
        end |> x->foreach(x) do i
            @test has_return_escape(result.state[SSAValue(i)], r)
        end
    end
    let result = code_escapes((String,)) do x
            broadcast(identity, Ref(x))
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
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
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(3)], r) # x
        @test has_return_escape(result.state[Argument(4)], r) # y
        i = only(findall(isϕ, result.ir.stmts.inst))
        @test is_load_forwardable(result.state[SSAValue(i)])
        for i in findall(isnew, result.ir.stmts.inst)
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
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(3)], r) # x
        @test has_return_escape(result.state[Argument(4)], r) # y
        for i in findall(isϕ, result.ir.stmts.inst)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
        for i in findall(isnew, result.ir.stmts.inst)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
    end
    # when ϕ-node merges values with different types
    let result = code_escapes((Bool,String,String,String)) do cond, x, y, z
            local out
            if cond
                ϕ = SafeRef(x)
                out = ϕ[]
            else
                ϕ = SafeRefs(z, y)
            end
            return @isdefined(out) ? out : throw(ϕ)
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        t = only(findall(iscall((result.ir, throw)), result.ir.stmts.inst))
        ϕ = only(findall(isT(Union{SafeRef{String},SafeRefs{String,String}}), result.ir.stmts.type))
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
            return r[]
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
        @test isaliased(Argument(2), val, result.state)
        @test !isaliased(Argument(2), SSAValue(i), result.state)
    end
    let result = code_escapes((String,)) do s
            r1 = SafeRef(s)
            r2 = SafeRef(r1)
            return r2[]
        end
        i1, i2 = findall(isnew, result.ir.stmts.inst)
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
        @test !isaliased(SSAValue(i1), SSAValue(i2), result.state)
        @test isaliased(SSAValue(i1), val, result.state)
        @test !isaliased(SSAValue(i2), val, result.state)
    end
    let result = code_escapes((String,)) do s
            r1 = SafeRef(s)
            r2 = SafeRef(r1)
            return r2[][]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
        @test isaliased(Argument(2), val, result.state)
        for i in findall(isnew, result.ir.stmts.inst)
            @test !isaliased(SSAValue(i), val, result.state)
        end
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef("Rx")
            $code_escapes((String,)) do s
                r = SafeRef(Rx)
                rx = r[] # rx aliased to Rx
                rx[] = s
                nothing
            end
        end
        i = findfirst(isnew, result.ir.stmts.inst)
        @test has_all_escape(result.state[Argument(2)])
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    # alias via getfield & setfield!
    let result = code_escapes((String,)) do s
            r = Ref{String}()
            r[] = s
            return r[]
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
        @test isaliased(Argument(2), val, result.state)
        @test !isaliased(Argument(2), SSAValue(i), result.state)
    end
    let result = code_escapes((String,)) do s
            r1 = Ref(s)
            r2 = Ref{Base.RefValue{String}}()
            r2[] = r1
            return r2[]
        end
        i1, i2 = findall(isnew, result.ir.stmts.inst)
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
        @test !isaliased(SSAValue(i1), SSAValue(i2), result.state)
        @test isaliased(SSAValue(i1), val, result.state)
        @test !isaliased(SSAValue(i2), val, result.state)
    end
    let result = code_escapes((String,)) do s
            r1 = Ref{String}()
            r2 = Ref{Base.RefValue{String}}()
            r2[] = r1
            r1[] = s
            return r2[][]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
        @test isaliased(Argument(2), val, result.state)
        for i in findall(isnew, result.ir.stmts.inst)
            @test !isaliased(SSAValue(i), val, result.state)
        end
        result = code_escapes((String,)) do s
            r1 = Ref{String}()
            r2 = Ref{Base.RefValue{String}}()
            r1[] = s
            r2[] = r1
            return r2[][]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
        @test isaliased(Argument(2), val, result.state)
        for i in findall(isnew, result.ir.stmts.inst)
            @test !isaliased(SSAValue(i), val, result.state)
        end
    end
    let result = @eval EATModule() begin
            const Rx = SafeRef("Rx")
            $code_escapes((SafeRef{String}, String,)) do _rx, s
                r = SafeRef(_rx)
                r[] = Rx
                rx = r[] # rx aliased to Rx
                rx[] = s
                nothing
            end
        end
        i = findfirst(isnew, result.ir.stmts.inst)
        @test has_all_escape(result.state[Argument(3)])
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    # alias via typeassert
    let result = code_escapes((Any,)) do a
            r = a::String
            return r
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
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
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
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
    let result = code_escapes((Bool,String)) do cond, x
            if cond
                ϕ2 = ϕ1 = SafeRef("foo")
            else
                ϕ2 = ϕ1 = SafeRef("bar")
            end
            ϕ2[] = x
            return ϕ1[]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
        @test has_return_escape(result.state[Argument(3)], r) # x
        @test isaliased(Argument(3), val, result.state) # x
        for i in findall(isϕ, result.ir.stmts.inst)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
        for i in findall(isnew, result.ir.stmts.inst)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
    end
    let result = code_escapes((Bool,Bool,String)) do cond1, cond2, x
            if cond1
                ϕ2 = ϕ1 = SafeRef("foo")
            else
                ϕ2 = ϕ1 = SafeRef("bar")
            end
            cond2 && (ϕ2[] = x)
            return ϕ1[]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        val = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
        @test has_return_escape(result.state[Argument(4)], r) # x
        @test isaliased(Argument(4), val, result.state) # x
        for i in findall(isϕ, result.ir.stmts.inst)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
        for i in findall(isnew, result.ir.stmts.inst)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
    end
    # alias via π-node
    let result = code_escapes((Any,)) do x
            if isa(x, String)
                return x
            end
            throw("error!")
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        rval = (result.ir.stmts.inst[r]::ReturnNode).val::SSAValue
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
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
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
        r = only(findall(isreturn, result.ir.stmts.inst))
        for i in findall(iscall((result.ir, getfield)), result.ir.stmts.inst)
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
        i = only(findall(isinvoke(:genr), result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[SSAValue(i)], r)
    end

    # dynamic semantics
    # -----------------

    # conservatively handle untyped objects
    let result = @eval code_escapes((Any,Any,)) do T, x
            obj = $(Expr(:new, :T, :x))
        end
        t = only(findall(isnew, result.ir.stmts.inst))
        @test #=T=# has_thrown_escape(result.state[Argument(2)], t) # T
        @test #=x=# has_thrown_escape(result.state[Argument(3)], t) # x
    end
    let result = @eval code_escapes((Any,Any,Any,Any)) do T, x, y, z
            obj = $(Expr(:new, :T, :x, :y))
            return getfield(obj, :x)
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test #=x=# has_return_escape(result.state[Argument(3)], r)
        @test #=y=# has_return_escape(result.state[Argument(4)], r)
        @test #=z=# !has_return_escape(result.state[Argument(5)], r)
    end
    let result = @eval code_escapes((Any,Any,Any,Any)) do T, x, y, z
            obj = $(Expr(:new, :T, :x))
            setfield!(obj, :x, y)
            return getfield(obj, :x)
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test #=x=# has_return_escape(result.state[Argument(3)], r)
        @test #=y=# has_return_escape(result.state[Argument(4)], r)
        @test #=z=# !has_return_escape(result.state[Argument(5)], r)
    end

    # conservatively handle unknown field:
    # all fields should be escaped, but the allocation itself doesn't need to be escaped
    let result = code_escapes((String, Symbol)) do a, fld
            obj = SafeRef(a)
            return getfield(obj, fld)
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end
    let result = code_escapes((String, String, Symbol)) do a, b, fld
            obj = SafeRefs(a, b)
            return getfield(obj, fld) # should escape both `a` and `b`
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test has_return_escape(result.state[Argument(3)], r) # b
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end
    let result = code_escapes((String, String, Int)) do a, b, idx
            obj = SafeRefs(a, b)
            return obj[idx] # should escape both `a` and `b`
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test has_return_escape(result.state[Argument(3)], r) # b
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end
    let result = code_escapes((String, String, Symbol)) do a, b, fld
            obj = SafeRefs("a", "b")
            setfield!(obj, fld, a)
            return obj[2] # should escape `a`
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test !has_return_escape(result.state[Argument(3)], r) # b
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end
    let result = code_escapes((String, Symbol)) do a, fld
            obj = SafeRefs("a", "b")
            setfield!(obj, fld, a)
            return obj[1] # this should escape `a`
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end
    let result = code_escapes((String, String, Int)) do a, b, idx
            obj = SafeRefs("a", "b")
            obj[idx] = a
            return obj[2] # should escape `a`
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test !has_return_escape(result.state[Argument(3)], r) # b
        @test !is_load_forwardable(result.state[SSAValue(i)]) # obj
    end

    # interprocedural
    # ---------------

    let result = @eval EATModule() begin
            @noinline getx(obj) = obj[]
            $code_escapes((String,)) do a
                obj = SafeRef(a)
                fld = getx(obj)
                return fld
            end
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)
        # NOTE we can't scalar replace `obj`, but still we may want to stack allocate it
        @test_broken is_load_forwardable(result.state[SSAValue(i)])
    end

    # TODO interprocedural alias analysis
    let result = code_escapes((SafeRef{String},)) do s
            s[] = "bar"
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
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[SSAValue(i)], r)
        @test has_return_escape(result.state[Argument(2)], r) # y
    end
    let result = @eval EATModule() begin
            @noinline setxy!(x, y) = x[] = y
            $code_escapes((String,)) do y
                x1 = SafeRef("init")
                x2 = SafeRef(y)
                setxy!(x1, x2[])
                return x1
            end
        end
        i1, i2 = findall(isnew, result.ir.stmts.inst)
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[SSAValue(i1)], r)
        @test !has_return_escape(result.state[SSAValue(i2)], r)
        @test has_return_escape(result.state[Argument(2)], r) # y
    end
    let result = @eval EATModule() begin
            @noinline mysetindex!(x, a) = x[1] = a
            const Ax = Vector{Any}(undef, 1)
            $code_escapes((String,)) do s
                mysetindex!(Ax, s)
            end
        end
        @test has_all_escape(result.state[Argument(2)]) # s
    end

    # TODO flow-sensitivity?
    # ----------------------

    let result = code_escapes((Any,Any)) do a, b
            r = SafeRef{Any}(a)
            r[] = b
            return r[]
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test_broken !has_return_escape(result.state[Argument(2)], r) # a
        @test has_return_escape(result.state[Argument(3)], r) # b
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    let result = code_escapes((Any,Any)) do a, b
            r = SafeRef{Any}(:init)
            r[] = a
            r[] = b
            return r[]
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test_broken !has_return_escape(result.state[Argument(2)], r) # a
        @test has_return_escape(result.state[Argument(3)], r) # b
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
    let result = code_escapes((Any,Any,Bool)) do a, b, cond
            r = SafeRef{Any}(:init)
            if cond
                r[] = a
                return r[]
            else
                r[] = b
                return nothing
            end
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        @test is_load_forwardable(result.state[SSAValue(i)])
        r = only(findall(result.ir.stmts.inst) do @nospecialize x
            isreturn(x) && isa(x.val, Core.SSAValue)
        end)
        @test has_return_escape(result.state[Argument(2)], r) # a
        @test_broken !has_return_escape(result.state[Argument(3)], r) # b
    end

    # handle conflicting field information correctly
    let result = code_escapes((Bool,String,String,)) do cnd, baz, qux
            if cnd
                o = SafeRef("foo")
            else
                o = SafeRefs("bar", baz)
                r = getfield(o, 2)
            end
            if cnd
                o = o::SafeRef
                setfield!(o, 1, qux)
                r = getfield(o, 1)
            end
            r
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(3)], r) # baz
        @test has_return_escape(result.state[Argument(4)], r) # qux
        for new in findall(isnew, result.ir.stmts.inst)
            @test is_load_forwardable(result.state[SSAValue(new)])
        end
    end
    let result = code_escapes((Bool,String,String,)) do cnd, baz, qux
            if cnd
                o = SafeRefs("foo", "bar")
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
        r = only(findall(isreturn, result.ir.stmts.inst))
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
        for i in findall(isnew, result.ir.stmts.inst)
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
                 inst = EscapeAnalysis.getinst(result.ir, idx)
                 stmt = inst[:inst]
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
let result = @code_escapes compute(MPoint(1+.5im, 2+.5im), MPoint(2+.25im, 4+.75im))
    idxs = findall(1:length(result.ir.stmts)) do idx
        inst = EscapeAnalysis.getinst(result.ir, idx)
        stmt = inst[:inst]
        return isnew(stmt) && inst[:type] <: MPoint
    end
    @assert length(idxs) == 2
    @test count(i->is_load_forwardable(result.state[SSAValue(i)]), idxs) == 1
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
    for i in findall(1:length(result.ir.stmts)) do idx
                 inst = EscapeAnalysis.getinst(result.ir, idx)
                 stmt = inst[:inst]
                 return isnew(stmt) && inst[:type] <: MPoint
             end
        @test is_load_forwardable(result.state[SSAValue(i)])
    end
end

@testset "array primitives" begin
    inbounds = Base.JLOptions().check_bounds == 0

    # arrayref
    let result = code_escapes((Vector{String},Int)) do xs, i
            s = Base.arrayref(true, xs, i)
            return s
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)   # xs
        @test has_thrown_escape(result.state[Argument(2)])      # xs
        @test !has_return_escape(result.state[Argument(3)], r)  # i
    end
    let result = code_escapes((Vector{String},Int)) do xs, i
            s = Base.arrayref(false, xs, i)
            return s
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)   # xs
        @test !has_thrown_escape(result.state[Argument(2)])     # xs
        @test !has_return_escape(result.state[Argument(3)], r)  # i
    end
    inbounds && let result = code_escapes((Vector{String},Int)) do xs, i
            s = @inbounds xs[i]
            return s
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r)   # xs
        @test !has_thrown_escape(result.state[Argument(2)])     # xs
        @test !has_return_escape(result.state[Argument(3)], r)  # i
    end
    let result = code_escapes((Vector{String},Bool)) do xs, i
            c = Base.arrayref(true, xs, i) # TypeError will happen here
            return c
        end
        t = only(findall(iscall((result.ir, Base.arrayref)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t) # xs
    end
    let result = code_escapes((String,Int)) do xs, i
            c = Base.arrayref(true, xs, i) # TypeError will happen here
            return c
        end
        t = only(findall(iscall((result.ir, Base.arrayref)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t) # xs
    end
    let result = code_escapes((AbstractVector{String},Int)) do xs, i
            c = Base.arrayref(true, xs, i) # TypeError may happen here
            return c
        end
        t = only(findall(iscall((result.ir, Base.arrayref)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t) # xs
    end
    let result = code_escapes((Vector{String},Any)) do xs, i
            c = Base.arrayref(true, xs, i) # TypeError may happen here
            return c
        end
        t = only(findall(iscall((result.ir, Base.arrayref)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t) # xs
    end

    # arrayset
    let result = code_escapes((Vector{String},String,Int,)) do xs, x, i
            Base.arrayset(true, xs, x, i)
            return xs
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r) # xs
        @test has_thrown_escape(result.state[Argument(2)])    # xs
        @test has_return_escape(result.state[Argument(3)], r) # x
    end
    let result = code_escapes((Vector{String},String,Int,)) do xs, x, i
            Base.arrayset(false, xs, x, i)
            return xs
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r) # xs
        @test !has_thrown_escape(result.state[Argument(2)])    # xs
        @test has_return_escape(result.state[Argument(3)], r) # x
    end
    inbounds && let result = code_escapes((Vector{String},String,Int,)) do xs, x, i
            @inbounds xs[i] = x
            return xs
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[Argument(2)], r) # xs
        @test !has_thrown_escape(result.state[Argument(2)])    # xs
        @test has_return_escape(result.state[Argument(3)], r) # x
    end
    let result = code_escapes((String,String,String,)) do s, t, u
            xs = Vector{String}(undef, 3)
            Base.arrayset(true, xs, s, 1)
            Base.arrayset(true, xs, t, 2)
            Base.arrayset(true, xs, u, 3)
            return xs
        end
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[SSAValue(i)], r)
        for i in 2:result.state.nargs
            @test has_return_escape(result.state[Argument(i)], r)
        end
    end
    let result = code_escapes((Vector{String},String,Bool,)) do xs, x, i
            Base.arrayset(true, xs, x, i) # TypeError will happen here
            return xs
        end
        t = only(findall(iscall((result.ir, Base.arrayset)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t) # xs
        @test has_thrown_escape(result.state[Argument(3)], t) # x
    end
    let result = code_escapes((String,String,Int,)) do xs, x, i
            Base.arrayset(true, xs, x, i) # TypeError will happen here
            return xs
        end
        t = only(findall(iscall((result.ir, Base.arrayset)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t) # xs::String
        @test has_thrown_escape(result.state[Argument(3)], t) # x::String
    end
    let result = code_escapes((AbstractVector{String},String,Int,)) do xs, x, i
            Base.arrayset(true, xs, x, i) # TypeError may happen here
            return xs
        end
        t = only(findall(iscall((result.ir, Base.arrayset)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t) # xs
        @test has_thrown_escape(result.state[Argument(3)], t) # x
    end
    let result = code_escapes((Vector{String},AbstractString,Int,)) do xs, x, i
            Base.arrayset(true, xs, x, i) # TypeError may happen here
            return xs
        end
        t = only(findall(iscall((result.ir, Base.arrayset)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t) # xs
        @test has_thrown_escape(result.state[Argument(3)], t) # x
    end

    # arrayref and arrayset
    let result = code_escapes() do
            a = Vector{Vector{Any}}(undef, 1)
            b = Any[]
            a[1] = b
            return a[1]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        ai = only(findall(result.ir.stmts.inst) do @nospecialize x
            isarrayalloc(x) && x.args[2] === Vector{Vector{Any}}
        end)
        bi = only(findall(result.ir.stmts.inst) do @nospecialize x
            isarrayalloc(x) && x.args[2] === Vector{Any}
        end)
        @test !has_return_escape(result.state[SSAValue(ai)], r)
        @test has_return_escape(result.state[SSAValue(bi)], r)
    end
    let result = code_escapes() do
            a = Vector{Vector{Any}}(undef, 1)
            b = Any[]
            a[1] = b
            return a
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        ai = only(findall(result.ir.stmts.inst) do @nospecialize x
            isarrayalloc(x) && x.args[2] === Vector{Vector{Any}}
        end)
        bi = only(findall(result.ir.stmts.inst) do @nospecialize x
            isarrayalloc(x) && x.args[2] === Vector{Any}
        end)
        @test has_return_escape(result.state[SSAValue(ai)], r)
        @test has_return_escape(result.state[SSAValue(bi)], r)
    end
    let result = code_escapes((Vector{Any},String,Int,Int)) do xs, s, i, j
            x = SafeRef(s)
            xs[i] = x
            xs[j] # potential error
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        t = only(findall(iscall((result.ir, Base.arrayref)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(3)], t) # s
        @test has_thrown_escape(result.state[SSAValue(i)], t) # x
    end

    # arraysize
    let result = code_escapes((Vector{Any},)) do xs
            Core.arraysize(xs, 1)
        end
        t = only(findall(iscall((result.ir, Core.arraysize)), result.ir.stmts.inst))
        @test !has_thrown_escape(result.state[Argument(2)], t)
    end
    let result = code_escapes((Vector{Any},Int,)) do xs, dim
            Core.arraysize(xs, dim)
        end
        t = only(findall(iscall((result.ir, Core.arraysize)), result.ir.stmts.inst))
        @test !has_thrown_escape(result.state[Argument(2)], t)
    end
    let result = code_escapes((Any,)) do xs
            Core.arraysize(xs, 1)
        end
        t = only(findall(iscall((result.ir, Core.arraysize)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t)
    end

    # arraylen
    let result = code_escapes((Vector{Any},)) do xs
            Base.arraylen(xs)
        end
        t = only(findall(iscall((result.ir, Base.arraylen)), result.ir.stmts.inst))
        @test !has_thrown_escape(result.state[Argument(2)], t) # xs
    end
    let result = code_escapes((String,)) do xs
            Base.arraylen(xs)
        end
        t = only(findall(iscall((result.ir, Base.arraylen)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t) # xs
    end
    let result = code_escapes((Vector{Any},)) do xs
            Base.arraylen(xs, 1)
        end
        t = only(findall(iscall((result.ir, Base.arraylen)), result.ir.stmts.inst))
        @test has_thrown_escape(result.state[Argument(2)], t) # xs
    end

    # array resizing
    # without BoundsErrors
    let result = code_escapes((Vector{Any},String)) do xs, x
            @ccall jl_array_grow_beg(xs::Any, 2::UInt)::Cvoid
            xs[1] = x
            xs
        end
        t = only(findall(isarrayresize, result.ir.stmts.inst))
        @test !has_thrown_escape(result.state[Argument(2)], t) # xs
        @test !has_thrown_escape(result.state[Argument(3)], t) # x
    end
    let result = code_escapes((Vector{Any},String)) do xs, x
            @ccall jl_array_grow_end(xs::Any, 2::UInt)::Cvoid
            xs[1] = x
            xs
        end
        t = only(findall(isarrayresize, result.ir.stmts.inst))
        @test !has_thrown_escape(result.state[Argument(2)], t) # xs
        @test !has_thrown_escape(result.state[Argument(3)], t) # x
    end
    # with possible BoundsErrors
    let result = code_escapes((String,)) do x
            xs = Any[1,2,3]
            xs[3] = x
            @ccall jl_array_del_beg(xs::Any, 2::UInt)::Cvoid # can potentially throw
            xs
        end
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        t = only(findall(isarrayresize, result.ir.stmts.inst))
        @test has_thrown_escape(result.state[SSAValue(i)], t) # xs
        @test has_thrown_escape(result.state[Argument(2)], t) # x
    end
    let result = code_escapes((String,)) do x
            xs = Any[1,2,3]
            xs[1] = x
            @ccall jl_array_del_end(xs::Any, 2::UInt)::Cvoid # can potentially throw
            xs
        end
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        t = only(findall(isarrayresize, result.ir.stmts.inst))
        @test has_thrown_escape(result.state[SSAValue(i)], t) # xs
        @test has_thrown_escape(result.state[Argument(2)], t) # x
    end
    let result = code_escapes((String,)) do x
            xs = Any[x]
            @ccall jl_array_grow_at(xs::Any, 1::UInt, 2::UInt)::Cvoid # can potentially throw
        end
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        t = only(findall(isarrayresize, result.ir.stmts.inst))
        @test has_thrown_escape(result.state[SSAValue(i)], t) # xs
        @test has_thrown_escape(result.state[Argument(2)], t) # x
    end
    let result = code_escapes((String,)) do x
            xs = Any[x]
            @ccall jl_array_del_at(xs::Any, 1::UInt, 2::UInt)::Cvoid # can potentially throw
        end
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        t = only(findall(isarrayresize, result.ir.stmts.inst))
        @test has_thrown_escape(result.state[SSAValue(i)], t) # xs
        @test has_thrown_escape(result.state[Argument(2)], t) # x
    end
    inbounds && let result = code_escapes((String,)) do x
            xs = @inbounds Any[x]
            @ccall jl_array_del_at(xs::Any, 1::UInt, 2::UInt)::Cvoid # can potentially throw
        end
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        t = only(findall(isarrayresize, result.ir.stmts.inst))
        @test has_thrown_escape(result.state[SSAValue(i)], t) # xs
        @test has_thrown_escape(result.state[Argument(2)], t) # x
    end

    # array copy
    let result = code_escapes((Vector{Any},)) do xs
            return copy(xs)
        end
        i = only(findall(isarraycopy, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[SSAValue(i)], r)
        @test_broken !has_return_escape(result.state[Argument(2)], r)
    end
    let result = code_escapes((String,)) do s
            xs = String[s]
            xs′ = copy(xs)
            return xs′[1]
        end
        i1 = only(findall(isarrayalloc, result.ir.stmts.inst))
        i2 = only(findall(isarraycopy, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test !has_return_escape(result.state[SSAValue(i1)])
        @test !has_return_escape(result.state[SSAValue(i2)])
        @test has_return_escape(result.state[Argument(2)], r) # s
    end
    let result = code_escapes((Vector{Any},)) do xs
            xs′ = copy(xs)
            return xs′[1] # may potentially throw BoundsError, should escape `xs` conservatively (i.e. escape its elements)
        end
        i = only(findall(isarraycopy, result.ir.stmts.inst))
        ref = only(findall(iscall((result.ir, Base.arrayref)), result.ir.stmts.inst))
        ret = only(findall(isreturn, result.ir.stmts.inst))
        @test_broken !has_thrown_escape(result.state[SSAValue(i)], ref)
        @test_broken !has_return_escape(result.state[SSAValue(i)], ret)
        @test has_thrown_escape(result.state[Argument(2)], ref)
        @test has_return_escape(result.state[Argument(2)], ret)
    end
    let result = code_escapes((String,)) do s
            xs = Vector{String}(undef, 1)
            xs[1] = s
            xs′ = copy(xs)
            length(xs′) > 2 && throw(xs′)
            return xs′
        end
        i1 = only(findall(isarrayalloc, result.ir.stmts.inst))
        i2 = only(findall(isarraycopy, result.ir.stmts.inst))
        t = only(findall(iscall((result.ir, throw)), result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test_broken !has_thrown_escape(result.state[SSAValue(i1)], t)
        @test_broken !has_return_escape(result.state[SSAValue(i1)], r)
        @test has_thrown_escape(result.state[SSAValue(i2)], t)
        @test has_return_escape(result.state[SSAValue(i2)], r)
        @test has_thrown_escape(result.state[Argument(2)], t)
        @test has_return_escape(result.state[Argument(2)], r)
    end

    # isassigned
    let result = code_escapes((Vector{Any},Int)) do xs, i
            return isassigned(xs, i)
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test !has_return_escape(result.state[Argument(2)], r)
        @test !has_thrown_escape(result.state[Argument(2)])
    end

    # indexing analysis
    # -----------------

    # safe case
    let result = code_escapes((String,String)) do s, t
            a = Vector{Any}(undef, 2)
            a[1] = s
            a[2] = t
            return a[1]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test !has_return_escape(result.state[SSAValue(i)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
        @test has_return_escape(result.state[Argument(2)], r) # s
        @test !has_return_escape(result.state[Argument(3)], r) # t
    end
    let result = code_escapes((String,String)) do s, t
            a = Matrix{Any}(undef, 1, 2)
            a[1, 1] = s
            a[1, 2] = t
            return a[1, 1]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test !has_return_escape(result.state[SSAValue(i)], r)
        @test is_load_forwardable(result.state[SSAValue(i)])
        @test has_return_escape(result.state[Argument(2)], r) # s
        @test !has_return_escape(result.state[Argument(3)], r) # t
    end
    let result = code_escapes((Bool,String,String,String)) do c, s, t, u
            a = Vector{Any}(undef, 2)
            if c
                a[1] = s
                a[2] = u
            else
                a[1] = t
                a[2] = u
            end
            return a[1]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test is_load_forwardable(result.state[SSAValue(i)])
        @test !has_return_escape(result.state[SSAValue(i)], r)
        @test has_return_escape(result.state[Argument(3)], r) # s
        @test has_return_escape(result.state[Argument(4)], r) # t
        @test !has_return_escape(result.state[Argument(5)], r) # u
    end
    let result = code_escapes((Bool,String,String,String)) do c, s, t, u
            a = Any[nothing, nothing] # TODO how to deal with loop indexing?
            if c
                a[1] = s
                a[2] = u
            else
                a[1] = t
                a[2] = u
            end
            return a[1]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test !has_return_escape(result.state[SSAValue(i)], r)
        @test_broken is_load_forwardable(result.state[SSAValue(i)])
        @test has_return_escape(result.state[Argument(3)], r) # s
        @test has_return_escape(result.state[Argument(4)], r) # t
        @test_broken !has_return_escape(result.state[Argument(5)], r) # u
    end
    let result = code_escapes((String,)) do s
            a = Vector{Vector{Any}}(undef, 1)
            b = Any[s]
            a[1] = b
            return a[1][1]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        is = findall(isarrayalloc, result.ir.stmts.inst)
        @assert length(is) == 2
        ia, ib = is
        @test !has_return_escape(result.state[SSAValue(ia)], r)
        @test is_load_forwardable(result.state[SSAValue(ia)])
        @test !has_return_escape(result.state[SSAValue(ib)], r)
        @test_broken is_load_forwardable(result.state[SSAValue(ib)])
        @test has_return_escape(result.state[Argument(2)], r) # s
    end
    let result = code_escapes((Bool,String,String,Regex,Regex,)) do c, s1, s2, t1, t2
            if c
                a = Vector{String}(undef, 2)
                a[1] = s1
                a[2] = s2
            else
                a = Vector{Regex}(undef, 2)
                a[1] = t1
                a[2] = t2
            end
            return a[1]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        for i in findall(isarrayalloc, result.ir.stmts.inst)
            @test !has_return_escape(result.state[SSAValue(i)], r)
            @test is_load_forwardable(result.state[SSAValue(i)])
        end
        @test has_return_escape(result.state[Argument(3)], r) # s1
        @test !has_return_escape(result.state[Argument(4)], r) # s2
        @test has_return_escape(result.state[Argument(5)], r) # t1
        @test !has_return_escape(result.state[Argument(6)], r) # t2
    end
    let result = code_escapes((String,String,Int)) do s, t, i
            a = Any[s]
            push!(a, t)
            return a[2]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test !has_return_escape(result.state[SSAValue(i)], r)
        @test_broken is_load_forwardable(result.state[SSAValue(i)])
        @test_broken !has_return_escape(result.state[Argument(2)], r) # s
        @test has_return_escape(result.state[Argument(3)], r) # t
    end
    # unsafe cases
    let result = code_escapes((String,String,Int)) do s, t, i
            a = Vector{Any}(undef, 2)
            a[1] = s
            a[2] = t
            return a[i]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test !has_return_escape(result.state[SSAValue(i)], r)
        @test !is_load_forwardable(result.state[SSAValue(i)])
        @test has_return_escape(result.state[Argument(2)], r) # s
        @test has_return_escape(result.state[Argument(3)], r) # t
    end
    let result = code_escapes((String,String,Int)) do s, t, i
            a = Vector{Any}(undef, 2)
            a[1] = s
            a[i] = t
            return a[1]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test !has_return_escape(result.state[SSAValue(i)], r)
        @test !is_load_forwardable(result.state[SSAValue(i)])
        @test has_return_escape(result.state[Argument(2)], r) # s
        @test has_return_escape(result.state[Argument(3)], r) # t
    end
    let result = code_escapes((String,String,Int,Int,Int)) do s, t, i, j, k
            a = Vector{Any}(undef, 2)
            a[3] = s # BoundsError
            a[1] = t
            return a[1]
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test !has_return_escape(result.state[SSAValue(i)], r)
        @test !is_load_forwardable(result.state[SSAValue(i)])
    end
    let result = @eval Module() begin
            @noinline some_resize!(a) = pushfirst!(a, nothing)
            $code_escapes((String,String,Int)) do s, t, i
                a = Vector{Any}(undef, 2)
                a[1] = s
                some_resize!(a)
                return a[2]
            end
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test_broken !has_return_escape(result.state[SSAValue(i)], r)
        @test !is_load_forwardable(result.state[SSAValue(i)])
    end

    # circular reference
    let result = code_escapes() do
            xs = Vector{Any}(undef, 1)
            xs[1] = xs
            return xs[1]
        end
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[SSAValue(i)], r)
    end
    let result = @eval Module() begin
            const Ax = Vector{Any}(undef, 1)
            Ax[1] = Ax
            $code_escapes() do
                xs = Ax[1]::Vector{Any}
                return xs[1]
            end
        end
        r = only(findall(isreturn, result.ir.stmts.inst))
        for i in findall(iscall((result.ir, Core.arrayref)), result.ir.stmts.inst)
            @test has_return_escape(result.state[SSAValue(i)], r)
        end
    end
    let result = @eval Module() begin
            @noinline function genxs()
                xs = Vector{Any}(undef, 1)
                xs[1] = xs
                return xs
            end
            $code_escapes() do
                xs = genxs()
                return xs[1]
            end
        end
        i = only(findall(isinvoke(:genxs), result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test has_return_escape(result.state[SSAValue(i)], r)
    end
end

# demonstrate array primitive support with a realistic end to end example
let result = code_escapes((Int,String,)) do n,s
        xs = String[]
        for i in 1:n
            push!(xs, s)
        end
        xs
    end
    i = only(findall(isarrayalloc, result.ir.stmts.inst))
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test has_return_escape(result.state[SSAValue(i)], r)
    Base.JLOptions().check_bounds ≠ 0 && @test has_thrown_escape(result.state[SSAValue(i)])
    @test has_return_escape(result.state[Argument(3)], r) # s
    Base.JLOptions().check_bounds ≠ 0 && @test has_thrown_escape(result.state[Argument(3)])    # s
end
let result = code_escapes((Int,String,)) do n,s
        xs = String[]
        for i in 1:n
            pushfirst!(xs, s)
        end
        xs
    end
    i = only(findall(isarrayalloc, result.ir.stmts.inst))
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test has_return_escape(result.state[SSAValue(i)], r) # xs
    @test has_thrown_escape(result.state[SSAValue(i)])    # xs
    @test has_return_escape(result.state[Argument(3)], r) # s
    @test has_thrown_escape(result.state[Argument(3)])    # s
end
let result = code_escapes((String,String,String)) do s, t, u
        xs = String[]
        resize!(xs, 3)
        xs[1] = s
        xs[1] = t
        xs[1] = u
        xs
    end
    i = only(findall(isarrayalloc, result.ir.stmts.inst))
    r = only(findall(isreturn, result.ir.stmts.inst))
    @test has_return_escape(result.state[SSAValue(i)], r)
    @test has_thrown_escape(result.state[SSAValue(i)])    # xs
    @test has_return_escape(result.state[Argument(2)], r) # s
    @test has_return_escape(result.state[Argument(3)], r) # t
    @test has_return_escape(result.state[Argument(4)], r) # u
end

@static if isdefined(Core, :ImmutableArray)

import Core: ImmutableArray, arrayfreeze, mutating_arrayfreeze, arraythaw

@testset "ImmutableArray" begin
    # arrayfreeze
    let result = code_escapes((Vector{Any},)) do xs
            arrayfreeze(xs)
        end
        @test !has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Vector,)) do xs
            arrayfreeze(xs)
        end
        @test !has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,)) do xs
            arrayfreeze(xs)
        end
        @test has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes((ImmutableArray{Any,1},)) do xs
            arrayfreeze(xs)
        end
        @test has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes() do
            xs = Any[]
            arrayfreeze(xs)
        end
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test has_no_escape(result.state[SSAValue(1)])
    end

    # mutating_arrayfreeze
    let result = code_escapes((Vector{Any},)) do xs
            mutating_arrayfreeze(xs)
        end
        @test !has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Vector,)) do xs
            mutating_arrayfreeze(xs)
        end
        @test !has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,)) do xs
            mutating_arrayfreeze(xs)
        end
        @test has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes((ImmutableArray{Any,1},)) do xs
            mutating_arrayfreeze(xs)
        end
        @test has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes() do
            xs = Any[]
            mutating_arrayfreeze(xs)
        end
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test has_no_escape(result.state[SSAValue(1)])
    end

    # arraythaw
    let result = code_escapes((ImmutableArray{Any,1},)) do xs
            arraythaw(xs)
        end
        @test !has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes((ImmutableArray,)) do xs
            arraythaw(xs)
        end
        @test !has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Any,)) do xs
            arraythaw(xs)
        end
        @test has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes((Vector{Any},)) do xs
            arraythaw(xs)
        end
        @test has_thrown_escape(result.state[Argument(2)])
    end
    let result = code_escapes() do
            xs = ImmutableArray(Any[])
            arraythaw(xs)
        end
        i = only(findall(isarrayalloc, result.ir.stmts.inst))
        @test has_no_escape(result.state[SSAValue(1)])
    end
end

# demonstrate some arrayfreeze optimizations
# !has_return_escape(ary) means ary is eligible for arrayfreeze to mutating_arrayfreeze optimization
let result = code_escapes((Int,)) do n
        xs = collect(1:n)
        ImmutableArray(xs)
    end
    i = only(findall(isarrayalloc, result.ir.stmts.inst))
    @test !has_return_escape(result.state[SSAValue(i)])
end
let result = code_escapes((Vector{Float64},)) do xs
        ys = sin.(xs)
        ImmutableArray(ys)
    end
    i = only(findall(isarrayalloc, result.ir.stmts.inst))
    @test !has_return_escape(result.state[SSAValue(i)])
end
let result = code_escapes((Vector{Pair{Int,String}},)) do xs
        n = maximum(first, xs)
        ys = Vector{String}(undef, n)
        for (i, s) in xs
            ys[i] = s
        end
        ImmutableArray(xs)
    end
    i = only(findall(isarrayalloc, result.ir.stmts.inst))
    @test !has_return_escape(result.state[SSAValue(i)])
end

end # @static if isdefined(Core, :ImmutableArray)

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
            f = o[]
            return f
        end
        i = only(findall(isnew, result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test !has_return_escape(result.state[SSAValue(i)], r)
    end

    # an escaped tuple stmt will not propagate to its Int argument (since `Int` is of bitstype)
    let result = code_escapes((Int,Any,)) do a, b
            t = tuple(a, b)
            return t
        end
        i = only(findall(iscall((result.ir, tuple)), result.ir.stmts.inst))
        r = only(findall(isreturn, result.ir.stmts.inst))
        @test !has_return_escape(result.state[Argument(2)], r)
        @test has_return_escape(result.state[Argument(3)], r)
    end
end

# # TODO implement a finalizer elision pass
# mutable struct WithFinalizer
#     v
#     function WithFinalizer(v)
#         x = new(v)
#         f(t) = @async println("Finalizing $t.")
#         return finalizer(x, x)
#     end
# end
# make_m(v = 10) = MyMutable(v)
# function simple(cond)
#     m = make_m()
#     if cond
#         # println(m.v)
#         return nothing # <= insert `finalize` call here
#     end
#     return m
# end
