# This file is a part of Julia. License is MIT: https://julialang.org/license

module MiniCassette
    # A minimal demonstration of the cassette mechanism. Doesn't support all the
    # fancy features, but sufficient to exercise this code path in the compiler.

    using Core.Compiler: method_instances, retrieve_code_info, CodeInfo,
        MethodInstance, SSAValue, GotoNode, GotoIfNot, ReturnNode, Slot, SlotNumber, quoted,
        signature_type
    using Base: _methods_by_ftype
    using Base.Meta: isexpr
    using Test

    export Ctx, overdub

    struct Ctx; end

    # A no-op cassette-like transform
    function transform_expr(expr, map_slot_number, map_ssa_value, sparams)
        transform(expr) = transform_expr(expr, map_slot_number, map_ssa_value, sparams)
        if isexpr(expr, :call)
            return Expr(:call, overdub, SlotNumber(2), map(transform, expr.args)...)
        elseif isa(expr, GotoIfNot)
            return GotoIfNot(transform(expr.cond), map_ssa_value(SSAValue(expr.dest)).id)
        elseif isexpr(expr, :static_parameter)
            return quoted(sparams[expr.args[1]])
        elseif isa(expr, ReturnNode)
            return ReturnNode(transform(expr.val))
        elseif isa(expr, Expr)
            return Expr(expr.head, map(transform, expr.args)...)
        elseif isa(expr, GotoNode)
            return GotoNode(map_ssa_value(SSAValue(expr.label)).id)
        elseif isa(expr, Slot)
            return map_slot_number(expr.id)
        elseif isa(expr, SSAValue)
            return map_ssa_value(expr)
        else
            return expr
        end
    end

    function transform!(ci, nargs, sparams)
        code = ci.code
        ci.slotnames = Symbol[Symbol("#self#"), :ctx, :f, :args, ci.slotnames[nargs+1:end]...]
        ci.slotflags = UInt8[(0x00 for i = 1:4)..., ci.slotflags[nargs+1:end]...]
        # Insert one SSAValue for every argument statement
        prepend!(code, [Expr(:call, getfield, SlotNumber(4), i) for i = 1:nargs])
        prepend!(ci.codelocs, [0 for i = 1:nargs])
        prepend!(ci.ssaflags, [0x00 for i = 1:nargs])
        ci.ssavaluetypes += nargs
        function map_slot_number(slot)
            if slot == 1
                # self in the original function is now `f`
                return SlotNumber(3)
            elseif 2 <= slot <= nargs + 1
                # Arguments get inserted as ssa values at the top of the function
                return SSAValue(slot - 1)
            else
                # The first non-argument slot will be 5
                return SlotNumber(slot - (nargs + 1) + 4)
            end
        end
        map_ssa_value(ssa::SSAValue) = SSAValue(ssa.id + nargs)
        for i = (nargs+1:length(code))
            code[i] = transform_expr(code[i], map_slot_number, map_ssa_value, sparams)
        end
    end

    function overdub_generator(self, c, f, args)
        if !isdefined(f, :instance)
            return :(return f(args...))
        end

        tt = Tuple{f, args...}
        match = Base._which(tt, typemax(UInt))
        mi = Core.Compiler.specialize_method(match)
        # Unsupported in this mini-cassette
        @assert !mi.def.isva
        code_info = retrieve_code_info(mi)
        @assert isa(code_info, CodeInfo)
        code_info = copy(code_info)
        if isdefined(code_info, :edges)
            code_info.edges = MethodInstance[mi]
        end
        transform!(code_info, length(args), match.sparams)
        code_info
    end

    @inline function overdub(c::Ctx, f::Union{Core.Builtin, Core.IntrinsicFunction}, args...)
        f(args...)
    end

    @eval function overdub(c::Ctx, f, args...)
        $(Expr(:meta, :generated_only))
        $(Expr(:meta,
                :generated,
                Expr(:new,
                    Core.GeneratedFunctionStub,
                    :overdub_generator,
                    Any[:overdub, :ctx, :f, :args],
                    Any[],
                    @__LINE__,
                    QuoteNode(Symbol(@__FILE__)),
                    true)))
    end
end

using .MiniCassette

# Test #265 for Cassette
f() = 1
@test overdub(Ctx(), f) === 1
f() = 2
@test overdub(Ctx(), f) === 2

# Test that MiniCassette is at least somewhat capable by overdubbing gcd
@test overdub(Ctx(), gcd, 10, 20) === gcd(10, 20)

# Test that pure propagates for Cassette
Base.@pure isbitstype(T) = Base.isbitstype(T)
f31012(T) = Val(isbitstype(T))
@test @inferred(overdub(Ctx(), f31012, Int64)) == Val(true)

@generated bar(::Val{align}) where {align} = :(42)
foo(i) = i+bar(Val(1))

@test @inferred(overdub(Ctx(), foo, 1)) == 43

# Check that misbehaving pure functions propagate their error
Base.@pure func1() = 42
Base.@pure func2() = (this_is_an_exception; func1())

let method = which(func2, ())
    mi = Core.Compiler.specialize_method(method, Tuple{typeof(func2)}, Core.svec())
    mi.inInference = true
end
func3() = func2()
@test_throws UndefVarError func3()



## overlay method tables

module OverlayModule

using Base.Experimental: @MethodTable, @overlay

@MethodTable(mt)

@overlay mt function sin(x::Float64)
    1
end

# short function def
@overlay mt cos(x::Float64) = 2

# parametric function def
@overlay mt tan(x::T) where {T} = 3

end

methods = Base._methods_by_ftype(Tuple{typeof(sin), Float64}, nothing, 1, Base.get_world_counter())
@test only(methods).method.module === Base.Math

methods = Base._methods_by_ftype(Tuple{typeof(sin), Float64}, OverlayModule.mt, 1, Base.get_world_counter())
@test only(methods).method.module === OverlayModule

methods = Base._methods_by_ftype(Tuple{typeof(sin), Int}, OverlayModule.mt, 1, Base.get_world_counter())
@test isempty(methods)

# precompilation

load_path = mktempdir()
depot_path = mktempdir()
try
    pushfirst!(LOAD_PATH, load_path)
    pushfirst!(DEPOT_PATH, depot_path)

    write(joinpath(load_path, "Foo.jl"),
          """
          module Foo
          Base.Experimental.@MethodTable(mt)
          Base.Experimental.@overlay mt sin(x::Int) = 1
          end
          """)

     # precompiling Foo serializes the overlay method through the `mt` binding in the module
     Foo = Base.require(Main, :Foo)
     @test length(Foo.mt) == 1

    write(joinpath(load_path, "Bar.jl"),
          """
          module Bar
          Base.Experimental.@MethodTable(mt)
          end
          """)

    write(joinpath(load_path, "Baz.jl"),
          """
          module Baz
          using Bar
          Base.Experimental.@overlay Bar.mt sin(x::Int) = 1
          end
          """)

     # when referring an method table in another module,
     # the overlay method needs to be discovered explicitly
     Bar = Base.require(Main, :Bar)
     @test length(Bar.mt) == 0
     Baz = Base.require(Main, :Baz)
     @test length(Bar.mt) == 1
finally
    rm(load_path, recursive=true, force=true)
    rm(depot_path, recursive=true, force=true)
    filter!((≠)(load_path), LOAD_PATH)
    filter!((≠)(depot_path), DEPOT_PATH)
end
