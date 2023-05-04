# This file is a part of Julia. License is MIT: https://julialang.org/license

# Cassette
# ========

module MiniCassette
    # A minimal demonstration of the cassette mechanism. Doesn't support all the
    # fancy features, but sufficient to exercise this code path in the compiler.

    using Core.Compiler: retrieve_code_info, CodeInfo,
        MethodInstance, SSAValue, GotoNode, GotoIfNot, ReturnNode, SlotNumber, quoted,
        signature_type, anymap
    using Base: _methods_by_ftype
    using Base.Meta: isexpr
    using Test

    export Ctx, overdub

    struct Ctx; end

    # A no-op cassette-like transform
    function transform_expr(expr, map_slot_number, map_ssa_value, sparams::Core.SimpleVector)
        @nospecialize expr
        transform(@nospecialize expr) = transform_expr(expr, map_slot_number, map_ssa_value, sparams)
        if isexpr(expr, :call)
            return Expr(:call, overdub, SlotNumber(2), anymap(transform, expr.args)...)
        elseif isa(expr, GotoIfNot)
            return GotoIfNot(transform(expr.cond), map_ssa_value(SSAValue(expr.dest)).id)
        elseif isexpr(expr, :static_parameter)
            return quoted(sparams[expr.args[1]])
        elseif isa(expr, ReturnNode)
            return ReturnNode(transform(expr.val))
        elseif isa(expr, Expr)
            return Expr(expr.head, anymap(transform, expr.args)...)
        elseif isa(expr, GotoNode)
            return GotoNode(map_ssa_value(SSAValue(expr.label)).id)
        elseif isa(expr, SlotNumber)
            return map_slot_number(expr.id)
        elseif isa(expr, SSAValue)
            return map_ssa_value(expr)
        else
            return expr
        end
    end

    function transform!(ci::CodeInfo, nargs::Int, sparams::Core.SimpleVector)
        code = ci.code
        ci.slotnames = Symbol[Symbol("#self#"), :ctx, :f, :args, ci.slotnames[nargs+1:end]...]
        ci.slotflags = UInt8[(0x00 for i = 1:4)..., ci.slotflags[nargs+1:end]...]
        # Insert one SSAValue for every argument statement
        prepend!(code, Any[Expr(:call, getfield, SlotNumber(4), i) for i = 1:nargs])
        prepend!(ci.codelocs, fill(0, nargs))
        prepend!(ci.ssaflags, fill(0x00, nargs))
        ci.ssavaluetypes += nargs
        function map_slot_number(slot::Int)
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

    function overdub_generator(world::UInt, source, self, c, f, args)
        @nospecialize
        if !Base.issingletontype(f)
            # (c, f, args..) -> f(args...)
            code_info = :(return f(args...))
            return Core.GeneratedFunctionStub(identity, Core.svec(:overdub, :c, :f, :args), Core.svec())(world, source, code_info)
        end

        tt = Tuple{f, args...}
        match = Base._which(tt; world)
        mi = Core.Compiler.specialize_method(match)
        # Unsupported in this mini-cassette
        @assert !mi.def.isva
        code_info = retrieve_code_info(mi, world)
        @assert isa(code_info, CodeInfo)
        code_info = copy(code_info)
        @assert code_info.edges === nothing
        code_info.edges = MethodInstance[mi]
        transform!(code_info, length(args), match.sparams)
        # TODO: this is mandatory: code_info.min_world = max(code_info.min_world, min_world[])
        # TODO: this is mandatory: code_info.max_world = min(code_info.max_world, max_world[])
        return code_info
    end

    @inline function overdub(c::Ctx, f::Union{Core.Builtin, Core.IntrinsicFunction}, args...)
        f(args...)
    end

    @eval function overdub(c::Ctx, f, args...)
        $(Expr(:meta, :generated_only))
        $(Expr(:meta, :generated, overdub_generator))
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

@generated bar(::Val{align}) where {align} = :(42)
foo(i) = i+bar(Val(1))

@test @inferred(overdub(Ctx(), foo, 1)) == 43

# overlay method tables
# =====================

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

end # module OverlayModule

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
    try
        rm(depot_path, force=true, recursive=true)
    catch err
        @show err
    end
    filter!((≠)(load_path), LOAD_PATH)
    filter!((≠)(depot_path), DEPOT_PATH)
end
