# This file is a part of Julia. License is MIT: https://julialang.org/license

module contextual

# N.B.: This file is also run from interpreter.jl, so needs to be standalone-executable
using Test

# Cassette
# ========

# TODO Use CassetteBase.jl instead of this mini-cassette?

module MiniCassette
    # A minimal demonstration of the cassette mechanism. Doesn't support all the
    # fancy features, but sufficient to exercise this code path in the compiler.

    using Core: SimpleVector
    using Core.IR
    using Base: Compiler as CC
    using .CC: retrieve_code_info, quoted, anymap
    using Base.Meta: isexpr

    export Ctx, overdub

    struct Ctx; end

    # A no-op cassette-like transform
    function transform_expr(expr, map_slot_number, map_ssa_value, sparams::SimpleVector)
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

    function transform!(mi::MethodInstance, ci::CodeInfo, nargs::Int, sparams::SimpleVector)
        code = ci.code
        di = CC.DebugInfoStream(mi, ci.debuginfo, length(code))
        ci.slotnames = Symbol[Symbol("#self#"), :ctx, :f, :args, ci.slotnames[nargs+2:end]...]
        ci.slotflags = UInt8[(0x00 for i = 1:4)..., ci.slotflags[nargs+2:end]...]
        # Insert one SSAValue for every argument statement
        prepend!(code, Any[Expr(:call, getfield, SlotNumber(4), i) for i = 1:nargs])
        prepend!(di.codelocs, fill(Int32(0), 3nargs))
        prepend!(ci.ssaflags, fill(0x00, nargs))
        ci.debuginfo = Core.DebugInfo(di, length(code))
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

    function overdub_generator(world::UInt, source, self, ctx, f, args)
        @nospecialize
        argnames = Core.svec(:overdub, :ctx, :f, :args)
        spnames = Core.svec()

        if !Base.issingletontype(f)
            # (c, f, args..) -> f(args...)
            return generate_lambda_ex(world, source, argnames, spnames, :(return f(args...)))
        end

        tt = Tuple{f, args...}
        match = Base._which(tt; world)
        mi = Base.specialize_method(match)
        # Unsupported in this mini-cassette
        !mi.def.isva ||
            return generate_lambda_ex(world, source, argnames, spnames, :(error("Unsupported vararg method")))
        src = retrieve_code_info(mi, world)
        isa(src, CodeInfo) ||
            return generate_lambda_ex(world, source, argnames, spnames, :(error("Unexpected code transformation")))
        src = copy(src)
        src.edges === Core.svec() ||
            return generate_lambda_ex(world, source, argnames, spnames, :(error("Unexpected code transformation")))
        src.edges = Any[mi]
        transform!(mi, src, length(args), match.sparams)
        # TODO: this is mandatory: code_info.min_world = max(code_info.min_world, min_world[])
        # TODO: this is mandatory: code_info.max_world = min(code_info.max_world, max_world[])
        # Match the generator, since that's what our transform! does
        src.nargs = 4
        src.isva = true
        errors = CC.validate_code(mi, src)
        if !isempty(errors)
            foreach(Core.println, errors)
            return generate_lambda_ex(world, source, argnames, spnames, :(error("Found errors in generated code")))
        end
        return src
    end

    function generate_lambda_ex(world::UInt, source::Method,
                                argnames::SimpleVector, spnames::SimpleVector,
                                body::Expr)
        stub = Core.GeneratedFunctionStub(identity, argnames, spnames)
        return stub(world, source, body)
    end

    @inline overdub(::Ctx, f::Union{Core.Builtin, Core.IntrinsicFunction}, args...) = f(args...)

    @eval function overdub(ctx::Ctx, f, args...)
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

morethan4args(a, b, c, d, e) = (((a + b) + c) + d) + e
@test overdub(Ctx(), morethan4args, 1, 2, 3, 4, 5) == 15

end # module contextual
