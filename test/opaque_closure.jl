using Test
using InteractiveUtils

const_int() = 1

const lno = LineNumberNode(1, :none)

let ci = @code_lowered const_int()
    @eval function oc_trivial()
        $(Expr(:new_opaque_closure, Tuple{}, false, Any, Any,
            Expr(:opaque_closure_method, 0, lno, ci)))
    end
end
@test isa(oc_trivial(), Core.OpaqueClosure{Tuple{}, Any})
@test oc_trivial()() == 1

let ci = @code_lowered const_int()
    @eval function oc_simple_inf()
        $(Expr(:new_opaque_closure, Tuple{}, false, Union{}, Any,
            Expr(:opaque_closure_method, 0, lno, ci)))
    end
end
@test_broken isa(oc_simple_inf(), Core.OpaqueClosure{Tuple{}, Int})
@test oc_simple_inf()() == 1

struct OcClos2Int
    a::Int
    b::Int
end
(a::OcClos2Int)() = getfield(a, 1) + getfield(a, 2)
let ci = @code_lowered OcClos2Int(1, 2)();
    @eval function oc_trivial_clos()
        $(Expr(:new_opaque_closure, Tuple{}, false, Int, Int,
            Expr(:opaque_closure_method, 0, lno, ci),
            1, 2))
    end
end
@test oc_trivial_clos()() == 3

let ci = @code_lowered OcClos2Int(1, 2)();
    @eval function oc_self_call_clos()
        $(Expr(:new_opaque_closure, Tuple{}, false, Int, Int,
            Expr(:opaque_closure_method, 0, lno, ci),
            1, 2))()
    end
end
@test oc_self_call_clos() == 3
let opt = @code_typed oc_self_call_clos()
    @test_broken length(opt[1].code) == 1
    @test_broken isa(opt[1].code[1], Core.ReturnNode)
end

struct OcClos1Any
    a
end
(a::OcClos1Any)() = getfield(a, 1)
let ci = @code_lowered OcClos1Any(1)()
    @eval function oc_pass_clos(x)
        $(Expr(:new_opaque_closure, Tuple{}, false, Any, Any,
            Expr(:opaque_closure_method, 0, lno, ci),
            :x))
    end
end
@test oc_pass_clos(1)() == 1
@test oc_pass_clos("a")() == "a"

let ci = @code_lowered OcClos1Any(1)()
    @eval function oc_infer_pass_clos(x)
        $(Expr(:new_opaque_closure, Tuple{}, false, Union{}, Any,
            Expr(:opaque_closure_method, 0, lno, ci),
            :x))
    end
end
@test_broken isa(oc_infer_pass_clos(1), Core.OpaqueClosure{Tuple{}, typeof(1)})
@test_broken isa(oc_infer_pass_clos("a"), Core.OpaqueClosure{Tuple{}, typeof("a")})
@test oc_infer_pass_clos(1)() == 1
@test oc_infer_pass_clos("a")() == "a"

let ci = @code_lowered identity(1)
    @eval function oc_infer_pass_id()
        $(Expr(:new_opaque_closure, Tuple{Any}, false, Any, Any,
            Expr(:opaque_closure_method, 1, lno, ci)))
    end
end
function complicated_identity(x)
    oc_infer_pass_id()(x)
end
@test_broken @inferred(complicated_identity(1)) == 1
@test_broken @inferred(complicated_identity("a")) == "a"
let ci = (@code_typed complicated_identity(1))[1]
    @test_broken length(ci.code) == 1
    @test_broken isa(ci.code[1], Core.ReturnNode)
end

struct OcOpt
    A
end

(A::OcOpt)() = ndims(getfield(A, 1))

let ci = @code_lowered OcOpt([1 2])()
    @eval function oc_opt_ndims(A)
        $(Expr(:new_opaque_closure, Tuple{}, false, Union{}, Any,
            Expr(:opaque_closure_method, 0, lno, ci),
            :A))
    end
end
oc_opt_ndims([1 2])

let A = [1 2]
    let Oc = oc_opt_ndims(A)
        @test_broken sizeof(Oc.env) == 0
        @test Oc() == 2
    end
end

using Base.Experimental: @opaque

@test @opaque(x->2x)(8) == 16
let f = @opaque (x::Int, y::Float64)->(2x, 3y)
    @test_throws TypeError f(1, 1)
    @test f(2, 3.0) === (4, 9.0)
end
function uses_frontend_opaque(x)
    @opaque y->x+y
end
@test uses_frontend_opaque(10)(8) == 18

# World age mechanism
function test_oc_world_age end
mk_oc_world_age() = @opaque ()->test_oc_world_age()
g_world_age = @opaque ()->test_oc_world_age()
h_world_age = mk_oc_world_age()
test_oc_world_age() = 1
@test_throws MethodError g_world_age()
@test_throws MethodError h_world_age()
@test mk_oc_world_age()() == 1
g_world_age = @opaque ()->test_oc_world_age()
@test g_world_age() == 1

# Evil, dynamic Vararg stuff (don't do this - made to work for consistency)
function maybe_opaque(isva::Bool)
    T = isva ? Vararg{Int, 1} : Int
    @opaque (x::T)->x
end
@test maybe_opaque(false)(1) == 1
@test maybe_opaque(true)(1) == (1,)

# Vargarg in complied mode
mk_va_opaque() = @opaque (x...)->x
@test mk_va_opaque()(1) == (1,)
@test mk_va_opaque()(1,2) == (1,2)

# OpaqueClosure show method
@test repr(@opaque x->1) == "(::Any)::Any->◌"

# Opaque closure in CodeInfo returned from generated functions
function mk_ocg(args...)
    ci = @code_lowered const_int()
    cig = Meta.lower(@__MODULE__, Expr(:new_opaque_closure, Tuple{}, false, Any, Any,
        Expr(:opaque_closure_method, 0, lno, ci))).args[1]
    cig.slotnames = Symbol[Symbol("#self#")]
    cig.slottypes = Any[Any]
    cig.slotflags = UInt8[0x00]
    cig
end

@eval function oc_trivial_generated()
    $(Expr(:meta, :generated_only))
    $(Expr(:meta,
            :generated,
            Expr(:new,
                Core.GeneratedFunctionStub,
                :mk_ocg,
                Any[:oc_trivial_generated],
                Any[],
                @__LINE__,
                QuoteNode(Symbol(@__FILE__)),
                true)))
end
@test isa(oc_trivial_generated(), Core.OpaqueClosure{Tuple{}, Any})
@test oc_trivial_generated()() == 1
