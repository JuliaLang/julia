using Test
using InteractiveUtils
using Core: OpaqueClosure

const_int() = 1

const lno = LineNumberNode(1, :none)

let ci = @code_lowered const_int()
    @eval function oc_trivial()
        $(Expr(:new_opaque_closure, Tuple{}, Any, Any,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci)))
    end
end
@test isa(oc_trivial(), Core.OpaqueClosure{Tuple{}, Any})
@test oc_trivial()() == 1

let ci = @code_lowered const_int()
    @eval function oc_simple_inf()
        $(Expr(:new_opaque_closure, Tuple{}, Union{}, Any,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci)))
    end
end
@test isa(oc_simple_inf(), Core.OpaqueClosure{Tuple{}, Int})
@test oc_simple_inf()() == 1

struct OcClos2Int
    a::Int
    b::Int
end
(a::OcClos2Int)() = getfield(a, 1) + getfield(a, 2)
let ci = @code_lowered OcClos2Int(1, 2)();
    @eval function oc_trivial_clos()
        $(Expr(:new_opaque_closure, Tuple{}, Int, Int,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci),
            1, 2))
    end
end
@test oc_trivial_clos()() == 3

let ci = @code_lowered OcClos2Int(1, 2)();
    @eval function oc_self_call_clos()
        $(Expr(:new_opaque_closure, Tuple{}, Int, Int,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci),
            1, 2))()
    end
end
@test @inferred(oc_self_call_clos()) == 3
let opt = @code_typed oc_self_call_clos()
    @test length(opt[1].code) == 1
    @test isa(opt[1].code[1], Core.ReturnNode)
end

struct OcClos1Any
    a
end
(a::OcClos1Any)() = getfield(a, 1)
let ci = @code_lowered OcClos1Any(1)()
    @eval function oc_pass_clos(x)
        $(Expr(:new_opaque_closure, Tuple{}, Any, Any,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci),
            :x))
    end
end
@test oc_pass_clos(1)() == 1
@test oc_pass_clos("a")() == "a"

let ci = @code_lowered OcClos1Any(1)()
    @eval function oc_infer_pass_clos(x)
        $(Expr(:new_opaque_closure, Tuple{}, Union{}, Any,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci),
            :x))
    end
end
@test isa(oc_infer_pass_clos(1), Core.OpaqueClosure{Tuple{}, typeof(1)})
@test isa(oc_infer_pass_clos("a"), Core.OpaqueClosure{Tuple{}, typeof("a")})
@test oc_infer_pass_clos(1)() == 1
@test oc_infer_pass_clos("a")() == "a"

let ci = @code_lowered identity(1)
    @eval function oc_infer_pass_id()
        $(Expr(:new_opaque_closure, Tuple{Any}, Any, Any,
            Expr(:opaque_closure_method, nothing, 1, false, lno, ci)))
    end
end
function complicated_identity(x)
    oc_infer_pass_id()(x)
end
@test @inferred(complicated_identity(1)) == 1
@test @inferred(complicated_identity("a")) == "a"
let ci = (@code_typed complicated_identity(1))[1]
    @test length(ci.code) == 1
    @test isa(ci.code[1], Core.ReturnNode)
end

struct OcOpt
    A
end

(A::OcOpt)() = ndims(getfield(A, 1))

let ci = @code_lowered OcOpt([1 2])()
    @eval function oc_opt_ndims(A)
        $(Expr(:new_opaque_closure, Tuple{}, Union{}, Any,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci),
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
@test isa(h_world_age, Core.OpaqueClosure{Tuple{}, Union{}})
test_oc_world_age() = 1
@test_throws MethodError g_world_age()
@test_throws MethodError h_world_age()
@test mk_oc_world_age()() == 1
g_world_age = @opaque ()->test_oc_world_age()
@test g_world_age() == 1
@test isa(mk_oc_world_age(), Core.OpaqueClosure{Tuple{}, Int})

function maybe_vararg(isva::Bool)
    T = isva ? Vararg{Int} : Int
    @opaque Tuple{T} (x...)->x
end
@test maybe_vararg(false)(1) == (1,)
@test_throws MethodError maybe_vararg(false)(1,2,3)
@test maybe_vararg(true)(1) == (1,)
@test maybe_vararg(true)(1,2,3) == (1,2,3)
@test (@opaque Tuple{Int, Int} (a, b, x...)->x)(1,2) === ()
@test (@opaque Tuple{Int, Int} (a, x...)->x)(1,2) === (2,)
@test (@opaque Tuple{Int, Vararg{Int}} (a, x...)->x)(1,2,3,4) === (2,3,4)
@test (@opaque (a::Int, x::Int...)->x)(1,2,3) === (2,3)

@test_throws ErrorException (@opaque Tuple{Vararg{Int}} x->x)
@test_throws ErrorException (@opaque Tuple{Int, Vararg{Int}} x->x)
@test_throws ErrorException (@opaque Tuple{Int, Int} x->x)
@test_throws ErrorException (@opaque Tuple{Any} (x,y)->x)
@test_throws ErrorException (@opaque Tuple{Vararg{Int}} (x,y...)->x)
@test_throws ErrorException (@opaque Tuple{Int} (x,y,z...)->x)

# cannot specify types both on arguments and separately
@test_throws ErrorException @eval @opaque Tuple{Any} (x::Int)->x

# Vargarg in complied mode
mk_va_opaque() = @opaque (x...)->x
@test mk_va_opaque()(1) == (1,)
@test mk_va_opaque()(1,2) == (1,2)

# OpaqueClosure show method
@test repr(@opaque x->1) == "(::Any)::Any->◌"

# Opaque closure in CodeInfo returned from generated functions
function mk_ocg(args...)
    ci = @code_lowered const_int()
    cig = Meta.lower(@__MODULE__, Expr(:new_opaque_closure, Tuple{}, Any, Any,
        Expr(:opaque_closure_method, nothing, 0, false, lno, ci))).args[1]
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

# Constprop through varargs OpaqueClosure
function oc_varargs_constprop()
    oc = @opaque (args...)->args[1]+args[2]+args[3]
    return Val{oc(1,2,3)}()
end
@test Base.return_types(oc_varargs_constprop, Tuple{}) == Any[Val{6}]

# OpaqueClosure ABI
f_oc_noinline(x) = @opaque function (y)
    @noinline
    x + y
end

let oc = Base.inferencebarrier(f_oc_noinline(1))
    @test oc(2) == 3
end

function f_oc_noinline_call(x, y)
    return f_oc_noinline(x)(y)
end
@test f_oc_noinline_call(1, 2) == 3

@test_throws MethodError (@opaque x->x+1)(1, 2)

# https://github.com/JuliaLang/julia/issues/40409
const GLOBAL_OPAQUE_CLOSURE = @opaque () -> 123
call_global_opaque_closure() = GLOBAL_OPAQUE_CLOSURE()
@test call_global_opaque_closure() == 123

let foo::Int = 42
    Base.Experimental.@force_compile
    oc = Base.Experimental.@opaque a::Int->sin(a) + cos(foo)

    @test only(Base.return_types(oc, (Int,))) === Float64
    code, rt = first(code_typed(oc, (Int,)))
    @test rt === Float64
end

let oc = @opaque a->sin(a)
    @test length(code_typed(oc, (Int,))) == 1
end

# constructing an opaque closure from IRCode
let ci = code_typed(+, (Int, Int))[1][1]
    ir = Core.Compiler.inflate_ir(ci)
    @test OpaqueClosure(ir; nargs=2, isva=false)(40, 2) == 42
    @test OpaqueClosure(ci)(40, 2) == 42

    ir = Core.Compiler.inflate_ir(ci, Any[], Any[Tuple{}, Int, Int])
    @test OpaqueClosure(ir; nargs=2, isva=false)(40, 2) == 42
    @test isa(OpaqueClosure(ir; nargs=2, isva=false), Core.OpaqueClosure{Tuple{Int, Int}, Int})
    @test_throws TypeError OpaqueClosure(ir; nargs=2, isva=false)(40.0, 2)
end

let ci = code_typed((x, y...)->(x, y), (Int, Int))[1][1]
    ir = Core.Compiler.inflate_ir(ci)
    @test OpaqueClosure(ir; nargs=2, isva=true)(40, 2) === (40, (2,))
    @test OpaqueClosure(ci)(40, 2) === (40, (2,))
end

let ci = code_typed((x, y...)->(x, y), (Int, Int))[1][1]
    ir = Core.Compiler.inflate_ir(ci)
    @test_throws MethodError OpaqueClosure(ir; nargs=2, isva=true)(1, 2, 3)
    @test_throws MethodError OpaqueClosure(ci)(1, 2, 3)
end
