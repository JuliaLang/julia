using Test
using InteractiveUtils
using Core: OpaqueClosure
using Base.Experimental: @opaque

const_int() = 1
const_int_barrier() = Base.inferencebarrier(1)::typeof(1)

const lno = LineNumberNode(1, :none)

let ci = @code_lowered const_int()
    @eval function oc_trivial()
        $(Expr(:new_opaque_closure, Tuple{}, Any, Any, true,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci)))
    end
end
@test isa(oc_trivial(), OpaqueClosure{Tuple{}, Any})
@test oc_trivial()() == 1

let ci = @code_lowered const_int()
    @eval function oc_simple_inf()
        $(Expr(:new_opaque_closure, Tuple{}, Union{}, Any, true,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci)))
    end
end
@test isa(oc_simple_inf(), OpaqueClosure{Tuple{}, Int})
@test oc_simple_inf()() == 1

struct OcClos2Int
    a::Int
    b::Int
end
(a::OcClos2Int)() = getfield(a, 1) + getfield(a, 2)
let ci = @code_lowered OcClos2Int(1, 2)();
    @eval function oc_trivial_clos()
        $(Expr(:new_opaque_closure, Tuple{}, Int, Int, true,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci),
            1, 2))
    end
end
@test oc_trivial_clos()() == 3

let ci = @code_lowered OcClos2Int(1, 2)();
    @eval function oc_self_call_clos()
        $(Expr(:new_opaque_closure, Tuple{}, Int, Int, true,
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
        $(Expr(:new_opaque_closure, Tuple{}, Any, Any, true,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci),
            :x))
    end
end
@test oc_pass_clos(1)() == 1
@test oc_pass_clos("a")() == "a"

let ci = @code_lowered OcClos1Any(1)()
    @eval function oc_infer_pass_clos(x)
        $(Expr(:new_opaque_closure, Tuple{}, Union{}, Any, true,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci),
            :x))
    end
end
@test isa(oc_infer_pass_clos(1), OpaqueClosure{Tuple{}, typeof(1)})
@test isa(oc_infer_pass_clos("a"), OpaqueClosure{Tuple{}, typeof("a")})
@test oc_infer_pass_clos(1)() == 1
@test oc_infer_pass_clos("a")() == "a"

let ci = @code_lowered identity(1)
    @eval function oc_infer_pass_id()
        $(Expr(:new_opaque_closure, Tuple{Any}, Any, Any, true,
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
        $(Expr(:new_opaque_closure, Tuple{}, Union{}, Any, true,
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
module test_world_age

using Test
using Core: OpaqueClosure
using Base.Experimental: @opaque

function test_oc_world_age end
mk_oc_world_age() = @opaque ()->test_oc_world_age()
g_world_age = @opaque ()->test_oc_world_age()
h_world_age = mk_oc_world_age()
@test isa(h_world_age, OpaqueClosure{Tuple{}, Union{}})
test_oc_world_age() = 1
@test_throws MethodError g_world_age()
@test_throws MethodError h_world_age()
@test mk_oc_world_age()() == 1
g_world_age = @opaque ()->test_oc_world_age()
@test g_world_age() == 1
@test isa(mk_oc_world_age(), OpaqueClosure{Tuple{}, Int})

end # module test_world_age

function maybe_vararg(isva::Bool)
    T = isva ? Vararg{Int} : Int
    @opaque Tuple{T}->_ (x...)->x
end
@test maybe_vararg(false)(1) == (1,)
@test_throws MethodError maybe_vararg(false)(1,2,3)
@test maybe_vararg(true)(1) == (1,)
@test maybe_vararg(true)(1,2,3) == (1,2,3)
@test (@opaque Tuple{Int, Int}->_ (a, b, x...)->x)(1,2) === ()
@test (@opaque Tuple{Int, Int}->Tuple{} (a, b, x...)->x)(1,2) === ()
@test (@opaque _->Tuple{Vararg{Int}} (a, b, x...)->x)(1,2) === ()
@test (@opaque Tuple{Int, Int}->_ (a, x...)->x)(1,2) === (2,)
@test (@opaque Tuple{Int, Int}->Tuple{Int} (a, x...)->x)(1,2) === (2,)
@test (@opaque _->Tuple{Vararg{Int}} (a, x...)->x)(1,2) === (2,)
@test (@opaque Tuple{Int, Vararg{Int}}->_ (a, x...)->x)(1,2,3,4) === (2,3,4)
@test (@opaque Tuple{Int, Vararg{Int}}->Tuple{Vararg{Int}} (a, x...)->x)(1,2,3,4) === (2,3,4)
@test (@opaque (a::Int, x::Int...)->x)(1,2,3) === (2,3)
@test (@opaque _->Tuple{Vararg{Int}} (a::Int, x::Int...)->x)(1,2,3) === (2,3)
@test (@opaque _->_ (a::Int, x::Int...)->x)(1,2,3) === (2,3)

@test_throws ErrorException (@opaque Tuple{Vararg{Int}}->_ x->x)
@test_throws ErrorException (@opaque Tuple{Int, Vararg{Int}}->_ x->x)
@test_throws ErrorException (@opaque Tuple{Int, Int}->_ x->x)
@test_throws ErrorException (@opaque Tuple{Any}->_ (x,y)->x)
@test_throws ErrorException (@opaque Tuple{Vararg{Int}}->_ (x,y...)->x)
@test_throws ErrorException (@opaque Tuple{Int}->_ (x,y,z...)->x)

# cannot specify types both on arguments and separately
@test_throws ErrorException @eval @opaque Tuple{Any}->_ (x::Int)->x

# Vargarg in complied mode
mk_va_opaque() = @opaque (x...)->x
@test mk_va_opaque()(1) == (1,)
@test mk_va_opaque()(1,2) == (1,2)

# OpaqueClosure show method
@test repr(@opaque x->Base.inferencebarrier(1)) == "(::Any)->◌::Any"

# Opaque closure in CodeInfo returned from generated functions
let ci = @code_lowered const_int()
    global function mk_ocg(world::UInt, source, args...)
        @nospecialize
        cig = Meta.lower(@__MODULE__, Expr(:new_opaque_closure, Tuple{}, Any, Any, true,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci))).args[1]
        cig.slotnames = Symbol[Symbol("#self#")]
        cig.slottypes = Any[Any]
        cig.slotflags = UInt8[0x00]
        cig.nargs = 1
        cig.isva = false
        return cig
    end
end

@eval function oc_trivial_generated()
    $(Expr(:meta, :generated_only))
    $(Expr(:meta, :generated, mk_ocg))
end
@test isa(oc_trivial_generated(), OpaqueClosure{Tuple{}, Any})
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
    let opt = code_typed(oc, (Int,))
        @test length(opt) == 1
        @test opt[1][2] === Float64
    end
    let unopt = code_typed(oc, (Int,); optimize=false)
        @test length(unopt) == 1
    end
end

# constructing an opaque closure from IRCode
let src = first(only(code_typed(+, (Int, Int))))
    ir = Core.Compiler.inflate_ir(src, Core.Compiler.VarState[], src.slottypes)
    ir.argtypes[1] = Tuple{}
    @test ir.debuginfo.def === nothing
    ir.debuginfo.def = Symbol(@__FILE__)
    @test OpaqueClosure(src; sig=Tuple{Int, Int}, rettype=Int, nargs=2)(40, 2) == 42
    oc = OpaqueClosure(ir)
    @test oc(40, 2) == 42
    @test isa(oc, OpaqueClosure{Tuple{Int,Int}, Int})
    @test_throws TypeError oc("40", 2)
    @test OpaqueClosure(ir)(40, 2) == 42 # the `OpaqueClosure(::IRCode)` constructor should be non-destructive
end
let ir = first(only(Base.code_ircode(sin, (Int,))))
    ir.argtypes[1] = Tuple{}
    @test OpaqueClosure(ir)(42) == sin(42)
    @test OpaqueClosure(ir)(42) == sin(42) # the `OpaqueClosure(::IRCode)` constructor should be non-destructive
    @test length(code_typed(OpaqueClosure(ir))) == 1
    ir = first(only(Base.code_ircode(sin, (Float64,))))
    ir.argtypes[1] = Tuple{}
    @test OpaqueClosure(ir)(42.) == sin(42.)
    @test OpaqueClosure(ir)(42.) == sin(42.) # the `OpaqueClosure(::IRCode)` constructor should be non-destructive
end

# variadic arguments
let src = code_typed((Int,Int)) do x, y...
        return (x, y)
    end |> only |> first
    src.slottypes[1] = Tuple{}
    let oc = OpaqueClosure(src; rettype=Tuple{Int, Tuple{Int}}, sig=Tuple{Int, Int}, nargs=2, isva=true)
        @test oc(1,2) === (1,(2,))
        @test_throws MethodError oc(1,2,3)
    end
    ir = Core.Compiler.inflate_ir(src, Core.Compiler.VarState[], src.slottypes)
    @test ir.debuginfo.def === nothing
    ir.debuginfo.def = Symbol(@__FILE__)
    let oc = OpaqueClosure(ir; isva=true)
        @test oc(1,2) === (1,(2,))
        @test_throws MethodError oc(1,2,3)
    end
end

# Check for correct handling in case of broken return type.
eval_oc_dyn(oc) = Base.inferencebarrier(oc)()
eval_oc_spec(oc) = oc()
for f in (const_int, const_int_barrier)
    ci = code_lowered(f, Tuple{})[1]
    for compiled in (true, false)
        oc_expr = Expr(:new_opaque_closure, Tuple{}, Union{}, Float64, true,
            Expr(:opaque_closure_method, nothing, 0, false, lno, ci))
        oc_mismatch = let ci = code_lowered(f, Tuple{})[1]
            if compiled
                eval(:((()->$oc_expr)()))
            else
                eval(oc_expr)
            end
        end
        @test isa(oc_mismatch, OpaqueClosure{Tuple{}, Union{}})
        @test_throws TypeError eval_oc_dyn(oc_mismatch)
        @test_throws TypeError eval_oc_spec(oc_mismatch)
    end
end


# Attempting to construct an opaque closure backtrace after the oc is GC'ed
f_oc_throws() = error("oops")
@noinline function make_oc_and_collect_bt()
    did_gc = Ref{Bool}(false)
    bt = let ir = first(only(Base.code_ircode(f_oc_throws, ())))
        ir.argtypes[1] = Tuple
        sentinel = Ref{Any}(nothing)
        oc = OpaqueClosure(ir, sentinel)
        finalizer(sentinel) do x
            did_gc[] = true
        end
        try
            oc()
            @test false
        catch e
            bt = catch_backtrace()
            @test isa(e, ErrorException)
            bt
        end
    end
    return bt, did_gc
end
let (bt, did_gc) = make_oc_and_collect_bt()
    GC.gc(true); GC.gc(true); GC.gc(true);
    @test did_gc[]
    @test any(stacktrace(bt)) do frame
        li = frame.linfo
        isa(li, Core.CodeInstance) && (li = li.def)
        isa(li, Core.ABIOverride) && (li = li.def)
        isa(li, Core.MethodInstance) || return false
        isa(li.def, Method) || return false
        return li.def.is_for_opaque_closure
    end
end

# Opaque closure with mismatch struct argtype
const op_arg_restrict2 = @opaque (x::Tuple{Int64}, y::Base.RefValue{Int64})->x+y
ccall_op_arg_restrict2_bad_args() = op_arg_restrict2((1.,), 2)

@test_throws TypeError ccall_op_arg_restrict2_bad_args()

# code_llvm for opaque closures
let ir = Base.code_ircode((Int,Int)) do x, y
        @noinline x * y
    end |> only |> first
    ir.argtypes[1] = Tuple{}
    oc = Core.OpaqueClosure(ir)
    io = IOBuffer()
    code_llvm(io, oc, Tuple{Int,Int})
    @test occursin("j_*_", String(take!(io)))
    code_llvm(io, oc, (Int,Int))
    @test occursin("j_*_", String(take!(io)))
end

foopaque() = Base.Experimental.@opaque(@noinline x::Int->println(x))(1)

code_llvm(devnull,foopaque,()) #shouldn't crash

let ir = first(only(Base.code_ircode(sin, (Int,))))
    ir.argtypes[1] = Tuple{}
    oc = Core.OpaqueClosure(ir)
    @test (Base.show_method(IOBuffer(), oc.source::Method); true)
end

let ir = first(only(Base.code_ircode(sin, (Int,))))
    ir.argtypes[1] = Tuple{}
    oc = Core.OpaqueClosure(ir; do_compile=false)
    @test oc(1) == sin(1)
end

function typed_add54236(::Type{T}) where T
    return @opaque (x::Int)->T(x) + T(1)
end
let f = typed_add54236(Float64)
    @test f isa Core.OpaqueClosure
    @test f(32) === 33.0
end

f54357(g, ::Type{AT}) where {AT} = Base.Experimental.@opaque AT->_ (args...) -> g((args::AT)...)
let f = f54357(+, Tuple{Int,Int})
    @test f isa Core.OpaqueClosure
    @test f(32, 34) === 66
    g = f54357(+, Tuple{Float64,Float64})
    @test g isa Core.OpaqueClosure
    @test g(32.0, 34.0) === 66.0
end
