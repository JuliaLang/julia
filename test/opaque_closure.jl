using Test
using InteractiveUtils

const_int() = 1

let ci = @code_lowered const_int()
    @eval function oc_trivial()
        $(Expr(:call, Core._opaque_closure, Tuple{}, Any, Any, ci))
    end
end
@test isa(oc_trivial(), Core.OpaqueClosure{Tuple{}, Any})
@test oc_trivial()() == 1

let ci = @code_lowered const_int()
    @eval function oc_simple_inf()
        $(Expr(:call, Core._opaque_closure, Tuple{}, Union{}, Any, ci))
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
        $(Expr(:call, Core._opaque_closure, Tuple{}, Int, Int, ci, 1, 2))
    end
end
@test oc_trivial_clos()() == 3

let ci = @code_lowered OcClos2Int(1, 2)();
    @eval function oc_self_call_clos()
        $(Expr(:call, Core._opaque_closure, Tuple{}, Int, Int, ci, 1, 2))()
    end
end
@test oc_self_call_clos() == 3
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
        $(Expr(:call, Core._opaque_closure, Tuple{}, Any, Any, ci, :x))
    end
end
@test oc_pass_clos(1)() == 1
@test oc_pass_clos("a")() == "a"

let ci = @code_lowered OcClos1Any(1)()
    @eval function oc_infer_pass_clos(x)
        $(Expr(:call, Core._opaque_closure, Tuple{}, Union{}, Any, ci, :x))
    end
end
@test isa(oc_infer_pass_clos(1), Core.OpaqueClosure{Tuple{}, typeof(1)})
@test isa(oc_infer_pass_clos("a"), Core.OpaqueClosure{Tuple{}, typeof("a")})
@test oc_infer_pass_clos(1)() == 1
@test oc_infer_pass_clos("a")() == "a"

let ci = @code_lowered identity(1)
    @eval function oc_infer_pass_id()
        $(Expr(:call, Core._opaque_closure, Tuple{Any}, Any, Any, ci))
    end
end
function complicated_identity(x)
    oc_infer_pass_id()(x)
end
@test @inferred(complicated_identity(1)) == 1
@test @inferred(complicated_identity("a")) == "a"

struct OcOpt
    A
end

(A::OcOpt)() = ndims(getfield(A, 1))

let ci = @code_lowered OcOpt([1 2])()
    @eval function oc_opt_ndims(A)
        $(Expr(:call, Core._opaque_closure, Tuple{}, Union{}, Any,  ci, :A))
    end
end
oc_opt_ndims([1 2])

let A = [1 2]
    let Oc = oc_opt_ndims(A)
        @test sizeof(Oc.env) == 0
        @test Oc() == 2
    end
end
