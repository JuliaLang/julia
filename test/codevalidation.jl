# This file is a part of Julia. License is MIT: https://julialang.org/license

function f22938(a, b, x...)
    d = 1
    a = d
    for i in 1:b
        d += i
    end
    return i * a
end

msig = Tuple{typeof(f22938),Int,Int,Int}
world = typemax(UInt)
_, msp, m = Base._methods_by_ftype(msig, -1, world)[]
mi = Core.Inference.code_for_method(m, msig, msp, world, false)
c0 = Core.Inference.retrieve_code_info(mi)

@test isempty(Core.Inference.validate_code(mi))
@test isempty(Core.Inference.validate_code(c0))

# INVALID_EXPR_HEAD
c = Core.Inference.copy_code_info(c0)
insert!(c.code, 4, Expr(:(=), SlotNumber(2), Expr(:invalid, 1)))
errors = Core.Inference.validate_code(c)
@test length(errors) == 1
@test errors[1].kind === Core.Inference.INVALID_EXPR_HEAD

# INVALID_LVALUE
c = Core.Inference.copy_code_info(c0)
insert!(c.code, 4, Expr(:(=), LabelNode(1), 1))
insert!(c.code, 2, Expr(:(=), :x, 1))
insert!(c.code, 10, Expr(:(=), 3, 1))
errors = Core.Inference.validate_code(c)
@test length(errors) == 3
@test all(e.kind === Core.Inference.INVALID_LVALUE for e in errors)

# INVALID_RVALUE
c = Core.Inference.copy_code_info(c0)
insert!(c.code, 2, Expr(:(=), SlotNumber(2), GotoNode(1)))
insert!(c.code, 4, Expr(:(=), SlotNumber(2), LabelNode(2)))
insert!(c.code, 10, Expr(:(=), SlotNumber(2), LineNumberNode(2)))
for h in (:gotoifnot, :line, :const, :meta)
    push!(c.code, Expr(:(=), SlotNumber(2), Expr(h)))
end
errors = Core.Inference.validate_code(c)
@test length(errors) == 10
@test count(e.kind === Core.Inference.INVALID_RVALUE for e in errors) == 7
@test count(e.kind === Core.Inference.INVALID_EXPR_NARGS for e in errors) == 3

# INVALID_CALL_ARG/INVALID_EXPR_NARGS
c = Core.Inference.copy_code_info(c0)
insert!(c.code, 2, Expr(:(=), SlotNumber(2), Expr(:call, :+, SlotNumber(2), GotoNode(1))))
insert!(c.code, 4, Expr(:call, :-, Expr(:call, :sin, LabelNode(2)), 3))
insert!(c.code, 10, Expr(:call, LineNumberNode(2)))
for h in (:gotoifnot, :line, :const, :meta)
    push!(c.code, Expr(:call, :f, Expr(h)))
end
errors = Core.Inference.validate_code(c)
@test length(errors) == 10
@test count(e.kind === Core.Inference.INVALID_CALL_ARG for e in errors) == 7
@test count(e.kind === Core.Inference.INVALID_EXPR_NARGS for e in errors) == 3

# EMPTY_SLOTNAMES
c = Core.Inference.copy_code_info(c0)
empty!(c.slotnames)
errors = Core.Inference.validate_code(c)
@test length(errors) == 2
@test any(e.kind === Core.Inference.EMPTY_SLOTNAMES for e in errors)
@test any(e.kind === Core.Inference.SLOTFLAGS_MISMATCH for e in errors)

# SLOTFLAGS_MISMATCH
c = Core.Inference.copy_code_info(c0)
push!(c.slotnames, :dummy)
errors = Core.Inference.validate_code(c)
@test length(errors) == 1
@test errors[1].kind === Core.Inference.SLOTFLAGS_MISMATCH

# SLOTTYPES_MISMATCH
c = @code_typed(f22938(1,2,3,4))[1]
pop!(c.slottypes)
errors = Core.Inference.validate_code(c)
@test length(errors) == 1
@test errors[1].kind === Core.Inference.SLOTTYPES_MISMATCH

# SLOTTYPES_MISMATCH_UNINFERRED
c = Core.Inference.copy_code_info(c0)
c.slottypes = 1
errors = Core.Inference.validate_code(c)
@test length(errors) == 1
@test errors[1].kind === Core.Inference.SLOTTYPES_MISMATCH_UNINFERRED

# SSAVALUETYPES_MISMATCH
c = @code_typed(f22938(1,2,3,4))[1]
empty!(c.ssavaluetypes)
errors = Core.Inference.validate_code(c)
@test length(errors) == 1
@test errors[1].kind === Core.Inference.SSAVALUETYPES_MISMATCH

# SSAVALUETYPES_MISMATCH_UNINFERRED
c = Core.Inference.copy_code_info(c0)
c.ssavaluetypes -= 1
errors = Core.Inference.validate_code(c)
@test length(errors) == 1
@test errors[1].kind === Core.Inference.SSAVALUETYPES_MISMATCH_UNINFERRED

# INVALID_ASSIGNMENT_SLOTFLAG
c = Core.Inference.copy_code_info(c0)
c.slotflags[8] = 0x00
errors = Core.Inference.validate_code(c)
@test length(errors) == 1
@test errors[1].kind === Core.Inference.INVALID_ASSIGNMENT_SLOTFLAG

# SIGNATURE_NARGS_MISMATCH/SIGNATURE_VARARG_MISMATCH
old_sig = mi.def.sig
mi.def.sig = Tuple{1,Vararg{Any}}
errors = Core.Inference.validate_code(mi)
mi.def.sig = old_sig
@test length(errors) == 2
@test count(e.kind === Core.Inference.SIGNATURE_NARGS_MISMATCH for e in errors) == 1
@test count(e.kind === Core.Inference.SIGNATURE_VARARG_MISMATCH for e in errors) == 1

# NON_TOP_LEVEL_METHOD
c = Core.Inference.copy_code_info(c0)
push!(c.code, Expr(:method, :dummy))
errors = Core.Inference.validate_code(c)
@test length(errors) == 1
@test errors[1].kind === Core.Inference.NON_TOP_LEVEL_METHOD

# NARGS_MISMATCH
mi.def.nargs += 20
errors = Core.Inference.validate_code(mi)
mi.def.nargs -= 20
@test length(errors) == 3
@test count(e.kind === Core.Inference.NARGS_MISMATCH for e in errors) == 1
@test count(e.kind === Core.Inference.SIGNATURE_NARGS_MISMATCH for e in errors) == 1
@test count(e.kind === Core.Inference.SIGNATURE_VARARG_MISMATCH for e in errors) == 1
