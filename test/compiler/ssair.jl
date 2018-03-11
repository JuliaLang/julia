using Core.IR
const Compiler = Core.Compiler

let code = Any[
    Expr(:gotoifnot, SlotNumber(2), 4),
    LabelNode(2),
    Expr(:(=), SlotNumber(3), 2),
    LabelNode(4),
    # Test a SlotNumber as a value of a PhiNode
    Expr(:(=), SSAValue(0), PhiNode(Any[2,3], Any[1, SlotNumber(3)])),
    Expr(:return, SSAValue(0))
]

    ci = eval(Expr(:new, CodeInfo,
        code,
        nothing,
        Any[Any, Any, Any],
        Any[Any],
        UInt8[0, 0, 0],
        Any[Symbol("Self"), :arg, :slot],
        false, false, false, false
    ))

    Compiler.run_passes(ci, 1, Compiler.LineInfoNode[Compiler.NullLineInfo])
end