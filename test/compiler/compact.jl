using Core.Compiler: IncrementalCompact, insert_node_here!, finish,
    NewInstruction, verify_ir, ReturnNode, SSAValue

foo_test_function(i) = i == 1 ? 1 : 2

@testset "IncrementalCompact statefulness" begin
    ir = only(Base.code_ircode(foo_test_function, (Int,)))[1]
    compact = IncrementalCompact(ir)

    # set up first iterator
    x = Core.Compiler.iterate(compact)
    x = Core.Compiler.iterate(compact, x[2])

    # set up second iterator
    x = Core.Compiler.iterate(compact)

    # consume remainder
    while x !== nothing
        x = Core.Compiler.iterate(compact, x[2])
    end

    ir = finish(compact)
    @test Core.Compiler.verify_ir(ir) === nothing
end

# Test early finish of IncrementalCompact
@testset "IncrementalCompact early finish" begin
    ir = only(Base.code_ircode(foo_test_function, (Int,)))[1]
    compact = IncrementalCompact(ir)

    insert_node_here!(compact, NewInstruction(ReturnNode(1), Union{}, ir[SSAValue(1)][:line]))
    new_ir = finish(compact)
    # TODO: Should IncrementalCompact be doing this internally?
    empty!(new_ir.cfg.blocks[1].succs)
    verify_ir(new_ir)
    @test length(new_ir.cfg.blocks) == 1
end

# Test reverse affinity insert at start of compact
@testset "IncrementalCompact reverse affinity insert" begin
    ir = only(Base.code_ircode(foo_test_function, (Int,)))[1]
    compact = IncrementalCompact(ir)
    @test !Core.Compiler.did_just_finish_bb(compact)

    insert_node_here!(compact, NewInstruction(ReturnNode(1), Union{}, ir[SSAValue(1)][:line]), true)
    new_ir = finish(compact)
    # TODO: Should IncrementalCompact be doing this internally?
    empty!(new_ir.cfg.blocks[1].succs)
    verify_ir(new_ir)
    @test length(new_ir.cfg.blocks) == 1
end
