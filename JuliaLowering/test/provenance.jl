using JuliaLowering: compress_sbt, uncompress_sbt, SourceByteTable,
    add_debuginfo!

test_mod = Module()

function test_byte_precise(di::Core.DebugInfo, nstmts::Int)
    for i in 1:nstmts
        sl = Base.Compiler.source_location(di, i)
        @test sl.byte > 0      context=di
        @test sl.byte_end > 0  context=di
        @test sl.col > 0       context=di
        @test sl.col_end > 0   context=di
        @test sl.line > 0      context=di
        @test sl.line_end > 0  context=di
    end
end

@testset "SourceByteTable roundtrip" begin
    st = jl_lower(test_mod, JuliaSyntax.parsestmt(SyntaxTree, "1 + 2 - \n 3"))
    JuliaSyntax.ensure_attributes!(st._graph; debuginfo=Any)
    add_debuginfo!(st)
    csbt = st.debuginfo
    usbt = uncompress_sbt(csbt)
    cusbt = compress_sbt(usbt)
    @test csbt isa Core.DebugInfo
    @test usbt isa SourceByteTable
    @test cusbt isa String
    @test cusbt === csbt.linetable

    @test length(usbt.spans) <= numchildren(st[1]) # lhs is unique locs
    test_byte_precise(csbt, numchildren(st[1]))
end

@testset "Attaching DebugInfo to methods" begin
    local f = JuliaLowering.include_string(test_mod, """
    function f_with_debuginfo(x)
        show(x)
        map(a->a+1, x)
    end
    """)
    di = methods(f)[1].debuginfo
    @test di.linetable isa String
    let nstmts = length(Base._uncompressed_ir(methods(f)[1]).code)
        test_byte_precise(di, nstmts)
    end
end
