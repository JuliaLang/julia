# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl

@testset "empty chained sysimage" begin
    binariesavailable = false
    try
        # TODO - consider using LLVM_full_jll to make this test multiplatform
        success(`ar --version`)
        success(`llvm-objcopy --version`)
        success(`ld --version`)
        binariesavailable = true
    catch
    end

    if !binariesavailable
        @test_broken false # Not supported for this OS yet
    else
        sysimg = Base.unsafe_string(Base.JLOptions().image_file)
        cd(mktempdir()) do
            sysoa = replace(sysimg, ".$(Libdl.dlext)" => "-o.a")
            cp("$sysoa", "sys-o.a", force=true)
            @test success(`ar x sys-o.a`)
            rm("data.o")
            mv("text.o", "text-old.o")
            @test success(`llvm-objcopy --remove-section .data.jl.sysimg_link text-old.o`)

            source_txt = """
Base.__init_build(); 
module PrecompileStagingArea;
    using Printf
end;
@ccall jl_precompiles_for_sysimage(1::Cuchar)::Cvoid;
println(0.1, 1, 0x2)
"""

            @test success(`$(Base.julia_cmd()) --sysimage-native-code=chained --startup-file=no --sysimage=$sysimg --output-o chained.o.a -e $source_txt`)
            @test success(`ar x chained.o.a`) # Extract new sysimage files
            @test success(`ld -shared -o chained.$(Libdl.dlext) text.o data.o text-old.o`)

            # Test if "println(0.1, 1, 0x2)" is precompiled
            source_txt2 = """
a = @allocated println(0.1, 1, 0x2);
b = @allocated println(0.1, 1, 0x2);
@assert a == b;
"""

            @test success(`$(Base.julia_cmd()) --sysimage=chained.$(Libdl.dlext) -e $source_txt2`)
        end
    end
end