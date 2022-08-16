# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl

try
    # TODO - This is not perfect because the test depends on an external package
    using LLVM_full_jll
    using PackageCompiler
catch
    using Pkg
    Pkg.add("LLVM_full_jll")
    using LLVM_full_jll
    Pkg.add("PackageCompiler")
    using PackageCompiler
end

@testset "empty chained sysimage" begin
    function get_llvm_cmd(exe) # Not all binaries are exported from LLVM_full_jll
        a = LLVM_full_jll.clang()
        name_replace(path) = joinpath(dirname(path), replace(basename(path), "clang" => exe))
        return Cmd(Cmd(name_replace.(a.exec)); env = a.env)
    end

    ar = get_llvm_cmd("llvm-ar")
    objcopy = get_llvm_cmd("llvm-objcopy")
    clang = LLVM_full_jll.clang()

    binariesavailable = false
    try
        @assert success(`$ar --version`)
        @assert success(`$objcopy --version`)
        @assert success(`$clang --version`)
        binariesavailable = true
    catch
    end

    if !binariesavailable
        @test_broken false # This test is not yet supported by this OS
    else
        sysimage_path = Base.unsafe_string(Base.JLOptions().image_file)
        object_file = replace(sysimage_path, ".$(Libdl.dlext)" => "-o.a")
        if !isfile(object_file)
            # Compile julia sysimage because `sys-o.a` file is not distributed
            object_file = tempname() * ".o"
            # Use the default values from PackageCompiler for incremental sysimage build (without re-using native code
            PackageCompiler.create_sysimg_object_file(object_file, String[], Set{Base.PkgId}();
                    project=dirname(Base.active_project()),
                    base_sysimage=sysimage_path,
                    precompile_execution_file=String[],
                    precompile_statements_file=String[],
                    cpu_target="native",
                    script=nothing,
                    sysimage_build_args=``,
                    extra_precompiles="",
                    incremental=true)
            # Use the newly compiled sysimage
            sysimage_path = tempname() * ".$(Libdl.dlext)"
            PackageCompiler.create_sysimg_from_object_file([object_file], sysimage_path;
                    compat_level="major",
                    version=nothing,
                    soname=nothing)
        end
        dir = mktempdir()
        dir = mkpath("chained")
        cd(dir) do
            cp("$object_file", "sys-o.a", force=true)
            @test success(`ar x sys-o.a`)
            rm("data.o")
            mv("text.o", "text-old.o", force = true)
            @test success(`$objcopy --remove-section .data.jl.sysimg_link text-old.o`)

            source_txt = """
Base.__init_build();
module PrecompileStagingArea;
    using Printf
end;
@ccall jl_precompiles_for_sysimage(1::Cuchar)::Cvoid;
println(0.1, 1, 0x2)
"""

            @test success(`$(Base.julia_cmd()) --sysimage-native-code=chained --startup-file=no --sysimage=$sysimage_path --output-o chained.o.a -e $source_txt`)
            @test success(`$ar x chained.o.a`) # Extract new sysimage files
            #@show `$clang -shared -o chained.$(Libdl.dlext) text.o data.o text-old.o`
            @test success(`$clang -shared -o chained.$(Libdl.dlext) text.o data.o text-old.o`)

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
