# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl

try
    # TODO - This is not perfect because the test depends on an external package
    using LLVM_full_jll
    using LLD_jll
    using PackageCompiler
catch
    using Pkg
    Pkg.add("LLVM_full_jll")
    using LLVM_full_jll
    Pkg.add("LLD_jll")
    using LLD_jll
    Pkg.add("PackageCompiler")
    using PackageCompiler
end

@testset "empty chained sysimage" begin

    is_debug() = ccall(:jl_is_debugbuild, Cint, ()) == 1

    function julia_libdir()
        libname = is_debug() ? "libjulia-debug" : "libjulia"
        return dirname(abspath(Libdl.dlpath(libname)))
    end

    function julia_private_libdir()
        libname = is_debug() ? "libjulia-internal-debug" : "libjulia-internal"
        return dirname(abspath(Libdl.dlpath(libname)))
    end

    function get_llvm_cmd(exe) # Not all binaries are exported from LLVM_full_jll
        a = LLVM_full_jll.clang()
        name_replace(path) = joinpath(dirname(path), replace(basename(path), "clang" => exe))
        return Cmd(Cmd(name_replace.(a.exec)); env = a.env)
    end

    ar = get_llvm_cmd("llvm-ar")
    objcopy = get_llvm_cmd("llvm-objcopy")
    ld = LLD_jll.lld()

    binariesavailable = false
    try
        @assert success(`$ar --version`)
        @assert success(`$objcopy --version`)
        @assert success(`$ld -flavor gnu --version`)
        binariesavailable = true
    catch
    end

    if !binariesavailable
        @test_broken false # This test is not yet supported by this OS
    else
        sysimage_path = Base.unsafe_string(Base.JLOptions().image_file)
        object_file = replace(sysimage_path, ".$(Libdl.dlext)" => "-o.a")
        # Speed up local testing (reuse the original sysimage)
        # TODO - remove this before merging
        if true || !isfile(object_file)
            # Compile julia sysimage because `sys-o.a` file is not distributed
            object_file = joinpath(tempdir(), "chainedsysimage-test-sys.o")
            # Use the default values from PackageCompiler for incremental sysimage build (without re-using native code
            isfile(object_file) || PackageCompiler.create_sysimg_object_file(object_file, String[], Set{Base.PkgId}();
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
            LIBS = is_debug() ? `-ljulia-debug -ljulia-internal-debug` : `-ljulia -ljulia-internal`
            OBJECT = `--whole-archive $object_file --no-whole-archive`
            run(`$ld -flavor gnu --shared --output $sysimage_path $OBJECT -L$(julia_libdir()) -L$(julia_private_libdir()) $LIBS`)
        end
        dir = mktempdir()
        dir = mkpath("chained")
        cd(dir) do
            cp("$object_file", "sys-o.a", force=true)
            run(`$ar x sys-o.a`)
            rm("data.o")
            mv("text.o", "text-old.o", force = true)
            run(`$objcopy --remove-section .data.jl.sysimg_link text-old.o`)

            source_txt = """
Base.__init_build();
module PrecompileStagingArea;
    using Printf;
    @ccall jl_precompiles_for_sysimage(1::Cuchar)::Cvoid;
    println(0.1, 1, 0x2);
end;
"""

            run(`$(Base.julia_cmd()) --sysimage-native-code=chained --startup-file=no --sysimage=$sysimage_path --output-o chained.o.a -e $source_txt`)
            run(`$ar x chained.o.a`) # Extract new sysimage files
            #@show `$clang -shared -o chained.$(Libdl.dlext) text.o data.o text-old.o`
            run(`$ld -flavor gnu --shared -output chained.$(Libdl.dlext) text.o data.o text-old.o`)

            # Test if "println(0.1, 1, 0x2)" is precompiled
            source_txt2 = """
a = @allocated println(0.1, 1, 0x2);
b = @allocated println(0.1, 1, 0x2);
@show a, b
@assert a + 1000 > b;
"""

            @test run(`$(Base.julia_cmd()) --sysimage=chained.$(Libdl.dlext) -e $source_txt2`).exitcode == 0
        end
    end
end
