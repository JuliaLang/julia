# This file is a part of Julia. License is MIT: https://julialang.org/license

catcmd = `cat`
if Sys.iswindows()
    busybox = joinpath(JULIA_HOME, "busybox.exe")
    havebb = try # use busybox-w32 on windows
        success(`$busybox`)
        true
    catch
        false
    end
    if havebb
        catcmd = `$busybox cat`
    end
end

let exename = `$(Base.julia_cmd()) --sysimage-native-code=yes --startup-file=no`
    # --version
    let v = split(read(`$exename -v`, String), "julia version ")[end]
        @test Base.VERSION_STRING == chomp(v)
    end
    @test read(`$exename -v`, String) == read(`$exename --version`, String)

    # --help
    let header = "julia [switches] -- [programfile] [args...]"
        @test startswith(read(`$exename -h`, String), header)
        @test startswith(read(`$exename --help`, String), header)
    end

    # --quiet, --banner
    let t(q,b) = "Base.JLOptions().quiet == $q && Base.JLOptions().banner == $b"
        @test success(`$exename                 -e $(t(0, -1))`)
        @test success(`$exename -q              -e $(t(1,  0))`)
        @test success(`$exename --quiet         -e $(t(1,  0))`)
        @test success(`$exename --banner=no     -e $(t(0,  0))`)
        @test success(`$exename --banner=yes    -e $(t(0,  1))`)
        @test success(`$exename -q --banner=no  -e $(t(1,  0))`)
        @test success(`$exename -q --banner=yes -e $(t(1,  1))`)
        @test success(`$exename --banner=no  -q -e $(t(1,  0))`)
        @test success(`$exename --banner=yes -q -e $(t(1,  1))`)
    end

    # --home
    @test success(`$exename -H $JULIA_HOME`)
    @test success(`$exename --home=$JULIA_HOME`)

    # --eval
    @test  success(`$exename -e "exit(0)"`)
    @test !success(`$exename -e "exit(1)"`)
    @test  success(`$exename --eval="exit(0)"`)
    @test !success(`$exename --eval="exit(1)"`)
    @test !success(`$exename -e`)
    @test !success(`$exename --eval`)
    # --eval --interactive (replaced --post-boot)
    @test  success(`$exename -i -e "exit(0)"`)
    @test !success(`$exename -i -e "exit(1)"`)

    # --print
    @test read(`$exename -E "1+1"`, String) == "2\n"
    @test read(`$exename --print="1+1"`, String) == "2\n"
    @test !success(`$exename -E`)
    @test !success(`$exename --print`)

    # --load
    let testfile = tempname()
        try
            write(testfile, "testvar = :test\nprintln(\"loaded\")\n")
            @test read(`$exename -i --load=$testfile -e "println(testvar)"`, String) == "loaded\ntest\n"
            @test read(`$exename -i -L $testfile -e "println(testvar)"`, String) == "loaded\ntest\n"
            # multiple, combined
            @test read(```$exename
                -e 'push!(ARGS, "hi")'
                -E "1+1"
                -E "2+2"
                -L $testfile
                -E '3+3'
                -L $testfile
                -E 'pop!(ARGS)'
                -e 'show(ARGS); println()'
                9 10
                ```, String) == """
                2
                4
                loaded
                6
                loaded
                "hi"
                ["9", "10"]
                """
        finally
            rm(testfile)
        end
    end
    # -L, --load requires an argument
    @test !success(`$exename -L`)
    @test !success(`$exename --load`)

    # --cpu-target
    # NOTE: this test only holds true if image_file is a shared library.
    if Libdl.dlopen_e(unsafe_string(Base.JLOptions().image_file)) != C_NULL
        @test !success(`$exename -C invalidtarget --sysimage-native-code=yes`)
        @test !success(`$exename --cpu-target=invalidtarget --sysimage-native-code=yes`)
    else
        warn("--cpu-target test not runnable")
    end

    # --procs
    @test readchomp(`$exename -q -p 2 -e "println(nworkers())"`) == "2"
    @test !success(`$exename -p 0`)
    @test !success(`$exename --procs=1.0`)

    # --machinefile
    # this does not check that machinefile works,
    # only that the filename gets correctly passed to the option struct
    let fname = tempname()
        touch(fname)
        fname = realpath(fname)
        try
            @test readchomp(`$exename --machinefile $fname -e
                "println(unsafe_string(Base.JLOptions().machinefile))"`) == fname
        finally
            rm(fname)
        end
    end

    # -i, isinteractive
    @test readchomp(`$exename -E "isinteractive()"`) == "false"
    @test readchomp(`$exename -E "isinteractive()" -i`) == "true"

    # --color
    @test readchomp(`$exename --color=yes -E "Base.have_color"`) == "true"
    @test readchomp(`$exename --color=no -E "Base.have_color"`) == "false"
    @test !success(`$exename --color=false`)

    # --history-file
    @test readchomp(`$exename -E "Bool(Base.JLOptions().historyfile)"
        --history-file=yes`) == "true"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().historyfile)"
        --history-file=no`) == "false"
    @test !success(`$exename --history-file=false`)

    # --code-coverage
    @test readchomp(`$exename -E "Bool(Base.JLOptions().code_coverage)"`) == "false"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().code_coverage)"
        --code-coverage=none`) == "false"

    @test readchomp(`$exename -E "Bool(Base.JLOptions().code_coverage)"
        --code-coverage`) == "true"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().code_coverage)"
        --code-coverage=user`) == "true"

    # --track-allocation
    @test readchomp(`$exename -E "Bool(Base.JLOptions().malloc_log)"`) == "false"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().malloc_log)"
        --track-allocation=none`) == "false"

    @test readchomp(`$exename -E "Bool(Base.JLOptions().malloc_log)"
        --track-allocation`) == "true"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().malloc_log)"
        --track-allocation=user`) == "true"

    # --optimize
    @test readchomp(`$exename -E "Base.JLOptions().opt_level"`) == "2"
    @test readchomp(`$exename -E "Base.JLOptions().opt_level" -O`) == "3"
    @test readchomp(`$exename -E "Base.JLOptions().opt_level" --optimize`) == "3"
    @test readchomp(`$exename -E "Base.JLOptions().opt_level" -O0`) == "0"

    # -g
    @test readchomp(`$exename -E "Base.JLOptions().debug_level" -g`) == "2"
    let code = read(`$exename -g0 -e "code_llvm(STDOUT, +, (Int64, Int64), false, true)"`, String)
        @test contains(code, "llvm.module.flags")
        @test !contains(code, "llvm.dbg.cu")
        @test !contains(code, "int.jl")
        @test !contains(code, "Int64")
    end
    let code = read(`$exename -g1 -e "code_llvm(STDOUT, +, (Int64, Int64), false, true)"`, String)
        @test contains(code, "llvm.module.flags")
        @test contains(code, "llvm.dbg.cu")
        @test contains(code, "int.jl")
        @test !contains(code, "Int64")
    end
    let code = read(`$exename -g2 -e "code_llvm(STDOUT, +, (Int64, Int64), false, true)"`, String)
        @test contains(code, "llvm.module.flags")
        @test contains(code, "llvm.dbg.cu")
        @test contains(code, "int.jl")
        @test contains(code, "\"Int64\"")
    end

    # --check-bounds
    let JL_OPTIONS_CHECK_BOUNDS_DEFAULT = 0,
        JL_OPTIONS_CHECK_BOUNDS_ON = 1,
        JL_OPTIONS_CHECK_BOUNDS_OFF = 2
        @test parse(Int,readchomp(`$exename -E "Int(Base.JLOptions().check_bounds)"`)) ==
            JL_OPTIONS_CHECK_BOUNDS_DEFAULT
        @test parse(Int,readchomp(`$exename -E "Int(Base.JLOptions().check_bounds)"
            --check-bounds=yes`)) == JL_OPTIONS_CHECK_BOUNDS_ON
        @test parse(Int,readchomp(`$exename -E "Int(Base.JLOptions().check_bounds)"
            --check-bounds=no`)) == JL_OPTIONS_CHECK_BOUNDS_OFF
    end
    # check-bounds takes yes/no as argument
    @test !success(`$exename -E "exit(0)" --check-bounds=false`)

    # --depwarn
    @test readchomp(`$exename --depwarn=no -E
        "Base.syntax_deprecation_warnings(true)"`) == "false"
    @test readchomp(`$exename --depwarn=yes -E
        "Base.syntax_deprecation_warnings(false)"`) == "true"
    @test !success(`$exename --depwarn=false`)
    # test deprecated syntax
    @test !success(`$exename -e "foo (x::Int) = x * x" --depwarn=error`)
    # test deprecated method
    @test !success(`$exename -e "
        foo() = :foo; bar() = :bar
        @deprecate foo() bar()
        foo()
    " --depwarn=error`)

    # test deprecated bindings, #13269
    let code = """
        module Foo
            import Base: @deprecate_binding

            const NotDeprecated = true
            @deprecate_binding Deprecated NotDeprecated
        end

        Foo.Deprecated
        """

        @test !success(`$exename -E "$code" --depwarn=error`)

        let out  = Pipe(),
            proc = spawn(pipeline(`$exename -E "$code" --depwarn=yes`, stderr=out)),
            output = @async readchomp(out)

            close(out.in)
            wait(proc)
            @test success(proc)
            @test wait(output) == "WARNING: Foo.Deprecated is deprecated.\n  likely near no file:5"
        end

        let out  = Pipe(),
            proc = spawn(pipeline(`$exename -E "$code" --depwarn=no`, stderr=out))
            output = @async read(out, String)

            wait(proc)
            close(out.in)
            @test success(proc)
            @test wait(output) == ""
        end
    end

    # --inline
    @test readchomp(`$exename -E "Bool(Base.JLOptions().can_inline)"`) == "true"
    @test readchomp(`$exename --inline=yes -E "Bool(Base.JLOptions().can_inline)"`) == "true"
    @test readchomp(`$exename --inline=no -E "Bool(Base.JLOptions().can_inline)"`) == "false"
    # --inline takes yes/no as argument
    @test !success(`$exename --inline=false`)

    # --polly
    @test readchomp(`$exename -E "Bool(Base.JLOptions().polly)"`) == "true"
    @test readchomp(`$exename --polly=yes -E "Bool(Base.JLOptions().polly)"`) == "true"
    @test readchomp(`$exename --polly=no -E "Bool(Base.JLOptions().polly)"`) == "false"
    # --polly takes yes/no as argument
    @test !success(`$exename --polly=false`)

    # --fast-math
    let JL_OPTIONS_FAST_MATH_DEFAULT = 0,
        JL_OPTIONS_FAST_MATH_ON = 1,
        JL_OPTIONS_FAST_MATH_OFF = 2
        @test parse(Int,readchomp(`$exename -E
            "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_DEFAULT
        @test parse(Int,readchomp(`$exename --math-mode=user -E
            "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_DEFAULT
        @test parse(Int,readchomp(`$exename --math-mode=ieee -E
            "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_OFF
        @test parse(Int,readchomp(`$exename --math-mode=fast -E
            "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_ON
    end

    # --worker takes default / custom as argument (default/custom arguments
    # tested in test/parallel.jl, test/examples.jl)
    @test !success(`$exename --worker=true`)

    # test passing arguments
    mktempdir() do dir
        testfile = joinpath(dir, tempname())
        # write a julia source file that just prints ARGS to STDOUT
        write(testfile, """
            println(ARGS)
            """)
        cp(testfile, joinpath(dir, ".juliarc.jl"))

        withenv((Sys.iswindows() ? "USERPROFILE" : "HOME") => dir) do
            output = "[\"foo\", \"-bar\", \"--baz\"]"
            @test readchomp(`$exename $testfile foo -bar --baz`) == output
            @test readchomp(`$exename $testfile -- foo -bar --baz`) == output
            @test readchomp(`$exename -L $testfile -e 'exit(0)' -- foo -bar --baz`) ==
                output
            @test readchomp(`$exename --startup-file=yes -e 'exit(0)' -- foo -bar --baz`) ==
                output

            output = "String[]\nString[]"
            @test readchomp(`$exename -L $testfile $testfile`) == output
            @test readchomp(`$exename --startup-file=yes $testfile`) == output

            @test !success(`$exename --foo $testfile`)
            @test readchomp(`$exename -L $testfile -e 'exit(0)' -- foo -bar -- baz`) ==
                "[\"foo\", \"-bar\", \"--\", \"baz\"]"
        end
    end

    # test the program name remains constant
    mktempdir() do dir
        a = joinpath(dir, "a.jl")
        b = joinpath(dir, "b.jl")
        c = joinpath(dir, ".juliarc.jl")

        write(a, """
            println(@__FILE__)
            println(PROGRAM_FILE)
            include(\"$(escape_string(b))\")
            """)
        write(b, """
            println(@__FILE__)
            println(PROGRAM_FILE)
            """)
        cp(b, c)

        readsplit(cmd) = split(readchomp(cmd), '\n')

        withenv((Sys.iswindows() ? "USERPROFILE" : "HOME") => dir) do
            @test readsplit(`$exename $a`) ==
                [a, a,
                 b, a]
            @test readsplit(`$exename -L $b -e 'exit(0)'`) ==
                [realpath(b), ""]
            @test readsplit(`$exename -L $b $a`) ==
                [realpath(b), a,
                 a, a,
                 b, a]
            @test readsplit(`$exename --startup-file=yes -e 'exit(0)'`) ==
                [c, ""]
            @test readsplit(`$exename --startup-file=yes -L $b -e 'exit(0)'`) ==
                [c, "",
                 realpath(b), ""]
            @test readsplit(`$exename --startup-file=yes -L $b $a`) ==
                [c, a,
                 realpath(b), a,
                 a, a,
                 b, a]
        end
    end

    # issue #10562
    @test readchomp(`$exename -e 'println(ARGS);' ''`) == "[\"\"]"

    # issue #12679
    @test readchomp(pipeline(ignorestatus(`$exename --startup-file=no --compile=yes -ioo`),
        stderr=catcmd)) == "ERROR: unknown option `-o`"
    @test readchomp(pipeline(ignorestatus(`$exename --startup-file=no -p`),
        stderr=catcmd)) == "ERROR: option `-p/--procs` is missing an argument"
    @test readchomp(pipeline(ignorestatus(`$exename --startup-file=no --inline`),
        stderr=catcmd)) == "ERROR: option `--inline` is missing an argument"
    @test readchomp(pipeline(ignorestatus(`$exename --startup-file=no -e "@show ARGS" -now -- julia RUN.jl`),
        stderr=catcmd)) == "ERROR: unknown option `-n`"

    # --compiled-modules={yes|no}
    @test readchomp(`$exename -E "Bool(Base.JLOptions().use_compiled_modules)"`) == "true"
    @test readchomp(`$exename --compiled-modules=yes -E
        "Bool(Base.JLOptions().use_compiled_modules)"`) == "true"
    @test readchomp(`$exename --compiled-modules=no -E
        "Bool(Base.JLOptions().use_compiled_modules)"`) == "false"
    @test !success(`$exename --compiled-modules=foo -e "exit(0)"`)

    # issue #12671, starting from a non-directory
    # rm(dir) fails on windows with Permission denied
    # and was an upstream bug in llvm <= v3.3
    if !Sys.iswindows() && Base.libllvm_version > v"3.3"
        testdir = mktempdir()
        cd(testdir) do
            rm(testdir)
            @test success(`$exename -e "exit(0)"`)
        end
    end
end


# Find the path of libjulia (or libjulia-debug, as the case may be)
# to use as a dummy shlib to open
libjulia = abspath(Libdl.dlpath((ccall(:jl_is_debugbuild, Cint, ()) != 0) ? "libjulia-debug" : "libjulia"))

# test error handling code paths of running --sysimage
let exename = joinpath(JULIA_HOME, Base.julia_exename()),
    sysname = unsafe_string(Base.JLOptions().image_file)
    for nonexist_image in (
            joinpath(@__DIR__, "nonexistent"),
            "$sysname.nonexistent",
            )
        let stderr = Pipe(),
            p = spawn(pipeline(`$exename --sysimage=$nonexist_image`, stderr=stderr))
            close(stderr.in)
            let s = read(stderr, String)
                @test contains(s, "ERROR: could not load library \"$nonexist_image\"\n")
                @test !contains(s, "Segmentation fault")
                @test !contains(s, "EXCEPTION_ACCESS_VIOLATION")
            end
            @test !success(p)
            @test !Base.process_signaled(p)
            @test p.exitcode == 1
        end
    end
    let stderr = Pipe(),
        p = spawn(pipeline(`$exename --sysimage=$libjulia`, stderr=stderr))
        close(stderr.in)
        let s = read(stderr, String)
            @test s == "ERROR: System image file failed consistency check: maybe opened the wrong version?\n"
        end
        @test !success(p)
        @test !Base.process_signaled(p)
        @test p.exitcode == 1
    end
end

let exename = `$(Base.julia_cmd()) --sysimage-native-code=yes`
    # --startup-file
    let JL_OPTIONS_STARTUPFILE_ON = 1,
        JL_OPTIONS_STARTUPFILE_OFF = 2
        # `HOME=$tmpdir` to avoid errors in the user .juliarc.jl, which hangs the tests.  Issue #17642
        mktempdir() do tmpdir
            withenv("HOME"=>tmpdir) do
                @test parse(Int,readchomp(`$exename -E "Base.JLOptions().startupfile" --startup-file=yes`)) == JL_OPTIONS_STARTUPFILE_ON
            end
        end
        @test parse(Int,readchomp(`$exename -E "Base.JLOptions().startupfile"
            --startup-file=no`)) == JL_OPTIONS_STARTUPFILE_OFF
    end
    @test !success(`$exename --startup-file=false`)
end

# Make sure `julia --lisp` doesn't break
run(pipeline(DevNull, `$(joinpath(JULIA_HOME, Base.julia_exename())) --lisp`, DevNull))

# Test that `julia [some other option] --lisp` is disallowed
@test_throws ErrorException run(pipeline(DevNull, pipeline(`$(joinpath(JULIA_HOME,
    Base.julia_exename())) -Cnative --lisp`, stderr=DevNull), DevNull))

# --sysimage-native-code={yes|no}
let exename = `$(Base.julia_cmd()) --startup-file=no`
    @test readchomp(`$exename --sysimage-native-code=yes -E
        "Bool(Base.JLOptions().use_sysimage_native_code)"`) == "true"
    @test readchomp(`$exename --sysimage-native-code=no -E
        "Bool(Base.JLOptions().use_sysimage_native_code)"`) == "false"
end

# backtrace contains type and line number info (esp. on windows #17179)
for precomp in ("yes", "no")
    bt = read(pipeline(ignorestatus(`$(Base.julia_cmd()) --startup-file=no --sysimage-native-code=$precomp
        -E 'include("____nonexistent_file")'`), stderr=catcmd), String)
    @test contains(bt, "include_relative(::Module, ::String) at $(joinpath(".", "loading.jl"))")
    lno = match(r"at \.[\/\\]loading\.jl:(\d+)", bt)
    @test length(lno.captures) == 1
    @test parse(Int, lno.captures[1]) > 0
end

# PR #23002
let exename = `$(Base.julia_cmd()) --startup-file=no`
    for (mac, flag, pfix, msg) in [("@test_nowarn", ``, "_1", ""),
                                   ("@test_warn",   `--warn-overwrite=yes`, "_2", "\"WARNING: Method definition\"")]
        str = """
        using Test
        try
            # issue #18725
            $mac $msg @eval Main begin
                f18725$(pfix)(x) = 1
                f18725$(pfix)(x) = 2
            end
            @test Main.f18725$(pfix)(0) == 2
            # PR #23030
            $mac $msg @eval Main module Module23030$(pfix)
                f23030$(pfix)(x) = 1
                f23030$(pfix)(x) = 2
            end
        catch
            exit(-1)
        end
        exit(0)
        """
        run(`$exename $flag -e $str`)
    end
end
