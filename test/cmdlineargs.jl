# This file is a part of Julia. License is MIT: http://julialang.org/license

catcmd = `cat`
if is_windows()
    try # use busybox-w32 on windows
        success(`busybox`)
        catcmd = `busybox cat`
    end
end

let exename = `$(Base.julia_cmd()) --precompiled=yes --startup-file=no`
    # --version
    let v = split(readstring(`$exename -v`), "julia version ")[end]
        @test Base.VERSION_STRING == chomp(v)
    end
    @test readstring(`$exename -v`) == readstring(`$exename --version`)

    # --help
    let header = "julia [switches] -- [programfile] [args...]"
        @test startswith(readstring(`$exename -h`), header)
        @test startswith(readstring(`$exename --help`), header)
    end

    # --quiet
    # This flag is indirectly tested in test/repl.jl

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
    @test readstring(`$exename -E "1+1"`) == "2\n"
    @test readstring(`$exename --print="1+1"`) == "2\n"
    @test !success(`$exename -E`)
    @test !success(`$exename --print`)

    # --load
    let testfile = tempname()
        try
            write(testfile, "testvar = :test\n")
            @test split(readchomp(`$exename -i --load=$testfile -e "println(testvar)"`),
                '\n')[end] == "test"
            @test split(readchomp(`$exename -i -e "println(testvar)" -L $testfile`),
                '\n')[end] == "test"
        finally
            rm(testfile)
        end
    end
    # -L, --load requires an argument
    @test !success(`$exename -L`)
    @test !success(`$exename --load`)

    # --cpu-target
    # NOTE: this test only holds true when there is a sys.{dll,dylib,so} shared library present.
    if Libdl.dlopen_e(splitext(unsafe_string(Base.JLOptions().image_file))[1]) != C_NULL
        @test !success(`$exename -C invalidtarget --precompiled=yes`)
        @test !success(`$exename --cpu-target=invalidtarget --precompiled=yes`)
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
    let code = readstring(`$exename -g0 -e "code_llvm(STDOUT, +, (Int64, Int64), false, true)"`)
        @test contains(code, "llvm.module.flags")
        @test !contains(code, "llvm.dbg.cu")
        @test !contains(code, "int.jl")
        @test !contains(code, "Int64")
    end
    let code = readstring(`$exename -g1 -e "code_llvm(STDOUT, +, (Int64, Int64), false, true)"`)
        @test contains(code, "llvm.module.flags")
        @test contains(code, "llvm.dbg.cu")
        @test contains(code, "int.jl")
        @test !contains(code, "Int64")
    end
    let code = readstring(`$exename -g2 -e "code_llvm(STDOUT, +, (Int64, Int64), false, true)"`)
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
            output = @async readstring(out)

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

    escape(str) = replace(str, "\\", "\\\\")

    # test passing arguments
    let testfile = tempname()
        try
            # write a julia source file that just prints ARGS to STDOUT
            write(testfile, """
                println(ARGS)
            """)
            @test readchomp(`$exename $testfile foo -bar --baz`) ==
                "String[\"foo\", \"-bar\", \"--baz\"]"
            @test readchomp(`$exename $testfile -- foo -bar --baz`) ==
                "String[\"foo\", \"-bar\", \"--baz\"]"
            @test readchomp(`$exename -L $testfile -e 'exit(0)' -- foo -bar --baz`) ==
                "String[\"foo\", \"-bar\", \"--baz\"]"
            @test split(readchomp(`$exename -L $testfile $testfile`), '\n') ==
                ["String[\"$(escape(testfile))\"]", "String[]"]
            @test !success(`$exename --foo $testfile`)
            @test readchomp(`$exename -L $testfile -e 'exit(0)' -- foo -bar -- baz`) == "String[\"foo\", \"-bar\", \"--\", \"baz\"]"
        finally
            rm(testfile)
        end
    end

    # test the script name
    let a = tempname(), b = tempname()
        try
            write(a, """
                println(@__FILE__)
                println(PROGRAM_FILE)
                println(length(ARGS))
                include(\"$(escape(b))\")
            """)
            write(b, """
                println(@__FILE__)
                println(PROGRAM_FILE)
                println(length(ARGS))
            """)
            @test split(readchomp(`$exename $a`), '\n') ==
                ["$a", "$a", "0", "$b", "$a", "0"]
            @test split(readchomp(`$exename -L $b -e 'exit(0)'`), '\n') ==
                ["$(realpath(b))", "", "0"]
            @test split(readchomp(`$exename -L $b $a`), '\n') ==
                ["$(realpath(b))", "", "1", "$a", "$a", "0", "$b", "$a", "0"]
        finally
            rm(a)
            rm(b)
        end
    end

    # issue #10562
    @test readchomp(`$exename -e 'println(ARGS);' ''`) == "String[\"\"]"

    # issue #12679
    @test readchomp(pipeline(ignorestatus(`$exename --startup-file=no --compile=yes -ioo`),
        stderr=catcmd)) == "ERROR: unknown option `-o`"
    @test readchomp(pipeline(ignorestatus(`$exename --startup-file=no -p`),
        stderr=catcmd)) == "ERROR: option `-p/--procs` is missing an argument"
    @test readchomp(pipeline(ignorestatus(`$exename --startup-file=no --inline`),
        stderr=catcmd)) == "ERROR: option `--inline` is missing an argument"
    @test readchomp(pipeline(ignorestatus(`$exename --startup-file=no -e "@show ARGS" -now -- julia RUN.jl`),
        stderr=catcmd)) == "ERROR: unknown option `-n`"

    # --compilecache={yes|no}
    @test readchomp(`$exename -E "Bool(Base.JLOptions().use_compilecache)"`) == "true"
    @test readchomp(`$exename --compilecache=yes -E
        "Bool(Base.JLOptions().use_compilecache)"`) == "true"
    @test readchomp(`$exename --compilecache=no -E
        "Bool(Base.JLOptions().use_compilecache)"`) == "false"
    @test !success(`$exename --compilecache=foo -e "exit(0)"`)

    # issue #12671, starting from a non-directory
    # rm(dir) fails on windows with Permission denied
    # and was an upstream bug in llvm <= v3.3
    if !is_windows() && VersionNumber(Base.libllvm_version) > v"3.3"
        testdir = mktempdir()
        cd(testdir) do
            rm(testdir)
            @test success(`$exename -e "exit(0)"`)
        end
    end
end

let exename = `$(Base.julia_cmd()) --precompiled=yes`
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

# --precompiled={yes|no}
let exename = `$(Base.julia_cmd()) --startup-file=no`
    @test readchomp(`$exename --precompiled=yes -E
        "Bool(Base.JLOptions().use_precompiled)"`) == "true"
    @test readchomp(`$exename --precompiled=no -E
        "Bool(Base.JLOptions().use_precompiled)"`) == "false"
end

# backtrace contains type and line number info (esp. on windows #17179)
for precomp in ("yes", "no")
    bt = readstring(pipeline(ignorestatus(`$(Base.julia_cmd()) --startup-file=no --precompiled=$precomp
        -E 'include("____nonexistent_file")'`), stderr=catcmd))
    @test contains(bt, "include_from_node1")
    if is_windows() && Sys.WORD_SIZE == 32 && precomp == "yes"
        # fixme, issue #17251
        @test_broken contains(bt, "include_from_node1(::String) at $(joinpath(".","loading.jl"))")
    else
        @test contains(bt, "include_from_node1(::String) at $(joinpath(".","loading.jl"))")
    end
    lno = match(r"at \.[\/\\]loading\.jl:(\d+)", bt)
    @test length(lno.captures) == 1
    @test parse(Int, lno.captures[1]) > 0
end
