# This file is a part of Julia. License is MIT: http://julialang.org/license

let exename = `$(Base.julia_cmd()) --precompiled=yes`
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

    # --print
    @test readstring(`$exename -E "1+1"`) == "2\n"
    @test readstring(`$exename --print="1+1"`) == "2\n"
    @test !success(`$exename -E`)
    @test !success(`$exename --print`)

    # --post-boot
    @test  success(`$exename -P "exit(0)"`)
    @test !success(`$exename -P "exit(1)"`)
    @test  success(`$exename --post-boot="exit(0)"`)
    @test !success(`$exename --post-boot="exit(1)"`)
    @test !success(`$exename -P`)
    @test !success(`$exename --post-boot`)

    # --load
    let testfile = tempname()
        try
            write(testfile, "testvar = :test\n")
            @test split(readchomp(`$exename --load=$testfile -P "println(testvar)"`), '\n')[end] == "test"
            @test split(readchomp(`$exename -P "println(testvar)" -L $testfile`), '\n')[end] == "test"
        finally
            rm(testfile)
        end
    end
    # -L, --load requires an argument
    @test !success(`$exename -L`)
    @test !success(`$exename --load`)

    # --cpu-target
    # NOTE: this test only holds true when there is a sys.{dll,dylib,so} shared library present.
    # The tests are also limited to unix platforms at the moment because loading the system image
    # not turned on for Window's binary builds at the moment.
    @unix_only if Libdl.dlopen_e(splitext(bytestring(Base.JLOptions().image_file))[1]) != C_NULL
        @test !success(`$exename -C invalidtarget`)
        @test !success(`$exename --cpu-target=invalidtarget`)
    end

    # --procs
    @test readchomp(`$exename -q -p 2 -P "println(nworkers()); exit(0)"`) == "2"
    @test !success(`$exename -p 0`)
    @test !success(`$exename --procs=1.0`)

    # --machinefile
    # this does not check that machinefile works,
    # only that the filename gets correctly passed to the option struct
    let fname = tempname()
        touch(fname)
        fname = realpath(fname)
        try
            @test readchomp(`$exename --machinefile $fname -e "println(bytestring(Base.JLOptions().machinefile))"`) == fname
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
    @test readchomp(`$exename -E "Bool(Base.JLOptions().historyfile)" --history-file=yes`) == "true"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().historyfile)" --history-file=no`) == "false"
    # deprecated
    @test readchomp(`$exename -E "Bool(Base.JLOptions().historyfile)" --no-history-file`) == "false"
    @test !success(`$exename --history-file=false`)

    # --startup-file
    let JL_OPTIONS_STARTUPFILE_ON = 1,
        JL_OPTIONS_STARTUPFILE_OFF = 2
        @test parse(Int,readchomp(`$exename -E "Base.JLOptions().startupfile" --startup-file=yes`)) == JL_OPTIONS_STARTUPFILE_ON
        @test parse(Int,readchomp(`$exename -E "Base.JLOptions().startupfile" --startup-file=no`)) == JL_OPTIONS_STARTUPFILE_OFF
    end
    @test !success(`$exename --startup-file=false`)

    # --code-coverage
    @test readchomp(`$exename -E "Bool(Base.JLOptions().code_coverage)"`) == "false"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().code_coverage)" --code-coverage=none`) == "false"

    @test readchomp(`$exename -E "Bool(Base.JLOptions().code_coverage)" --code-coverage`) == "true"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().code_coverage)" --code-coverage=user`) == "true"

    # --track-allocation
    @test readchomp(`$exename -E "Bool(Base.JLOptions().malloc_log)"`) == "false"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().malloc_log)" --track-allocation=none`) == "false"

    @test readchomp(`$exename -E "Bool(Base.JLOptions().malloc_log)" --track-allocation`) == "true"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().malloc_log)" --track-allocation=user`) == "true"

    # --optimize
    @test readchomp(`$exename -E "Base.JLOptions().opt_level"`) == "2"
    @test readchomp(`$exename -E "Base.JLOptions().opt_level" -O`) == "3"
    @test readchomp(`$exename -E "Base.JLOptions().opt_level" --optimize`) == "3"
    @test readchomp(`$exename -E "Base.JLOptions().opt_level" -O0`) == "0"

    # --check-bounds
    let JL_OPTIONS_CHECK_BOUNDS_DEFAULT = 0,
        JL_OPTIONS_CHECK_BOUNDS_ON = 1,
        JL_OPTIONS_CHECK_BOUNDS_OFF = 2
        @test parse(Int,readchomp(`$exename -E "Int(Base.JLOptions().check_bounds)"`)) == JL_OPTIONS_CHECK_BOUNDS_DEFAULT
        @test parse(Int,readchomp(`$exename -E "Int(Base.JLOptions().check_bounds)" --check-bounds=yes`)) == JL_OPTIONS_CHECK_BOUNDS_ON
        @test parse(Int,readchomp(`$exename -E "Int(Base.JLOptions().check_bounds)" --check-bounds=no`)) == JL_OPTIONS_CHECK_BOUNDS_OFF
    end
    # check-bounds takes yes/no as argument
    @test !success(`$exename -E "exit(0)" --check-bounds=false`)

    # --depwarn
    @test readchomp(`$exename --depwarn=no -E "Base.syntax_deprecation_warnings(true)"`) == "false"
    @test readchomp(`$exename --depwarn=yes -E "Base.syntax_deprecation_warnings(false)"`) == "true"
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

        # FIXME these should also be run on windows once the bug causing them to hang gets fixed
        @unix_only let out  = Pipe(),
                       proc = spawn(pipeline(`$exename -E "$code" --depwarn=yes`, stderr=out))

            wait(proc)
            close(out.in)
            @test success(proc)
            @test readchomp(out) == "WARNING: Foo.Deprecated is deprecated.\n  likely near no file:5"
        end

        @unix_only let out  = Pipe(),
                       proc = spawn(pipeline(`$exename -E "$code" --depwarn=no`, stderr=out))

            wait(proc)
            close(out.in)
            @test success(proc)
            @test isempty(readstring(out))
        end
    end

    # --inline
    @test readchomp(`$exename -E "Bool(Base.JLOptions().can_inline)"`) == "true"
    @test readchomp(`$exename --inline=yes -E "Bool(Base.JLOptions().can_inline)"`) == "true"
    @test readchomp(`$exename --inline=no -E "Bool(Base.JLOptions().can_inline)"`) == "false"
    # --inline takes yes/no as arugment
    @test !success(`$exename --inline=false`)

    # --fast-math
    let JL_OPTIONS_FAST_MATH_DEFAULT = 0,
        JL_OPTIONS_FAST_MATH_ON = 1,
        JL_OPTIONS_FAST_MATH_OFF = 2
        @test parse(Int,readchomp(`$exename -E "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_DEFAULT
        @test parse(Int,readchomp(`$exename --math-mode=user -E "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_DEFAULT
        @test parse(Int,readchomp(`$exename --math-mode=ieee -E "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_OFF
        @test parse(Int,readchomp(`$exename --math-mode=fast -E "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_ON
    end

    # --worker takes default / custom as arugment (default/custom arguments tested in test/parallel.jl, test/examples.jl)
    @test !success(`$exename --worker=true`)

    escape(str) = replace(str, "\\", "\\\\")

    # test passing arguments
    let testfile = tempname()
        try
            # write a julia source file that just prints ARGS to STDOUT
            write(testfile, """
                println(ARGS)
            """)
            @test readchomp(`$exename $testfile foo -bar --baz`) == "String[\"foo\",\"-bar\",\"--baz\"]"
            @test readchomp(`$exename $testfile -- foo -bar --baz`) == "String[\"foo\",\"-bar\",\"--baz\"]"
            @test readchomp(`$exename -L $testfile -e 'exit(0)' -- foo -bar --baz`) == "String[\"foo\",\"-bar\",\"--baz\"]"
            @test split(readchomp(`$exename -L $testfile $testfile`), '\n') == ["String[\"$(escape(testfile))\"]", "String[]"]
            @test !success(`$exename --foo $testfile`)
            @test !success(`$exename -L $testfile -e 'exit(0)' -- foo -bar -- baz`)
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
            @test split(readchomp(`$exename $a`), '\n') == ["$a", "$a", "0", "$b", "$a", "0"]
            @test split(readchomp(`$exename -L $b -e 'exit(0)'`), '\n') == ["$(realpath(b))", "", "0"]
            @test split(readchomp(`$exename -L $b $a`), '\n') == ["$(realpath(b))", "", "1", "$a", "$a", "0", "$b", "$a", "0"]
        finally
            rm(a)
            rm(b)
        end
    end

    # issue #10562
    @test readchomp(`$exename -e 'println(ARGS);' ''`) == "String[\"\"]"

    # issue #12679
    extrapath = @windows? joinpath(JULIA_HOME,"..","Git","usr","bin")*";" : ""
    withenv("PATH" => extrapath * ENV["PATH"]) do
        @test readchomp(pipeline(ignorestatus(`$exename -f --compile=yes -foo`),stderr=`cat`)) == "ERROR: unknown option `-o`"
        @test readchomp(pipeline(ignorestatus(`$exename -f -p`),stderr=`cat`)) == "ERROR: option `-p/--procs` is missing an argument"
        @test readchomp(pipeline(ignorestatus(`$exename -f --inline`),stderr=`cat`)) == "ERROR: option `--inline` is missing an argument"
        @test readchomp(pipeline(ignorestatus(`$exename -f -e "@show ARGS" -now -- julia RUN.jl`),stderr=`cat`)) == "ERROR: unknown option `-n`"
    end

    # --compilecache={yes|no}
    @test readchomp(`$exename -E "Bool(Base.JLOptions().use_compilecache)"`) == "true"
    @test readchomp(`$exename --compilecache=yes -E "Bool(Base.JLOptions().use_compilecache)"`) == "true"
    @test readchomp(`$exename --compilecache=no -E "Bool(Base.JLOptions().use_compilecache)"`) == "false"
    @test !success(`$exename --compilecache=foo -e "exit(0)"`)

    # issue #12671, starting from a non-directory
    @unix_only if VersionNumber(Base.libllvm_version) > v"3.3"
        testdir = mktempdir()
        cd(testdir) do
            rm(testdir)
            @test success(`$exename -e "exit(0)"`)
        end
    end
end
