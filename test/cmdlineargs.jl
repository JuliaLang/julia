# This file is a part of Julia. License is MIT: http://julialang.org/license

let exename = joinpath(JULIA_HOME, Base.julia_exename())
    # --version
    let v = split(readall(`$exename -v`), "julia version ")[end]
        @test Base.VERSION_STRING == chomp(v)
    end
    @test readall(`$exename -v`) == readall(`$exename --version`)

    # --help
    @test startswith(readall(`$exename -h`), "julia [options] [program] [args...]")
    @test startswith(readall(`$exename --help`), "julia [options] [program] [args...]")

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
    @test readall(`$exename -E "1+1"`) == "2\n"
    @test readall(`$exename --print="1+1"`) == "2\n"
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
            open(testfile, "w") do io
                println(io, "testvar = :test")
            end
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
    @test readchomp(`$exename -E "Bool(Base.JLOptions().opt_level)"`) == "false"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().opt_level)" -O`) == "true"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().opt_level)" --optimize`) == "true"

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

    # --inline
    @test readchomp(`$exename -E "Bool(Base.JLOptions().can_inline)"`) == "true"
    @test readchomp(`$exename --inline=yes -E "Bool(Base.JLOptions().can_inline)"`) == "true"
    @test readchomp(`$exename --inline=no -E "Bool(Base.JLOptions().can_inline)"`) == "false"
    # --inline takes yes/no as arugment
    @test !success(`$exename --inline=false`)

    # --fast-math
    let JL_OPTIONS_FAST_MATH_DEFAULT = 0,
        JL_OPTIONS_FAST_MATH_OFF = 2
        @test parse(Int,readchomp(`$exename -E "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_DEFAULT
        @test parse(Int,readchomp(`$exename --math-mode=user -E "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_DEFAULT
        @test parse(Int,readchomp(`$exename --math-mode=ieee -E "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_OFF
    end
    # --math-mode takes ieee/user as argument
    @test !success(`$exename --math-mode=fast`)

    # --worker takes default / custom as arugment (default/custom arguments tested in test/parallel.jl, test/examples.jl)
    @test !success(`$exename --worker=true`)

    # test passing arguments
    let testfile = tempname()
        try
            # write a julia source file that just prints ARGS to STDOUT and exits
            open(testfile, "w") do io
                println(io, "println(ARGS)")
                println(io, "exit(0)")
            end
            @test readchomp(`$exename $testfile foo -bar --baz`) ==  "UTF8String[\"foo\",\"-bar\",\"--baz\"]"
            @test readchomp(`$exename $testfile -- foo -bar --baz`) ==  "UTF8String[\"foo\",\"-bar\",\"--baz\"]"
            @test readchomp(`$exename -L $testfile -- foo -bar --baz`) ==  "UTF8String[\"foo\",\"-bar\",\"--baz\"]"
            @test !success(`$exename --foo $testfile`)
            @test !success(`$exename -L $testfile -- foo -bar -- baz`)
        finally
            rm(testfile)
        end
    end

    # issue #10562
    @test readchomp(`$exename -e 'println(ARGS);' ''`) == "UTF8String[\"\"]"
end
