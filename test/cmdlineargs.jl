# This file is a part of Julia. License is MIT: https://julialang.org/license

import Libdl

# helper function for passing input to stdin
# and returning the stdout result
function writereadpipeline(input, exename)
    p = open(exename, "w+")
    @async begin
        write(p.in, input)
        close(p.in)
    end
    return (read(p.out, String), success(p))
end

# helper function for returning stderr and stdout
# from running a command (ignoring failure status)
function readchomperrors(exename::Cmd)
    out = Base.PipeEndpoint()
    err = Base.PipeEndpoint()
    p = run(exename, devnull, out, err, wait=false)
    o = @async(readchomp(out))
    e = @async(readchomp(err))
    return (success(p), fetch(o), fetch(e))
end

function format_filename(s)
    p = ccall(:jl_format_filename, Cstring, (Cstring,), s)
    r = unsafe_string(p)
    ccall(:free, Cvoid, (Cstring,), p)
    return r
end

# Returns true if the given command errors, but doesn't signal
function errors_not_signals(cmd::Cmd)
    p = run(pipeline(ignorestatus(cmd); stdout=devnull, stderr=devnull))
    return errors_not_signals(p)
end
function errors_not_signals(p::Base.Process)
    wait(p)
    return process_exited(p) && !Base.process_signaled(p) && !success(p)
end

let
    fn = format_filename("a%d %p %i %L %l %u z")
    hd = withenv("HOME" => nothing) do
        # get the homedir, as reported by uv_os_get_passwd, as used by jl_format_filename
        try
            homedir()
        catch ex
            (ex isa Base.IOError && ex.code == Base.UV_ENOENT) || rethrow(ex)
            ""
        end
    end
    @test startswith(fn, "a$hd ")
    @test endswith(fn, " z")
    @test !occursin('%', fn)
    @test occursin(" $(getpid()) ", fn)
    @test occursin(" $(Libc.gethostname()) ", fn)
    @test format_filename("%a%%b") == "a%b"
end

let exename = `$(Base.julia_cmd()) --startup-file=no --color=no`
    # tests for handling of ENV errors
    let v = writereadpipeline("println(\"REPL: \", @which(less), @isdefined(InteractiveUtils))",
                setenv(`$exename -i -E 'empty!(LOAD_PATH); @isdefined InteractiveUtils'`,
                    "JULIA_LOAD_PATH" => "",
                    "JULIA_DEPOT_PATH" => "",
                    "HOME" => homedir()))
        @test v == ("false\nREPL: InteractiveUtilstrue\n", true)
    end
    let v = writereadpipeline("println(\"REPL: \", InteractiveUtils)",
                setenv(`$exename -i -e 'const InteractiveUtils = 3'`,
                    "JULIA_LOAD_PATH" => ";;;:::",
                    "JULIA_DEPOT_PATH" => ";;;:::",
                    "HOME" => homedir()))
        # TODO: ideally, `@which`, etc. would still work, but Julia can't handle `using $InterativeUtils`
        @test v == ("REPL: 3\n", true)
    end
    @testset let v = readchomperrors(`$exename -i -e '
            empty!(LOAD_PATH)
            @eval Sys STDLIB=mktempdir()
            Base.unreference_module(Base.PkgId(Base.UUID(0xb77e0a4c_d291_57a0_90e8_8db25a27a240), "InteractiveUtils"))
            '`)
        # simulate not having a working version of InteractiveUtils,
        # make sure this is a non-fatal error and the REPL still loads
        @test v[1]
        @test isempty(v[2])
        @test startswith(v[3], "┌ Warning: Failed to import InteractiveUtils into module Main\n")
    end
    real_threads = string(ccall(:jl_cpu_threads, Int32, ()))
    for nc in ("0", "-2", "x", "2x", " ", "")
        v = readchomperrors(setenv(`$exename -i -E 'Sys.CPU_THREADS'`, "JULIA_CPU_THREADS" => nc, "HOME" => homedir()))
        @test v == (true, real_threads,
            "WARNING: couldn't parse `JULIA_CPU_THREADS` environment variable. Defaulting Sys.CPU_THREADS to $real_threads.")
    end
    for nc in ("1", " 1 ", " +1 ", " 0x1 ")
        @testset let v = readchomperrors(setenv(`$exename -i -E 'Sys.CPU_THREADS'`, "JULIA_CPU_THREADS" => nc, "HOME" => homedir()))
            @test v[1]
            @test v[2] == "1"
            @test isempty(v[3])
        end
    end

    @testset let v = readchomperrors(setenv(`$exename -e 0`, "JULIA_LLVM_ARGS" => "-print-options", "HOME" => homedir()))
        @test v[1]
        @test contains(v[2], r"print-options + = 1")
        @test contains(v[2], r"combiner-store-merge-dependence-limit + = 4")
        @test contains(v[2], r"enable-tail-merge + = 2")
        @test isempty(v[3])
    end
    @testset let v = readchomperrors(setenv(`$exename -e 0`, "JULIA_LLVM_ARGS" => "-print-options -enable-tail-merge=1 -combiner-store-merge-dependence-limit=6", "HOME" => homedir()))
        @test v[1]
        @test contains(v[2], r"print-options + = 1")
        @test contains(v[2], r"combiner-store-merge-dependence-limit + = 6")
        @test contains(v[2], r"enable-tail-merge + = 1")
        @test isempty(v[3])
    end
    @testset let v = readchomperrors(setenv(`$exename -e 0`, "JULIA_LLVM_ARGS" => "-print-options -enable-tail-merge=1 -enable-tail-merge=1", "HOME" => homedir()))
        @test !v[1]
        @test isempty(v[2])
        @test v[3] == "julia: for the --enable-tail-merge option: may only occur zero or one times!"
    end
end

let exename = `$(Base.julia_cmd()) --startup-file=no --color=no`
    # --version
    let v = split(read(`$exename -v`, String), "julia version ")[end]
        @test Base.VERSION_STRING == chomp(v)
    end
    @test read(`$exename -v`, String) == read(`$exename --version`, String)

    # --help
    let header = "\n    julia [switches] -- [programfile] [args...]"
        @test startswith(read(`$exename -h`, String), header)
        @test startswith(read(`$exename --help`, String), header)
    end

    # ~ expansion in --project and JULIA_PROJECT
    if !Sys.iswindows()
        let expanded = abspath(expanduser("~/foo/Project.toml"))
            @test expanded == readchomp(`$exename --project='~/foo' -e 'println(Base.active_project())'`)
            @test expanded == readchomp(setenv(`$exename -e 'println(Base.active_project())'`, "JULIA_PROJECT" => "~/foo", "HOME" => homedir()))
        end
    end

    # handling of @projectname in --project and JULIA_PROJECT
    let expanded = abspath(Base.load_path_expand("@foo"))
        @test expanded == readchomp(`$exename --project='@foo' -e 'println(Base.active_project())'`)
        @test expanded == readchomp(addenv(`$exename -e 'println(Base.active_project())'`, "JULIA_PROJECT" => "@foo", "HOME" => homedir()))
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
    @test success(`$exename -H $(Sys.BINDIR)`)
    @test success(`$exename --home=$(Sys.BINDIR)`)

    # --eval
    @test  success(`$exename -e "exit(0)"`)
    @test errors_not_signals(`$exename -e "exit(1)"`)
    @test  success(`$exename --eval="exit(0)"`)
    @test errors_not_signals(`$exename --eval="exit(1)"`)
    @test errors_not_signals(`$exename -e`)
    @test errors_not_signals(`$exename --eval`)
    # --eval --interactive (replaced --post-boot)
    @test  success(`$exename -i -e "exit(0)"`)
    @test errors_not_signals(`$exename -i -e "exit(1)"`)
    # issue #34924
    @test  success(`$exename -e 'const LOAD_PATH=1'`)

    # --print
    @test read(`$exename -E "1+1"`, String) == "2\n"
    @test read(`$exename --print="1+1"`, String) == "2\n"
    @test errors_not_signals(`$exename -E`)
    @test errors_not_signals(`$exename --print`)

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
    @test errors_not_signals(`$exename -L`)
    @test errors_not_signals(`$exename --load`)

    # --cpu-target (requires LLVM enabled)
    # Strictly test for failed error, not a segfault, since we had a false positive with just `success()` before.
    @test errors_not_signals(`$exename -C invalidtarget`)
    @test errors_not_signals(`$exename --cpu-target=invalidtarget`)

    # -t, --threads
    code = "print(Threads.nthreads())"
    cpu_threads = ccall(:jl_effective_threads, Int32, ())
    @test string(cpu_threads) ==
          read(`$exename --threads auto -e $code`, String) ==
          read(`$exename --threads=auto -e $code`, String) ==
          read(`$exename -tauto -e $code`, String) ==
          read(`$exename -t auto -e $code`, String)
    for nt in (nothing, "1")
        withenv("JULIA_NUM_THREADS" => nt) do
            @test read(`$exename --threads=2 -e $code`, String) ==
                  read(`$exename -t 2 -e $code`, String) == "2"
        end
    end
    # We want to test oversubscription, but on manycore machines, this can
    # actually exhaust limited PID spaces
    cpu_threads = max(2*cpu_threads, min(50, 10*cpu_threads))
    if Sys.WORD_SIZE == 32
        cpu_threads = min(cpu_threads, 50)
    end
    @test read(`$exename -t $cpu_threads -e $code`, String) == string(cpu_threads)
    withenv("JULIA_NUM_THREADS" => string(cpu_threads)) do
        @test read(`$exename -e $code`, String) == string(cpu_threads)
    end
    @test errors_not_signals(`$exename -t 0`)
    @test errors_not_signals(`$exename -t -1`)

    # Combining --threads and --procs: --threads does propagate
    withenv("JULIA_NUM_THREADS" => nothing) do
        code = "print(sum(remotecall_fetch(Threads.nthreads, x) for x in procs()))"
        @test read(`$exename -p2 -t2 -e $code`, String) == "6"
    end

    # Combining --threads and invalid -C should yield a decent error
    @test errors_not_signals(`$exename -t 2 -C invalidtarget`)

    # --procs
    @test readchomp(`$exename -q -p 2 -e "println(nworkers())"`) == "2"
    @test errors_not_signals(`$exename -p 0`)
    let p = run(`$exename --procs=1.0`, wait=false)
        wait(p)
        @test p.exitcode == 1 && p.termsignal == 0
    end

    # --machine-file
    # this does not check that machine file works,
    # only that the filename gets correctly passed to the option struct
    let fname = tempname()
        touch(fname)
        fname = realpath(fname)
        try
            @test readchomp(`$exename --machine-file $fname -e
                "println(unsafe_string(Base.JLOptions().machine_file))"`) == fname
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
    @test errors_not_signals(`$exename --color=false`)

    # --history-file
    @test readchomp(`$exename -E "Bool(Base.JLOptions().historyfile)"
        --history-file=yes`) == "true"
    @test readchomp(`$exename -E "Bool(Base.JLOptions().historyfile)"
        --history-file=no`) == "false"
    @test errors_not_signals(`$exename --history-file=false`)

    # --code-coverage
    mktempdir() do dir
        helperdir = joinpath(@__DIR__, "testhelpers")
        inputfile = joinpath(helperdir, "coverage_file.jl")
        expected = replace(read(joinpath(helperdir, "coverage_file.info.bad"), String),
            "<FILENAME>" => realpath(inputfile))
        expected_good = replace(read(joinpath(helperdir, "coverage_file.info"), String),
            "<FILENAME>" => realpath(inputfile))
        covfile = replace(joinpath(dir, "coverage.info"), "%" => "%%")
        @test !isfile(covfile)
        defaultcov = readchomp(`$exename -E "Base.JLOptions().code_coverage != 0" -L $inputfile`)
        opts = Base.JLOptions()
        coverage_file = (opts.output_code_coverage != C_NULL) ?  unsafe_string(opts.output_code_coverage) : ""
        @test !isfile(covfile)
        @test defaultcov == string(opts.code_coverage != 0 && (isempty(coverage_file) || occursin("%p", coverage_file)))
        @test readchomp(`$exename -E "Base.JLOptions().code_coverage" -L $inputfile
            --code-coverage=$covfile --code-coverage=none`) == "0"
        @test !isfile(covfile)
        @test readchomp(`$exename -E "Base.JLOptions().code_coverage" -L $inputfile
            --code-coverage=$covfile --code-coverage`) == "1"
        @test isfile(covfile)
        got = read(covfile, String)
        rm(covfile)
        @test occursin(expected, got) || (expected, got)
        @test_broken occursin(expected_good, got)
        @test readchomp(`$exename -E "Base.JLOptions().code_coverage" -L $inputfile
            --code-coverage=$covfile --code-coverage=user`) == "1"
        @test isfile(covfile)
        got = read(covfile, String)
        rm(covfile)
        @test occursin(expected, got) || (expected, got)
        @test_broken occursin(expected_good, got)
        @test readchomp(`$exename -E "Base.JLOptions().code_coverage" -L $inputfile
            --code-coverage=$covfile --code-coverage=all`) == "2"
        @test isfile(covfile)
        got = read(covfile, String)
        rm(covfile)
        @test occursin(expected, got) || (expected, got)
        @test_broken occursin(expected_good, got)

        # Ask for coverage in specific file
        tfile = realpath(inputfile)
        @test readchomp(`$exename -E "(Base.JLOptions().code_coverage, unsafe_string(Base.JLOptions().tracked_path))" -L $inputfile
            --code-coverage=$covfile --code-coverage=@$tfile`) == "(3, $(repr(tfile)))"
        @test isfile(covfile)
        got = read(covfile, String)
        rm(covfile)
        @test occursin(expected, got) || (expected, got)
        @test_broken occursin(expected_good, got)

        # Ask for coverage in directory
        tdir = dirname(realpath(inputfile))
        @test readchomp(`$exename -E "(Base.JLOptions().code_coverage, unsafe_string(Base.JLOptions().tracked_path))" -L $inputfile
            --code-coverage=$covfile --code-coverage=@$tdir`) == "(3, $(repr(tdir)))"
        @test isfile(covfile)
        got = read(covfile, String)
        rm(covfile)
        @test occursin(expected, got) || (expected, got)
        @test_broken occursin(expected_good, got)

        # Ask for coverage in a different directory
        tdir = mktempdir() # a dir that contains no code
        @test readchomp(`$exename -E "(Base.JLOptions().code_coverage, unsafe_string(Base.JLOptions().tracked_path))" -L $inputfile
            --code-coverage=$covfile --code-coverage=@$tdir`) == "(3, $(repr(tdir)))"
        @test isfile(covfile)
        got = read(covfile, String)
        @test isempty(got)
        rm(covfile)
    end

    # --track-allocation
    @test readchomp(`$exename -E "Base.JLOptions().malloc_log != 0"`) == "false"
    @test readchomp(`$exename -E "Base.JLOptions().malloc_log != 0" --track-allocation=none`) == "false"

    @test readchomp(`$exename -E "Base.JLOptions().malloc_log != 0" --track-allocation`) == "true"
    @test readchomp(`$exename -E "Base.JLOptions().malloc_log != 0" --track-allocation=user`) == "true"
    mktempdir() do dir
        helperdir = joinpath(@__DIR__, "testhelpers")
        inputfile = joinpath(dir, "allocation_file.jl")
        cp(joinpath(helperdir,"allocation_file.jl"), inputfile)
        pid = readchomp(`$exename -E "getpid()" -L $inputfile --track-allocation=user`)
        memfile = "$inputfile.$pid.mem"
        got = readlines(memfile)
        rm(memfile)
        @test popfirst!(got) == "        0 g(x) = x + 123456"
        @test popfirst!(got) == "        - function f(x)"
        @test popfirst!(got) == "        -     []"
        if Sys.WORD_SIZE == 64
            # P64 pools with 64 bit tags
            @test popfirst!(got) == "       16     Base.invokelatest(g, 0)"
            @test popfirst!(got) == "       32     Base.invokelatest(g, x)"
        elseif 12 == (() -> @allocated ccall(:jl_gc_allocobj, Ptr{Cvoid}, (Csize_t,), 8))()
            # See if we have a 12-byte pool with 32 bit tags (MAX_ALIGN = 4)
            @test popfirst!(got) == "       12     Base.invokelatest(g, 0)"
            @test popfirst!(got) == "       24     Base.invokelatest(g, x)"
        else # MAX_ALIGN >= 8
            @test popfirst!(got) == "        8     Base.invokelatest(g, 0)"
            @test popfirst!(got) == "       32     Base.invokelatest(g, x)"
        end
        if Sys.WORD_SIZE == 64
            @test popfirst!(got) == "       48     []"
        else
            @test popfirst!(got) == "       32     []"
        end
        @test popfirst!(got) == "        - end"
        @test popfirst!(got) == "        - f(1.23)"
        @test isempty(got) || got
    end


    # --optimize
    @test readchomp(`$exename -E "Base.JLOptions().opt_level"`) == "2"
    @test readchomp(`$exename -E "Base.JLOptions().opt_level" -O`) == "3"
    @test readchomp(`$exename -E "Base.JLOptions().opt_level" --optimize`) == "3"
    @test readchomp(`$exename -E "Base.JLOptions().opt_level" -O0`) == "0"

    @test readchomp(`$exename -E "Base.JLOptions().opt_level_min"`) == "0"
    @test readchomp(`$exename -E "Base.JLOptions().opt_level_min" --min-optlevel=2`) == "2"

    # -g
    @test readchomp(`$exename -E "Base.JLOptions().debug_level" -g`) == "2"
    let code = writereadpipeline("code_llvm(stdout, +, (Int64, Int64), raw=true, dump_module=true)", `$exename -g0`)
        @test code[2]
        code = code[1]
        @test occursin("llvm.module.flags", code)
        @test !occursin("llvm.dbg.cu", code)
        @test !occursin("int.jl", code)
        @test !occursin("Int64", code)
    end
    let code = writereadpipeline("code_llvm(stdout, +, (Int64, Int64), raw=true, dump_module=true)", `$exename -g1`)
        @test code[2]
        code = code[1]
        @test occursin("llvm.module.flags", code)
        @test occursin("llvm.dbg.cu", code)
        @test occursin("int.jl", code)
        @test !occursin("Int64", code)
    end
    let code = writereadpipeline("code_llvm(stdout, +, (Int64, Int64), raw=true, dump_module=true)", `$exename -g2`)
        @test code[2]
        code = code[1]
        @test occursin("llvm.module.flags", code)
        @test occursin("llvm.dbg.cu", code)
        @test occursin("int.jl", code)
        @test occursin("\"Int64\"", code)
    end

    # --check-bounds
    let JL_OPTIONS_CHECK_BOUNDS_DEFAULT = 0,
        JL_OPTIONS_CHECK_BOUNDS_ON = 1,
        JL_OPTIONS_CHECK_BOUNDS_OFF = 2
        exename_default_checkbounds = `$exename`
        filter!(a -> !startswith(a, "--check-bounds="), exename_default_checkbounds.exec)
        @test parse(Int, readchomp(`$exename_default_checkbounds -E "Int(Base.JLOptions().check_bounds)"`)) ==
            JL_OPTIONS_CHECK_BOUNDS_DEFAULT
        @test parse(Int, readchomp(`$exename -E "Int(Base.JLOptions().check_bounds)"
            --check-bounds=auto`)) == JL_OPTIONS_CHECK_BOUNDS_DEFAULT
        @test parse(Int, readchomp(`$exename -E "Int(Base.JLOptions().check_bounds)"
            --check-bounds=yes`)) == JL_OPTIONS_CHECK_BOUNDS_ON
        @test parse(Int, readchomp(`$exename -E "Int(Base.JLOptions().check_bounds)"
            --check-bounds=no`)) == JL_OPTIONS_CHECK_BOUNDS_OFF
    end
    # check-bounds takes yes/no as argument
    @test errors_not_signals(`$exename -E "exit(0)" --check-bounds=false`)

    # --depwarn
    @test readchomp(`$exename --depwarn=no  -E "Base.JLOptions().depwarn"`) == "0"
    @test readchomp(`$exename --depwarn=yes -E "Base.JLOptions().depwarn"`) == "1"
    @test errors_not_signals(`$exename --depwarn=false`)
    # test deprecated syntax
    @test errors_not_signals(`$exename -e "foo (x::Int) = x * x" --depwarn=error`)
    # test deprecated method
    @test errors_not_signals(`$exename -e "
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

        @test errors_not_signals(`$exename -E "$code" --depwarn=error`)

        @test readchomperrors(`$exename -E "$code" --depwarn=yes`) ==
            (true, "true", "WARNING: Foo.Deprecated is deprecated, use NotDeprecated instead.\n  likely near none:8")

        @test readchomperrors(`$exename -E "$code" --depwarn=no`) ==
            (true, "true", "")
    end

    # --inline
    @test readchomp(`$exename -E "Bool(Base.JLOptions().can_inline)"`) == "true"
    @test readchomp(`$exename --inline=yes -E "Bool(Base.JLOptions().can_inline)"`) == "true"
    @test readchomp(`$exename --inline=no -E "Bool(Base.JLOptions().can_inline)"`) == "false"
    # --inline takes yes/no as argument
    @test errors_not_signals(`$exename --inline=false`)

    # --polly
    @test readchomp(`$exename -E "Bool(Base.JLOptions().polly)"`) == "true"
    @test readchomp(`$exename --polly=yes -E "Bool(Base.JLOptions().polly)"`) == "true"
    @test readchomp(`$exename --polly=no -E "Bool(Base.JLOptions().polly)"`) == "false"
    # --polly takes yes/no as argument
    @test errors_not_signals(`$exename --polly=false`)

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
            "Int(Base.JLOptions().fast_math)"`)) == JL_OPTIONS_FAST_MATH_DEFAULT
    end

    # --worker takes default / custom as argument (default/custom arguments
    # tested in test/parallel.jl)
    @test errors_not_signals(`$exename --worker=true`)

    # test passing arguments
    mktempdir() do dir
        testfile, io = mktemp(dir)
        # write a julia source file that just prints ARGS to stdout
        write(io, """
            println(ARGS)
            """)
        close(io)
        mkpath(joinpath(dir, "config"))
        cp(testfile, joinpath(dir, "config", "startup.jl"))

        withenv("JULIA_DEPOT_PATH" => dir) do
            output = "[\"foo\", \"-bar\", \"--baz\"]"
            @test readchomp(`$exename $testfile foo -bar --baz`) == output
            @test readchomp(`$exename -- $testfile foo -bar --baz`) == output
            @test readchomp(`$exename -L $testfile -e 'exit(0)' -- foo -bar --baz`) ==
                output
            @test readchomp(`$exename --startup-file=yes -e 'exit(0)' -- foo -bar --baz`) ==
                output

            output = "[\"foo\", \"--\", \"-bar\", \"--baz\"]"
            @test readchomp(`$exename $testfile foo -- -bar --baz`) == output
            @test readchomp(`$exename -- $testfile foo -- -bar --baz`) == output
            @test readchomp(`$exename -L $testfile -e 'exit(0)' foo -- -bar --baz`) ==
                output
            @test readchomp(`$exename -L $testfile -e 'exit(0)' -- foo -- -bar --baz`) ==
                output
            @test readchomp(`$exename --startup-file=yes -e 'exit(0)' foo -- -bar --baz`) ==
                output

            output = "String[]\nString[]"
            @test readchomp(`$exename -L $testfile $testfile`) == output
            @test readchomp(`$exename --startup-file=yes $testfile`) == output

            @test errors_not_signals(`$exename --foo $testfile`)
        end
    end

    # test the program name remains constant
    mktempdir() do dir
        # dir can be case-incorrect sometimes
        dir = realpath(dir)

        a = joinpath(dir, "a.jl")
        b = joinpath(dir, "b.jl")
        c = joinpath(dir, "config", "startup.jl")

        write(a, """
            println(@__FILE__)
            println(PROGRAM_FILE)
            include(\"$(escape_string(b))\")
            """)
        write(b, """
            println(@__FILE__)
            println(PROGRAM_FILE)
            """)
        mkpath(dirname(c))
        cp(b, c)

        readsplit(cmd) = split(readchomp(cmd), '\n')

        withenv("JULIA_DEPOT_PATH" => dir) do
            @test readsplit(`$exename $a`) ==
                [a, a,
                 b, a]
            @test readsplit(`$exename -L $b -e 'exit(0)'`) ==
                [b, ""]
            @test readsplit(`$exename -L $b $a`) ==
                [b, a,
                 a, a,
                 b, a]
            @test readsplit(`$exename --startup-file=yes -e 'exit(0)'`) ==
                [c, ""]
            @test readsplit(`$exename --startup-file=yes -L $b -e 'exit(0)'`) ==
                [c, "",
                 b, ""]
            @test readsplit(`$exename --startup-file=yes -L $b $a`) ==
                [c, a,
                 b, a,
                 a, a,
                 b, a]
        end
    end

    # issue #10562
    @test readchomp(`$exename -e 'println(ARGS);' ''`) == "[\"\"]"

    # issue #12679
    @test readchomperrors(`$exename --startup-file=no --compile=yes -ioo`) ==
        (false, "", "ERROR: unknown option `-o`")
    @test readchomperrors(`$exename --startup-file=no -p`) ==
        (false, "", "ERROR: option `-p/--procs` is missing an argument")
    @test readchomperrors(`$exename --startup-file=no --inline`) ==
        (false, "", "ERROR: option `--inline` is missing an argument")
    @test readchomperrors(`$exename --startup-file=no -e "@show ARGS" -now -- julia RUN.jl`) ==
        (false, "", "ERROR: unknown option `-n`")

    # --compiled-modules={yes|no}
    @test readchomp(`$exename -E "Bool(Base.JLOptions().use_compiled_modules)"`) == "true"
    @test readchomp(`$exename --compiled-modules=yes -E
        "Bool(Base.JLOptions().use_compiled_modules)"`) == "true"
    @test readchomp(`$exename --compiled-modules=no -E
        "Bool(Base.JLOptions().use_compiled_modules)"`) == "false"
    @test errors_not_signals(`$exename --compiled-modules=foo -e "exit(0)"`)

    # issue #12671, starting from a non-directory
    # rm(dir) fails on windows with Permission denied
    # and was an upstream bug in llvm <= v3.3
    if !Sys.iswindows() && Base.libllvm_version > v"3.3"
        testdir = mktempdir()
        cd(testdir) do
            rm(testdir)
            @test Base.current_project() === nothing
            @test success(`$exename -e "exit(0)"`)
            for load_path in ["", "@", "@."]
                withenv("JULIA_LOAD_PATH" => load_path) do
                    @test success(`$exename -e "exit(!(Base.load_path() == []))"`)
                end
            end
        end
    end
end


# Find the path of libjulia (or libjulia-debug, as the case may be)
# to use as a dummy shlib to open
libjulia = if Base.DARWIN_FRAMEWORK
    abspath(Libdl.dlpath(Base.DARWIN_FRAMEWORK_NAME *
        (ccall(:jl_is_debugbuild, Cint, ()) != 0 ? "_debug" : "")))
else
    abspath(Libdl.dlpath((ccall(:jl_is_debugbuild, Cint, ()) != 0) ? "libjulia-debug" : "libjulia"))
end


# test error handling code paths of running --sysimage
let exename = `$(Base.julia_cmd().exec[1]) -t 1`
    sysname = unsafe_string(Base.JLOptions().image_file)
    for nonexist_image in (
            joinpath(@__DIR__, "nonexistent"),
            "$sysname.nonexistent",
            )
        let err = Pipe(),
            p = run(pipeline(`$exename --sysimage=$nonexist_image`, stderr=err), wait=false)
            close(err.in)
            let s = read(err, String)
                @test occursin("ERROR: could not load library \"$nonexist_image\"\n", s)
                @test !occursin("Segmentation fault", s)
                @test !occursin("EXCEPTION_ACCESS_VIOLATION", s)
            end
            @test errors_not_signals(p)
            @test p.exitcode == 1
        end
    end
    let err = Pipe(),
        p = run(pipeline(`$exename --sysimage=$libjulia`, stderr=err), wait=false)
        close(err.in)
        let s = read(err, String)
            @test s == "ERROR: System image file failed consistency check: maybe opened the wrong version?\n"
        end
        @test errors_not_signals(p)
        @test p.exitcode == 1
    end
end

let exename = Base.julia_cmd()
    # --startup-file
    let JL_OPTIONS_STARTUPFILE_ON = 1,
        JL_OPTIONS_STARTUPFILE_OFF = 2
        # `JULIA_DEPOT_PATH=$tmpdir` to avoid errors in the user startup.jl, which hangs the tests. Issue #17642
        mktempdir() do tmpdir
            withenv("JULIA_DEPOT_PATH"=>tmpdir) do
                @test parse(Int,readchomp(`$exename -E "Base.JLOptions().startupfile" --startup-file=yes`)) == JL_OPTIONS_STARTUPFILE_ON
            end
        end
        @test parse(Int,readchomp(`$exename -E "Base.JLOptions().startupfile"
            --startup-file=no`)) == JL_OPTIONS_STARTUPFILE_OFF
    end
    @test errors_not_signals(`$exename --startup-file=false`)
end

# Make sure `julia --lisp` doesn't break
run(pipeline(devnull, `$(joinpath(Sys.BINDIR, Base.julia_exename())) --lisp`, devnull))

# Test that `julia [some other option] --lisp` is disallowed
@test readchomperrors(`$(joinpath(Sys.BINDIR, Base.julia_exename())) -Cnative --lisp`) ==
    (false, "", "ERROR: --lisp must be specified as the first argument")

# --sysimage-native-code={yes|no}
let exename = `$(Base.julia_cmd()) --startup-file=no`
    @test readchomp(`$exename --sysimage-native-code=yes -E
        "Bool(Base.JLOptions().use_sysimage_native_code)"`) == "true"
    @test readchomp(`$exename --sysimage-native-code=no -E
        "Bool(Base.JLOptions().use_sysimage_native_code)"`) == "false"
end

# backtrace contains line number info (esp. on windows #17179)
for precomp in ("yes", "no")
    succ, out, bt = readchomperrors(`$(Base.julia_cmd()) --startup-file=no --sysimage-native-code=$precomp -E 'sqrt(-2)'`)
    @test !succ
    @test out == ""
    @test occursin(r"\.jl:(\d+)", bt)
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

# issue #6310
let exename = `$(Base.julia_cmd()) --startup-file=no --color=no`
    @test writereadpipeline("2+2", exename) == ("4\n", true)
    @test writereadpipeline("2+2\n3+3\n4+4", exename) == ("4\n6\n8\n", true)
    @test writereadpipeline("", exename) == ("", true)
    @test writereadpipeline("print(2)", exename) == ("2", true)
    @test writereadpipeline("print(2)\nprint(3)", exename) == ("23", true)
    let infile = tempname()
        touch(infile)
        try
            @test read(pipeline(exename, stdin=infile), String) == ""
            write(infile, "(1, 2+3)")
            @test read(pipeline(exename, stdin=infile), String) == "(1, 5)\n"
            write(infile, "1+2\n2+2\n1-2\n")
            @test read(pipeline(exename, stdin=infile), String) == "3\n4\n-1\n"
            write(infile, "print(2)")
            @test read(pipeline(exename, stdin=infile), String) == "2"
            write(infile, "print(2)\nprint(3)")
            @test read(pipeline(exename, stdin=infile), String) == "23"
        finally
            rm(infile)
        end
    end
end

# incomplete inputs to stream REPL
let exename = `$(Base.julia_cmd()) --startup-file=no --color=no`
    in = Pipe(); out = Pipe(); err = Pipe()
    proc = run(pipeline(exename, stdin = in, stdout = out, stderr = err), wait=false)
    write(in, "f(\n")
    close(in)
    close(err.in)
    txt = readline(err)
    @test startswith(txt, "ERROR: syntax: incomplete")
end

# Issue #29855
for yn in ("no", "yes")
    exename = `$(Base.julia_cmd()) --inline=no --startup-file=no --color=no --inline=$yn`
    v = writereadpipeline("Base.julia_cmd()", exename)
    if yn == "no"
        @test occursin(r" --inline=no", v[1])
    else
        @test !occursin(" --inline", v[1])
    end
    @test v[2]
end

# issue #39259, shadowing `ARGS`
@test success(`$(Base.julia_cmd()) --startup-file=no -e 'ARGS=1'`)

@testset "- as program file reads from stdin" begin
    for args in (`- foo bar`, `-- - foo bar`)
        cmd = `$(Base.julia_cmd()) --startup-file=no $(args)`
        io = IOBuffer()
        open(cmd, io; write=true) do proc
            write(proc, """
                println(PROGRAM_FILE)
                println(@__FILE__)
                foreach(println, ARGS)
            """)
        end
        lines = collect(eachline(seekstart(io)))
        @test lines[1] == "-"
        @test lines[2] == "stdin"
        @test lines[3] == "foo"
        @test lines[4] == "bar"
    end
end
