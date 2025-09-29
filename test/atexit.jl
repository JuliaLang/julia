# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

@testset "atexit.jl" begin
    function _atexit_tests_gen_cmd_eval(expr::String)
        # We run the atexit tests with 2 threads, for the parallelism tests at the end.
        cmd_eval = ```
        $(Base.julia_cmd()) -t2 -e $(expr)
        ```
        return cmd_eval
    end
    function _atexit_tests_gen_cmd_script(temp_dir::String, expr::String)
        script, io = mktemp(temp_dir)
        println(io, expr)
        close(io)
        # We run the atexit tests with 2 threads, for the parallelism tests at the end.
        cmd_script = ```
        $(Base.julia_cmd()) -t2 $(script)
        ```
        return cmd_script
    end
    atexit_temp_dir = mktempdir()
    atexit(() -> rm(atexit_temp_dir; force = true, recursive = true))
    @testset "these should exit with exit code 0" begin
        julia_expr_list = Dict(
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> exit(0))
            exit(22)
            """ => 0,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(exitcode -> exitcode > 10 && exit(0))
            exit(22)
            """ => 0,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            )
        for julia_expr in keys(julia_expr_list)
            cmd_eval = _atexit_tests_gen_cmd_eval(julia_expr)
            cmd_script = _atexit_tests_gen_cmd_script(atexit_temp_dir, julia_expr)
            expected_exit_code = julia_expr_list[julia_expr]
            @test success(cmd_eval)
            @test success(cmd_script)
            p_eval = run(cmd_eval; wait = false)
            p_script = run(cmd_script; wait = false)
            wait(p_eval)
            wait(p_script)
            @test p_eval.exitcode == expected_exit_code
            @test p_script.exitcode == expected_exit_code
        end
    end
    @testset "these should exit with exit code 1" begin
        julia_expr_list = Dict(
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> exit(1))
            exit(22)
            """ => 1,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> ("No error"))
            atexit(() -> exit(1))
            exit(22)
            """ => 1,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> exit(1))
            atexit(() -> exit(1))
            exit(22)
            """ => 1,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            )
        for julia_expr in keys(julia_expr_list)
            cmd_eval = _atexit_tests_gen_cmd_eval(julia_expr)
            cmd_script = _atexit_tests_gen_cmd_script(atexit_temp_dir, julia_expr)
            expected_exit_code = julia_expr_list[julia_expr]
            @test_throws ProcessFailedException run(cmd_eval)
            @test_throws ProcessFailedException run(cmd_script)
            p_eval = run(cmd_eval; wait = false)
            p_script = run(cmd_script; wait = false)
            wait(p_eval)
            wait(p_script)
            @test p_eval.exitcode == expected_exit_code
            @test p_script.exitcode == expected_exit_code
        end
    end
    @testset "test exit codes other than 0 or 1" begin
        julia_expr_list = Dict(
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> exit(13))
            exit(22)
            """ => 13,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(exitcode -> exit(exitcode+3))
            exit(22)
            """ => 25,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> ("No error"))
            atexit(() -> exit(5))
            exit(22)
            """ => 5,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> exit(33))
            atexit(() -> exit(33))
            exit(22)
            """ => 33,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> exit(21))
            atexit(() -> exit(21))
            exit(22)
            """ => 21,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            )
        for julia_expr in keys(julia_expr_list)
            cmd_eval = _atexit_tests_gen_cmd_eval(julia_expr)
            cmd_script = _atexit_tests_gen_cmd_script(atexit_temp_dir, julia_expr)
            expected_exit_code = julia_expr_list[julia_expr]
            @test_throws(ProcessFailedException, run(cmd_eval))
            @test_throws(ProcessFailedException, run(cmd_script))
            p_eval = run(cmd_eval; wait = false)
            p_script = run(cmd_script; wait = false)
            wait(p_eval)
            wait(p_script)
            @test p_eval.exitcode == expected_exit_code
            @test p_script.exitcode == expected_exit_code
        end
    end
    @testset "test what happens if multiple places call exit(n)" begin
        julia_expr_list = Dict(
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> exit(22))
            atexit(() -> exit(11))
            atexit(() -> exit(33))
            """ => 22,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> exit(4))
            atexit(() -> exit(16))
            atexit(() -> exit(7))
            exit(22)
            """ => 4,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(() -> exit(21))
            atexit(exitcode -> exit(exitcode+3))
            exit(22)
            """ => 21,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            """
            atexit(exitcode -> exit(exitcode+3))
            atexit(() -> exit(21))
            exit(22)
            """ => 24,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            )
        for julia_expr in keys(julia_expr_list)
            cmd_eval = _atexit_tests_gen_cmd_eval(julia_expr)
            cmd_script = _atexit_tests_gen_cmd_script(atexit_temp_dir, julia_expr)
            expected_exit_code = julia_expr_list[julia_expr]
            @test_throws(ProcessFailedException, run(cmd_eval))
            @test_throws(ProcessFailedException, run(cmd_script))
            p_eval = run(cmd_eval; wait = false)
            p_script = run(cmd_script; wait = false)
            wait(p_eval)
            wait(p_script)
            @test p_eval.exitcode == expected_exit_code
            @test p_script.exitcode == expected_exit_code
        end
    end
    @testset "test calling atexit() in parallel with running atexit hooks." begin
        # These tests cover 3 parallelism cases, as described by the following comments.
        julia_expr_list = Dict(
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            # 1. registering a hook from inside a hook
            """
            atexit() do
                atexit() do
                    exit(11)
                end
            end
            # This will attempt to exit 0, but the execution of the atexit hook will
            # register another hook, which will exit 11.
            exit(0)
            """ => 11,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            # 2. registering a hook from another thread while hooks are running
            """
            c = Channel()
            # This hook must execute _last_. (Execution is LIFO.)
            atexit() do
                put!(c, nothing)
                put!(c, nothing)
            end
            atexit() do
                # This will run in a concurrent task, testing that we can register atexit
                # hooks from another task while running atexit hooks.
                Threads.@spawn begin
                    take!(c) # block on c
                    atexit() do
                        exit(11)
                    end
                    take!(c) # keep the _atexit() loop alive until we've added another item.
                end
            end
            exit(0)
            """ => 11,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            # 3. attempting to register a hook after all hooks have finished (disallowed)
            """
            const atexit_has_finished = Threads.Atomic{Int}(0)
            atexit() do
                Threads.@spawn begin
                    # Block until the atexit hooks have all finished. We use a manual "spin
                    # lock" because task switch is disallowed inside the finalizer, below.
                    atexit_has_finished[] = 1
                    while atexit_has_finished[] == 1; GC.safepoint(); end
                    try
                        # By the time this runs, all the atexit hooks will be done.
                        # So this will throw.
                        atexit() do
                            exit(11)
                        end
                    catch
                        # Meaning we _actually_ exit 22.
                        exit(22)
                    end
                end
                while atexit_has_finished[] == 0; GC.safepoint(); end
            end
            # Finalizers run after the atexit hooks, so this blocks exit until the spawned
            # task above gets a chance to run.
            x = []
            finalizer(x) do x
                # Allow the spawned task to finish
                atexit_has_finished[] = 2
                # Then spin forever to prevent exit.
                while atexit_has_finished[] == 2; GC.safepoint(); end
            end
            exit(0)
            """ => 22,
            # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            )
        for julia_expr in keys(julia_expr_list)
            cmd_eval = _atexit_tests_gen_cmd_eval(julia_expr)
            cmd_script = _atexit_tests_gen_cmd_script(atexit_temp_dir, julia_expr)
            expected_exit_code = julia_expr_list[julia_expr]
            @test_throws(ProcessFailedException, run(cmd_eval))
            @test_throws(ProcessFailedException, run(cmd_script))
            p_eval = run(cmd_eval; wait = false)
            p_script = run(cmd_script; wait = false)
            wait(p_eval)
            wait(p_script)
            @test p_eval.exitcode == expected_exit_code
            @test p_script.exitcode == expected_exit_code
        end
    end
    rm(atexit_temp_dir; force = true, recursive = true)
end
