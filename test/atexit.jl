# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

@testset "atexit.jl" begin
    function _atexit_tests_gen_cmd_eval(expr::String)
        cmd_eval = ```
        $(Base.julia_cmd()) -e $(expr)
        ```
        return cmd_eval
    end
    function _atexit_tests_gen_cmd_script(temp_dir::String, expr::String)
        script, io = mktemp(temp_dir)
        println(io, expr)
        close(io)
        cmd_script = ```
        $(Base.julia_cmd()) $(script)
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
