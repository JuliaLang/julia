using Test

@testset "atexit.jl" begin
    function _atexit_tests_gen_cmd_eval(expr::String)
        cmd_eval = ```
        $(Base.julia_cmd()) --check-bounds=yes --code-coverage=all
        --inline=no --compiled-modules=no --eval $(expr)
        ```
        return cmd_eval
    end
    function _atexit_tests_gen_cmd_script(temp_dir::String, expr::String)
        script, io = mktemp(temp_dir)
        println(io, expr)
        close(io)
        cmd_script = ```
        $(Base.julia_cmd()) --check-bounds=yes --code-coverage=all
        --inline=no --compiled-modules=no $(script)
        ```
        return cmd_script
    end
    atexit_temp_dir = mktempdir()
    atexit(() -> rm(atexit_temp_dir; force = true, recursive = true))
    @testset "these should exit with exit code 0" begin
        julia_expr_list = Dict(
            """
            atexit(() -> println("No error"))
            """ => 0,
            """
            atexit(() -> println("No error"); exitcode=0)
            """ => 0,
            """
            atexit(() -> error("I throw an error but it gets ignored."))
            """ => 0,
            """
            atexit(() -> error("I throw an error but it gets ignored."); exitcode=0)
            """ => 0,
            """
            atexit(() -> println("No error"); exitcode=1)
            """ => 0,
            """
            atexit(() -> println("No error"))
            atexit(() -> println("No error"); exitcode=1)
            """ => 0,
            """
            atexit(() -> println("No error"); exitcode=0)
            atexit(() -> println("No error"); exitcode=1)
            """ => 0,
            """
            atexit(() -> error("I throw an error but it gets ignored."))
            atexit(() -> println("No error"); exitcode=1)
            """ => 0,
            """
            atexit(() -> error("I throw an error but it gets ignored."); exitcode=0)
            atexit(() -> println("No error"); exitcode=1)
            """ => 0,
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
            """
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=1)
            """ => 1,
            """
            atexit(() -> println("No error"))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=1)
            """ => 1,
            """
            atexit(() -> println("No error"); exitcode=false)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=1)
            """ => 1,
            """
            atexit(() -> error("I throw an error but it gets ignored."))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=1)
            """ => 1,
            """
            atexit(() -> error("I throw an error but it gets ignored."); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=1)
            """ => 1,
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
            """
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=13)
            """ => 13,
            """
            atexit(() -> println("No error"))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=5)
            """ => 5,
            """
            atexit(() -> println("No error"); exitcode=false)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=2)
            """ => 2,
            """
            atexit(() -> error("I throw an error but it gets ignored."))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=33)
            """ => 33,
            """
            atexit(() -> error("I throw an error but it gets ignored."); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=21)
            """ => 21,
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
    @testset "test what happens if multiple exit hooks throw exceptions" begin
        julia_expr_list = Dict(
            """
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=1)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=2)
            """ => 2,
            """
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=2)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=1)
            """ => 1,
            """
            atexit(() -> println("No error"))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=3)
            atexit(() -> println("No error"))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=4)
            """ => 4,
            """
            atexit(() -> println("No error"))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=4)
            atexit(() -> println("No error"))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=2)
            """ => 2,
            """
            atexit(() -> println("No error"); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=11)
            atexit(() -> println("No error"); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=21)
            atexit(() -> println("No error"); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=22)
            """ => 22,
            """
            atexit(() -> println("No error"); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=22)
            atexit(() -> println("No error"); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=11)
            atexit(() -> println("No error"); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=11)
            """ => 11,
            """
            atexit(() -> error("I throw an error but it gets ignored."))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=3)
            atexit(() -> error("I throw an error but it gets ignored."))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=7)
            atexit(() -> error("I throw an error but it gets ignored."))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=4)
            """ => 4,
            """
            atexit(() -> error("I throw an error but it gets ignored."))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=7)
            atexit(() -> error("I throw an error but it gets ignored."))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=4)
            atexit(() -> error("I throw an error but it gets ignored."))
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=3)
            """ => 3,
            """
            atexit(() -> error("I throw an error but it gets ignored."); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=9)
            atexit(() -> error("I throw an error but it gets ignored."); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=15)
            atexit(() -> error("I throw an error but it gets ignored."); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=2)
            """ => 2,
            """
            atexit(() -> error("I throw an error but it gets ignored."); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=2)
            atexit(() -> error("I throw an error but it gets ignored."); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=15)
            atexit(() -> error("I throw an error but it gets ignored."); exitcode=0)
            atexit(() -> error("I throw an error that changes the exit code."); exitcode=9)
            """ => 9,
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
    @testset "make sure you can't supply a `exitcode` larger than a Cint" begin
        if isdefined(Main, :Int64)
            julia_expr_list = [
                """
                atexit(() -> println("I don't follow the rules."); exitcode=typemax(Int64))
                """,
                """
                atexit(() -> println("I don't follow the rules."); exitcode=typemin(Int64))
                """,
                ]
            for julia_expr in julia_expr_list
                cmd_eval = _atexit_tests_gen_cmd_eval(julia_expr)
                cmd_script = _atexit_tests_gen_cmd_script(atexit_temp_dir, julia_expr)
                @test_throws ProcessFailedException run(cmd_eval)
                @test_throws ProcessFailedException run(cmd_script)
            end
        end
    end

    rm(atexit_temp_dir; force = true, recursive = true)
end
