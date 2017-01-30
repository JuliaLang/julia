# This file is a part of Julia. License is MIT: http://julialang.org/license

@test length(ExponentialBackOff(n=10)) == 10
@test collect(ExponentialBackOff(n=10, first_delay=0.01))[1] == 0.01
@test maximum(ExponentialBackOff(n=10, max_delay=0.06)) == 0.06
ratio(x) = x[2:end]./x[1:end-1]
@test all(x->x ≈ 10.0, ratio(collect(ExponentialBackOff(n=10, max_delay=Inf, factor=10, jitter=0.0))))
srand(12345)
@test (mean(ratio(collect(ExponentialBackOff(n=100, max_delay=Inf, factor=1, jitter=0.1)))) - 1.0) < 1e-4

let
    function foo_error(c, n)
        c[1] += 1
        if c[1] <= n
            error("foo")
        end
        return 7
    end

    # Success on first attempt
    c = [0]
    @test retry(foo_error)(c,0) == 7
    @test c[1] == 1

    # Success on second attempt
    c = [0]
    @test retry(foo_error)(c,1) == 7
    @test c[1] == 2

    # 2 failed retry attempts, so exception is raised
    c = [0]
    ex = try retry(foo_error, delays=ExponentialBackOff(n=2))(c,3) catch e; e end
    @test ex.msg == "foo"
    @test c[1] == 3

    c = [0]
    ex = try retry(foo_error, check=(s,e)->(s,isa(e, ErrorException)))(c,2) catch e; e end
    @test typeof(ex) == ErrorException
    @test ex.msg == "foo"
    @test c[1] == 2

    c = [0]
    ex = try retry(foo_error, check=(s,e)->(s,e.msg == "foo"))(c,2) catch e; e end
    @test typeof(ex) == ErrorException
    @test ex.msg == "foo"
    @test c[1] == 2

    # No retry if condition does not match
    c = [0]
    ex = try retry(foo_error, check=(s,e)->(s,e.msg == "bar"))(c,2) catch e; e end
    @test typeof(ex) == ErrorException
    @test ex.msg == "foo"
    @test c[1] == 1

    c = [0]
    ex = try retry(foo_error, check=(s,e)->(s,try e.http_status_code == "503" end != true))(c,2) catch e; e end
    @test typeof(ex) == ErrorException
    @test ex.msg == "foo"
    @test c[1] == 2

    c = [0]
    ex = try retry(foo_error, check=(s,e)->(s,isa(e,SystemError)))(c,2) catch e; e end
    @test typeof(ex) == ErrorException
    @test ex.msg == "foo"
    @test c[1] == 1
end
