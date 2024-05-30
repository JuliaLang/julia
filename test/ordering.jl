# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

import Base.Order: Forward, Reverse, ord, Lt, By, ReverseOrdering

# every argument can flip the integer order by passing the right value. Here,
# we enumerate a few of these combinations and check that all these flips
# compound so that in total we either have an increasing or decreasing sort.
for (s1, rev) in enumerate([true, false])
    for (s2, lt) in enumerate([(a, b)->isless(b, a), isless, >, <, (a, b) -> a - b > 0, (a, b) -> a - b < 0])
        for (s3, by) in enumerate([-, +])
            for (s4, order) in enumerate([Reverse, Forward])
                is_fwd = iseven(s1 + s2 + s3 + s4)
                target = is_fwd ? (1:3) : (3:-1:1)
                # arrays, integer and float ranges sometimes have different code paths
                @test target == sort([2, 3, 1], rev=rev, lt=lt, by=by, order=order)

                @test target == sort(1:3, rev=rev, lt=lt, by=by, order=order)
                @test target == sort(3:-1:1, rev=rev, lt=lt, by=by, order=order)
                @test float(target) == sort(1.0:3, rev=rev, lt=lt, by=by, order=order)
                @test float(target) == sort(3.0:-1:1, rev=rev, lt=lt, by=by, order=order)
            end
        end
    end
end

@test [1 => 3, 2 => 5, 3 => 1] ==
            sort([1 => 3, 2 => 5, 3 => 1]) ==
            sort([1 => 3, 2 => 5, 3 => 1], by=first) ==
            sort([1 => 3, 2 => 5, 3 => 1], rev=true, order=Reverse) ==
            sort([1 => 3, 2 => 5, 3 => 1], lt= >, order=Reverse)

@test [3 => 1, 1 => 3, 2 => 5] ==
            sort([1 => 3, 2 => 5, 3 => 1], by=last) ==
            sort([1 => 3, 2 => 5, 3 => 1], by=last, rev=true, order=Reverse) ==
            sort([1 => 3, 2 => 5, 3 => 1], by=last, lt= >, order=Reverse)


struct SomeOtherOrder <: Base.Order.Ordering end

@test_throws ErrorException sort([1, 2, 3], lt=(a, b) -> a - b < 0, order=SomeOtherOrder())

@test reverse(Forward) === Reverse
@test reverse(Reverse) === Forward

@test ord(isless, identity, false, Forward) === Forward
@test ord(isless, identity, true, Forward) === Reverse
@test ord(<, identity, false, Forward) === Lt(<)
@test ord(isless, abs, false, Forward) === By(abs)
@test ord(<, abs, false, Forward) === By(abs, Lt(<))
@test ord(<, abs, true, Forward) === ReverseOrdering(By(abs, Lt(<)))
@test ord(<, abs, true, Reverse) === By(abs, Lt(<))

@testset "Base.Order docstrings" begin
    undoc = Docs.undocumented_names(Base.Order)
    @test_broken isempty(undoc)
    @test undoc == [:DirectOrdering, :ForwardOrdering, :Order, :ordtype]
end
