using Test

import Base.Order: Forward, Reverse

# every argument can flip the integer order by passing the right value. Here,
# we enumerate a few of these combinations and check that all these flips
# compound so that in total we either have an increasing or decreasing sort.
for (s1, rev) in enumerate([true, false])
    for (s2, lt) in enumerate([>, <, (a, b) -> a - b > 0, (a, b) -> a - b < 0])
        for (s3, by) in enumerate([-, +])
            for (s4, order) in enumerate([Reverse, Forward])
                if iseven(s1 + s2 + s3 + s4)
                    target = [1, 2, 3]
                else
                    target = [3, 2, 1]
                end
                @test target == sort([2, 3, 1], rev=rev, lt=lt, by=by, order=order)
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

