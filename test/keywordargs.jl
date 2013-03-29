kwf1(ones; tens=0, hundreds=0) = ones + 10*tens + 100*hundreds
@test kwf1(2) == 2
@test kwf1(2, hundreds=6) == 602
@test kwf1(2, tens=6) == 62
@test kwf1(1, hundreds=2, tens=7) == 271
@test kwf1(3, tens=7, hundreds=2) == 273

@test_fails kwf1()        # no method, too few args
@test_fails kwf1(1, z=0)  # unsupported keyword
@test_fails kwf1(1, 2)    # no method, too many positional args

kwf2(x, rest...; y=1) = (x, y, rest)
@test isequal(kwf2(0), (0, 1, ()))
@test isequal(kwf2(0,1,2), (0, 1, (1,2)))
@test isequal(kwf2(0,1,2,y=88), (0, 88, (1,2)))
@test isequal(kwf2(0,y=88,1,2), (0, 88, (1,2)))
@test_fails kwf2(0, z=1)
@test_fails kwf2(y=1)

kwf3(x; y::Float64 = 1.0) = x + y
@test kwf3(2) == 3.0
@test kwf3(2, y=3.0) == 5.0
@test_fails kwf3(2, y=3)  # wrong type keyword
