# simple keyword args case
kwf1(ones; tens=0, hundreds=0) = ones + 10*tens + 100*hundreds
@test kwf1(2) == 2
@test kwf1(2, hundreds=6) == 602
@test kwf1(2, tens=6) == 62
@test kwf1(1, hundreds=2, tens=7) == 271
@test kwf1(3, tens=7, hundreds=2) == 273

@test_fails kwf1()        # no method, too few args
@test_fails kwf1(1, z=0)  # unsupported keyword
@test_fails kwf1(1, 2)    # no method, too many positional args

# keyword args plus varargs
kwf2(x, rest...; y=1) = (x, y, rest)
@test isequal(kwf2(0), (0, 1, ()))
@test isequal(kwf2(0,1,2), (0, 1, (1,2)))
@test isequal(kwf2(0,1,2,y=88), (0, 88, (1,2)))
@test isequal(kwf2(0,y=88,1,2), (0, 88, (1,2)))
@test_fails kwf2(0, z=1)
@test_fails kwf2(y=1)

# keyword arg with declared type
kwf3(x; y::Float64 = 1.0) = x + y
@test kwf3(2) == 3.0
@test kwf3(2, y=3.0) == 5.0
@test_fails kwf3(2, y=3)  # wrong type keyword

# function with only keyword args
kwf4(;a=1,b=2) = (a,b)
@test isequal(kwf4(), (1,2))
@test isequal(kwf4(b=10), (1,10))

# rest keywords
kwdelegator(ones;kw...) = kwf1(ones;kw...)
@test kwdelegator(4,hundreds=8) == 804

# optional positional args
opaf1(a,b=1,c=2,d=3) = (a,b,c,d)
@test isequal(opaf1(0), (0,1,2,3))
@test isequal(opaf1(0,2), (0,2,2,3))
@test isequal(opaf1(0,2,4), (0,2,4,3))
@test isequal(opaf1(0,2,4,6), (0,2,4,6))
@test_fails opaf1()
@test_fails opaf1(0,1,2,3,4)

# optional positional plus varargs
opaf2(a=1,rest...) = (a,rest)
@test isequal(opaf2(), (1,()))
@test isequal(opaf2(2), (2,()))
@test isequal(opaf2(2,3), (2,(3,)))
