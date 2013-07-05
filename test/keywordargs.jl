# simple keyword args case
kwf1(ones; tens=0, hundreds=0) = ones + 10*tens + 100*hundreds
@test kwf1(2) == 2
@test kwf1(2, hundreds=6) == 602
@test kwf1(2, tens=6) == 62
@test kwf1(1, hundreds=2, tens=7) == 271
@test kwf1(3, tens=7, hundreds=2) == 273

@test_throws kwf1()        # no method, too few args
@test_throws kwf1(1, z=0)  # unsupported keyword
@test_throws kwf1(1, 2)    # no method, too many positional args

# keyword args plus varargs
kwf2(x, rest...; y=1) = (x, y, rest)
@test isequal(kwf2(0), (0, 1, ()))
@test isequal(kwf2(0,1,2), (0, 1, (1,2)))
@test isequal(kwf2(0,1,2,y=88), (0, 88, (1,2)))
@test isequal(kwf2(0,y=88,1,2), (0, 88, (1,2)))
@test_throws kwf2(0, z=1)
@test_throws kwf2(y=1)

# keyword arg with declared type
kwf3(x; y::Float64 = 1.0) = x + y
@test kwf3(2) == 3.0
@test kwf3(2, y=3.0) == 5.0
@test_throws kwf3(2, y=3)  # wrong type keyword

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
@test_throws opaf1()
@test_throws opaf1(0,1,2,3,4)

# optional positional plus varargs
opaf2(a=1,rest...) = (a,rest)
@test isequal(opaf2(), (1,()))
@test isequal(opaf2(2), (2,()))
@test isequal(opaf2(2,3), (2,(3,)))

# optional positional plus keyword args
opkwf1(a=0,b=1;k=2) = (a,b,k)
@test isequal(opkwf1(), (0,1,2))
@test isequal(opkwf1(10), (10,1,2))
@test isequal(opkwf1(10,20), (10,20,2))
@test_throws opkwf1(10,20,30)
@test isequal(opkwf1(10,20,k=8), (10,20,8))
@test isequal(opkwf1(11;k=8),    (11, 1,8))
@test isequal(opkwf1(k=8),       ( 0, 1,8))

# dictionaries as keywords
@test kwf1(4; {:hundreds=>9, :tens=>5}...) == 954

# with inner function
let
    function kwf_maker()
        f(;k=0) = 2k+1
    end
    kwf5 = kwf_maker()
    @test kwf5() == 1
    @test kwf5(k=2) == 5
    @test_throws kwf5(1)
end

# with every feature!
extravagant_args(x,y=0,rest...;color="blue",kw...) =
    (x,y,rest,color,kwf1(6;tens=8,kw...))

@test isequal(extravagant_args(1), (1,0,(),"blue",86))
@test isequal(extravagant_args(1;hundreds=7), (1,0,(),"blue",786))
@test isequal(extravagant_args(1,2,3;{:color=>"red", :hundreds=>3}...),
              (1,2,(3,),"red",386))

# passing empty kw container to function with no kwargs
@test sin(1.0) == sin(1.0;{}...)

# passing junk kw container
@test_throws extravagant_args(1; {[]}...)
