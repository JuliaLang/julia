struct reffoo3
    c::Int
end

struct reffoo2
    b::reffoo3
end

mutable struct reffoo
    a::reffoo2
end

let x = reffoo(reffoo2(reffoo3(1)))
    x@a.b.c = 2
    @test x.a.b.c == 2
    (x@a.b.c)[] = 3
    @test x.a.b.c == 3
    let x = x@a.b
        x@c = 4
    end
    @test x.a.b.c == 4
    ya = @setfield(x.a, b.c = 5)
    @test ya.b.c == 5
    @test x.a.b.c == 4
    @test_throws ErrorException x.a@b.c = 2
    @test @setfield(x.a, b.c = 5).b.c == 5
end

let t = ntuple(identity, 4)
    r = Ref{typeof(t)}(t)
    r@[][2] = 1
    @test r[] == (1,1,3,4)
    @test @setfield(r, [][3] = 1)[] == (1,1,1,4)
    @test r[] == (1,1,3,4)
    @test_throws ErrorException t@[1]
    @test @setfield(t, [3] = 1) == (1,2,1,4)
end

let t = ntuple(i->reffoo2(reffoo3(i)), 3)
    r = Ref{typeof(t)}(t)
    r@[][2].b.c = 6
    @test r[][2].b.c == 6
end
