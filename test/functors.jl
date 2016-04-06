# This file is a part of Julia. License is MIT: http://julialang.org/license

## Testing functors and specialization-by-value

for op in (+, -, *, /, \, div, ^, &, |)
    @test Base.specialized_binary(op)(2,10) == Base.specialized_binary((x,y)->op(x,y))(2,10) == op(2,10)
end

for op in (&, *, min, |, max, $, !=, >=, ^, <=, ==, <, >)
    for p in (true, false), q in (true, false)
        @test Base.specialized_bitwise_binary(op)(p, q) == Base.specialized_bitwise_binary((x,y)->op(x,y))(p, q) == op(p, q)
    end
end

for tt in (true, false), tf in (true, false), ft in (true, false), ff in (true, false)
    functor = Base.BitFunctorBinary{tt,tf,ft,ff}()
    @test (functor(0b1100, 0b1010) & 0b1111) == (Int(tt)<<3 | Int(tf)<<2 | Int(ft)<<1 | Int(ff))
end
