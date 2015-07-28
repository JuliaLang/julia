# This file is a part of Julia. License is MIT: http://julialang.org/license

## Testing functors and specialization-by-value

for op in (identity, abs, abs2, exp, log)
    @test Base.specialized_unary(op)(3) == Base.specialized_unary(x->op(x))(3) == op(3)
    @test Base.specialized_unary(op)(-5+im) == Base.specialized_unary(x->op(x))(-5+im) == op(-5+im)
end
for op in (+, -, *, /, \, div, ^, &, |)
    @test Base.specialized_binary(op)(2,10) == Base.specialized_binary((x,y)->op(x,y))(2,10) == op(2,10)
end

for op in (!, ~, identity)
    @test Base.specialized_bitwise_unary(op)(true)  == Base.specialized_bitwise_unary(x->op(x))(true)  == op(true)
    @test Base.specialized_bitwise_unary(op)(false) == Base.specialized_bitwise_unary(x->op(x))(false) == op(false)
end
@test Base.specialized_bitwise_unary(~)(0x123456789abcdef) == Base.specialized_bitwise_unary(x->~(x))(0x123456789abcdef) == ~(0x123456789abcdef)
@test Base.specialized_bitwise_unary(identity)(0x123456789abcdef) == Base.specialized_bitwise_unary(x->identity(x))(0x123456789abcdef) == (0x123456789abcdef)

for op in (&, *, min, |, max, $, !=, >=, ^, <=, ==, <, >)
    for p in (true, false), q in (true, false)
        @test Base.specialized_bitwise_binary(op)(p, q) == Base.specialized_bitwise_binary((x,y)->op(x,y))(p, q) == op(p, q)
    end
end

for t in (true, false), f in (true, false)
    functor = Base.BitFunctorUnary{t, f}()
    @test (functor(0b10) & 0b11) == Int(t)<<1 | Int(f)
end
for tt in (true, false), tf in (true, false), ft in (true, false), ff in (true, false)
    functor = Base.BitFunctorBinary{tt,tf,ft,ff}()
    @test (functor(0b1100, 0b1010) & 0b1111) == (Int(tt)<<3 | Int(tf)<<2 | Int(ft)<<1 | Int(ff))
end
