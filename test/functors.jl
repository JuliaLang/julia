## Testing functors and Base.specialization-by-value

for op in (identity, ~, abs, abs2, exp, log)
    @test Base.specialized_unary(op)(1) == Base.specialized_unary(x->op(x))(1) == op(1)
end
for op in (+, *, &, |)
    @test Base.specialized_binary(op)(1,2) == Base.specialized_binary((x,y)->op(x,y))(1,2) == op(1,2)
end

for op in (!, ~, identity)
    @test Base.specialized_bitwise_unary(op)(true) == Base.specialized_bitwise_unary(x->op(x))(true) == op(true)
    @test Base.specialized_bitwise_unary(op)(false) == Base.specialized_bitwise_unary(x->op(x))(false) == op(false)
end
for op in (&, *, min, |, max, $, !=, >=, ^, <=, ==, <, >)
    @test Base.specialized_bitwise_binary(op)(true, true)   == Base.specialized_bitwise_binary((x,y)->op(x,y))(true, true)   == op(true, true)
    @test Base.specialized_bitwise_binary(op)(false, false) == Base.specialized_bitwise_binary((x,y)->op(x,y))(false, false) == op(false, false)
    @test Base.specialized_bitwise_binary(op)(true, false)  == Base.specialized_bitwise_binary((x,y)->op(x,y))(true, false)  == op(true, false)
    @test Base.specialized_bitwise_binary(op)(false, true)  == Base.specialized_bitwise_binary((x,y)->op(x,y))(false, true)  == op(false, true)
end
