stagedfunction t1(a,b)
    if a == Int64
        return :(a+b)
    else
        return :(a*b)
    end
end

@test t1(1,2) == 3
@test t1(1.0,0.5) == 0.5
@test t1(1,0.5) == 1.5

tinline(a,b) = t1(a,b)

@test !isa(tinline(1,2),Expr)
@test tinline(1,0.5) == 1.5