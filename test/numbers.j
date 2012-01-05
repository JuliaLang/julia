# basic booleans
@assert true
@assert !false
@assert !!true
@assert !!!false

@assert true  == true
@assert false == false
@assert true  != false
@assert false != true

@assert ~true == false
@assert ~false == true

@assert false & false == false
@assert true  & false == false
@assert false & true  == false
@assert true  & true  == true

@assert false | false == false
@assert true  | false == true
@assert false | true  == true
@assert true  | true  == true

@assert false $ false == false
@assert true  $ false == true
@assert false $ true  == true
@assert true  $ true  == false

# the bool operator
@assert bool(false) == false
@assert bool(true) == true
@assert bool(0) == false
@assert bool(1) == true
@assert bool(-1) == true
@assert bool(0.0) == false
@assert bool(1.0) == true
@assert bool(0.1) == true
@assert bool(-1.0) == true
@assert bool(ComplexPair(0,0)) == false
@assert bool(ComplexPair(1,0)) == true
@assert bool(ComplexPair(0,1)) == true
@assert bool(0//1) == false
@assert bool(1//1) == true
@assert bool(1//2) == true

# basic arithmetic
@assert 2+3 == 5
@assert 2.+3. == 5.
@assert 2*3 == 6
@assert 2.*3 == 6
@assert 2. * 3. == 6.
@assert min(1.0,1) == 1

# signs
@assert sign(1) == 1
@assert sign(-1) == -1
@assert sign(0) == 0
@assert sign(1.0) == 1
@assert sign(-1.0) == -1
@assert sign(0.0) == 0
@assert sign(-0.0) == 0
@assert sign( 1.0/0.0) == 1
@assert sign(-1.0/0.0) == -1
@assert sign(Inf) == 1
@assert sign(-Inf) == -1
@assert isequal(sign(NaN), NaN)
@assert isequal(sign(-NaN), NaN)
@assert sign(2//3) == 1
@assert sign(-2//3) == -1
@assert sign(0//1) == 0
@assert sign(-0//1) == 0
@assert sign(1//0) == 1
@assert sign(-1//0) == -1

@assert signbit(1) == 1
@assert signbit(-1) == -1
@assert signbit(0) == 1
@assert signbit(1.0) == 1
@assert signbit(-1.0) == -1
@assert signbit(0.0) == 1
@assert signbit(-0.0) == -1
@assert signbit(1.0/0.0) == 1
@assert signbit(-1.0/0.0) == -1
@assert signbit(Inf) == 1
@assert signbit(-Inf) == -1
@assert signbit(NaN) == 1
@assert signbit(-NaN) == -1
@assert signbit(2//3) == 1
@assert signbit(-2//3) == -1
@assert signbit(0//1) == 1
@assert signbit(-0//1) == 1
@assert signbit(1//0) == 1
@assert signbit(-1//0) == -1

@assert isnan(1)     == false
@assert isnan(1.0)   == false
@assert isnan(-1.0)  == false
@assert isnan(Inf)   == false
@assert isnan(-Inf)  == false
@assert isnan(NaN)   == true
@assert isnan(1//2)  == false
@assert isnan(-2//3) == false
@assert isnan(5//0)  == false
@assert isnan(-3//0) == false

@assert isinf(1)     == false
@assert isinf(1.0)   == false
@assert isinf(-1.0)  == false
@assert isinf(Inf)   == true
@assert isinf(-Inf)  == true
@assert isinf(NaN)   == false
@assert isinf(1//2)  == false
@assert isinf(-2//3) == false
@assert isinf(5//0)  == true
@assert isinf(-3//0) == true

@assert isfinite(1)     == true
@assert isfinite(1.0)   == true
@assert isfinite(-1.0)  == true
@assert isfinite(Inf)   == false
@assert isfinite(-Inf)  == false
@assert isfinite(NaN)   == false
@assert isfinite(1//2)  == true
@assert isfinite(-2//3) == true
@assert isfinite(5//0)  == false
@assert isfinite(-3//0) == false

@assert isequal(-Inf,-Inf)
@assert isequal(-1.0,-1.0)
@assert isequal(-0.0,-0.0)
@assert isequal(+0.0,+0.0)
@assert isequal(+1.0,+1.0)
@assert isequal(+Inf,+Inf)
@assert isequal(-NaN,-NaN)
@assert isequal(-NaN,+NaN)
@assert isequal(+NaN,-NaN)
@assert isequal(+NaN,+NaN)

@assert !isequal(-Inf,+Inf)
@assert !isequal(-1.0,+1.0)
@assert !isequal(-0.0,+0.0)
@assert !isequal(+0.0,-0.0)
@assert !isequal(+1.0,-1.0)
@assert !isequal(+Inf,-Inf)

@assert !isless(-Inf,-Inf)
@assert  isless(-Inf,-1.0)
@assert  isless(-Inf,-0.0)
@assert  isless(-Inf,+0.0)
@assert  isless(-Inf,+1.0)
@assert  isless(-Inf,+Inf)
@assert  isless(-Inf,-NaN)
@assert  isless(-Inf,+NaN)

@assert !isless(-1.0,-Inf)
@assert !isless(-1.0,-1.0)
@assert  isless(-1.0,-0.0)
@assert  isless(-1.0,+0.0)
@assert  isless(-1.0,+1.0)
@assert  isless(-1.0,+Inf)
@assert  isless(-1.0,-NaN)
@assert  isless(-1.0,+NaN)

@assert !isless(-0.0,-Inf)
@assert !isless(-0.0,-1.0)
@assert !isless(-0.0,-0.0)
@assert  isless(-0.0,+0.0)
@assert  isless(-0.0,+1.0)
@assert  isless(-0.0,+Inf)
@assert  isless(-0.0,-NaN)
@assert  isless(-0.0,+NaN)

@assert !isless(+0.0,-Inf)
@assert !isless(+0.0,-1.0)
@assert !isless(+0.0,-0.0)
@assert !isless(+0.0,+0.0)
@assert  isless(+0.0,+1.0)
@assert  isless(+0.0,+Inf)
@assert  isless(+0.0,-NaN)
@assert  isless(+0.0,+NaN)

@assert !isless(+1.0,-Inf)
@assert !isless(+1.0,-1.0)
@assert !isless(+1.0,-0.0)
@assert !isless(+1.0,+0.0)
@assert !isless(+1.0,+1.0)
@assert  isless(+1.0,+Inf)
@assert  isless(+1.0,-NaN)
@assert  isless(+1.0,+NaN)

@assert !isless(+Inf,-Inf)
@assert !isless(+Inf,-1.0)
@assert !isless(+Inf,-0.0)
@assert !isless(+Inf,+0.0)
@assert !isless(+Inf,+1.0)
@assert !isless(+Inf,+Inf)
@assert  isless(+Inf,-NaN)
@assert  isless(+Inf,+NaN)

@assert !isless(-NaN,-Inf)
@assert !isless(-NaN,-1.0)
@assert !isless(-NaN,-0.0)
@assert !isless(-NaN,+0.0)
@assert !isless(-NaN,+1.0)
@assert !isless(-NaN,+Inf)
@assert !isless(-NaN,-NaN)
@assert !isless(-NaN,+NaN)

@assert !isless(+NaN,-Inf)
@assert !isless(+NaN,-1.0)
@assert !isless(+NaN,-0.0)
@assert !isless(+NaN,+0.0)
@assert !isless(+NaN,+1.0)
@assert !isless(+NaN,+Inf)
@assert !isless(+NaN,-NaN)
@assert !isless(+NaN,+NaN)

for x=-5:5, y=-5:5
    @assert (x==y)==(float64(x)==int64(y))
    @assert (x!=y)==(float64(x)!=int64(y))
    @assert (x< y)==(float64(x)< int64(y))
    @assert (x> y)==(float64(x)> int64(y))
    @assert (x<=y)==(float64(x)<=int64(y))
    @assert (x>=y)==(float64(x)>=int64(y))

    @assert (x==y)==(int64(x)==float64(y))
    @assert (x!=y)==(int64(x)!=float64(y))
    @assert (x< y)==(int64(x)< float64(y))
    @assert (x> y)==(int64(x)> float64(y))
    @assert (x<=y)==(int64(x)<=float64(y))
    @assert (x>=y)==(int64(x)>=float64(y))

    if x < 0 || y < 0; continue; end

    @assert (x==y)==(float64(x)==uint64(y))
    @assert (x!=y)==(float64(x)!=uint64(y))
    @assert (x< y)==(float64(x)< uint64(y))
    @assert (x> y)==(float64(x)> uint64(y))
    @assert (x<=y)==(float64(x)<=uint64(y))
    @assert (x>=y)==(float64(x)>=uint64(y))

    @assert (x==y)==(uint64(x)==float64(y))
    @assert (x!=y)==(uint64(x)!=float64(y))
    @assert (x< y)==(uint64(x)< float64(y))
    @assert (x> y)==(uint64(x)> float64(y))
    @assert (x<=y)==(uint64(x)<=float64(y))
    @assert (x>=y)==(uint64(x)>=float64(y))
end

function _cmp_(x::Union(Int64,Uint64), y::Float64)
    if x==int64(2)^53-2 && y==2.0^53-2; return  0; end
    if x==int64(2)^53-2 && y==2.0^53-1; return -1; end
    if x==int64(2)^53-2 && y==2.0^53  ; return -1; end
    if x==int64(2)^53-2 && y==2.0^53+2; return -1; end
    if x==int64(2)^53-2 && y==2.0^53+3; return -1; end
    if x==int64(2)^53-2 && y==2.0^53+4; return -1; end

    if x==int64(2)^53-1 && y==2.0^53-2; return +1; end
    if x==int64(2)^53-1 && y==2.0^53-1; return  0; end
    if x==int64(2)^53-1 && y==2.0^53  ; return -1; end
    if x==int64(2)^53-1 && y==2.0^53+2; return -1; end
    if x==int64(2)^53-1 && y==2.0^53+3; return -1; end
    if x==int64(2)^53-1 && y==2.0^53+4; return -1; end

    if x==int64(2)^53   && y==2.0^53-2; return +1; end
    if x==int64(2)^53   && y==2.0^53-1; return +1; end
    if x==int64(2)^53   && y==2.0^53  ; return  0; end
    if x==int64(2)^53   && y==2.0^53+2; return -1; end
    if x==int64(2)^53   && y==2.0^53+4; return -1; end

    if x==int64(2)^53+1 && y==2.0^53-2; return +1; end
    if x==int64(2)^53+1 && y==2.0^53-1; return +1; end
    if x==int64(2)^53+1 && y==2.0^53  ; return +1; end
    if x==int64(2)^53+1 && y==2.0^53+2; return -1; end
    if x==int64(2)^53+1 && y==2.0^53+4; return -1; end

    if x==int64(2)^53+2 && y==2.0^53-2; return +1; end
    if x==int64(2)^53+2 && y==2.0^53-1; return +1; end
    if x==int64(2)^53+2 && y==2.0^53  ; return +1; end
    if x==int64(2)^53+2 && y==2.0^53+2; return  0; end
    if x==int64(2)^53+2 && y==2.0^53+4; return -1; end

    if x==int64(2)^53+3 && y==2.0^53-2; return +1; end
    if x==int64(2)^53+3 && y==2.0^53-1; return +1; end
    if x==int64(2)^53+3 && y==2.0^53  ; return +1; end
    if x==int64(2)^53+3 && y==2.0^53+2; return +1; end
    if x==int64(2)^53+3 && y==2.0^53+4; return -1; end

    if x==int64(2)^53+4 && y==2.0^53-2; return +1; end
    if x==int64(2)^53+4 && y==2.0^53-1; return +1; end
    if x==int64(2)^53+4 && y==2.0^53  ; return +1; end
    if x==int64(2)^53+4 && y==2.0^53+2; return +1; end
    if x==int64(2)^53+4 && y==2.0^53+4; return  0; end

    error("invalid: _cmp_($x,$y)")
end

for x=int64(2)^53-2:int64(2)^53+4, y=int64(2)^53-2:int64(2)^53+3
    # println("x=$x; y=float64($y);")
    y = float64(y)
    c = _cmp_(x,y)
    if c < 0
        @assert !(x == y)
        @assert !(y == x)
        # @assert  (x <  y)
        # @assert !(y <  x)
        # @assert  (x <= y)
        # @assert !(y <= x)
    elseif c > 0
        @assert !(x == y)
        @assert !(y == x)
        # @assert !(x <  y)
        # @assert  (y <  x)
        # @assert !(x <= y)
        # @assert  (y <= x)
    else
        @assert  (x == y)
        @assert  (y == x)
        # @assert !(x <  y)
        # @assert !(y <  x)
        # @assert  (x <= y)
        # @assert  (y <= x)
    end
end

@assert int64(2)^62-1 != 2.0^62
@assert int64(2)^62   == 2.0^62
@assert int64(2)^62+1 != 2.0^62
@assert 2.0^62 != int64(2)^62-1
@assert 2.0^62 == int64(2)^62
@assert 2.0^62 != int64(2)^62+1

@assert typemax(Int64)   != +2.0^63
@assert typemin(Int64)   == -2.0^63
@assert typemin(Int64)+1 != -2.0^63

@assert uint64(2)^60-1 != 2.0^60
@assert uint64(2)^60   == 2.0^60
@assert uint64(2)^60+1 != 2.0^60
@assert 2.0^60 != uint64(2)^60-1
@assert 2.0^60 == uint64(2)^60
@assert 2.0^60 != uint64(2)^60+1

@assert uint64(2)^63-1 != 2.0^63
@assert uint64(2)^63   == 2.0^63
@assert uint64(2)^63+1 != 2.0^63
@assert 2.0^63 != uint64(2)^63-1
@assert 2.0^63 == uint64(2)^63
@assert 2.0^63 != uint64(2)^63+1

@assert typemax(Uint64) != 2.0^64

@assert 1//1 == 1
@assert 2//2 == 1
@assert 1//1 == 1//1
@assert 2//2 == 1//1
@assert 2//4 == 3//6
@assert 1//2 + 1//2 == 1
@assert (-1)//3 == -(1//3)
@assert 1//2 + 3//4 == 5//4
@assert 1//3 * 3//4 == 1//4
@assert 1//2 / 3//4 == 2//3
@assert 1//0 == 1//0
@assert 5//0 == 1//0
@assert -1//0 == -1//0
@assert -7//0 == -1//0

for a = -5:5, b = -5:5
    if a == b == 0; continue; end
    @assert a//b == a/b
    @assert a//b == a//b
    @assert a//b == rational(a/b)
    @assert integer(a//b) == integer(a/b)
    for c = -5:5
        @assert (a//b == c) == (a/b == c)
        @assert (a//b != c) == (a/b != c)
        @assert (a//b <= c) == (a/b <= c)
        @assert (a//b <  c) == (a/b <  c)
        @assert (a//b >= c) == (a/b >= c)
        @assert (a//b >  c) == (a/b >  c)
        for d = -5:5
            if c == d == 0; continue; end
            @assert (a//b == c//d) == (a/b == c/d)
            @assert (a//b != c//d) == (a/b != c/d)
            @assert (a//b <= c//d) == (a/b <= c/d)
            @assert (a//b <  c//d) == (a/b <  c/d)
            @assert (a//b >= c//d) == (a/b >= c/d)
            @assert (a//b >  c//d) == (a/b >  c/d)
        end
    end
end

@assert sqrt(2) == 1.4142135623730951

@assert 1+1.5 == 2.5
@assert 1.5+1 == 2.5
@assert 1+1.5+2 == 4.5
@assert is(typeof(convert(ComplexPair{Int16},1)),ComplexPair{Int16})
@assert ComplexPair(1,2)+1 == ComplexPair(2,2)
@assert ComplexPair(1,2)+1.5 == ComplexPair(2.5,2.0)
@assert 1/ComplexPair(2,2) == ComplexPair(.25,-.25)
@assert ComplexPair(1.5,1.0) + 1//2 == ComplexPair(2.0,1.0)
@assert real(ComplexPair(1//2,2//3)) == 1//2
@assert imag(ComplexPair(1//2,2//3)) == 2//3
@assert ComplexPair(1,2) + 1//2 == ComplexPair(3//2,2//1)
@assert ComplexPair(1,2) + 1//2 * 0.5 == ComplexPair(1.25,2.0)
@assert (ComplexPair(1,2) + 1//2) * 0.5 == ComplexPair(0.75,1.0)
@assert (ComplexPair(1,2)/ComplexPair(2.5,3.0))*ComplexPair(2.5,3.0) == ComplexPair(1,2)
@assert 0.7 < real(sqrt(ComplexPair(0,1))) < 0.707107

for S = {Int8,  Int16,  Int32,  Int64},
    U = {Uint8, Uint16, Uint32, Uint64}
    @assert !(-one(S) == typemax(U))
    @assert -one(S) != typemax(U)
    @assert -one(S) < typemax(U)
    @assert !(typemax(U) <= -one(S))
end

# check type of constructed rationals
int_types = {Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64}
for N = int_types, D = int_types
    T = promote_type(N,D)
    @assert typeof(convert(N,2)//convert(D,3)) <: Rational{T}
end

# check type of constructed complexes
real_types = {Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64, Float32, Float64,
              Rational{Int8}, Rational{Uint8}, Rational{Int16}, Rational{Uint16},
              Rational{Int32}, Rational{Uint32}, Rational{Int64}, Rational{Uint64}}
for A = real_types, B = real_types
    T = promote_type(A,B)
    @assert typeof(ComplexPair(convert(A,2),convert(B,3))) <: ComplexPair{T}
end

# div, fld, rem, mod
for nr = {
    Range1(1,6),
    Range(0.25,0.25,6.0),
    Range(1//4,1//4,6//1)
}, ar = {
    Range1(0,6),
    Range(0.0,0.25,6.0),
    Range(0//1,1//4,6//1)
}
    for n = nr, a = ar
        # check basic div functionality
        if 0n <= a < 1n
            @assert div(+a,+n) == 0
            @assert div(+a,-n) == 0
            @assert div(-a,+n) == 0
            @assert div(-a,-n) == 0
        end
        if 1n <= a < 2n
            @assert div(+a,+n) == +1
            @assert div(+a,-n) == -1
            @assert div(-a,+n) == -1
            @assert div(-a,-n) == +1
        end
        if 2n <= a < 3n
            @assert div(+a,+n) == +2
            @assert div(+a,-n) == -2
            @assert div(-a,+n) == -2
            @assert div(-a,-n) == +2
        end

        # check basic fld functionality
        if 0n == a
            @assert fld(+a,+n) == 0
            @assert fld(+a,-n) == 0
            @assert fld(-a,+n) == 0
            @assert fld(-a,-n) == 0
        end
        if 0n < a < 1n
            @assert fld(+a,+n) == +0
            @assert fld(+a,-n) == -1
            @assert fld(-a,+n) == -1
            @assert fld(-a,-n) == +0
        end
        if 1n == a
            @assert fld(+a,+n) == +1
            @assert fld(+a,-n) == -1
            @assert fld(-a,+n) == -1
            @assert fld(-a,-n) == +1
        end
        if 1n < a < 2n
            @assert fld(+a,+n) == +1
            @assert fld(+a,-n) == -2
            @assert fld(-a,+n) == -2
            @assert fld(-a,-n) == +1
        end
        if 2n == a
            @assert fld(+a,+n) == +2
            @assert fld(+a,-n) == -2
            @assert fld(-a,+n) == -2
            @assert fld(-a,-n) == +2
        end
        if 2n < a < 3n
            @assert fld(+a,+n) == +2
            @assert fld(+a,-n) == -3
            @assert fld(-a,+n) == -3
            @assert fld(-a,-n) == +2
        end

        # check everything else in terms of div & fld
        d = div(a,n)
        f = fld(a,n)
        r = rem(a,n)
        m = mod(a,n)

        t1 = isa(a,Rational) && isa(n,Rational) ?
                               promote_type(typeof(num(a)),typeof(num(n))) :
             isa(a,Rational) ? promote_type(typeof(num(a)),typeof(n)) :
             isa(n,Rational) ? promote_type(typeof(a),typeof(num(n))) :
                               promote_type(typeof(a),typeof(n))

        t2 = promote_type(typeof(a),typeof(n))

        @assert typeof(d) <: t1
        @assert typeof(f) <: t1
        @assert typeof(r) <: t2
        @assert typeof(m) <: t2

        @assert d == f
        @assert r == m
        @assert 0 <= r < n
        @assert a == n*d + r

        for A=[-1,1], N=[-1,1]
            sa = A*a
            sn = N*n

            sd = div(sa,sn)
            sf = fld(sa,sn)
            sr = rem(sa,sn)
            sm = mod(sa,sn)

            @assert typeof(sd) <: t1
            @assert typeof(sf) <: t1
            @assert typeof(sr) <: t2
            @assert typeof(sm) <: t2

            @assert sa < 0 ? -n < sr <= 0 : 0 <= sr < +n
            @assert sn < 0 ? -n < sm <= 0 : 0 <= sm < +n
            @assert sa == sn*sd + sr
            @assert sa == sn*sf + sm
        end
    end
end
