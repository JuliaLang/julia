macro assert_approx_eq(a, b)
    va, vb, diff, sdiff = gensym(4)
    quote
        local $va = $a
        local $vb = $b
        local $diff = abs($va - $vb)
        local $sdiff = strcat("|", $string(a), " - ", $string(b), "| < 1e-6")
        ($diff < 1e-6) ? nothing : error("assertion failed: ", $sdiff, "\n  ", $string(a), " = ", $va, "\n  ", $string(b), " = ", $vb)
    end
end

# besselj
@assert besselj(0,0) == 1
for i = 1:5
    @assert besselj(i,0) == 0
    @assert besselj(-i,0) == 0
end

j33 = besselj(3,3.)
@assert besselj(3,3) == j33
@assert besselj(-3,-3) == j33
@assert besselj(-3,3) == -j33
@assert besselj(3,-3) == -j33

j43 = besselj(4,3.)
@assert besselj(4,3) == j43
@assert besselj(-4,-3) == j43
@assert besselj(-4,3) == j43
@assert besselj(4,-3) == j43

@assert_approx_eq j33 0.309063
@assert_approx_eq j43 0.132034
@assert_approx_eq besselj(0.1, -0.4) 0.820422 + 0.266571im
@assert_approx_eq besselj(3.2, 1.3+0.6im) 0.0113531 + 0.0392772im
@assert_approx_eq besselj(1, 3im) 3.95337im
