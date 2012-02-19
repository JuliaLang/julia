# besselj
@assert besselj(0,0) == 1
for i = 1:5
    @assert besselj(i,0) == 0
    @assert besselj(-i,0) == 0
end

function assert_number_eq(a, b)
    @assert abs(a - b) < 1e-6
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

assert_number_eq(j33, 0.309063)
assert_number_eq(j43, 0.132034)
assert_number_eq(besselj(0.1, -0.4), 0.820422 + 0.266571im)
assert_number_eq(besselj(3.2, 1.3+0.6im), 0.0113531 + 0.0392772im)
assert_number_eq(besselj(1, 3im), 3.95337im)
