using Base.Test
using LibFoo

@test call_fooifier(2.2, 1.1) ≈ 2*2.2^2 - 1.1
@test call_libfoo(2.2, 1.1) ≈ 2*2.2^2 - 1.1