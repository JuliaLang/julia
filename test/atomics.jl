using Base.Test
using Base.Threading
using Base.Atomics

x = Atomic()

@threads all for i = 1:10000
    atomic_add!(x, 1)
end

@test x[] == 10000

