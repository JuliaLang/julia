include("../perfutil.jl")

include("indexing.jl")

briefname(A) = typeof(A).name.name
briefname(A::Base.Reshaped.ReshapedArray) = string(typeof(A).name.name, '{', typeof(A.parent).name.name, '}')

# Small array tests
sz = (3,5)
Alist = makearrays(Int, sz)
for Ar in Alist
    @timeit sumelt(Ar, 10^5) string("sumeltIs ", briefname(Ar)) string("for a in A indexing, ", briefname(Ar)) sz
    @timeit sumeach(Ar, 10^5) string("sumeachIs ", briefname(Ar)) string("for I in eachindex(A), ", briefname(Ar)) sz
    @timeit sumfast(Ar, 10^5) string("sumfastIs ", briefname(Ar)) string("for I in fastindex(A), ", briefname(Ar)) sz
end

Alist = makearrays(Float32, sz)   # SIMD-able
for Ar in Alist
    @timeit sumelt(Ar, 10^5) string("sumeltFs ", briefname(Ar)) string("for a in A indexing, ", briefname(Ar)) sz
    @timeit sumeach(Ar, 10^5) string("sumeachFs ", briefname(Ar)) string("for I in eachindex(A), ", briefname(Ar)) sz
    @timeit sumfast(Ar, 10^5) string("sumfastFs ", briefname(Ar)) string("for I in fastindex(A), ", briefname(Ar)) sz
end

# Big array tests
sz = (300,500)
Alist = makearrays(Int, sz)
for Ar in Alist
    @timeit sumelt(Ar, 100) string("sumeltIb ", briefname(Ar)) string("for a in A indexing, ", briefname(Ar)) sz
    @timeit sumeach(Ar, 100) string("sumeachIb ", briefname(Ar)) string("for I in eachindex(A), ", briefname(Ar)) sz
    @timeit sumfast(Ar, 100) string("sumfastIb ", briefname(Ar)) string("for I in fastindex(A), ", briefname(Ar)) sz
end

Alist = makearrays(Float32, sz)   # SIMD-able
for Ar in Alist
    @timeit sumelt(Ar, 100) string("sumeltFb ", briefname(Ar)) string("for a in A indexing, ", briefname(Ar)) sz
    @timeit sumeach(Ar, 100) string("sumeachFb ", briefname(Ar)) string("for I in eachindex(A), ", briefname(Ar)) sz
    @timeit sumfast(Ar, 100) string("sumfastFb ", briefname(Ar)) string("for I in fastindex(A), ", briefname(Ar)) sz
end
