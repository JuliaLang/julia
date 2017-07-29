# This file is a part of Julia. License is MIT: https://julialang.org/license

function egal_svecs()
    a = Core.svec(:a, :b)
    b = Core.svec(:a, :b)
    a === b
end
@test egal_svecs()
@test Core.svec(:a, :b) === Core.svec(:a, :b)

# issue #22582
function issue22582!(a::AbstractArray, b)
    len = length(a)
    if b
        ccall(:jl_array_grow_end, Void, (Any, Csize_t), a, 1)
    end
    return len
end
let c = [1,2,3]
    len1 = length(c)
    len2 = issue22582!(c, true)
    @test len1 == len2
end
