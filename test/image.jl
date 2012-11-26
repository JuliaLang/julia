require("../extras/image")

all_close(ar, v) = all(abs(ar-v) .< sqrt(eps(v)))

@assert all_close(imfilter(ones(4,4), ones(3,3)), 9.0)
@assert all_close(imfilter(ones(3,3), ones(3,3)), 9.0)
@assert all_close(imfilter(ones(3,3), [1 1 1;1 0.0 1;1 1 1]), 8.0)
