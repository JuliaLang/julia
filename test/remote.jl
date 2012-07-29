# Check that serializer hasn't gone out-of-frame
@assert Base._jl_ser_tag[Symbol] == 2
@assert Base._jl_ser_tag[()] == 49
@assert Base._jl_ser_tag[false] == 125
