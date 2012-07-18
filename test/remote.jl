# Check that serializer hasn't gone out-of-frame
@assert _jl_ser_tag[Symbol] == 2
@assert _jl_ser_tag[()] == 49
@assert _jl_ser_tag[false] == 125
