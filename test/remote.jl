# Check that serializer hasn't gone out-of-frame
@test Base._jl_ser_tag[Symbol] == 2
@test Base._jl_ser_tag[()] == 49
@test Base._jl_ser_tag[false] == 125
