@test rand() != rand()
@test 0.0 <= rand() < 1.0
@test rand(Uint32) >= 0
@test -10 <= rand(-10:-5) <= -5
@test -10 <= rand(-10:5) <= 5
@test min([rand(int32(1):int32(7^7)) for i = 1:100000]) > 0
