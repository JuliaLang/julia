# issue #8266

@test ndigits(-15, 10) == 2
@test ndigits(-15, -10) == 2
@test ndigits(-1, 10) == 1
@test ndigits(-1, -10) == 2
@test ndigits(2, 10) == 1
@test ndigits(2, -10) == 1
@test ndigits(10, 10) == 2
@test ndigits(10, -10) == 3
@test ndigits(17, 10) == 2
@test ndigits(17, -10) == 3
@test ndigits(unsigned(17), -10) == 3

@test ndigits(146, -3) == 5

