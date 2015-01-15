@test gcd(3, 5) == 1
@test gcd(3, 15) == 3
@test gcd(0, 15) == 15
@test gcd(3, -15) == 3
@test gcd(-3, -15) == 3
@test gcd(0, 0) == 0

@test gcd(2, 4, 6) == 2

@test typeof(gcd(int32(3), int32(15))) == Int # this returns Int32 on 0.4

@test lcm(2, 3) == 6
@test lcm(4, 6) == 12
@test lcm(3, 0) == 0
@test lcm(0, 0) == 0
@test lcm(4, -6) == 12
@test lcm(-4, -6) == 12

@test lcm(2, 4, 6) == 12

@test typeof(lcm(int32(2), int32(3))) == Int # this returns Int32 on 0.4

@test gcdx(5, 12) == (1, 5, -2)
@test gcdx(5, -12) == (1, 5, 2)
@test gcdx(-25, -4) == (1, -1, 6)

@test invmod(6, 31) == 26
@test invmod(-1, 3) == 2
@test invmod(-1, -3) == 2
@test_throws ErrorException invmod(0, 3)

@test powermod(2, 3, 5) == 3
@test powermod(2, 3, -5) == -2
@test_throws DomainError powermod(2, -3, 5)

@test nextpow2(3) == 4
@test nextpow(2, 3) == 4
@test nextpow(2, 4) == 4
@test nextpow(2, 7) == 8
@test_throws DomainError nextpow(0, 3)
@test_throws DomainError nextpow(3, 0)

@test prevpow2(3) == 2
@test prevpow(2, 4) == 4
@test prevpow(2, 5) == 4
@test_throws DomainError prevpow(0, 3)
@test_throws DomainError prevpow(0, 3)

@test bin(3) == "11"
@test bin(3, 2) == "11"
@test bin(3, 3) == "011"
@test bin(-3) == "-11"
@test bin(-3, 3) == "-011"

@test oct(9) == "11"
@test oct(-9) == "-11"

@test dec(121) == "121"

@test hex(12) == "c"
@test hex(-12, 3) == "-00c"
@test num2hex(1243) == (Int == Int32 ? "000004db" : "00000000000004db")

@test base(2, 5, 7) == "0000101"

@test bits(1035) == (Int == Int32 ? "00000000000000000000010000001011" :
    "0000000000000000000000000000000000000000000000000000010000001011")

@test digits(4, 2) == [0, 0, 1]
@test digits(5, 3) == [2, 1]

@test isqrt(4) == 2
@test isqrt(5) == 2
