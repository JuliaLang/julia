# ranges
@test size(10:1:0) == (0,)
@test length(1:.2:2) == 6
@test length(1.:.2:2.) == 6
@test length(2:-.2:1) == 6
@test length(2.:-.2:1.) == 6
@test length(2:.2:1) == 0
@test length(2.:.2:1.) == 0

@test length(1:0) == 0
@test length(0.0:-0.5) == 0
@test length(1:2:0) == 0
L32 = linspace(int32(1), int32(4), 4)
L64 = linspace(int64(1), int64(4), 4)
@test L32[1] == 1 && L64[1] == 1
@test L32[2] == 2 && L64[2] == 2
@test L32[3] == 3 && L64[3] == 3
@test L32[4] == 4 && L64[4] == 4

r = [5:-1:1]
@test r[1]==5
@test r[2]==4
@test r[3]==3
@test r[4]==2
@test r[5]==1

@test length(.1:.1:.3) == 3
@test length(1.1:1.1:3.3) == 3
@test length(1.1:1.3:3) == 2
@test length(1:1:1.8) == 1

let
    span = 5:20
    r = -7:3:42
    @test findin(r, span) == 5:10
    r = 15:-2:-38
    @test findin(r, span) == 1:6
end
@test isempty(findin(5+0*(1:6), 2:4))
@test findin(5+0*(1:6), 2:5) == 1:6
@test findin(5+0*(1:6), 2:7) == 1:6
@test findin(5+0*(1:6), 5:7) == 1:6
@test isempty(findin(5+0*(1:6), 6:7))
@test findin(5+0*(1:6), 5:5) == 1:6

@test intersect(1:5, 2:3) == 2:3
@test intersect(-3:5, 2:8) == 2:5
@test intersect(-8:-3, -8:-3) == -8:-3
@test intersect(1:5, 5:13) == 5:5
@test isempty(intersect(-8:-3, -2:2))
@test isempty(intersect(-3:7, 2:1))
@test intersect(1:11, -2:3:15) == 1:3:10
@test intersect(1:11, -2:2:15) == 2:2:10
@test intersect(1:11, -2:1:15) == 1:11
@test intersect(1:11, 15:-1:-2) == 1:11
@test intersect(1:11, 15:-4:-2) == 3:4:11
@test intersect(-20:-5, -10:3:-2) == -10:3:-7
@test isempty(intersect(-5:5, -6:13:20))
@test isempty(intersect(1:11, 15:4:-2))
@test isempty(intersect(11:1, 15:-4:-2))
@test intersect(-5:5, 1+0*(1:3)) == 1:1
@test isempty(intersect(-5:5, 6+0*(1:3)))
@test intersect(-15:4:7, -10:-2) == -7:4:-3
@test intersect(13:-2:1, -2:8) == 7:-2:1
@test isempty(intersect(13:2:1, -2:8))
@test isempty(intersect(13:-2:1, 8:-2))
@test intersect(5+0*(1:4), 2:8) == 5+0*(1:4)
@test isempty(intersect(5+0*(1:4), -7:3))
@test intersect(0:3:24, 0:4:24) == 0:12:24
@test intersect(0:4:24, 0:3:24) == 0:12:24
@test intersect(0:3:24, 24:-4:0) == 0:12:24
@test intersect(24:-3:0, 0:4:24) == 24:-12:0
@test intersect(24:-3:0, 24:-4:0) == 24:-12:0
@test intersect(1:3:24, 0:4:24) == 4:12:16
@test intersect(0:6:24, 0:4:24) == 0:12:24
@test isempty(intersect(1:6:2400, 0:4:2400))
@test intersect(-51:5:100, -33:7:125) == -26:35:79
@test intersect(-51:5:100, -32:7:125) == -11:35:94
@test intersect(0:6:24, 6+0*(0:4:24)) == 6:6:6
@test intersect(12+0*(0:6:24), 0:4:24) == Range(12, 0, 5)
@test isempty(intersect(6+0*(0:6:24), 0:4:24))
@test intersect(-10:3:24, -10:3:24) == -10:3:23
@test isempty(intersect(-11:3:24, -10:3:24))

@test !(3.5 in 1:5)
@test (3 in 1:5)
@test (3 in 5:-1:1)
@test (3 in 3+0*(1:5))
@test !(4 in 3+0*(1:5))

r = 0.0:0.01:1.0
@test (r[30] in r)
r = (-4*int64(maxintfloat(is(Int,Int32) ? Float32 : Float64))):5
@test (3 in r)
@test (3.0 in r)

# indexing range with empty range (#4309)
@test (3:6)[5:4] == 7:6
@test_throws (3:6)[5:5] BoundsError
@test_throws (3:6)[5] BoundsError
@test (0:2:10)[7:6] == 12:2:10
@test_throws (0:2:10)[7:7] BoundsError
