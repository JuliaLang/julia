# This file is a part of Julia. License is MIT: https://julialang.org/license

# NOTES on range reduction
# [1] compute numbers near pi: http://www.cs.berkeley.edu/~wkahan/testpi/nearpi.c
# [2] range reduction: http://hal-ujm.ccsd.cnrs.fr/docs/00/08/69/04/PDF/RangeReductionIEEETC0305.pdf
# [3] precise addition, see Add22: http://ftp.nluug.nl/pub/os/BSD/FreeBSD/distfiles/crlibm/crlibm-1.0beta3.pdf

# Examples
# ΓΓ = 6411027962775774 / 2^45  # see [2] above, section 1.2
# julia> mod(ΓΓ, 2pi)    # "naive" way - easily wrong
# 7.105427357601002e-15
# julia> mod2pi(ΓΓ)      # using function provided here
# 2.475922546353431e-18
# Wolfram Alpha: mod(6411027962775774 / 2^45, 2pi)
# 2.475922546353430800060268586243862383453213646146648435... × 10^-18

# Test Cases. Each row contains: x and x mod 2pi (as from Wolfram Alpha)
# The values x are:
# -pi/2, pi/2, -pi, pi, 2pi, -2pi
#   (or rather, the Float64 approx to those numbers.
#   Thus, x mod pi will result in a small, but positive number)
# ΓΓ = 6411027962775774 / 2^47
#   from [2], section 1.2:
#   the Float64 greater than 8, and less than 2**63 − 1 closest to a multiple of π/4 is
#   Γ = 6411027962775774 / 2^48. We take ΓΓ = 2*Γ to get cancellation with pi/2 already
# 3.14159265359, -3.14159265359
# pi/16*k +/- 0.00001 for k in [-20:20] # to cover all quadrants
# numerators of continuous fraction approximations to pi
#   see http://oeis.org/A002485
#   (reason: for max cancellation, we want x = k*pi + eps for small eps, so x/k ≈ pi)

testCases = [
      -1.5707963267948966         4.71238898038469
       1.5707963267948966       1.5707963267948966
       -3.141592653589793       3.1415926535897936
        3.141592653589793        3.141592653589793
        6.283185307179586        6.283185307179586
       -6.283185307179586   2.4492935982947064e-16
          45.553093477052       1.5707963267948966
            3.14159265359            3.14159265359
           -3.14159265359       3.1415926535895866
      -3.9269808169872418        2.356204490192345
        -3.73063127613788       2.5525540310417068
      -3.5342817352885176        2.748903571891069
       -3.337932194439156        2.945253112740431
      -3.1415826535897935        3.141602653589793
      -2.9452331127404316        3.337952194439155
      -2.7488835718910694       3.5343017352885173
      -2.5525340310417075        3.730651276137879
       -2.356184490192345       3.9270008169872415
      -2.1598349493429834        4.123350357836603
      -1.9634854084936209        4.319699898685966
      -1.7671358676442588        4.516049439535328
      -1.5707863267948967         4.71239898038469
      -1.3744367859455346        4.908748521234052
      -1.1780872450961726        5.105098062083414
      -0.9817377042468104        5.301447602932776
      -0.7853881633974483       5.4977971437821385
      -0.5890386225480863          5.6941466846315
      -0.3926890816987242        5.890496225480862
      -0.1963395408493621       6.0868457663302244
                   1.0e-5                   1.0e-5
      0.19635954084936205      0.19635954084936205
       0.3927090816987241       0.3927090816987241
       0.5890586225480862       0.5890586225480862
       0.7854081633974482       0.7854081633974482
       0.9817577042468103       0.9817577042468103
       1.1781072450961723       1.1781072450961723
       1.3744567859455343       1.3744567859455343
       1.5708063267948964       1.5708063267948964
       1.7671558676442585       1.7671558676442585
       1.9635054084936205       1.9635054084936205
        2.159854949342982        2.159854949342982
       2.3562044901923445       2.3562044901923445
       2.5525540310417063       2.5525540310417063
       2.7489035718910686       2.7489035718910686
       2.9452531127404304       2.9452531127404304
       3.1416026535897927       3.1416026535897927
       3.3379521944391546       3.3379521944391546
        3.534301735288517        3.534301735288517
       3.7306512761378787       3.7306512761378787
        3.927000816987241        3.927000816987241
      -3.9270008169872415        2.356184490192345
      -3.7306512761378796        2.552534031041707
      -3.5343017352885173       2.7488835718910694
      -3.3379521944391555        2.945233112740431
       -3.141602653589793       3.1415826535897935
      -2.9452531127404313       3.3379321944391553
       -2.748903571891069       3.5342817352885176
       -2.552554031041707       3.7306312761378795
       -2.356204490192345       3.9269808169872418
       -2.159854949342983        4.123330357836603
      -1.9635054084936208       4.3196798986859655
      -1.7671558676442587        4.516029439535328
      -1.5708063267948966         4.71237898038469
      -1.3744567859455346        4.908728521234052
      -1.1781072450961725        5.105078062083414
      -0.9817577042468104        5.301427602932776
      -0.7854081633974483        5.497777143782138
      -0.5890586225480863        5.694126684631501
     -0.39270908169872415        5.890476225480862
     -0.19635954084936208        6.086825766330224
                  -1.0e-5        6.283175307179587
      0.19633954084936206      0.19633954084936206
      0.39268908169872413      0.39268908169872413
       0.5890386225480861       0.5890386225480861
       0.7853881633974482       0.7853881633974482
       0.9817377042468103       0.9817377042468103
       1.1780872450961724       1.1780872450961724
       1.3744367859455344       1.3744367859455344
       1.5707863267948965       1.5707863267948965
       1.7671358676442586       1.7671358676442586
       1.9634854084936206       1.9634854084936206
       2.1598349493429825       2.1598349493429825
       2.3561844901923448       2.3561844901923448
       2.5525340310417066       2.5525340310417066
        2.748883571891069        2.748883571891069
       2.9452331127404308       2.9452331127404308
        3.141582653589793        3.141582653589793
        3.337932194439155        3.337932194439155
        3.534281735288517        3.534281735288517
        3.730631276137879        3.730631276137879
       3.9269808169872413       3.9269808169872413
                     22.0       3.1504440784612404
                    333.0       6.2743640266615035
                    355.0       3.1416227979431572
                 103993.0        6.283166177843807
                 104348.0        3.141603668607378
                 208341.0        3.141584539271598
                 312689.0    2.9006993893361787e-6
                 833719.0       3.1415903406703767
               1.146408e6       3.1415932413697663
               4.272943e6        6.283184757600089
               5.419351e6       3.1415926917902683
              8.0143857e7        6.283185292406739
             1.65707065e8       3.1415926622445745
             2.45850922e8        3.141592647471728
             4.11557987e8    2.5367160519636766e-9
            1.068966896e9         3.14159265254516
            2.549491779e9    4.474494938161497e-10
            6.167950454e9        3.141592653440059
          1.4885392687e10   1.4798091093322177e-10
          2.1053343141e10         3.14159265358804
        1.783366216531e12    6.969482408757582e-13
        3.587785776203e12        3.141592653589434
        5.371151992734e12       3.1415926535901306
        8.958937768937e12        6.283185307179564
      1.39755218526789e14          3.1415926535898
      4.28224593349304e14       3.1415926535897927
     5.706674932067741e15    4.237546464512562e-16
     6.134899525417045e15        3.141592653589793
]

function testModPi()
    numTestCases = size(testCases,1)
    modFns = [mod2pi]
    xDivisors = [2pi]
    errsNew, errsOld = Vector{Float64}(), Vector{Float64}()
    for rowIdx in 1:numTestCases
        xExact = testCases[rowIdx,1]
        for colIdx in 1:1
            xSoln = testCases[rowIdx,colIdx+1]
            xDivisor = xDivisors[colIdx]
            modFn = modFns[colIdx]
            # 2. want: xNew := modFn(xExact)  ≈  xSoln  <--- this is the crucial bit, xNew close to xSoln
            # 3. know: xOld := mod(xExact,xDivisor) might be quite a bit off from xSoln - that's expected
            xNew = modFn(xExact)
            xOld = mod(xExact,xDivisor)

            newDiff  = abs(xNew - xSoln)  # should be zero, ideally (our new function)
            oldDiff  = abs(xOld - xSoln)  # should be zero in a perfect world, but often bigger due to cancellation
            oldDiff  = min(oldDiff, abs(xDivisor - oldDiff)) # we are being generous here:
            # if xOld happens to end up "on the wrong side of 0", eg
            # if xSoln = 3.14 (correct), but xOld reports 0.01,
            # we don't take the long way around the circle of 3.14 - 0.01, but the short way of 3.1415.. - (3.14 - 0.1)
            push!(errsNew,abs(newDiff))
            push!(errsOld,abs(oldDiff))
        end
    end
    sort!(errsNew)
    sort!(errsOld)
    totalErrNew = sum(errsNew)
    totalErrOld = sum(errsOld)
    @test totalErrNew ≈ 0.0
end
testModPi()

# 2pi
@test mod2pi(10) ≈ mod(10,2pi)
@test mod2pi(-10) ≈ mod(-10,2pi)
@test mod2pi(355) ≈ 3.1416227979431572
@test mod2pi(Int32(355)) ≈ 3.1416227979431572
@test mod2pi(355.0) ≈ 3.1416227979431572
@test mod2pi(355.0f0) ≈ 3.1416228f0
@test mod2pi(Int64(2)^60) == mod2pi(2.0^60)
@test_throws ArgumentError mod2pi(Int64(2)^60-1)

@testset "rem_pio2_kernel" begin
    # test worst case
    x = 6381956970095103.0 * 2.0^797
    a = setprecision(BigFloat, 4096) do
        rem(big(x), big(pi)/2, RoundNearest)
    end

    n, yrem = Base.Math.rem_pio2_kernel(x)
    y=yrem.hi+yrem.lo
    @test a-y<nextfloat(y)/2
    # The following has easy and hard cases in each interval. A hard case is one
    # where x ≈ k*pi/2 for some integer k.
    cases = [0.0, pi/6, # -π/4 <= x <= π/4
             2*pi/4-0.1, 2*pi/4, # -2π/4 <= x <= 2π/4
             3*pi/4-0.1, # -3π/4 <= x <= 3π/4
             pi-0.1, Float64(pi), # -4π/4 <= x <= 4π/4
             5*pi/4-0.1, # -5π/4 <= x <= 5π/4
             6*pi/4-0.1, 6*pi/4, # -6π/4 <= x <= 6π/4
             7*pi/4-0.1, # -7π/4 <= x <= 7π/4
             2*pi, 2*pi-0.1, # -8π/4 <= x <= 8π/4
             9*pi/4-0.1, # -9π/4 <= x <= 9π/4
             2.0^10*pi/4, # -2.0^20π/2 <= x <= 2.0^20π/2
             2.0^30*pi/4, # |x| >= 2.0^20π/2, idx < 0
             2.0^80*pi/4] # |x| >= 2.0^20π/2, idx > 0-0.22370138542135648

     # ieee754_rem_pio2_return contains the returned value from the ieee754_rem_pio2
     # function in openlibm: https://github.com/JuliaLang/openlibm/blob/0598080ca09468490a13ae393ba17d8620c1b201/src/e_rem_pio2.c
     ieee754_rem_pio2_return = [ 1.5707963267948966       1.5707963267948966;
                                 1.0471975511965979      -1.0471975511965979;
                                 0.10000000000000014     -0.10000000000000014;
                                 6.123233995736766e-17   -6.123233995736766e-17;
                                -0.6853981633974481       0.6853981633974481;
                                 0.10000000000000021     -0.10000000000000021;
                                 1.2246467991473532e-16  -1.2246467991473532e-16;
                                -0.6853981633974481       0.6853981633974481;
                                 0.09999999999999983     -0.09999999999999983;
                                 1.8369701987210297e-16  -1.8369701987210297e-16;
                                -0.6853981633974484       0.6853981633974484;
                                 2.4492935982947064e-16  -2.4492935982947064e-16;
                                 0.0999999999999999      -0.0999999999999999;
                                -0.6853981633974484       0.6853981633974484;
                                 3.135095805817224e-14   -3.135095805817224e-14;
                                 3.287386219680602e-8    -3.287386219680602e-8;
                                -0.1757159771004682       0.1757159771004682      ]'

    for (i, case) in enumerate(cases)
        # negative argument
        n, ret = Base.Math.rem_pio2_kernel(-case)
        ret_sum = ret.hi+ret.lo
        ulp_error = (ret_sum-ieee754_rem_pio2_return[1, i])/eps(ieee754_rem_pio2_return[1, i])
        @test ulp_error <= 0.5
        diff = Float64(mod(big(-case), big(pi)/2))-(ret.hi+ret.lo)
        @test abs(diff) in (0.0, 1.5707963267948966, 1.5707963267948968)
        # positive argument
        n, ret = Base.Math.rem_pio2_kernel(case)
        ret_sum = ret.hi+ret.lo
        ulp_error = (ret_sum-ieee754_rem_pio2_return[2, i])/eps(ieee754_rem_pio2_return[2, i])
        @test ulp_error <= 0.5
        diff = Float64(mod(big(case), big(pi)/2))-(ret.hi+ret.lo)
        @test abs(diff) in (0.0, 1.5707963267948966, 1.5707963267948968)
    end
end
@testset "rem_pio2_kernel and mod2pi" begin
    for int in (3632982096228748, 1135326194816)
        bignum = int*big(pi)/2+0.00001
        bigrem = rem(bignum, big(pi)/2, RoundDown)
        fnum = Float64(bignum)
        n, ret = Base.Math.rem_pio2_kernel(fnum)
        @test mod2pi(fnum) == (ret.hi+ret.lo)
    end
end
