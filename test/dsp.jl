# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "dsp" begin
# Filter
b = [1., 2., 3., 4.]
x = [1., 1., 0., 1., 1., 0., 0., 0.]
@test filt(b, 1., x)  == [1., 3., 5., 8., 7., 5., 7., 4.]
@test filt(b, [1., -0.5], x)  == [1., 3.5, 6.75, 11.375, 12.6875, 11.34375, 12.671875, 10.3359375]
# With ranges
@test filt(b, 1., 1.0:10.0) == [1., 4., 10., 20., 30., 40., 50., 60., 70., 80.]
@test filt(1.:4., 1., 1.0:10.0) == [1., 4., 10., 20., 30., 40., 50., 60., 70., 80.]
# Across an array is the same as channel-by-channel
@test filt(b, 1., [x 1.0:8.0]) == [filt(b, 1., x) filt(b, 1., 1.0:8.0)]
@test filt(b, [1., -0.5], [x 1.0:8.0]) == [filt(b, [1., -0.5], x) filt(b, [1., -0.5], 1.0:8.0)]
si = zeros(3)
@test filt(b, 1., [x 1.0:8.0], si) == [filt(b, 1., x, si) filt(b, 1., 1.0:8.0, si)]
@test si == zeros(3) # Will likely fail if/when arrayviews are implemented
si = [zeros(3) ones(3)]
@test filt(b, 1., [x 1.0:8.0], si) == [filt(b, 1., x, zeros(3)) filt(b, 1., 1.0:8.0, ones(3))]
# With initial conditions: a lowpass 5-pole butterworth filter with W_n = 0.25,
# and a stable initial filter condition matched to the initial value
b = [0.003279216306360201,0.016396081531801006,0.03279216306360201,0.03279216306360201,0.016396081531801006,0.003279216306360201]
a = [1.0,-2.4744161749781606,2.8110063119115782,-1.703772240915465,0.5444326948885326,-0.07231566910295834]
si = [0.9967207836936347,-1.4940914728163142,1.2841226760316475,-0.4524417279474106,0.07559488540931815]
@test filt(b, a, ones(10), si) ≈ ones(10) # Shouldn't affect DC offset

@test_throws ArgumentError filt!([1, 2], [1], [1], [1])
@test xcorr([1, 2], [3, 4]) == [4, 11, 6]

@test fftshift([1 2 3]) == [3 1 2]
@test fftshift([1, 2, 3]) == [3, 1, 2]
@test fftshift([1 2 3; 4 5 6]) == [6 4 5; 3 1 2]
@test ifftshift([1 2 3]) == [2 3 1]
@test ifftshift([1, 2, 3]) == [2, 3, 1]
@test ifftshift([1 2 3; 4 5 6]) == [5 6 4; 2 3 1]

# Convolution
a = [1., 2., 1., 2.]
b = [1., 2., 3.]
@test conv(a, b) ≈ [1., 4., 8., 10., 7., 6.]
@test conv(complex(a, ones(4)), complex(b)) ≈ complex([1., 4., 8., 10., 7., 6.], [1., 3., 6., 6., 5., 3.])

# Discrete cosine transform (DCT) tests

if Base.fftw_vendor() != :mkl
    a = rand(8,11) + im*rand(8,11)
    @test norm(idct(dct(a)) - a) < 1e-8

    X = reshape([1,2,7,2,1,5,9,-1,3,4,6,9],3,4)
    Y = rand(17,14)
    Y[3:5,9:12] = X
    sX = view(Y,3:5,9:12)

    true_Xdct = [  13.856406460551018  -3.863239728836245   2.886751345948129  -0.274551994240164; -2.828427124746190  -2.184015211898548  -4.949747468305834   3.966116180118245; 4.898979485566356  -0.194137576915510  -2.857738033247041   2.731723009609389 ]

    true_Xdct_1 = [    5.773502691896258   4.618802153517007   6.350852961085884  10.969655114602890; -4.242640687119286  -2.121320343559643   4.242640687119286  -3.535533905932738; 1.632993161855452   2.041241452319315   5.715476066494083   0.408248290463863 ]

    true_Xdct_2 = [    8.  -3.854030797826254  -3.0  3.761176226848022;
        4.0  -2.071929829606556   4.0  -2.388955165168770; 12.  -0.765366864730179   4.0  -1.847759065022573 ]

    Xdct = dct(X)
    Xdct! = float(X); dct!(Xdct!)
    Xdct_1 = dct(X,1)
    Xdct!_1 = float(X); dct!(Xdct!_1,1)
    Xdct_2 = dct(X,2)
    Xdct!_2 = float(X); dct!(Xdct!_2,2)

    Xidct = idct(true_Xdct)
    Xidct! = copy(true_Xdct); idct!(Xidct!)
    Xidct_1 = idct(true_Xdct_1,1)
    Xidct!_1 = copy(true_Xdct_1); idct!(Xidct!_1,1)
    Xidct_2 = idct(true_Xdct_2,2)
    Xidct!_2 = copy(true_Xdct_2); idct!(Xidct!_2,2)

    pXdct = plan_dct(X)*(X)
    pXdct! = float(X); plan_dct!(pXdct!)*(pXdct!)
    pXdct_1 = plan_dct(X,1)*(X)
    pXdct!_1 = float(X); plan_dct!(pXdct!_1,1)*(pXdct!_1)
    pXdct_2 = plan_dct(X,2)*(X)
    pXdct!_2 = float(X); plan_dct!(pXdct!_2,2)*(pXdct!_2)

    pXidct = plan_idct(true_Xdct)*(true_Xdct)
    pXidct! = copy(true_Xdct); plan_idct!(pXidct!)*(pXidct!)
    pXidct_1 = plan_idct(true_Xdct_1,1)*(true_Xdct_1)
    pXidct!_1 = copy(true_Xdct_1); plan_idct!(pXidct!_1,1)*(pXidct!_1)
    pXidct_2 = plan_idct(true_Xdct_2,2)*(true_Xdct_2)
    pXidct!_2 = copy(true_Xdct_2); plan_idct!(pXidct!_2,2)*(pXidct!_2)

    sXdct = dct(sX)
    psXdct = plan_dct(sX)*(sX)
    sYdct! = copy(Y); sXdct! = view(sYdct!,3:5,9:12); dct!(sXdct!)
    psYdct! = copy(Y); psXdct! = view(psYdct!,3:5,9:12); plan_dct!(psXdct!)*(psXdct!)

    for i = 1:length(X)
        @test Xdct[i] ≈ true_Xdct[i]
        @test Xdct![i] ≈ true_Xdct[i]
        @test Xdct_1[i] ≈ true_Xdct_1[i]
        @test Xdct!_1[i] ≈ true_Xdct_1[i]
        @test Xdct_2[i] ≈ true_Xdct_2[i]
        @test Xdct!_2[i] ≈ true_Xdct_2[i]

        @test pXdct[i] ≈ true_Xdct[i]
        @test pXdct![i] ≈ true_Xdct[i]
        @test pXdct_1[i] ≈ true_Xdct_1[i]
        @test pXdct!_1[i] ≈ true_Xdct_1[i]
        @test pXdct_2[i] ≈ true_Xdct_2[i]
        @test pXdct!_2[i] ≈ true_Xdct_2[i]

        @test Xidct[i] ≈ X[i]
        @test Xidct![i] ≈ X[i]
        @test Xidct_1[i] ≈ X[i]
        @test Xidct!_1[i] ≈ X[i]
        @test Xidct_2[i] ≈ X[i]
        @test Xidct!_2[i] ≈ X[i]

        @test pXidct[i] ≈ X[i]
        @test pXidct![i] ≈ X[i]
        @test pXidct_1[i] ≈ X[i]
        @test pXidct!_1[i] ≈ X[i]
        @test pXidct_2[i] ≈ X[i]
        @test pXidct!_2[i] ≈ X[i]

        @test sXdct[i] ≈ true_Xdct[i]
        @test psXdct[i] ≈ true_Xdct[i]
        @test sXdct![i] ≈ true_Xdct[i]
        @test psXdct![i] ≈ true_Xdct[i]
    end
end # fftw_vendor() != :mkl
end
