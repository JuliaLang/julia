# Discrete cosine transform (DCT) tests

a = rand(8,11) + im*rand(8,11)
@test norm(idct(dct(a)) - a) < 1e-8

X = reshape([1,2,7,2,1,5,9,-1,3,4,6,9],3,4)
Y = rand(17,14)
Y[3:5,9:12] = X
sX = slice(Y,3:5,9:12)

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

pXdct = plan_dct(X)(X)
pXdct! = float(X); plan_dct!(pXdct!)(pXdct!)
pXdct_1 = plan_dct(X,1)(X)
pXdct!_1 = float(X); plan_dct!(pXdct!_1,1)(pXdct!_1)
pXdct_2 = plan_dct(X,2)(X)
pXdct!_2 = float(X); plan_dct!(pXdct!_2,2)(pXdct!_2)

pXidct = plan_idct(true_Xdct)(true_Xdct)
pXidct! = copy(true_Xdct); plan_idct!(pXidct!)(pXidct!)
pXidct_1 = plan_idct(true_Xdct_1,1)(true_Xdct_1)
pXidct!_1 = copy(true_Xdct_1); plan_idct!(pXidct!_1,1)(pXidct!_1)
pXidct_2 = plan_idct(true_Xdct_2,2)(true_Xdct_2)
pXidct!_2 = copy(true_Xdct_2); plan_idct!(pXidct!_2,2)(pXidct!_2)

sXdct = dct(sX)
psXdct = plan_dct(sX)(sX)
sYdct! = copy(Y); sXdct! = slice(sYdct!,3:5,9:12); dct!(sXdct!)
psYdct! = copy(Y); psXdct! = slice(psYdct!,3:5,9:12); plan_dct!(psXdct!)(psXdct!)


for i = 1:length(X)
    @test_approx_eq Xdct[i] true_Xdct[i]
    @test_approx_eq Xdct![i] true_Xdct[i]
    @test_approx_eq Xdct_1[i] true_Xdct_1[i]
    @test_approx_eq Xdct!_1[i] true_Xdct_1[i]
    @test_approx_eq Xdct_2[i] true_Xdct_2[i]
    @test_approx_eq Xdct!_2[i] true_Xdct_2[i]

    @test_approx_eq pXdct[i] true_Xdct[i]
    @test_approx_eq pXdct![i] true_Xdct[i]
    @test_approx_eq pXdct_1[i] true_Xdct_1[i]
    @test_approx_eq pXdct!_1[i] true_Xdct_1[i]
    @test_approx_eq pXdct_2[i] true_Xdct_2[i]
    @test_approx_eq pXdct!_2[i] true_Xdct_2[i]

    @test_approx_eq Xidct[i] X[i]
    @test_approx_eq Xidct![i] X[i]
    @test_approx_eq Xidct_1[i] X[i]
    @test_approx_eq Xidct!_1[i] X[i]
    @test_approx_eq Xidct_2[i] X[i]
    @test_approx_eq Xidct!_2[i] X[i]

    @test_approx_eq pXidct[i] X[i]
    @test_approx_eq pXidct![i] X[i]
    @test_approx_eq pXidct_1[i] X[i]
    @test_approx_eq pXidct!_1[i] X[i]
    @test_approx_eq pXidct_2[i] X[i]
    @test_approx_eq pXidct!_2[i] X[i]

    @test_approx_eq sXdct[i] true_Xdct[i]
    @test_approx_eq psXdct[i] true_Xdct[i]
    @test_approx_eq sXdct![i] true_Xdct[i]
    @test_approx_eq psXdct![i] true_Xdct[i]
end
