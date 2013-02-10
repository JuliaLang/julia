@test median([1.]) == 1.
@test median([1.,3]) == 2.
@test median([1.,3,2]) == 2.

@test median([1,3,2]) == 2.0
@test median([1,3,2,4]) == 2.5

@test median([0.0,Inf]) == Inf
@test median([0.0,-Inf]) == -Inf
@test median([0.,Inf,-Inf]) == 0.0
@test median([1.,-1.,Inf,-Inf]) == 0.0
@test isnan(median([-Inf,Inf]))

@test_fails median([])
@test_fails median([NaN])
@test_fails median([0.0,NaN])
@test_fails median([NaN,0.0])

@test mean([1,2,3]) == 2.
@test var([1,2,3]) == 1.
@test std([1,2,3]) == 1.
@test hist([1,2,3],10) == [1,0,0,0,0,1,0,0,0,1]
@test hist([1,2,3],[0,2,4]) == [1,2,0]

@test quantile([1,2,3,4],0.5) == 2.5
@test quantile([1., 3],[.25,.5,.75])[2] == median([1., 3])
@test quantile([0.:100.],[.1,.2,.3,.4,.5,.6,.7,.8,.9])[1] == 10.0

# Test covariance
X = [1 0; 2 1; 3 0; 4 1; 5 10]
y = [5, 3, 4, 2, 5]
@assert_approx_eq cov(X[:,1], X[:,2]) cov(X)[1,2]
@assert issym(cov(X))
