using Base.Experimental: ImmutableArray
using Test

function test_allocate1()
    a = Vector{Float64}(undef, 5)
    for i = 1:5
        a[i] = i
    end
    Core.ImmutableArray(a)
end

function test_allocate2()
    a = [1,2,3,4,5]
    Core.ImmutableArray(a)
end

function test_allocate3()
    a = Matrix{Float64}(undef, 5, 2)
    for i = 1:5
        for j = 1:2
            a[i, j] = i + j
        end
    end
    Core.ImmutableArray(a)
end

function test_allocate4()
    a = Core.ImmutableArray{Float64}(undef, 5)
end

function test_broadcast1()
    a = Core.ImmutableArray([1,2,3])
    typeof(a .+ a) <: Core.ImmutableArray
end

function test_allocate5() # test that throwing boundserror doesn't escape
    a = [1,2,3]
    try
        getindex(a, 4)
    catch end
    Core.ImmutableArray(a)
end

# function test_maybecopy1()
#     a = Vector{Int64}(undef, 5)
#     b = Core.maybecopy(a) # doesn't escape in this function - so a !=== b
#     @test !(a === b)
# end

# function test_maybecopy2()
#     a = Vector{Int64}(undef, 5)
#     try
#         a[6]
#     catch e
#         @test !(e.a === a)
#     end
# end

# function test_maybecopy3()
#     @noinline function escaper(arr)
#         return arr
#     end

#     a = Vector{Int64}(undef, 5)
#     escaper(a)
#     b = Core.maybecopy(a)
#     @test a === b # this time, it does escape, so we give back the actual object
# end

# function test_maybecopy4()
#     @noinline function escaper(arr)
#         return arr
#     end

#     a = Vector{Int64}(undef, 5)
#     escaper(a)
#     try
#         a[6]
#     catch e
#         if isa(e, BoundsError)
#             @test e.a === a # already escaped so we don't copy
#         end
#     end
# end




let
    # warmup for @allocated
    a,b,c,d,e = test_allocate1(), test_allocate2(), test_allocate3(), test_allocate4(), test_allocate5()

    # these magic values are ~ what the mutable array version would allocate
    @test @allocated(test_allocate1()) < 100
    @test @allocated(test_allocate2()) < 100
    @test @allocated(test_allocate3()) < 150
    @test @allocated(test_allocate4()) < 100
    @test @allocated(test_allocate5()) < 170
    @test test_broadcast1() == true

    # test_maybecopy1()
    # test_maybecopy2()
    # test_maybecopy3()
    # test_maybecopy4()
end


# DiffEq Performance Tests

# using DifferentialEquations
# using StaticArrays

# function _build_atsit5_caches(::Type{T}) where {T}

#     cs = SVector{6, T}(0.161, 0.327, 0.9, 0.9800255409045097, 1.0, 1.0)

#     as = SVector{21, T}(
#         #=a21=# convert(T,0.161),
#         #=a31=# convert(T,-0.008480655492356989),
#         #=a32=# convert(T,0.335480655492357),
#         #=a41=# convert(T,2.8971530571054935),
#         #=a42=# convert(T,-6.359448489975075),
#         #=a43=# convert(T,4.3622954328695815),
#         #=a51=# convert(T,5.325864828439257),
#         #=a52=# convert(T,-11.748883564062828),
#         #=a53=# convert(T,7.4955393428898365),
#         #=a54=# convert(T,-0.09249506636175525),
#         #=a61=# convert(T,5.86145544294642),
#         #=a62=# convert(T,-12.92096931784711),
#         #=a63=# convert(T,8.159367898576159),
#         #=a64=# convert(T,-0.071584973281401),
#         #=a65=# convert(T,-0.028269050394068383),
#         #=a71=# convert(T,0.09646076681806523),
#         #=a72=# convert(T,0.01),
#         #=a73=# convert(T,0.4798896504144996),
#         #=a74=# convert(T,1.379008574103742),
#         #=a75=# convert(T,-3.290069515436081),
#         #=a76=# convert(T,2.324710524099774)
#     )

#     btildes = SVector{7,T}(
#                 convert(T,-0.00178001105222577714),
#                 convert(T,-0.0008164344596567469),
#                 convert(T,0.007880878010261995),
#                 convert(T,-0.1447110071732629),
#                 convert(T,0.5823571654525552),
#                 convert(T,-0.45808210592918697),
#                 convert(T,0.015151515151515152)
#     )
#     rs = SVector{22, T}(
#         #=r11=# convert(T,1.0),
#         #=r12=# convert(T,-2.763706197274826),
#         #=r13=# convert(T,2.9132554618219126),
#         #=r14=# convert(T,-1.0530884977290216),
#         #=r22=# convert(T,0.13169999999999998),
#         #=r23=# convert(T,-0.2234),
#         #=r24=# convert(T,0.1017),
#         #=r32=# convert(T,3.9302962368947516),
#         #=r33=# convert(T,-5.941033872131505),
#         #=r34=# convert(T,2.490627285651253),
#         #=r42=# convert(T,-12.411077166933676),
#         #=r43=# convert(T,30.33818863028232),
#         #=r44=# convert(T,-16.548102889244902),
#         #=r52=# convert(T,37.50931341651104),
#         #=r53=# convert(T,-88.1789048947664),
#         #=r54=# convert(T,47.37952196281928),
#         #=r62=# convert(T,-27.896526289197286),
#         #=r63=# convert(T,65.09189467479366),
#         #=r64=# convert(T,-34.87065786149661),
#         #=r72=# convert(T,1.5),
#         #=r73=# convert(T,-4),
#         #=r74=# convert(T,2.5),
#     )
#     return cs, as, btildes, rs
# end

# function test_imarrays()
#     function lorenz(u, p, t)
#         a,b,c = u
#         x,y,z = p
#         dx_dt = x * (b - a)
#         dy_dt = a*(y - c) - b
#         dz_dt = a*b - z * c
#         res = Vector{Float64}(undef, 3)
#         res[1], res[2], res[3] = dx_dt, dy_dt, dz_dt
#         Core.ImmutableArray(res)
#     end

#     _u0 = Core.ImmutableArray([1.0, 1.0, 1.0])
#     _tspan = (0.0, 100.0)
#     _p = (10.0, 28.0, 8.0/3.0)
#     prob = ODEProblem(lorenz, _u0, _tspan, _p)

#     u0 = prob.u0
#     tspan = prob.tspan
#     f = prob.f
#     p = prob.p

#     dt = 0.1f0
#     saveat = nothing
#     save_everystep = true
#     abstol = 1f-6
#     reltol = 1f-3

#     t = tspan[1]
#     tf = prob.tspan[2]

#     beta1 = 7/50
#     beta2 = 2/25
#     qmax = 10.0
#     qmin = 1/5
#     gamma = 9/10
#     qoldinit = 1e-4

#     if saveat === nothing
#         ts = Vector{eltype(dt)}(undef,1)
#         ts[1] = prob.tspan[1]
#         us = Vector{typeof(u0)}(undef,0)
#         push!(us,recursivecopy(u0))
#     else
#         ts = saveat
#         cur_t = 1
#         us = MVector{length(ts),typeof(u0)}(undef)
#         if prob.tspan[1] == ts[1]
#             cur_t += 1
#             us[1] = u0
#         end
#     end

#     u = u0
#     qold = 1e-4
#     k7 = f(u, p, t)

#     cs, as, btildes, rs = _build_atsit5_caches(eltype(u0))
#     c1, c2, c3, c4, c5, c6 = cs
#     a21, a31, a32, a41, a42, a43, a51, a52, a53, a54,
#     a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76 = as
#     btilde1, btilde2, btilde3, btilde4, btilde5, btilde6, btilde7 = btildes

#     # FSAL
#     while t < tspan[2]
#         uprev = u
#         k1 = k7
#         EEst = Inf

#         while EEst > 1
#             dt < 1e-14 && error("dt<dtmin")

#             tmp = uprev+dt*a21*k1
#             k2 = f(tmp, p, t+c1*dt)
#             tmp = uprev+dt*(a31*k1+a32*k2)
#             k3 = f(tmp, p, t+c2*dt)
#             tmp = uprev+dt*(a41*k1+a42*k2+a43*k3)
#             k4 = f(tmp, p, t+c3*dt)
#             tmp = uprev+dt*(a51*k1+a52*k2+a53*k3+a54*k4)
#             k5 = f(tmp, p, t+c4*dt)
#             tmp = uprev+dt*(a61*k1+a62*k2+a63*k3+a64*k4+a65*k5)
#             k6 = f(tmp, p, t+dt)
#             u = uprev+dt*(a71*k1+a72*k2+a73*k3+a74*k4+a75*k5+a76*k6)
#             k7 = f(u, p, t+dt)

#             tmp = dt*(btilde1*k1+btilde2*k2+btilde3*k3+btilde4*k4+
#                         btilde5*k5+btilde6*k6+btilde7*k7)
#             tmp = tmp./(abstol.+max.(abs.(uprev),abs.(u))*reltol)
#             EEst = DiffEqBase.ODE_DEFAULT_NORM(tmp, t)

#             if iszero(EEst)
#                 q = inv(qmax)
#             else
#                 @fastmath q11 = EEst^beta1
#                 @fastmath q = q11/(qold^beta2)
#             end

#             if EEst > 1
#                 dt = dt/min(inv(qmin),q11/gamma)
#             else # EEst <= 1
#                 @fastmath q = max(inv(qmax),min(inv(qmin),q/gamma))
#                 qold = max(EEst,qoldinit)
#                 dtold = dt
#                 dt = dt/q #dtnew
#                 dt = min(abs(dt),abs(tf-t-dtold))
#                 told = t

#                 if (tf - t - dtold) < 1e-14
#                     t = tf
#                 else
#                     t += dtold
#                 end

#                 if saveat === nothing && save_everystep
#                     push!(us,recursivecopy(u))
#                     push!(ts,t)
#                 else saveat !== nothing
#                     while cur_t <= length(ts) && ts[cur_t] <= t
#                         savet = ts[cur_t]
#                         θ = (savet - told)/dtold
#                         b1θ, b2θ, b3θ, b4θ, b5θ, b6θ, b7θ = bθs(rs, θ)
#                         us[cur_t] = uprev + dtold*(
#                             b1θ*k1 + b2θ*k2 + b3θ*k3 + b4θ*k4 + b5θ*k5 + b6θ*k6 + b7θ*k7)
#                         cur_t += 1
#                     end
#                 end
#             end
#         end
#     end

#     if saveat === nothing && !save_everystep
#         push!(us,u)
#         push!(ts,t)
#     end

#     sol = DiffEqBase.build_solution(prob,Tsit5(),ts,us,calculate_error = false)

#     DiffEqBase.has_analytic(prob.f) && DiffEqBase.calculate_solution_errors!(sol;timeseries_errors=true,dense_errors=false)

#     sol
# end

# function test_marrays()
#     function lorenz(u, p, t)
#         a,b,c = u
#         x,y,z = p
#         dx_dt = x * (b - a)
#         dy_dt = a*(y - c) - b
#         dz_dt = a*b - z * c
#         res = Vector{Float64}(undef, 3)
#         res[1], res[2], res[3] = dx_dt, dy_dt, dz_dt
#         res
#     end

#     _u0 = [1.0, 1.0, 1.0]
#     _tspan = (0.0, 100.0)
#     _p = (10.0, 28.0, 8.0/3.0)
#     prob = ODEProblem(lorenz, _u0, _tspan, _p)

#     u0 = prob.u0
#     tspan = prob.tspan
#     f = prob.f
#     p = prob.p

#     dt = 0.1f0
#     saveat = nothing
#     save_everystep = true
#     abstol = 1f-6
#     reltol = 1f-3

#     t = tspan[1]
#     tf = prob.tspan[2]

#     beta1 = 7/50
#     beta2 = 2/25
#     qmax = 10.0
#     qmin = 1/5
#     gamma = 9/10
#     qoldinit = 1e-4

#     if saveat === nothing
#         ts = Vector{eltype(dt)}(undef,1)
#         ts[1] = prob.tspan[1]
#         us = Vector{typeof(u0)}(undef,0)
#         push!(us,recursivecopy(u0))
#     else
#         ts = saveat
#         cur_t = 1
#         us = MVector{length(ts),typeof(u0)}(undef)
#         if prob.tspan[1] == ts[1]
#             cur_t += 1
#             us[1] = u0
#         end
#     end

#     u = u0
#     qold = 1e-4
#     k7 = f(u, p, t)

#     cs, as, btildes, rs = _build_atsit5_caches(eltype(u0))
#     c1, c2, c3, c4, c5, c6 = cs
#     a21, a31, a32, a41, a42, a43, a51, a52, a53, a54,
#     a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76 = as
#     btilde1, btilde2, btilde3, btilde4, btilde5, btilde6, btilde7 = btildes

#     # FSAL
#     while t < tspan[2]
#         uprev = u
#         k1 = k7
#         EEst = Inf

#         while EEst > 1
#             dt < 1e-14 && error("dt<dtmin")

#             tmp = uprev+dt*a21*k1
#             k2 = f(tmp, p, t+c1*dt)
#             tmp = uprev+dt*(a31*k1+a32*k2)
#             k3 = f(tmp, p, t+c2*dt)
#             tmp = uprev+dt*(a41*k1+a42*k2+a43*k3)
#             k4 = f(tmp, p, t+c3*dt)
#             tmp = uprev+dt*(a51*k1+a52*k2+a53*k3+a54*k4)
#             k5 = f(tmp, p, t+c4*dt)
#             tmp = uprev+dt*(a61*k1+a62*k2+a63*k3+a64*k4+a65*k5)
#             k6 = f(tmp, p, t+dt)
#             u = uprev+dt*(a71*k1+a72*k2+a73*k3+a74*k4+a75*k5+a76*k6)
#             k7 = f(u, p, t+dt)

#             tmp = dt*(btilde1*k1+btilde2*k2+btilde3*k3+btilde4*k4+
#                         btilde5*k5+btilde6*k6+btilde7*k7)
#             tmp = tmp./(abstol.+max.(abs.(uprev),abs.(u))*reltol)
#             EEst = DiffEqBase.ODE_DEFAULT_NORM(tmp, t)

#             if iszero(EEst)
#                 q = inv(qmax)
#             else
#                 @fastmath q11 = EEst^beta1
#                 @fastmath q = q11/(qold^beta2)
#             end

#             if EEst > 1
#                 dt = dt/min(inv(qmin),q11/gamma)
#             else # EEst <= 1
#                 @fastmath q = max(inv(qmax),min(inv(qmin),q/gamma))
#                 qold = max(EEst,qoldinit)
#                 dtold = dt
#                 dt = dt/q #dtnew
#                 dt = min(abs(dt),abs(tf-t-dtold))
#                 told = t

#                 if (tf - t - dtold) < 1e-14
#                     t = tf
#                 else
#                     t += dtold
#                 end

#                 if saveat === nothing && save_everystep
#                     push!(us,recursivecopy(u))
#                     push!(ts,t)
#                 else saveat !== nothing
#                     while cur_t <= length(ts) && ts[cur_t] <= t
#                         savet = ts[cur_t]
#                         θ = (savet - told)/dtold
#                         b1θ, b2θ, b3θ, b4θ, b5θ, b6θ, b7θ = bθs(rs, θ)
#                         us[cur_t] = uprev + dtold*(
#                             b1θ*k1 + b2θ*k2 + b3θ*k3 + b4θ*k4 + b5θ*k5 + b6θ*k6 + b7θ*k7)
#                         cur_t += 1
#                     end
#                 end
#             end
#         end
#     end

#     if saveat === nothing && !save_everystep
#         push!(us,u)
#         push!(ts,t)
#     end

#     sol = DiffEqBase.build_solution(prob,Tsit5(),ts,us,calculate_error = false)

#     DiffEqBase.has_analytic(prob.f) && DiffEqBase.calculate_solution_errors!(sol;timeseries_errors=true,dense_errors=false)

#     sol
# end

