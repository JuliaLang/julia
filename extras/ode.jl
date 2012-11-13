#ODE23  Solve non-stiff differential equations.
#
#   ODE23(F,TSPAN,Y0) with TSPAN = [T0 TFINAL] integrates the system
#   of differential equations dy/dt = f(t,y) from t = T0 to t = TFINAL.
#   The initial condition is y(T0) = Y0.
#
#   The first argument, F, is a function handle or an anonymous function
#   that defines f(t,y).  This function must have two input arguments,
#   t and y, and must return a column vector of the derivatives, dy/dt.
#
#   With two output arguments, [T,Y] = ODE23(...) returns a column 
#   vector T and an array Y where Y(:,k) is the solution at T(k).
#
#   More than four input arguments, ODE23(F,TSPAN,Y0,RTOL,P1,P2,...),
#   are passed on to F, F(T,Y,P1,P2,...).
#
#   ODE23 uses the Runge-Kutta (2,3) method of Bogacki and Shampine (BS23).
#
#   Example    
#      tspan = [0, 2*pi]
#      y_0 = [1, 0]
#      F = (t, y) -> [0 1; -1 0]*y
#      ode23(F, tspan, y_0)
#
#   See also ODE23.

# Initialize variables.
# Adapted from Cleve Moler's textbook
# http://www.mathworks.com/moler/ncm/ode23tx.m

load("poly")

function ode23(F::Function, tspan::AbstractVector, y_0::AbstractVector)

    rtol = 1.e-5
    atol = 1.e-8

    t0 = tspan[1]
    tfinal = tspan[end]
    tdir = sign(tfinal - t0)
    threshold = atol / rtol
    hmax = abs(0.1*(tfinal-t0))
    t = t0
    y = y_0[:]

    tout = t
    yout = y.'

    tlen = length(t)

    # Compute initial step size.

    s1 = F(t, y)
    r = norm(s1./max(abs(y), threshold), Inf) + realmin() # TODO: fix type bug in max()
    h = tdir*0.8*rtol^(1/3)/r

    # The main loop.

    while t != tfinal
        
        hmin = 16*eps()*abs(t)
        if abs(h) > hmax; h = tdir*hmax; end
        if abs(h) < hmin; h = tdir*hmin; end
        
        # Stretch the step if t is close to tfinal.

        if 1.1*abs(h) >= abs(tfinal - t)
            h = tfinal - t;
        end
        
        # Attempt a step.

        s2 = F(t+h/2, y+h/2*s1)
        s3 = F(t+3*h/4, y+3*h/4*s2)
        tnew = t + h
        ynew = y + h*(2*s1 + 3*s2 + 4*s3)/9
        s4 = F(tnew, ynew)
        
        # Estimate the error.

        e = h*(-5*s1 + 6*s2 + 8*s3 - 9*s4)/72
        err = norm(e./max(max(abs(y), abs(ynew)), threshold), Inf) + realmin()

        # Accept the solution if the estimated error is less than the tolerance.

        if err <= rtol
            t = tnew
            y = ynew
            tout = [tout; t]
            yout = [yout; y.']
            s1 = s4   # Reuse final function value to start new step
        end
            
        # Compute a new step size.
        
        h = h*min(5, 0.8*(rtol/err)^(1/3))
        
        # Exit early if step size is too small.
        
        if abs(h) <= hmin
            println("Step size ", h, " too small at t = ", t)
            t = tfinal
        end

    end # while (t != tfinal)

    return (tout, yout)    

end # ode23


# ode45 adapted from http://users.powernet.co.uk/kienzle/octave/matcompat/scripts/ode_v1.11/ode45.m

# ode45 (v1.11) integrates a system of ordinary differential equations using
# 4th & 5th order embedded formulas from Dormand & Prince or Fehlberg.
#
# The Fehlberg 4(5) pair is established and works well, however, the
# Dormand-Prince 4(5) pair minimizes the local truncation error in the
# 5th-order estimate which is what is used to step forward (local extrapolation.)
# Generally it produces more accurate results and costs roughly the same
# computationally.  The Dormand-Prince pair is the default.
#
# This is a 4th-order accurate integrator therefore the local error normally
# expected would be O(h^5).  However, because this particular implementation
# uses the 5th-order estimate for xout (i.e. local extrapolation) moving
# forward with the 5th-order estimate should yield errors on the order of O(h^6).
#
# The order of the RK method is the order of the local *truncation* error, d.
# The local truncation error is defined as the principle error term in the
# portion of the Taylor series expansion that gets dropped.  This portion of
# the Taylor series exapansion is within the group of terms that gets multipled
# by h in the solution definition of the general RK method.  Therefore, the
# order-p solution created by the RK method will be roughly accurate to
# within O(h^(p+1)).  The difference between two different-order solutions is
# the definition of the "local error," l.  This makes the local error, l, as
# large as the error in the lower order method, which is the truncation
# error, d, times h, resulting in O(h^(p+1)).
# Summary:   For an RK method of order-p,
#            - the local truncation error is O(h^p)
#            - the local error (used for stepsize adjustment) is O(h^(p+1))
#
# This requires 6 function evaluations per integration step.
#
# The error estimate formula and slopes are from
# Numerical Methods for Engineers, 2nd Ed., Chappra & Cannle, McGraw-Hill, 1985
#
# Usage:
#         (tout, xout) = ode45(F, tspan, x0)
#
# INPUT:
# F     - User-defined function
#         Call: xprime = F(t,x)
#         t      - Time (scalar).
#         x      - Solution column-vector.
#         xprime - Returned derivative COLUMN-vector; xprime(i) = dx(i)/dt.
# tspan - [ tstart, tfinal ]
# x0    - Initial value COLUMN-vector.
#
# OUTPUT:
# tout  - Returned integration time points (column-vector).
# xout  - Returned solution, one solution column-vector per tout-value.
#
# Original Octave implementation:
# Marc Compere
# CompereM@asme.org
# created : 06 October 1999
# modified: 17 January 2001

function oderkf{T}(F::Function, tspan::AbstractVector, x0::AbstractVector{T}, a, b4, b5)
    tol = 1.0e-5
    
    # see p.91 in the Ascher & Petzold reference for more infomation.
    pow = 1/6; 

    c = sum(a, 2)

    # Initialization
    t = tspan[1]
    tfinal = tspan[end]
    hmax = (tfinal - t)/2.5
    hmin = (tfinal - t)/1e9
    h = (tfinal - t)/100  # initial guess at a step size
    x = x0
    tout = [t]            # first output time
    xout = x.'            # first output solution
        
    k = zeros(eltype(x), (length(c),length(x)))
    
    while (t < tfinal) & (h >= hmin)
        if t + h > tfinal
            h = tfinal - t
        end
        
        # Compute the slopes by computing the k[:,j+1]'th column based on the previous k[:,1:j] columns
        # notes: k needs to end up as an Nxs, a is 7x6, which is s by (s-1),
        #        s is the number of intermediate RK stages on [t (t+h)] (Dormand-Prince has s=7 stages)
        if -eps() < c[end]-1 < eps() && t != tspan[1]
            # Assign the last stage for x(k) as the first stage for computing x[k+1].
            # This is part of the Dormand-Prince pair caveat.
            # k[:,7] has already been computed, so use it instead of recomputing it
            # again as k[:,1] during the next step.
            k[1,:] = k[end,:]
        else
            k[1,:] = F(t,x) # first stage
        end

        for j = 2:length(c)
            k[j,:] = F(t + h.*c[j], x + h.*(a[j,1:j-1]*k[1:j-1,:]).')
        end
                
        # compute the 4th order estimate
        x4 = x + h.*(b4*k).'
                    
        # compute the 5th order estimate
        x5 = x + h.*(b5*k).'
                
        # estimate the local truncation error
        gamma1 = x5 - x4
                
        # Estimate the error and the acceptable error
        delta = norm(gamma1, Inf)       # actual error
        tau = tol*max(norm(x,Inf), 1.0) # allowable error
        
        # Update the solution only if the error is acceptable
        if (delta <= tau)
            t = t + h
            x = x5    # <-- using the higher order estimate is called 'local extrapolation'
            tout = [tout; t]
            xout = [xout; x.']
        end

        # Update the step size
        if delta == 0.0
            delta = 1e-16
        end
        h = min(hmax, 0.8*h*(tau/delta)^pow)
    end # while (t < tfinal) & (h >= hmin)

    if (t < tfinal)
      println("Step size grew too small. t=", t, ", h=", h, ", x=", x)
    end

    return (tout, xout)   
end

# Both the Dormand-Prince and Fehlberg 4(5) coefficients are from a tableau in
# U.M. Ascher, L.R. Petzold, Computer Methods for  Ordinary Differential Equations
# and Differential-Agebraic Equations, Society for Industrial and Applied Mathematics
# (SIAM), Philadelphia, 1998
#
# Dormand-Prince coefficients
dp_coefficients = ([    0           0          0         0         0        0
                        1/5         0          0         0         0        0
                        3/40        9/40       0         0         0        0
                       44/45      -56/15      32/9       0         0        0
                    19372/6561 -25360/2187 64448/6561 -212/729     0        0
                     9017/3168   -355/33   46732/5247   49/176 -5103/18656  0
                       35/384       0        500/1113  125/192 -2187/6784  11/84],
                   # 4th order b-coefficients
                   [5179/57600 0 7571/16695 393/640 -92097/339200 187/2100 1/40],
                   # 5th order b-coefficients
                   [35/384 0 500/1113 125/192 -2187/6784 11/84 0],
                   )
ode45_dp(F, tspan, x0) = oderkf(F, tspan, x0, dp_coefficients...)

# Fehlberg coefficients
fb_coefficients = ([    0         0          0         0        0
                       1/4        0          0         0        0
                       3/32       9/32       0         0        0
                    1932/2197 -7200/2197  7296/2197    0        0
                     439/216     -8       3680/513  -845/4104   0
                      -8/27       2      -3544/2565 1859/4104 -11/40],
                   # 4th order b-coefficients
                   [25/216 0 1408/2565 2197/4104 -1/5 0],
                   # 5th order b-coefficients
                   [16/135 0 6656/12825 28561/56430 -9/50 2/55],
                   )
ode45_fb(F, tspan, x0) = oderkf(F, tspan, x0, fb_coefficients...)

# Cash-Karp coefficients
# Numerical Recipes in Fortran 77
ck_coefficients = ([   0         0       0           0          0
                       1/5       0       0           0          0
                       3/40      9/40    0           0          0
                       3/10     -9/10    6/5         0          0
                     -11/54      5/2   -70/27       35/27       0
                    1631/55296 175/512 575/13824 44275/110592 253/4096],
                   # 4th order b-coefficients
                   [37/378 0 250/621 125/594 0 512/1771],
                   # 5th order b-coefficients
                   [2825/27648 0 18575/48384 13525/55296 277/14336 1/4],
                   )
ode45_ck(F, tspan, x0) = oderkf(F, tspan, x0, ck_coefficients...)

# Use Dormand Prince version of ode45 by default
ode45 = ode45_dp

#ODE4    Solve non-stiff differential equations, fourth order
#   fixed-step Runge-Kutta method.
#
#   [T,X] = ODE4(ODEFUN, TSPAN, X0) with TSPAN = [T0:H:TFINAL]
#   integrates the system of differential equations x' = f(t,x) from time
#   T0 to TFINAL in steps of H with initial conditions X0. Function
#   ODEFUN(T,X) must return a column vector corresponding to f(t,x). Each
#   row in the solution array X corresponds to a time returned in the
#   column vector T.
function ode4{T}(F::Function, tspan::AbstractVector, x0::AbstractVector{T})
    h = diff(tspan)
    x = Array(T, (length(tspan), length(x0)))
    x[1,:] = x0'

    midxdot = Array(T, (4, length(x0)))
    for i = 1:(length(tspan)-1)
        # Compute midstep derivatives
        midxdot[1,:] = F(tspan[i],         x[i,:]')
        midxdot[2,:] = F(tspan[i]+h[i]./2, x[i,:]' + midxdot[1,:]'.*h[i]./2)
        midxdot[3,:] = F(tspan[i]+h[i]./2, x[i,:]' + midxdot[2,:]'.*h[i]./2)
        midxdot[4,:] = F(tspan[i]+h[i],    x[i,:]' + midxdot[3,:]'.*h[i])

        # Integrate
        x[i+1,:] = x[i,:] + 1./6.*h[i].*[1 2 2 1]*midxdot
    end
    return (tspan, x)
end

#ODEROSENBROCK Solve stiff differential equations, Rosenbrock method
#    with provided coefficients.
function oderosenbrock{T}(F::Function, G::Function, tspan::AbstractVector, x0::AbstractVector{T}, gamma, a, b, c)
    h = diff(tspan)
    x = Array(T, length(tspan), length(x0))
    x[1,:] = x0'

    solstep = 1
    while tspan[solstep] < max(tspan)
        ts = tspan[solstep]
        hs = h[solstep]
        xs = reshape(x[solstep,:], size(x0))
        dFdx = G(ts, xs)
        jac = eye(size(dFdx)[1])./gamma./hs-dFdx

        g = zeros(size(a)[1], length(x0))
        g[1,:] = jac \ F(ts + b[1].*hs, xs)
        for i = 2:size(a)[1]
            g[i,:] = jac \ (F(ts + b[i].*hs, xs + (a[i,1:i-1]*g[1:i-1,:]).') + (c[i,1:i-1]*g[1:i-1,:]).'./hs)
        end

        x[solstep+1,:] = x[solstep,:] + b*g
        solstep += 1
    end
    return (tspan, x)
end

function oderosenbrock{T}(F::Function, tspan::AbstractVector, x0::AbstractVector{T}, gamma, a, b, c)
    # Crude forward finite differences estimator as fallback
    function jacobian(F::Function, t::Number, x::AbstractVector)
        ftx = F(t, x)
        dFdx = zeros(length(x), length(x))
        for j = 1:length(x)
            dx = zeros(size(x))
            # The 100 below is heuristic
            dx[j] = (x[j]+(x[j]==0))./100
            dFdx[:,j] = (F(t,x+dx)-ftx)./dx[j]
        end
        return dFdx
    end
    oderosenbrock(F, (t, x)->jacobian(F, t, x), tspan, x0, gamma, a, b, c)
end

# Kaps-Rentrop coefficients
kr4_coefficients = (0.231,
                    [0         0        0 0
                     2         0        0 0
                     4.4524708 4.163528 0 0
                     4.4524708 4.163528 0 0],
                    [3.957037 4.624892 0.617477 1.282613],
                    [ 0         0         0        0
                     -5.071675  0         0        0
                      6.020153  0.159750  0        0
                     -1.856344 -8.505381 -2.084075 0],)
ode4s_kr(F, tspan, x0) = oderosenbrock(F, tspan, x0, kr4_coefficients...)
ode4s_kr(F, G, tspan, x0) = oderosenbrock(F, G, tspan, x0, kr4_coefficients...)
# Shampine coefficients
s4_coefficients = (0.5,
                   [ 0    0    0 0
                     2    0    0 0
                    48/25 6/25 0 0
                    48/25 6/25 0 0],
                   [19/9 1/2 25/108 125/108],
                   [   0       0      0   0
                      -8       0      0   0
                     372/25   12/5    0   0
                    -112/125 -54/125 -2/5 0],)
ode4s_s(F, tspan, x0) = oderosenbrock(F, tspan, x0, s4_coefficients...)
ode4s_s(F, G, tspan, x0) = oderosenbrock(F, G, tspan, x0, s4_coefficients...)

# Use Shampine coefficients by default (matching Numerical Recipes)
const ode4s = ode4s_s

# ODE_MS Fixed-step, fixed-order multi-step numerical method with Adams-Bashforth-Moulton coefficients
function ode_ms{T}(F::Function, tspan::AbstractVector, x0::AbstractVector{T}, order::Integer)
    h = diff(tspan)
    x = zeros(T,(length(tspan), length(x0)))
    x[1,:] = x0

    if 1 <= order <= 4
        b = [ 1      0      0     0
             -1/2    3/2    0     0
             5/12  -16/12  23/12 0
             -9/24   37/24 -59/24 55/24];
    else
        for steporder = size(b,1):order
            s = steporder - 1;
            for j = 0:s
                # Assign in correct order for multiplication below
                #                    (a factor depending on j and s)      .* (an integral of a polynomial with -(0:s), except -j, as roots)
                b[steporder,s-j+1] = (-1).^j./factorial(j)./factorial(s-j).*diff(polyval(polyint(poly(diagm(-[0:j-1; j+1:s]))),0:1));
            end
        end
    end

    # TODO: use a better data structure here (should be an order-element circ buffer)
    xdot = similar(x)
    for i = 1:length(tspan)-1
        # Need to run the first several steps at reduced order
        steporder = min(i, order)
        xdot[i,:] = F(tspan[i], x[i,:]')
        x[i+1,:] = x[i,:] + b[steporder,1:steporder]*xdot[i-(steporder-1):i,:].*h[i]
    end
    return (tspan, x)
end

ode4ms(F, tspan, x0) = ode_ms(F, tspan, x0, 4)
