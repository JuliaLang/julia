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
#      tspan = [0 2*pi()]
#      y_0 = [1 0]'
#      F = (t, y) -> [0 1; -1 0]*y
#      ode23(F, tspan, y_0)
#
#   See also ODE23.

# Initialize variables.
# Adapted from Cleve Moler's textbook
# http://www.mathworks.com/moler/ncm/ode23tx.m

realmin() = eps(0.0)
eps() = eps(1.0)

function ode23(F::Function, tspan::Vector, y_0)

    rtol = 1.e-3
    atol = 1.e-6

    t0 = tspan[1]
    tfinal = tspan[2]
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
    r = norm(s1./max(abs(y), threshold), Inf) + realmin()
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
