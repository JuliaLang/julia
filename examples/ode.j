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
# Both the Dormand-Prince and Fehlberg 4(5) coefficients are from a tableu in
# U.M. Ascher, L.R. Petzold, Computer Methods for  Ordinary Differential Equations
# and Differential-Agebraic Equations, Society for Industrial and Applied Mathematics
# (SIAM), Philadelphia, 1998
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
# Marc Compere
# CompereM@asme.org
# created : 06 October 1999
# modified: 17 January 2001

function ode45(F, tspan, x0)
    tol = 1.0e-3

    # see p.91 in the Ascher & Petzold reference for more infomation.
    pow = 1/6; 

    dormand_prince = true
    fehlberg = false

    a_ = zeros(7,7)
    b4_ = zeros(7)
    b5_ = zeros(7)
    c_ = zeros(7)

    if dormand_prince
      a_[1,1]=0;
      a_[2,1]=1/5;
      a_[3,1]=3/40; a_[3,2]=9/40;
      a_[4,1]=44/45; a_[4,2]=-56/15; a_[4,3]=32/9;
      a_[5,1]=19372/6561; a_[5,2]=-25360/2187; a_[5,3]=64448/6561; a_[5,4]=-212/729;
      a_[6,1]=9017/3168; a_[6,2]=-355/33; a_[6,3]=46732/5247; a_[6,4]=49/176; a_[6,5]=-5103/18656;
      a_[7,1]=35/384; a_[7,2]=0; a_[7,3]=500/1113; a_[7,4]=125/192; a_[7,5]=-2187/6784; a_[7,6]=11/84;
      # 4th order b-coefficients
      b4_[1]=5179/57600; b4_[2]=0; b4_[3]=7571/16695; b4_[4]=393/640; b4_[5]=-92097/339200; b4_[6]=187/2100; b4_[7]=1/40;
      # 5th order b-coefficients
      b5_[1]=35/384; b5_[2]=0; b5_[3]=500/1113; b5_[4]=125/192; b5_[5]=-2187/6784; b5_[6]=11/84; b5_[7]=0;
      for i=1:7
        c_[i]=sum(a_[i,:])
      end
    end
    
    if fehlberg 
      a_[1,1]=0;
      a_[2,1]=1/4;
      a_[3,1]=3/32; a_[3,2]=9/32;
      a_[4,1]=1932/2197; a_[4,2]=-7200/2197; a_[4,3]=7296/2197;
      a_[5,1]=439/216; a_[5,2]=-8; a_[5,3]=3680/513; a_[5,4]=-845/4104;
      a_[6,1]=-8/27; a_[6,2]=2; a_[6,3]=-3544/2565; a_[6,4]=1859/4104; a_[6,5]=-11/40;
      # 4th order b-coefficients (guaranteed to be a column vector)
      b4_[1]=25/216; b4_[2]=0; b4_[3]=1408/2565; b4_[4]=2197/4104; b4_[5]=-1/5;
      # 5th order b-coefficients (also guaranteed to be a column vector)
      b5_[1]=16/135; b5_[2]=0; b5_[3]=6656/12825; b5_[4]=28561/56430; b5_[5]=-9/50; b5_[6]=2/55;
      for i=1:6
        c_[i]=sum(a_[i,:])
      end
 
      a_ = a_[1:6, 1:6]
      b4_ = b4_[1:5]
      b5_ = b5_[1:6]
      c_ = c_[1:6]
   end 

    # Initialization
    t0 = tspan[1]
    tfinal = tspan[2]
    t = t0
    hmax = (tfinal - t)/2.5
    hmin = (tfinal - t)/1e9
    h = (tfinal - t)/100  # initial guess at a step size
    x = x0[:]             # this always creates a column vector, x
    tout = t              # first output time
    xout = x.'            # first output solution
        
    if dormand_prince
  
      # k_ needs to be initialized as an Nx7 matrix where N=number of rows in x
      # (just for speed so octave doesn't need to allocate more memory at each stage value)
      k_ = zeros(length(x),7)

      # Compute the first stage prior to the main loop.  This is part of the Dormand-Prince pair caveat
      # Normally, during the main loop the last stage for x(k) is the first stage for computing x(k+1].
      # So, the very first integration step requires 7 function evaluations, then each subsequent step
      # 6 function evaluations because the first stage is simply assigned from the last step's last stage.
      # note: you can see this by the last element in c_ is 1.0, thus t+c_[7)*h = t+h, ergo, the next step.
      
      k_[:,1] = F(t,x) # first stage
                
      # The main loop using Dormand-Prince 4(5) pair
      while (t < tfinal) & (h >= hmin)
        if t + h > tfinal; h = tfinal - t; end
        
        # Compute the slopes by computing the k(:,j+1]'th column based on the previous k(:,1:j) columns
        # notes: k_ needs to end up as an Nxs, a_ is 7x6, which is s by (s-1],
        #        s is the number of intermediate RK stages on [t (t+h)] (Dormand-Prince has s=7 stages)

        for j = 1:6
          k_[:,j+1] = F(t+c_[j+1]*h, x + reshape(h*k_[:,1:j]*a_[j+1,1:j]', length(x)))
        end
                
	# compute the 4th order estimate
        x4=x + h* (k_*b4_) # k_ is Nxs (or Nx7) and b4_ is a 7x1
                    
        # compute the 5th order estimate
        x5=x + h*(k_*b5_) # k_ is Nxs (or Nx7) and b5_ is a 7x1
                
        # estimate the local truncation error
        gamma1 = x5 - x4
                
        # Estimate the error and the acceptable error
        delta = norm(gamma1, Inf)       # actual error
        tau = tol*max(norm(x,Inf), 1.0) # allowable error
                    
	# Update the solution only if the error is acceptable
        if (delta<=tau)
          t = t + h
          x = x5    # <-- using the higher order estimate is called 'local extrapolation'
          tout = [tout; t]
          xout = [xout; x.']
        end

	# Update the step size
        if (delta==0.0); delta = 1e-16; end
        h = min(hmax, 0.8*h*(tau/delta)^pow)
                    
        # Assign the last stage for x(k) as the first stage for computing x(k+1].
        # This is part of the Dormand-Prince pair caveat.
	# k_[:,7] has already been computed, so use it instead of recomputing it
	# again as k_[:,1] during the next step.
        k_[:,1]=k_[:,7]

      end # while (t < tfinal) & (h >= hmin)
    end # if dormand_prince ...

    if fehlberg

      # k_ needs to be initialized as an Nx6 matrix where N=number of rows in x
      # (just for speed so octave doesn't need to allocate more memory at each stage value)
      k_ = zeros(length(x),6)
                                    
      while (t < tfinal) & (h >= hmin)
        if t + h > tfinal; h = tfinal - t; end
          
        # Compute the slopes by computing the k(:,j+1]'th column based on the previous k(:,1:j) columns
        # notes: k_ needs to end up as an Nx6, a_ is 6x5, which is s by (s-1],  (RK-Fehlberg has s=6 stages)
        #        s is the number of intermediate RK stages on [t (t+h)]

        k_[:,1]=F(t,x) # first stage
          
        for j = 1:5
          k_[:,j+1] = F(t+c_[j+1]*h, x + reshape(h*k_[:,1:j]*a_[j+1,1:j]', length(x)))
        end
        
        # compute the 4th order estimate
        x4=x + h* (k_[:,1:5]*b4_) # k_[:,1:5] is an Nx5 and b4_ is a 5x1
          
	# compute the 5th order estimate
        x5=x + h*(k_*b5_) # k_ is the same as k_[:,1:6) and is an Nx6 and b5_ is a 6x1
        
        # estimate the local truncation error
        gamma1 = x5 - x4
          
        # Estimate the error and the acceptable error
        delta = norm(gamma1, Inf)       # actual error
        tau = tol*max(norm(x, Inf),1.0) # allowable error
          
	# update the solution only if the error is acceptable
        if (delta<=tau)
          t = t + h
          x = x5    # <-- using the higher order estimate is called 'local extrapolation'
          tout = [tout; t]
          xout = [xout; x.']
        end 
                                                
        # Update the step size
        if (delta==0.0); delta = 1e-16; end
        h = min(hmax, 0.8*h*(tau/delta)^pow)
          
      end # while (t < tfinal) & (h >= hmin)
    end # if fehlberg
        
    if (t < tfinal)
      println("Step size grew too small. t=", t, ", h=", h, ", x=", x)
    end

    return (tout, xout)
    
end # ode45
