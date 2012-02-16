function tracywidom()
    t0 = 5.
    tn = -8.
    dx = .005
    
    deq = function (t::Float64, y::Vector{Float64})
        [y[2]; t*y[1]+2*y[1]^3; y[4]; y[1]^2]
    end
    
    y0 = [airy(t0); airy(1,t0); 0; airy(t0)^2]
    
    (t, y) = ode23(deq, [t0, tn], y0)
    
    F2 = exp(-y[:,3][:])
    f2 = gradient(F2,t)
    return (t, f2)
end
