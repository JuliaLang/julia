#  This example performs least squares fitting to a polynomial of
#  degree N.  The input data is a set of (x, y) vectors.
#  I create artifical (x, y) data, but one could easily read in
#  a file of data to fit.
#
#  To test it out, you can scrape the code into a Julia command line
#  or you can run it from within Julia like this:
#  include("examples/LstSquares.jl")
#
#  July 2012 -- SDB

#  Feel free to change these parameters to experiemnt with the program. 
N = 4     # Degree of fitting polynomial.
Len = 31  # length of input vectors.

#--------------------------------------------------
#  Create some artifical data to use for example
#  Create x coordinates
#  This creates a sweep in x like [..., -3, -2, -1, 0, 1, 2, 3 ...]
x = linspace(-(Len-1)/2, (Len-1)/2,  Len)

#  Create y data values.  In this case I create a noisy cubic.
#  Feel free to create a different function.  
y = 4 + 3*x + 2*x.*x + 1*x.*x.*x + 0.4 *randn(Len)

#--------------------------------------------------
# Now perform fit computation.
# First create Vandermonde matrix used in least squares fit.
# Note that writing for loops are the natural way to create
# the Vandermonde matrix -- no need to use tortured vectorized
# constructs as is required by other matrix-oriented languages.
A = zeros(N+1, N+1)
for r = 1:(N+1)
  for c = 1:(N+1)
    power = (r-1)+(c-1)
    A[r, c] = sum( x.^power )
  end
end

# Next create RHS vector
b = zeros(N+1)
for idx = 1:(N+1)
  power = idx-1
  b[idx] = sum(y.*(x.^power))
end

# Solve Ac = B to get coefficient vector c
c = A\b

#-----------------------------------------------------
# Validation of results.
# First see what result we have -- print fitting coefficients
println("Fit coeffs = ", c) 

# Next print predicted output based upon
# extracted coefficients.
for idx = 1:Len
  runsum = c[N+1]
  for coeff = N:-1:1
    runsum = runsum*x[idx] + c[coeff]
  end
  @printf("x = %6.2f,   y(input) = %11.4f,   y(fit) = %11.4f,   error = %11.4f\n", x[idx], y[idx], runsum, y[idx]-runsum)
end
