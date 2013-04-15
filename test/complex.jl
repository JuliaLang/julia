# TODO:
# ^ (cpow)
#  equivalent to exp(y*log(x))
#    except for 0^0?
#  conj(x)^conj(y) = conj(x^y)

# cos (based on cosh)
# tan (based on tanh)
# asin (based on asinh)
# atan (based on atanh)



# sqrt: 
# tests special values from csqrt man page
# as well as conj(sqrt(z)) = sqrt(conj(z))
@test isequal(sqrt(complex(0.0,0.0)), complex(0.0,0.0))
@test isequal(sqrt(complex(-0.0,0.0)), complex(0.0,0.0))
@test isequal(sqrt(complex(0.0,-0.0)), complex(0.0,-0.0))
@test isequal(sqrt(complex(-0.0,-0.0)), complex(0.0,-0.0))

@test isequal(sqrt(complex(5.0,0.0)), complex(sqrt(5.0),0.0))
@test isequal(sqrt(complex(5.0,-0.0)), complex(sqrt(5.0),-0.0))

@test isequal(sqrt(complex(-5.0,0.0)), complex(0.0,sqrt(5.0)))
@test isequal(sqrt(complex(-5.0,-0.0)), complex(0.0,-sqrt(5.0)))

@test isequal(sqrt(complex(0.0,Inf)), complex(Inf,Inf))
@test isequal(sqrt(complex(0.0,-Inf)), complex(Inf,-Inf))

@test isequal(sqrt(complex(NaN,Inf)), complex(Inf,Inf))
@test isequal(sqrt(complex(NaN,-Inf)), complex(Inf,-Inf))

@test isequal(sqrt(complex(Inf,Inf)), complex(Inf,Inf))
@test isequal(sqrt(complex(Inf,-Inf)), complex(Inf,-Inf))
@test isequal(sqrt(complex(-Inf,Inf)), complex(Inf,Inf))
@test isequal(sqrt(complex(-Inf,-Inf)), complex(Inf,-Inf))

@test isequal(sqrt(complex(0.0,NaN)), complex(NaN,NaN))

@test isequal(sqrt(complex(-Inf,0.0)), complex(0.0,Inf))
@test isequal(sqrt(complex(-Inf,5.0)), complex(0.0,Inf))
@test isequal(sqrt(complex(-Inf,-0.0)), complex(0.0,-Inf))
@test isequal(sqrt(complex(-Inf,-5.0)), complex(0.0,-Inf))

@test isequal(sqrt(complex(Inf,0.0)), complex(Inf,0.0))
@test isequal(sqrt(complex(Inf,5.0)), complex(Inf,0.0))
@test isequal(sqrt(complex(Inf,-0.0)), complex(Inf,-0.0))
@test isequal(sqrt(complex(Inf,-5.0)), complex(Inf,-0.0))

@test isequal(sqrt(complex(-Inf,NaN)), complex(NaN,Inf))
@test isequal(sqrt(complex(Inf,NaN)), complex(Inf,NaN))

@test isequal(sqrt(complex(NaN,0.0)), complex(NaN,NaN))
@test isequal(sqrt(complex(NaN,0.0)), complex(NaN,NaN))



# log: 
#  log(conj(z)) = conj(log(z))
@test isequal(log(complex(5.0,0.0)),complex(log(5.0),0.0))
@test isequal(log(complex(5.0,-0.0)),complex(log(5.0),-0.0))

@test isequal(log(complex(0.0,1.0)),complex(0.0,pi/2))
@test isequal(log(complex(0.0,-1.0)),complex(0.0,-pi/2))

# special values

# raise divide-by-zero flag
@test isequal(log(complex(-0.0,0.0)), complex(-Inf,pi))
@test isequal(log(complex(-0.0,-0.0)), complex(-Inf,-pi))

# raise divide-by-zero flag
@test isequal(log(complex(0.0,0.0)), complex(-Inf,0.0))
@test isequal(log(complex(0.0,-0.0)), complex(-Inf,-0.0))

@test isequal(log(complex(0.0,Inf)), complex(Inf,pi/2))
@test isequal(log(complex(0.0,-Inf)), complex(Inf,-pi/2))

@test isequal(log(complex(0.0,NaN)), complex(NaN,NaN))

@test isequal(log(complex(-Inf,5.0)), complex(Inf,pi))
@test isequal(log(complex(-Inf,-5.0)), complex(Inf,-pi))

@test isequal(log(complex(Inf,5.0)), complex(Inf,0.0))
@test isequal(log(complex(Inf,-5.0)), complex(Inf,-0.0))

@test isequal(log(complex(-Inf,Inf)), complex(Inf,3*pi/4))
@test isequal(log(complex(-Inf,-Inf)), complex(Inf,-3*pi/4))

@test isequal(log(complex(Inf,Inf)), complex(Inf,pi/4))
@test isequal(log(complex(Inf,-Inf)), complex(Inf,-pi/4))

@test isequal(log(complex(Inf,NaN)), complex(Inf,NaN))
@test isequal(log(complex(-Inf,NaN)), complex(Inf,NaN))

@test isequal(log(complex(NaN,0.0)), complex(NaN,NaN))

@test isequal(log(complex(NaN,Inf)), complex(Inf,NaN))
@test isequal(log(complex(NaN,-Inf)), complex(Inf,NaN))

@test isequal(log(complex(NaN,NaN)), complex(NaN,NaN))

# exp:
#  exp(conj(z)) = conj(exp(z))

@test isequal(exp(complex(0.0,0.0)), complex(1.0,0.0))
@test isequal(exp(complex(0.0,-0.0)), complex(1.0,-0.0))
@test isequal(exp(complex(-0.0,0.0)), complex(1.0,0.0))
@test isequal(exp(complex(-0.0,-0.0)), complex(1.0,-0.0))

# raise invalid flag
@test_fails exp(complex(0.0,Inf) #complex(NaN,NaN)
@test_fails exp(complex(0.0,-Inf) #complex(NaN,NaN)
@test_fails exp(complex(5.0,Inf) #complex(NaN,NaN)

@test isequal(exp(complex(0.0,NaN)), complex(NaN,NaN))

@test isequal(exp(complex(Inf,0.0)), complex(Inf,0.0))
@test isequal(exp(complex(Inf,-0.0)), complex(Inf,-0.0))

@test isequal(exp(complex(-Inf,0.0)), complex(0.0,0.0))
@test isequal(exp(complex(-Inf,-0.0)), complex(0.0,-0.0))
@test isequal(exp(complex(-Inf,5.0)), complex(cos(5.0)*0.0,sin(5.0)*0.0))
@test isequal(exp(complex(-Inf,-5.0)), complex(cos(5.0)*0.0,sin(5.0)*-0.0))

@test isequal(exp(complex(Inf,5.0)), complex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(exp(complex(Inf,-5.0)), complex(cos(5.0)*Inf,sin(5.0)*-Inf))

@test isequal(exp(complex(-Inf,Inf)), complex(-0.0,0.0))
@test isequal(exp(complex(-Inf,-Inf)), complex(-0.0,-0.0))

# raise invalid flag
@test_fails exp(complex(Inf,Inf)) #complex(-Inf,NaN)
@test_fails exp(complex(Inf,-Inf)) #complex(-Inf,NaN)

@test isequal(exp(complex(-Inf,NaN)), complex(-0.0,0.0))

@test isequal(exp(complex(Inf,NaN)), complex(-Inf,NaN))

@test isequal(exp(complex(NaN,0.0)), complex(NaN,0.0))
@test isequal(exp(complex(NaN,-0.0)), complex(NaN,-0.0))

@test isequal(exp(complex(NaN,5.0)), complex(NaN,NaN))

@test isequal(exp(complex(NaN,NaN)), complex(NaN,NaN))

# ^ (cpow)
#  equivalent to exp(y*log(x))
#    except for 0^0?
#  conj(x)^conj(y) = conj(x^y)
@test isequal(complex(0.0,0.0)^complex(0.0,0.0), complex(1.0,0.0))

@test isequal(complex(0.0,-0.0)^complex(0.0,-0.0), complex(1.0,-0.0))



# sinh: has properties
#  sinh(conj(z)) = conj(sinh(z))
#  sinh(-z) = -sinh(z)

@test isequal(sinh(complex(0.0,0.0)),complex(0.0,0.0))
@test isequal(sinh(complex(0.0,-0.0)),complex(0.0,-0.0))
@test isequal(sinh(complex(-0.0,0.0)),complex(-0.0,0.0))
@test isequal(sinh(complex(-0.0,-0.0)),complex(-0.0,-0.0))

# raise invalid flag
@test_fails sinh(complex(0.0,Inf)) #complex(0.0,NaN)
@test_fails sinh(complex(0.0,-Inf)) #complex(0.0,NaN)
@test_fails sinh(complex(-0.0,Inf)) #complex(-0.0,NaN)
@test_fails sinh(complex(-0.0,-Inf)) #complex(-0.0,NaN)

@test isequal(sinh(complex(0.0,NaN)),complex(0.0,NaN))
@test isequal(sinh(complex(-0.0,NaN)),complex(-0.0,NaN))

# raise invalid flag
@test_fails sinh(complex(5.0,Inf)) #complex(NaN,NaN)

@test isequal(sinh(complex(5.0,NaN)),complex(NaN,NaN))

@test isequal(sinh(complex(Inf,0.0)),complex(Inf,0.0))
@test isequal(sinh(complex(Inf,-0.0)),complex(Inf,-0.0))
@test isequal(sinh(complex(-Inf,0.0)),complex(-Inf,0.0))
@test isequal(sinh(complex(-Inf,-0.0)),complex(-Inf,-0.0))

@test isequal(sinh(complex(Inf,5.0)),complex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(sinh(complex(-Inf,5.0)),complex(cos(5.0)*-Inf,sin(5.0)*Inf))

# raise invalid flag
@test_fails sinh(complex(Inf,Inf)) #complex(Inf,NaN)
@test_fails sinh(complex(Inf,-Inf)) #complex(Inf,NaN)
@test_fails sinh(complex(-Inf,Inf)) #complex(-Inf,NaN)
@test_fails sinh(complex(-Inf,-Inf)) #complex(-Inf,NaN)

@test isequal(sinh(complex(Inf,NaN)),complex(Inf,NaN))
@test isequal(sinh(complex(-Inf,NaN)),complex(-Inf,NaN))

@test isequal(sinh(complex(NaN,0.0)),complex(NaN,0.0))
@test isequal(sinh(complex(NaN,-0.0)),complex(NaN,-0.0))

@test isequal(sinh(complex(NaN,5.0)),complex(NaN,NaN))

@test isequal(sinh(complex(NaN,NaN)),complex(NaN,NaN))



# sin: defined in terms of sinh
#  sin(z) = -i*sinh(i*z)
#  i.e. if sinh(a+ib) = x+iy
#    then  sin(b-ia) = y-ix

@test isequal(sin(complex(0.0,-0.0)),complex(0.0,-0.0))
@test isequal(sin(complex(-0.0,-0.0)),complex(-0.0,-0.0))
@test isequal(sin(complex(0.0,0.0)),complex(0.0,0.0))
@test isequal(sin(complex(-0.0,0.0)),complex(-0.0,0.0))

# raise invalid flag
@test_fails sin(complex(Inf,0.0)) #complex(NaN,0.0)
@test_fails sin(complex(-Inf,0.0)) #complex(NaN,0.0)
@test_fails sin(complex(Inf,-0.0)) #complex(NaN,-0.0)
@test_fails sin(complex(-Inf,-0.0)) #complex(NaN,-0.0)

@test isequal(sin(complex(NaN,0.0)),complex(NaN,0.0))
@test isequal(sin(complex(NaN,-0.0)),complex(NaN,-0.0))

# raise invalid flag
@test_fails sin(complex(Inf,5.0)) #complex(NaN,NaN)

@test isequal(sin(complex(NaN,5.0)),complex(NaN,NaN))

@test isequal(sin(complex(0.0,Inf)),complex(0.0,Inf))
@test isequal(sin(complex(-0.0,Inf)),complex(-0.0,Inf))
@test isequal(sin(complex(0.0,-Inf)),complex(0.0,-Inf))
@test isequal(sin(complex(-0.0,-Inf)),complex(-0.0,-Inf))

@test isequal(sin(complex(5.0,Inf)),complex(sin(5.0)*Inf,cos(5.0)*Inf))
@test isequal(sin(complex(5.0,-Inf)),complex(sin(5.0)*Inf,cos(5.0)*-Inf))

# raise invalid flag
@test_fails sin(complex(Inf,Inf)) #complex(Inf,NaN)
@test_fails sin(complex(Inf,-Inf)) #complex(Inf,NaN)
@test_fails sin(complex(-Inf,Inf)) #complex(-Inf,NaN)
@test_fails sin(complex(-Inf,-Inf)) #complex(-Inf,NaN)

@test isequal(sin(complex(NaN,Inf)),complex(NaN,Inf))
@test isequal(sin(complex(NaN,-Inf)),complex(NaN,-Inf))

@test isequal(sin(complex(0.0,NaN)),complex(0.0,NaN))
@test isequal(sin(complex(-0.0,NaN)),complex(-0.0,NaN))

@test isequal(sin(complex(5.0,NaN)),complex(NaN,NaN))

@test isequal(sin(complex(NaN,NaN)),complex(NaN,NaN))






# cosh: has properties
#  cosh(conj(z)) = conj(cosh(z))
#  coshh(-z) = cosh(z)

@test isequal(cosh(complex(0.0,0.0)),complex(1.0,0.0))
@test isequal(cosh(complex(0.0,-0.0)),complex(1.0,-0.0))
@test isequal(cosh(complex(-0.0,-0.0)),complex(1.0,0.0))
@test isequal(cosh(complex(-0.0,0.0)),complex(1.0,-0.0))

# raise invalid flag
@test_fails cosh(complex(0.0,Inf)) #complex(NaN,0.0)
@test_fails cosh(complex(0.0,-Inf)) #complex(NaN,-0.0)

@test isequal(cosh(complex(0.0,NaN)),complex(NaN,0.0))
@test isequal(cosh(complex(-0.0,NaN)),complex(NaN,0.0))

# raise invalid flag
@test_fails cosh(complex(5.0,Inf)) #complex(NaN,NaN)

@test isequal(cosh(complex(5.0,NaN)),complex(NaN,NaN))

@test isequal(cosh(complex(Inf,0.0)),complex(Inf,0.0))
@test isequal(cosh(complex(Inf,-0.0)),complex(Inf,-0.0))
@test isequal(cosh(complex(-Inf,-0.0)),complex(Inf,0.0))
@test isequal(cosh(complex(-Inf,0.0)),complex(Inf,-0.0))

@test isequal(cosh(complex(Inf,5.0)),complex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(cosh(complex(Inf,-5.0)),complex(cos(5.0)*Inf,sin(5.0)*-Inf))
@test isequal(cosh(complex(-Inf,-5.0)),complex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(cosh(complex(-Inf,5.0)),complex(cos(5.0)*Inf,sin(5.0)*-Inf))

# raise invalid flag
@test_fails cosh(complex(Inf,Inf)) #complex(Inf,NaN)
@test_fails cosh(complex(Inf,-Inf)) #complex(Inf,NaN)
@test_fails cosh(complex(-Inf,-Inf)) #complex(Inf,NaN)
@test_fails cosh(complex(-Inf,Inf)) #complex(Inf,NaN)

@test isequal(cosh(complex(Inf,NaN)),complex(Inf,NaN))
@test isequal(cosh(complex(-Inf,NaN)),complex(Inf,NaN))

@test isequal(cosh(complex(NaN,0.0)),complex(NaN,0.0))
@test isequal(cosh(complex(NaN,-0.0)),complex(NaN,-0.0))

@test isequal(cosh(complex(NaN,5.0)),complex(NaN,NaN))
@test isequal(cosh(complex(NaN,-5.0)),complex(NaN,NaN))

@test isequal(cosh(complex(NaN,NaN)),complex(NaN,NaN))



# tanh
#  tanh(conj(z)) = conj(tanh(z))
#  tanh(-z) = -tanh(z)
@test isequal(tanh(complex(0.0,0.0)),complex(0.0,0.0))
@test isequal(tanh(complex(0.0,-0.0)),complex(0.0,-0.0))
@test isequal(tanh(complex(-0.0,0.0)),complex(-0.0,0.0))
@test isequal(tanh(complex(-0.0,-0.0)),complex(-0.0,-0.0))

# raise invalid flag
@test_fails tanh(complex(0.0,Inf)) #complex(NaN,0.0)
@test_fails tanh(complex(0.0,-Inf)) #complex(NaN,-0.0)

@test isequal(tanh(complex(0.0,NaN)),complex(NaN,NaN))

# raise invalid flag
@test_fails tanh(complex(5.0,Inf)) #complex(NaN,NaN)

@test isequal(tanh(complex(5.0,NaN)),complex(NaN,NaN))

@test isequal(tanh(complex(Inf,0.0)),complex(1.0,0.0))
@test isequal(tanh(complex(Inf,-0.0)),complex(1.0,-0.0))
@test isequal(tanh(complex(-Inf,0.0)),complex(-1.0,0.0))
@test isequal(tanh(complex(-Inf,-0.0)),complex(-1.0,-0.0))

@test isequal(tanh(complex(Inf,5.0)),complex(1.0,sin(2*5.0)*0.0))
@test isequal(tanh(complex(Inf,-5.0)),complex(1.0,sin(2*5.0)*-0.0))
@test isequal(tanh(complex(-Inf,5.0)),complex(-1.0,sin(2*5.0)*0.0))
@test isequal(tanh(complex(-Inf,-5.0)),complex(-1.0,sin(2*5.0)*-0.0))

@test isequal(tanh(complex(Inf,Inf)),complex(1.0,0.0))
@test isequal(tanh(complex(Inf,-Inf)),complex(1.0,-0.0))
@test isequal(tanh(complex(-Inf,Inf)),complex(-1.0,0.0))
@test isequal(tanh(complex(-Inf,-Inf)),complex(-1.0,-0.0))

@test isequal(tanh(complex(Inf,NaN)),complex(1.0,0.0))
@test isequal(tanh(complex(-Inf,NaN)),complex(-1.0,0.0))

@test isequal(tanh(complex(NaN,0.0)),complex(NaN,0.0))
@test isequal(tanh(complex(NaN,-0.0)),complex(NaN,-0.0))

@test isequal(tanh(complex(NaN,5.0)),complex(NaN,NaN))
@test isequal(tanh(complex(NaN,-5.0)),complex(NaN,NaN))

@test isequal(tanh(complex(NaN,NaN)),complex(NaN,NaN))



# acosh
#  acosh(conj(z)) = conj(acosh(z))
@test isequal(acosh(complex(0.0,0.0)),complex(0.0,pi/2))
@test isequal(acosh(complex(0.0,-0.0)),complex(0.0,-pi/2))
@test isequal(acosh(complex(-0.0,0.0)),complex(0.0,pi/2))
@test isequal(acosh(complex(-0.0,-0.0)),complex(0.0,-pi/2))

@test isequal(acosh(complex(0.0,Inf)),complex(Inf,pi/2))
@test isequal(acosh(complex(0.0,-Inf)),complex(Inf,-pi/2))
@test isequal(acosh(complex(5.0,Inf)),complex(Inf,pi/2))
@test isequal(acosh(complex(5.0,-Inf)),complex(Inf,-pi/2))

@test isequal(acosh(complex(5.0,NaN)),complex(NaN,NaN))

@test isequal(acosh(complex(-Inf,0.0)),complex(Inf,pi))
@test isequal(acosh(complex(-Inf,-0.0)),complex(Inf,-pi))
@test isequal(acosh(complex(-Inf,5.0)),complex(Inf,pi))
@test isequal(acosh(complex(-Inf,-5.0)),complex(Inf,-pi))

@test isequal(acosh(complex(Inf,0.0)),complex(Inf,0.0))
@test isequal(acosh(complex(Inf,-0.0)),complex(Inf,-0.0))
@test isequal(acosh(complex(Inf,5.0)),complex(Inf,0.0))
@test isequal(acosh(complex(Inf,-5.0)),complex(Inf,-0.0))

@test isequal(acosh(complex(-Inf,Inf)),complex(Inf,3*pi/4))
@test isequal(acosh(complex(-Inf,-Inf)),complex(Inf,-3*pi/4))

@test isequal(acosh(complex(Inf,Inf)),complex(Inf,pi/4))
@test isequal(acosh(complex(Inf,-Inf)),complex(Inf,-pi/4))

@test isequal(acosh(complex(Inf,NaN)),complex(Inf,NaN))
@test isequal(acosh(complex(-Inf,NaN)),complex(Inf,NaN))

@test isequal(acosh(complex(NaN,Inf)),complex(Inf,NaN))
@test isequal(acosh(complex(NaN,-Inf)),complex(Inf,NaN))

@test isequal(acosh(complex(NaN,NaN)),complex(NaN,NaN))


# acos
#  acos(conj(z)) = conj(acos(z))
@test isequal(acos(complex(0.0,0.0)),complex(pi/2,-0.0))
@test isequal(acos(complex(0.0,-0.0)),complex(pi/2,0.0))
@test isequal(acos(complex(-0.0,0.0)),complex(pi/2,-0.0))
@test isequal(acos(complex(-0.0,-0.0)),complex(pi/2,0.0))

@test isequal(acos(complex(0.0,NaN)),complex(pi/2,NaN))
@test isequal(acos(complex(-0.0,NaN)),complex(pi/2,NaN))

@test isequal(acos(complex(0.0,Inf)),complex(pi/2,-Inf))
@test isequal(acos(complex(0.0,-Inf)),complex(pi/2,Inf))
@test isequal(acos(complex(5.0,Inf)),complex(pi/2,-Inf))
@test isequal(acos(complex(5.0,-Inf)),complex(pi/2,Inf))

@test isequal(acos(complex(5.0,NaN)),complex(NaN,NaN))

@test isequal(acos(complex(-Inf,0.0)),complex(pi,-Inf))
@test isequal(acos(complex(-Inf,-0.0)),complex(pi,Inf))
@test isequal(acos(complex(-Inf,5.0)),complex(pi,-Inf))
@test isequal(acos(complex(-Inf,-5.0)),complex(pi,Inf))

@test isequal(acos(complex(Inf,0.0)),complex(0.0,-Inf))
@test isequal(acos(complex(Inf,-0.0)),complex(0.0,Inf))
@test isequal(acos(complex(Inf,5.0)),complex(0.0,-Inf))
@test isequal(acos(complex(Inf,-5.0)),complex(0.0,Inf))

@test isequal(acos(complex(-Inf,Inf)),complex(3*pi/4,-Inf))
@test isequal(acos(complex(-Inf,-Inf)),complex(3*pi/4,Inf))

@test isequal(acos(complex(Inf,Inf)),complex(pi/4,-Inf))
@test isequal(acos(complex(Inf,-Inf)),complex(pi/4,Inf))

@test isequal(acos(complex(Inf,NaN)),complex(NaN,Inf))
@test isequal(acos(complex(-Inf,NaN)),complex(NaN,Inf))

@test isequal(acos(complex(NaN,0.0)),complex(NaN,NaN))
@test isequal(acos(complex(NaN,5.0)),complex(NaN,NaN))

@test isequal(acos(complex(NaN,Inf)),complex(NaN,-Inf))
@test isequal(acos(complex(NaN,-Inf)),complex(NaN,Inf))

@test isequal(acos(complex(NaN,NaN)),complex(NaN,NaN))



# asinh
#  asinh(conj(z)) = conj(asinh(z))
#  asinh(-z) = -asinh(z)
@test isequal(asinh(complex(0.0,0.0)),complex(0.0,0.0))
@test isequal(asinh(complex(0.0,-0.0)),complex(0.0,-0.0))
@test isequal(asinh(complex(-0.0,0.0)),complex(-0.0,0.0))
@test isequal(asinh(complex(-0.0,-0.0)),complex(-0.0,-0.0))

@test isequal(asinh(complex(0.0,Inf)),complex(Inf,pi/2))
@test isequal(asinh(complex(0.0,-Inf)),complex(Inf,-pi/2))
@test isequal(asinh(complex(-0.0,Inf)),complex(-Inf,pi/2))
@test isequal(asinh(complex(-0.0,-Inf)),complex(-Inf,-pi/2))

@test isequal(asinh(complex(5.0,Inf)),complex(Inf,pi/2))
@test isequal(asinh(complex(5.0,-Inf)),complex(Inf,-pi/2))
@test isequal(asinh(complex(-5.0,Inf)),complex(-Inf,pi/2))
@test isequal(asinh(complex(-5.0,-Inf)),complex(-Inf,-pi/2))

@test isequal(asinh(complex(0.0,NaN)),complex(NaN,NaN))
@test isequal(asinh(complex(5.0,NaN)),complex(NaN,NaN))

@test isequal(asinh(complex(Inf,Inf)),complex(Inf,pi/4))
@test isequal(asinh(complex(Inf,-Inf)),complex(Inf,-pi/4))
@test isequal(asinh(complex(-Inf,Inf)),complex(-Inf,pi/4))
@test isequal(asinh(complex(-Inf,-Inf)),complex(-Inf,-pi/4))

@test isequal(asinh(complex(Inf,NaN)),complex(Inf,NaN))
@test isequal(asinh(complex(-Inf,NaN)),complex(-Inf,NaN))

@test isequal(asinh(complex(NaN,0.0)),complex(NaN,0.0))
@test isequal(asinh(complex(NaN,-0.0)),complex(NaN,-0.0))

@test isequal(asinh(complex(NaN,5.0)),complex(NaN,NaN))

@test isequal(asinh(complex(NaN,Inf)),complex(Inf,NaN))
@test isequal(asinh(complex(NaN,-Inf)),complex(Inf,NaN))

@test isequal(asinh(complex(NaN,NaN)),complex(NaN,NaN))


# atanh
#  atanh(conj(z)) = conj(atanh(z))
#  atang(-z) = -atanh(z)

@test isequal(atanh(complex(0.0,0.0)),complex(0.0,0.0))
@test isequal(atanh(complex(0.0,-0.0)),complex(0.0,-0.0))
@test isequal(atanh(complex(-0.0,0.0)),complex(-0.0,0.0))
@test isequal(atanh(complex(-0.0,-0.0)),complex(-0.0,-0.0))

@test isequal(atanh(complex(0.0,NaN)),complex(0.0,NaN))
@test isequal(atanh(complex(-0.0,NaN)),complex(-0.0,NaN))

# raise divide-by-zero flag
@test isequal(atanh(complex(1.0,0.0)),complex(Inf,0.0))

@test isequal(atanh(complex(0.0,Inf)),complex(0.0,pi/2))
@test isequal(atanh(complex(0.0,-Inf)),complex(0.0,-pi/2))
@test isequal(atanh(complex(-0.0,Inf)),complex(-0.0,pi/2))
@test isequal(atanh(complex(-0.0,-Inf)),complex(-0.0,-pi/2))

@test isequal(atanh(complex(5.0,Inf)),complex(0.0,pi/2))
@test isequal(atanh(complex(5.0,-Inf)),complex(0.0,-pi/2))
@test isequal(atanh(complex(-5.0,Inf)),complex(-0.0,pi/2))
@test isequal(atanh(complex(-5.0,-Inf)),complex(-0.0,-pi/2))

@test isequal(atanh(complex(5.0,NaN)),complex(NaN,NaN))
@test isequal(atanh(complex(-5.0,NaN)),complex(NaN,NaN))

@test isequal(atanh(complex(Inf,0.0)),complex(0.0,pi/2))
@test isequal(atanh(complex(Inf,-0.0)),complex(0.0,-pi/2))
@test isequal(atanh(complex(-Inf,0.0)),complex(-0.0,pi/2))
@test isequal(atanh(complex(-Inf,-0.0)),complex(-0.0,-pi/2))

@test isequal(atanh(complex(Inf,5.0)),complex(0.0,pi/2))
@test isequal(atanh(complex(Inf,-5.0)),complex(0.0,-pi/2))
@test isequal(atanh(complex(-Inf,5.0)),complex(-0.0,pi/2))
@test isequal(atanh(complex(-Inf,-5.0)),complex(-0.0,-pi/2))

@test isequal(atanh(complex(Inf,Inf)),complex(0.0,pi/2))
@test isequal(atanh(complex(Inf,-Inf)),complex(0.0,-pi/2))
@test isequal(atanh(complex(-Inf,Inf)),complex(-0.0,pi/2))
@test isequal(atanh(complex(-Inf,-Inf)),complex(-0.0,-pi/2))

@test isequal(atanh(complex(Inf,NaN)),complex(0.0,NaN))
@test isequal(atanh(complex(-Inf,NaN)),complex(-0.0,NaN))

@test isequal(atanh(complex(NaN,0.0)),complex(NaN,NaN))
@test isequal(atanh(complex(NaN,-0.0)),complex(NaN,NaN))
@test isequal(atanh(complex(NaN,5.0)),complex(NaN,NaN))
@test isequal(atanh(complex(NaN,-5.0)),complex(NaN,NaN))

@test isequal(atanh(complex(NaN,Inf)),complex(0,pi/2))
@test isequal(atanh(complex(NaN,-Inf)),complex(0,-pi/2))

@test isequal(atanh(complex(NaN,NaN)),complex(NaN,NaN))