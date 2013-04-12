
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
@test isequal(log(complex(-0.0,0.0)), complex(-Inf,pi))
@test isequal(log(complex(-0.0,-0.0)), complex(-Inf,-pi))

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


# sinh: has properties
#  sinh(conj(z)) = conj(sinh(z))
#  sinh(-z) = -sinh(z)

@test isequal(sinh(complex(0.0,0.0)),complex(0.0,0.0))
@test isequal(sinh(complex(0.0,-0.0)),complex(0.0,-0.0))
@test isequal(sinh(complex(-0.0,0.0)),complex(-0.0,0.0))
@test isequal(sinh(complex(-0.0,-0.0)),complex(-0.0,-0.0))

@test isequal(sinh(complex(0.0,Inf)),complex(0.0,NaN))
@test isequal(sinh(complex(0.0,-Inf)),complex(0.0,NaN))
@test isequal(sinh(complex(-0.0,Inf)),complex(-0.0,NaN))
@test isequal(sinh(complex(-0.0,-Inf)),complex(-0.0,NaN))

@test isequal(sinh(complex(0.0,NaN)),complex(0.0,NaN))
@test isequal(sinh(complex(-0.0,NaN)),complex(-0.0,NaN))

@test isequal(sinh(complex(5.0,Inf)),complex(NaN,NaN))

@test isequal(sinh(complex(5.0,NaN)),complex(NaN,NaN))

@test isequal(sinh(complex(Inf,0.0)),complex(Inf,0.0))
@test isequal(sinh(complex(Inf,-0.0)),complex(Inf,-0.0))
@test isequal(sinh(complex(-Inf,0.0)),complex(-Inf,0.0))
@test isequal(sinh(complex(-Inf,-0.0)),complex(-Inf,-0.0))

@test isequal(sinh(complex(Inf,5.0)),complex(sign(cos(5.0))*Inf,sign(sin(5.0))*Inf))
@test isequal(sinh(complex(-Inf,5.0)),complex(sign(cos(5.0))*-Inf,sign(sin(5.0))*Inf))

@test isequal(sinh(complex(Inf,Inf)),complex(Inf,NaN))
@test isequal(sinh(complex(Inf,-Inf)),complex(Inf,NaN))
@test isequal(sinh(complex(-Inf,Inf)),complex(-Inf,NaN))
@test isequal(sinh(complex(-Inf,-Inf)),complex(-Inf,NaN))

@test isequal(sinh(complex(Inf,NaN)),complex(Inf,NaN))
@test isequal(sinh(complex(-Inf,NaN)),complex(-Inf,NaN))

@test isequal(sinh(complex(NaN,0.0)),complex(NaN,0.0))
@test isequal(sinh(complex(NaN,-0.0)),complex(NaN,-0.0))

@test isequal(sinh(complex(NaN,5.0)),complex(NaN,NaN))

@test isequal(sinh(complex(NaN,NaN)),complex(NaN,NaN))


# cosh: has properties
#  cosh(conj(z)) = conj(cosh(z))
#  coshh(-z) = cosh(z)

@test isequal(cosh(complex(0.0,0.0)),complex(1.0,0.0))
@test isequal(cosh(complex(0.0,-0.0)),complex(1.0,-0.0))
@test isequal(cosh(complex(-0.0,-0.0)),complex(1.0,0.0))
@test isequal(cosh(complex(-0.0,0.0)),complex(1.0,-0.0))

@test isequal(cosh(complex(0.0,Inf)),complex(NaN,0.0))
@test isequal(cosh(complex(0.0,-Inf)),complex(NaN,-0.0))

@test isequal(cosh(complex(0.0,NaN)),complex(NaN,0.0))
@test isequal(cosh(complex(-0.0,NaN)),complex(NaN,0.0))

@test isequal(cosh(complex(5.0,Inf)),complex(NaN,NaN))

@test isequal(cosh(complex(5.0,NaN)),complex(NaN,NaN))

@test isequal(cosh(complex(Inf,0.0)),complex(Inf,0.0))
@test isequal(cosh(complex(Inf,-0.0)),complex(Inf,-0.0))
@test isequal(cosh(complex(-Inf,-0.0)),complex(Inf,0.0))
@test isequal(cosh(complex(-Inf,0.0)),complex(Inf,-0.0))

@test isequal(cosh(complex(Inf,5.0)),complex(sign(cos(5.0))*Inf,sign(sin(5.0))*Inf))
@test isequal(cosh(complex(Inf,-5.0)),complex(sign(cos(5.0))*Inf,sign(sin(5.0))*-Inf))
@test isequal(cosh(complex(-Inf,-5.0)),complex(sign(cos(5.0))*Inf,sign(sin(5.0))*Inf))
@test isequal(cosh(complex(-Inf,5.0)),complex(sign(cos(5.0))*Inf,sign(sin(5.0))*-Inf))

@test isequal(cosh(complex(Inf,Inf)),complex(Inf,NaN))
@test isequal(cosh(complex(Inf,-Inf)),complex(Inf,NaN))
@test isequal(cosh(complex(-Inf,-Inf)),complex(Inf,NaN))
@test isequal(cosh(complex(-Inf,Inf)),complex(Inf,NaN))

@test isequal(cosh(complex(Inf,NaN)),complex(Inf,NaN))
@test isequal(cosh(complex(-Inf,NaN)),complex(Inf,NaN))

@test isequal(cosh(complex(NaN,0.0)),complex(NaN,0.0))
@test isequal(cosh(complex(NaN,-0.0)),complex(NaN,-0.0))

@test isequal(cosh(complex(NaN,5.0)),complex(NaN,NaN))
@test isequal(cosh(complex(NaN,-5.0)),complex(NaN,NaN))

@test isequal(cosh(complex(NaN,NaN)),complex(NaN,NaN))

