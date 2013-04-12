
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
