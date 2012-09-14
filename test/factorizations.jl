# blas, lapack

Eps = sqrt(eps())

begin
    local n
    n    = 10
    srand(1234321)
    sqd  = rand(n,n)
    sqz  = complex(sqd)
    mmd  = rand(3*n, n)
    mmz  = complex(mmd)
    symd = mmd' * mmd
    herz = mmz' * mmz
    bd   = rand(n)
    bz   = complex(bd)
                                        # Cholesky decomposition
    chd  = chol(symd)
    chz  = chol(herz)
    xd   = chd \ bd
    xz   = chz \ bz    
    @assert norm(symd * xd - bd) < Eps
    @assert norm(herz * xz - bz) < Eps
                                        # LU decomposition
    lud  = lu(sqd)
    luz  = lu(sqz)    
    xd   = lud \ bd
    xz   = luz \ bz
    @assert norm(sqd * xd - bd) < Eps
    @assert norm(sqz * xz - bz) < Eps
    invd = inv(lud)
    invz = inv(luz)
    @assert norm(invd * sqd - eye(size(sqd, 1))) < Eps
    @assert norm(invz * sqz - eye(size(sqz, 1))) < Eps

    @assert_approx_eq det(sqd) prod(eig(sqd)[1]) # determinant
    @assert_approx_eq det(sqz) prod(eig(sqz)[1])
    @assert_approx_eq det(symd) prod(eig(symd)[1])
#    @assert_approx_eq det(herz) prod(eig(herz)[1])

    qrd  = qr(mmd)                      # QR and QRP decompositions
    qrz  = qr(mmz)
    qrpd = qrp(mmd)
    qrpz = qrp(mmz)
    yyd  = randn(3*n)
    yyz  = complex(yyd)
    qyd  = qrd * yyd
    qpyd = qrd' * yyd
    @assert abs(norm(qyd) - norm(yyd)) < Eps # Q is orthogonal
    @assert abs(norm(qpyd) - norm(yyd)) < Eps 
    qyz  = qrz * yyz
    qpyz = qrz' * yyz
    @assert abs(norm(qyz) - norm(yyz)) < Eps # Q is unitary
    @assert abs(norm(qpyz) - norm(yyz)) < Eps # Q is unitary
    
end

# Test det(A::Matrix)
# In the long run, these tests should step through Strang's
#  axiomatic definition of determinants.
# If all axioms are satisfied and all the composition rules work,
#  all determinants will be correct except for floating point errors.

# The determinant of the identity matrix should always be 1.
for n = 1:10
  A = eye(n)
  @assert abs(det(A) - 1.0) < Eps
end

# The determinant of a Householder reflection matrix should always be -1.
for i = 1:10
  A = eye(10)
  A[i, i] = -1.0
  @assert abs(det(A) - (-1.0)) < Eps
end

# The determinant of a rotation matrix should always be 1.
for theta = pi ./ [1:4]
  R = [cos(theta) -sin(theta);
       sin(theta) cos(theta)]
  @assert abs(det(R) - 1.0) < Eps
end
