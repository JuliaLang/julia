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
    chd  = Cholesky(symd)
    chz  = Cholesky(herz)
    xd   = chd \ bd
    xz   = chz \ bz    
    @assert norm(symd * xd - bd) < Eps
    @assert norm(herz * xz - bz) < Eps
                                        # LU decomposition
    lud  = LU(sqd)
    luz  = LU(sqz)    
    xd   = lud \ bd
    xz   = luz \ bz
    @assert norm(sqd * xd - bd) < Eps
    @assert norm(sqz * xz - bz) < Eps
    invd = inv(lud)
    invz = inv(luz)
    @assert norm(invd * sqd - eye(size(sqd, 1))) < Eps
    @assert norm(invz * sqz - eye(size(sqz, 1))) < Eps

    qrd  = QR(mmd)                      # QR and QRP decompositions
    qrz  = QR(mmz)
    qrpd = QRP(mmd)
    qrpz = QRP(mmz)
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
