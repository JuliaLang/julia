# blas, lapack
load("../extras/factorizations.jl")

Eps = sqrt(eps())

begin
    local n
    n    = 10
    sqd  = rand(n,n)
    sqz  = reshape([complex128(x) for x in sqd], size(sqd))
    mmd  = rand(3*n, n)
    mmz  = reshape([complex128(x) for x in mmd], size(mmd))
    symd = mmd' * mmd
    herz = conj(mmz)' * mmz
    bd   = rand(n)
    bz   = [complex128(x) for x in bd]
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
end
