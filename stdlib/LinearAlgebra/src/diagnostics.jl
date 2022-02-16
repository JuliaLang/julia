# LinearAlgebra.__diagnostics__()
function __diagnostics__(io::IO)
    versioninfo(io) # LinearAlgebra.versioninfo()
    blas_threads = BLAS.get_num_threads()
    println(io, "LinearAlgebra.BLAS.get_num_threads() = ", blas_threads)
    return nothing
end
