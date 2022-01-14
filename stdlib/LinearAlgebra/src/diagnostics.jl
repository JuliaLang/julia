# LinearAlgebra.__diagnostics__()
function __diagnostics__(io::IO)
    versioninfo(io) # LinearAlgebra.versioninfo()
    return nothing
end
