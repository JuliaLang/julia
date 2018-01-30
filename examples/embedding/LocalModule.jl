__precompile__()
module LocalModule

using Distributed
export myapp

function myapp()
    p = addprocs(1)
    @everywhere p println("Taking over the world...")
    rmprocs(p)
    nothing
end

end
