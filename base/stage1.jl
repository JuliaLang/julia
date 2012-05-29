module NewBase

include("sysimg.jl")

# invoke type inference, running the existing inference code on the new
# inference code to cache an optimized version of it.
begin
    local atypes = (LambdaStaticData, Tuple, (), LambdaStaticData, Bool)
    local minf = getmethods(typeinf, atypes)
    typeinf_ext(minf[1][3], atypes, (), minf[1][3])
end

ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      "$JULIA_HOME/../lib/julia/sys.ji", "start_image.jl")

end # module
