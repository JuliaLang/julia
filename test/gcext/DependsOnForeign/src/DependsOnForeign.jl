module DependsOnForeign

using Foreign

f(obj::FObj) = Base.pointer_from_objref(obj)
precompile(f, (FObj,))

const FObjRef = Ref{FObj}()

function __init__()
    FObjRef[] = FObj()
end

end # module DependsOnForeign
