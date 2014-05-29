
type Interfaces
    functions::Dict{Symbol,Vector{DataType}}
    signatures::Dict{(Symbol,DataType),Any}

    Interfaces() = new(Dict{Symbol,Vector{DataType}}(), Dict{(Symbol,DataType),Any}())
end

_interfaces = Interfaces()
 
function add_interface(d::DataType, f::Symbol, signature=nothing)
 
    if haskey(_interfaces.functions, f)
       push!(_interfaces.functions[f], d) 
    else
       _interfaces.functions[f] = DataType[d]
    end

    if signature == nothing # now the signature will not be checked anymore
        _interfaces.signatures[(f,d)] = nothing
    else
        if haskey(_interfaces.signatures, (f,d))
            if _interfaces.signatures[(f,d)] != nothing
                push!(_interfaces.signatures[(f,d)], signature)
            end
        else
           _interfaces.signatures[(f,d)] = [signature]
        end
    end
    nothing
end
 
function verify_interface(d::DataType)
    for (f,itypes) in _interfaces.functions
    
        isInInterfaceTable = false
 
        for it in itypes
            if d <: it
            
                isInMethodTable = false
            
                if !isdefined(f)
                    error("Interface not implemented! $d has to implement ", string(f), " in order to be subtype of $it !")
                end
                mt = methods(eval(Main,f))
            
                if _interfaces.signatures[(f,it)] == nothing
                    for m in mt
                        if d <: m.sig[1]
                            isInMethodTable = true
                        end
                    end
                    if !isInMethodTable
                        error("Interface not implemented! $d has to implement ", string(f), " in order to be subtype of $it !")
                    end                    
                else 
                    for ex in _interfaces.signatures[(f,it)]
                        isInMethodTable = false
                        typeargs = eval(Main,ex)
                        for m in mt
                            if length(m.sig) == length(typeargs)
                                same_signature = true
                                if !( d <: m.sig[1] )
                                    same_signature = false
                                end
                                for i=2:length(m.sig)
                                    if !( typeargs[i] <: m.sig[i] )
                                        same_signature = false
                                    end
                                end
                                if same_signature
                                    isInMethodTable = true
                                end
                            end
                        end
                        if !isInMethodTable
                            error("Interface not implemented! $d has to implement ", string(f), " in order to be subtype of $it !")
                        end                        
                    end
                end
            end
        end
    end    
    
    return true
end

function isinterface(f::Symbol, args)
    if !haskey(_interfaces.functions,f) || length(args) == 0
        return (false,Nothing)
    end

    itypes = _interfaces.functions[f]
    d = typeof(args[1])
    for it in itypes
        if d <: eval(Main,it)
            if _interfaces.signatures[(f,it)] == nothing
                return (true,it)                  
            else 
                for ex in _interfaces.signatures[(f,it)]
                    isInMethodTable = false
                    typeargs = eval(Main,ex)
                        
                    if length(args) == length(typeargs)
                        same_signature = true
                        if !( d <: typeof(args[1]))
                            same_signature = false
                        end
                        for i=2:length(args)
                            if !( typeargs[i] <: typeof(args[i]) )
                                same_signature = false
                            end
                        end
                        if same_signature
                           return (true,it)
                        end                        
                    end
                end
            end
        end
    end    
 
    return (false,Nothing)
end

