# from Patrick O'Leary

macro ccallWrap(lib, fnSym, retType, argTypes)
    args = [gensym() for i in 1:length(argTypes.args)]
    fnArgs = [:($(args[i])::$(argTypes[i]))
                 for i in 1:length(argTypes)]

    :(($fnSym)($fnArgs...) = ccall(dlsym($lib, $fnSym), $retType, $argTypes, $args...))
end 

