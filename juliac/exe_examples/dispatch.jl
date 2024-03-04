
module Bar

@noinline foo(@nospecialize x) = bar(x)

@noinline function bar(x::Int)
    return x
end

@noinline function bar(x::Float64)
    return 2x
end

@noinline function bar(x::Float32)
    return 3x
end

@noinline function bar(x::Float16)
    return 4x
end

function myrand()
    UInt(0) + ccall(:rand, UInt32, ()) + (ccall(:rand, UInt32, ()) << 32)
end

Base.@ccallable function main() :: Cint
    # println("Hello, world!")
    task = current_task()
    ccall(:srand, Cvoid, (UInt32,), ccall(:time, UInt32,(Ptr{Cvoid},), C_NULL))
    task.rngState0 = myrand()
    task.rngState1 = myrand()
    task.rngState2 = myrand()
    task.rngState3 = myrand()
    task.rngState4 = myrand()
    a = rand()
    b = foo(a)
    ccall(:printf, Int32, (Ptr{UInt8},Float64...), "hello_world %lf %lf \n", b, a)
    return 0
end

main()

precompile(main,())

end

