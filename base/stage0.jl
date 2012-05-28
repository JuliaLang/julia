module Base

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    length(a::Array) = arraylen(a)
    print(x) = print(stdout_stream, x)
    show(x) = show(stdout_stream, x)
    write(io, a::Array{Uint8,1}) =
        ccall(:ios_write, Uint, (Ptr{Void}, Ptr{Void}, Uint),
              io.ios, a, length(a))
    print(io, s::Symbol) = ccall(:jl_print_symbol, Void, (Ptr{Void},Any,),
                                 io.ios, s)
    print(io, s::ASCIIString) = (write(io, s.data);nothing)
    print(io, x) = show(io, x)
    println(io, x) = (print(io, x); print(io, "\n"))
    show(io, x) = ccall(:jl_show_any, Void, (Any, Any,), io, x)
    show(io, s::ASCIIString) = (write(io, s.data);nothing)
    show(io, s::Symbol) = print(io, s)
    show(io, b::Bool) = print(io, b ? "true" : "false")
    show(io, n::Int64) = ccall(:jl_print_int64, Void, (Ptr{Void}, Int64,), io, n)
    show(io, n::Integer)  = show(io, int64(n))
    print(io, a...) = for x=a; print(io, x); end
    function show(io, e::Expr)
        print(io, e.head)
        print(io, "(")
        for i=1:arraylen(e.args)
            show(io, arrayref(e.args,i))
            print(io, ", ")
        end
        print(io, ")\n")
    end
end

include("sysimg.jl")

ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      "$JULIA_HOME/../lib/julia/sys0.ji", "start_image.jl")

end # module
