module Base

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    length(a::Array) = arraylen(a)
    print(a::Array{Uint8,1}) = ccall(:jl_print_array_uint8, Void, (Any,), a)
    print(s::Symbol) = ccall(:jl_print_symbol, Void, (Any,), s)
    print(s::ASCIIString) = print(s.data)
    print(x) = show(x)
    println(x) = (print(x);print("\n"))
    show(x) = ccall(:jl_show_any, Void, (Any,), x)
    show(s::ASCIIString) = print(s.data)
    show(s::Symbol) = print(s)
    show(b::Bool) = print(b ? "true" : "false")
    show(n::Int64) = ccall(:jl_print_int64, Void, (Int64,), n)
    show(n::Integer)  = show(int64(n))
    print(a...) = for x=a; print(x); end
    function show(e::Expr)
        print(e.head)
        print("(")
        for i=1:arraylen(e.args)
            show(arrayref(e.args,i))
            print(", ")
        end
        print(")\n")
    end
end

include("sysimg.jl")

ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      "$JULIA_HOME/sys0.ji", "start_image.jl")

end # module
