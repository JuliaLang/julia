module System

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
    function show(bt::BackTrace)
        show(bt.e)
        i = 1
        t = bt.trace
        while i < length(t)
            print("\n")
            lno = t[i+2]
            print("in ", t[i], ", ", t[i+1], ":", lno)
            i += 3
        end
        print("\n")
    end
end

load("sysimg.j")

ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      cstring("sys.ji"), cstring("j/start_image.j"))

end # module
