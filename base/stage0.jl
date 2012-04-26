module Base

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    length(a::Array) = arraylen(a)
    fprint(f,a::Array{Uint8,1}) = ccall(:jl_print_array_uint8, Void, (Any,), a)
    fprint(f,s::Symbol) = ccall(:jl_print_symbol, Void, (Any,), s)
    fprint(f,s::ASCIIString) = fprint(f,s.data)
    fprint(f,x) = show(f,x)
    fprintln(f,x) = (fprint(f,x);fprint(f,"\n"))
    show(f,x) = ccall(:jl_show_any, Void, (Ptr{Void}, Any,), f, x)
    show(f,s::ASCIIString) = fprint(f,s.data)
    show(f,s::Symbol) = fprint(f,s)
    show(f,b::Bool) = fprint(f,b ? "true" : "false")
    show(f,n::Int64) = ccall(:jl_print_int64, Void, (Ptr{Void}, Int64,), f, n)
    show(f,n::Integer)  = show(f,int64(n))
    print(a...) = for x=a; print(x); end
    function show(f,e::Expr)
        fprint(f,e.head)
        fprint(f,"(")
        for i=1:arraylen(e.args)
            show(f,arrayref(e.args,i))
            fprint(f,", ")
        end
        fprint(f,")\n")
    end
end

include("sysimg.jl")

ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      "$JULIA_HOME/sys0.ji", "start_image.jl")

end # module
