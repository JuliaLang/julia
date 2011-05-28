# libc = dlopen("libc")

## time-related functions ##

sleep(s::Real) = ccall(dlsym(libc, :usleep), Uint32, (Uint32,), uint32(round(s*1e6)))
unixtime() = ccall(dlsym(libc, :time), Uint32, (Ptr{Uint32},), C_NULL)

## process-related functions ##

system(cmd::String) =
    ccall(dlsym(libc, :system), Int32, (Ptr{Uint8},), cstring(cmd))

## file and directory ##

function getcwd()
    b = Array(Uint8,1024)
    p = ccall(dlsym(libc, :getcwd), Ptr{Uint8}, (Ptr{Uint8}, Ulong),
              b, ulong(length(b)))
    if p == C_NULL
        error("path too long")
    end
    string(p)
end
