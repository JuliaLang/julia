libc = dlopen("libc")

sleep(s::Scalar) = ccall(dlsym(libc,"usleep"), Uint32, (Uint32,), uint32(round(s*1e6)))
unixtime() = ccall(dlsym(libc,"time"), Uint32, (Ptr{Uint32},), C_NULL)
function ftime()
    t = Array(Uint64,2)
    ccall(dlsym(libc,"gettimeofday"),
          Int32,
          (Ptr{Uint64}, Ptr{Uint8}),
          t, C_NULL)
    # TODO: this is a hack and probably doesn't work on big-endian systems
    return float64(t[1]) + float64(t[2])/1e6
end

system(cmd::String) = ccall(dlsym(libc,"system"), Int32, (Ptr{Uint8},), cmd)
fork() = ccall(dlsym(libc,"fork"), Int32, ())
function exec(cmd::String, args::String...)
    arr = Array(String, length(args))
    for i = i:length(args); arr[i] = args[i]; end
    ccall(dlsym(libc,"execvp"),
          Int32,
          (Ptr{Uint8}, Ptr{Ptr{Uint8}}),
          cmd, arr)
end
