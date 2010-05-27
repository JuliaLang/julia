libc = dlopen("libc")

sleep(s::Scalar) = ccall(dlsym(libc,"usleep"), Uint32, (Uint32,), uint32(round(s*1e6)))
unixtime() = ccall(dlsym(libc,"time"), Uint32, (Ptr{Uint32},), C_NULL)
function ftime()
    t = Array(Uint64,2)
    ccall(dlsym(libc,"gettimeofday"),
          Int32,
          (Ptr{Uint64}, Ptr{Char}),
          t, C_NULL)
    # TODO: this is a hack and probably doesn't work on big-endian systems
    return float64(t[1]) + float64(t[2])/1e6
end

system(cmd::String) = ccall(dlsym(libc,"system"), Int32, (Ptr{Char},), cmd)
fork() = ccall(dlsym(libc,"fork"), Int32, ())
function exec(cmd::String, args::String...)
    arr = Array(Ptr{Char}, length(args)+2)
    arr[1] = cmd
    for i = 1:length(args); arr[i+1] = args[i]; end
    arr[length(args)+2] = C_NULL
    ccall(dlsym(libc,"execvp"),
          Int32,
          (Ptr{Char}, Ptr{Ptr{Char}}),
          cmd, arr)
end
