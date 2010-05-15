libc = dlopen("libc")

unixtime() = ccall(dlsym(libc,"time"), Uint32, (Int32,), 0)
sleep(s::Scalar) = ccall(dlsym(libc,"usleep"), Uint32, (Uint32,), uint32(round(s*1e6)))
system(cmd::String) = ccall(dlsym(libc,"system"), Int32, (Pointer{Uint8},), cmd)
fork() = ccall(dlsym(libc,"fork"), Int32, ())
