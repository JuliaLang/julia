libc = dlopen("libc")

sleep(s::Scalar) = ccall(dlsym(libc,"usleep"), Uint32, (Uint32,), uint32(round(s*1e6)))
fork() = ccall(dlsym(libc,"fork"), Int32, ())
