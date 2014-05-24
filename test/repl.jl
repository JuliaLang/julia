# REPL tests
let exename=joinpath(JULIA_HOME,(ccall(:jl_is_debugbuild,Cint,())==0?"julia":"julia-debug"))

# Test REPL in dumb mode
@unix_only begin

const O_RDWR = Base.FS.JL_O_RDWR
const O_NOCTTY = Base.FS.JL_O_NOCTTY

fdm = ccall(:posix_openpt,Cint,(Cint,),O_RDWR|O_NOCTTY)
fdm == -1 && error("Failed to open PTY master")
rc = ccall(:grantpt,Cint,(Cint,),fdm)
rc != 0 && error("grantpt failed")
rc = ccall(:unlockpt,Cint,(Cint,),fdm)
rc != 0 && error("unlockpt")

fds = ccall(:open,Cint,(Ptr{Uint8},Cint),ccall(:ptsname,Ptr{Uint8},(Cint,),fdm), O_RDWR|O_NOCTTY)

# slave
slave   = RawFD(fds)
master = Base.TTY(RawFD(fdm); readable = true)

nENV = copy(ENV)
nENV["TERM"] = "dumb"
p = spawn(setenv(`$exename -f --quiet`,nENV),slave,slave,slave)
start_reading(master)
Base.wait_readnb(master,1)
write(master,"1\nquit()\n")

wait(p)

output = readavailable(master)
@test output == "julia> 1\r\nquit()\r\n1\r\n\r\njulia> "

close(master)
ccall(:close,Cint,(Cint,),fds)

end

# Test stream mode
outs, ins, p = readandwrite(`$exename -f --quiet`)
write(ins,"1\nquit()\n")
@test readall(outs) == "1\n"
end
