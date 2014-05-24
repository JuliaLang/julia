# REPL tests

@unix_only begin

exename=joinpath(JULIA_HOME,(ccall(:jl_is_debugbuild,Cint,())==0?"julia":"julia-debug"))

const O_RDWR = Base.FS.JL_O_RDWR
const O_NOCTTY = 0x20000

fdm = ccall(:posix_openpt,Cint,(Cint,),O_RDWR|O_NOCTTY)
fdm == -1 && error("Failed to open PTY master")
rc = ccall(:grantpt,Cint,(Cint,),fdm)
rc != 0 && error("grantpt failed")
rc = ccall(:unlockpt,Cint,(Cint,),fdm)
rc != 0 && error("unlockpt")

fds = ccall(:open,Cint,(Ptr{Uint8},Cint),ccall(:ptsname,Ptr{Uint8},(Cint,),fdm), O_RDWR)

# slave
slaveSTDIN   = Base.TTY(RawFD(fds); readable = true)
slaveSTDERR  = Base.TTY(RawFD(fds))
slaveSTDOUT  = Base.TTY(RawFD(fds))
master = Base.TTY(RawFD(fdm); readable = true)

# First test REPL in dumb mode
nENV = copy(ENV)
nENV["TERM"] = "dumb"
p = spawn(setenv(`$exename --quiet`,nENV),slaveSTDIN,slaveSTDOUT,slaveSTDERR)
write(master,"1\nquit()\n")

wait(p)

@test readavailable(master) == "julia> 1\r\nquit()\r\n1\r\n\r\njulia> "

close(master)
close(slaveSTDIN)
close(slaveSTDOUT)
close(slaveSTDERR)

end