# REPL tests

# Writing ^C to the repl will cause sigint, so let's not die on that
ccall(:jl_exit_on_sigint, Void, (Cint,), 0)
# These are integration tests. If you want to unit test test e.g. completion, or
# exact LineEdit behavior, put them in the appropriate test files.
# Furthermore since we are emulating an entire terminal, there may be control characters
# in the mix. If verification needs to be done, keep it to the bare minimum. Basically
# this should make sure nothing crashes without depending on how exactly the control
# characters are being used.
begin
# Use pipes so we can easily do blocking reads
stdin_read,stdin_write = (Base.Pipe(C_NULL), Base.Pipe(C_NULL))
stdout_read,stdout_write = (Base.Pipe(C_NULL), Base.Pipe(C_NULL))
stderr_read,stderr_write = (Base.Pipe(C_NULL), Base.Pipe(C_NULL))
Base.link_pipe(stdin_read,true,stdin_write,true)
Base.link_pipe(stdout_read,true,stdout_write,true)
Base.link_pipe(stderr_read,true,stderr_write,true)

type FakeTerminal <: Base.Terminals.UnixTerminal
    in_stream::Base.IO
    out_stream::Base.IO
    err_stream::Base.IO
    hascolor::Bool
    raw::Bool
    FakeTerminal(stdin,stdout,stderr,hascolor=true) =
        new(stdin,stdout,stderr,hascolor,false)
end

Base.Terminals.hascolor(t::FakeTerminal) = t.hascolor
Base.Terminals.raw!(t::FakeTerminal, raw::Bool) = t.raw = raw
Base.Terminals.size(t::FakeTerminal) = (24, 80)

repl = Base.REPL.LineEditREPL(FakeTerminal(stdin_read, stdout_write, stderr_write))
# In the future if we want we can add a test that the right object
# gets displayed by intercepting the display
repl.specialdisplay = Base.REPL.REPLDisplay(repl)
repl.no_history_file = true

repltask = @async begin
    Base.REPL.run_repl(repl)
end

sendrepl(cmd) = write(stdin_write,"inc || wait(b); r = $cmd; notify(c); r\r")

inc = false
b = Condition()
c = Condition()
sendrepl("\"Hello REPL\"")
inc=true
begin
    notify(b)
    wait(c)
end
# Latex completions
write(stdin_write, "\x32\\alpha\t")
readuntil(stdout_read, "α")
# Bracketed paste in search mode
write(stdin_write, "\e[200~paste here ;)\e[201~")
# Abort search (^C)
write(stdin_write, '\x03')
# Test basic completion in main mode
write(stdin_write, "Base.REP\t")
readuntil(stdout_read, "Base.REPL")
write(stdin_write, '\x03')
write(stdin_write, "\\alpha\t")
readuntil(stdout_read,"α")
write(stdin_write, '\x03')
# Close REPL ^D
write(stdin_write, '\x04')
wait(repltask)
end

ccall(:jl_exit_on_sigint, Void, (Cint,), 1)

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
