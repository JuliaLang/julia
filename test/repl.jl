# REPL tests
using TestHelpers
import Base: REPL, LineEdit

function fake_repl()
    # Use pipes so we can easily do blocking reads
    # In the future if we want we can add a test that the right object
    # gets displayed by intercepting the display
    stdin_read,stdin_write = (Base.Pipe(C_NULL), Base.Pipe(C_NULL))
    stdout_read,stdout_write = (Base.Pipe(C_NULL), Base.Pipe(C_NULL))
    stderr_read,stderr_write = (Base.Pipe(C_NULL), Base.Pipe(C_NULL))
    Base.link_pipe(stdin_read,true,stdin_write,true)
    Base.link_pipe(stdout_read,true,stdout_write,true)
    Base.link_pipe(stderr_read,true,stderr_write,true)

    repl = Base.REPL.LineEditREPL(TestHelpers.FakeTerminal(stdin_read, stdout_write, stderr_write))
    stdin_write, stdout_read, stdout_read, repl
end

# Writing ^C to the repl will cause sigint, so let's not die on that
ccall(:jl_exit_on_sigint, Void, (Cint,), 0)
# These are integration tests. If you want to unit test test e.g. completion, or
# exact LineEdit behavior, put them in the appropriate test files.
# Furthermore since we are emulating an entire terminal, there may be control characters
# in the mix. If verification needs to be done, keep it to the bare minimum. Basically
# this should make sure nothing crashes without depending on how exactly the control
# characters are being used.
begin
stdin_write, stdout_read, stdout_read, repl = fake_repl()

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
# Test cd feature in shell mode.  We limit to 40 characters when
# calling readuntil() to suppress the warning it (currently) gives for
# long strings.
origpwd = pwd()
tmpdir = mktempdir()
write(stdin_write, ";")
readuntil(stdout_read, "shell> ")
write(stdin_write, "cd $(escape_string(tmpdir))\n")
readuntil(stdout_read, "cd $(escape_string(tmpdir))"[max(1,end-39):end])
readuntil(stdout_read, realpath(tmpdir)[max(1,end-39):end])
readuntil(stdout_read, "\n")
readuntil(stdout_read, "\n")
@test pwd() == realpath(tmpdir)
write(stdin_write, ";")
readuntil(stdout_read, "shell> ")
write(stdin_write, "cd -\n")
readuntil(stdout_read, origpwd[max(1,end-39):end])
readuntil(stdout_read, "\n")
readuntil(stdout_read, "\n")
@test pwd() == origpwd
write(stdin_write, ";")
readuntil(stdout_read, "shell> ")
write(stdin_write, "cd\n")
readuntil(stdout_read, homedir()[max(1,end-39):end])
readuntil(stdout_read, "\n")
readuntil(stdout_read, "\n")
@test pwd() == homedir()
rm(tmpdir)
cd(origpwd)
# Close REPL ^D
write(stdin_write, '\x04')
wait(repltask)
end

function buffercontents(buf::IOBuffer)
    p = position(buf)
    seek(buf,0)
    c = readall(buf)
    seek(buf,p)
    c
end

# Test various history related issues
begin
    stdin_write, stdout_read, stdout_read, repl = fake_repl()
    # In the future if we want we can add a test that the right object
    # gets displayed by intercepting the display
    repl.specialdisplay = Base.REPL.REPLDisplay(repl)

    interface = REPL.setup_interface(repl)
    repl_mode = interface.modes[1]
    shell_mode = interface.modes[2]
    help_mode = interface.modes[3]
    histp = interface.modes[4]

    hp = REPL.REPLHistoryProvider(Dict{Symbol,Any}(:julia => repl_mode,
                                                   :shell => shell_mode,
                                                   :help  => help_mode))
    fakehistory =
    """
    # time: 2014-06-30 17:32:49 EDT
    # mode: julia
    \tshell
    # time: 2014-06-30 17:32:59 EDT
    # mode: shell
    \tll
    # time: 2014-06-30 17:32:49 EDT
    # mode: julia
    \t1 + 1
    # time: 2014-06-30 17:35:39 EDT
    # mode: foobar
    \tbarfoo
    # time: 2014-06-30 18:44:29 EDT
    # mode: shell
    \tls
    # time: 2014-06-30 19:44:29 EDT
    # mode: foobar
    \tls
    # time: 2014-06-30 20:44:29 EDT
    # mode: julia
    \t2 + 2"""

    REPL.hist_from_file(hp, IOBuffer(fakehistory))
    REPL.history_reset_state(hp)

    histp.hp = repl_mode.hist = shell_mode.hist = help_mode.hist = hp

    # Some manual setup
    s = LineEdit.init_state(repl.t, interface)

    # Test that navigating history skips invalid modes
    # (in both directions)
    LineEdit.history_prev(s, hp)
    @test LineEdit.mode(s) == repl_mode
    @test buffercontents(LineEdit.buffer(s)) == "2 + 2"
    LineEdit.history_prev(s, hp)
    @test LineEdit.mode(s) == shell_mode
    @test buffercontents(LineEdit.buffer(s)) == "ls"
    LineEdit.history_prev(s, hp)
    @test LineEdit.mode(s) == repl_mode
    @test buffercontents(LineEdit.buffer(s)) == "1 + 1"
    LineEdit.history_next(s, hp)
    @test LineEdit.mode(s) == shell_mode
    @test buffercontents(LineEdit.buffer(s)) == "ls"
    LineEdit.history_next(s, hp)
    @test LineEdit.mode(s) == repl_mode
    @test buffercontents(LineEdit.buffer(s)) == "2 + 2"
    LineEdit.history_next(s, hp)

    # Test that searching backwards puts you into the correct mode and
    # skips invalid modes.
    LineEdit.enter_search(s, histp, true)
    ss = LineEdit.state(s, histp)
    write(ss.query_buffer, "l")
    LineEdit.update_display_buffer(ss, ss)
    LineEdit.accept_result(s, histp)
    @test LineEdit.mode(s) == shell_mode
    @test buffercontents(LineEdit.buffer(s)) == "ls"

    # Test that searching for `ll` actually matches `ll` after
    # both letters are types rather than jumping to `shell`
    LineEdit.history_prev(s, hp)
    LineEdit.enter_search(s, histp, true)
    write(ss.query_buffer, "l")
    LineEdit.update_display_buffer(ss, ss)
    write(ss.query_buffer, "l")
    LineEdit.update_display_buffer(ss, ss)
    LineEdit.accept_result(s, histp)
    @test LineEdit.mode(s) == shell_mode
    @test buffercontents(LineEdit.buffer(s)) == "ll"

    # Issue #7551
    # Enter search mode and try accepting an empty result
    REPL.history_reset_state(hp)
    LineEdit.edit_clear(s)
    cur_mode = LineEdit.mode(s)
    LineEdit.enter_search(s, histp, true)
    LineEdit.accept_result(s, histp)
    @test LineEdit.mode(s) == cur_mode
    @test buffercontents(LineEdit.buffer(s)) == ""
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

fds = ccall(:open,Cint,(Ptr{UInt8},Cint),ccall(:ptsname,Ptr{UInt8},(Cint,),fdm), O_RDWR|O_NOCTTY)

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

ccall(:close,Cint,(Cint,),fds)
output = readall(master)
@test output == "julia> 1\r\nquit()\r\n1\r\n\r\njulia> "
close(master)

end

# Test stream mode
outs, ins, p = readandwrite(`$exename -f --quiet`)
write(ins,"1\nquit()\n")
@test readall(outs) == "1\n"
end
