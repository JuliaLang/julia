# This file is a part of Julia. License is MIT: http://julialang.org/license

const curmod = current_module()
const curmod_name = fullname(curmod)
const curmod_prefix = "$(["$m." for m in curmod_name]...)"

# REPL tests
isdefined(Main, :TestHelpers) || @eval Main include(joinpath(dirname(@__FILE__), "TestHelpers.jl"))
using TestHelpers
import Base: REPL, LineEdit

function fake_repl()
    # Use pipes so we can easily do blocking reads
    # In the future if we want we can add a test that the right object
    # gets displayed by intercepting the display
    stdin_read,stdin_write = (Base.PipeEndpoint(), Base.PipeEndpoint())
    stdout_read,stdout_write = (Base.PipeEndpoint(), Base.PipeEndpoint())
    stderr_read,stderr_write = (Base.PipeEndpoint(), Base.PipeEndpoint())
    Base.link_pipe(stdin_read,true,stdin_write,true)
    Base.link_pipe(stdout_read,true,stdout_write,true)
    Base.link_pipe(stderr_read,true,stderr_write,true)

    repl = Base.REPL.LineEditREPL(TestHelpers.FakeTerminal(stdin_read, stdout_write, stderr_write))
    stdin_write, stdout_read, stderr_read, repl
end

# Writing ^C to the repl will cause sigint, so let's not die on that
ccall(:jl_exit_on_sigint, Void, (Cint,), 0)
# These are integration tests. If you want to unit test test e.g. completion, or
# exact LineEdit behavior, put them in the appropriate test files.
# Furthermore since we are emulating an entire terminal, there may be control characters
# in the mix. If verification needs to be done, keep it to the bare minimum. Basically
# this should make sure nothing crashes without depending on how exactly the control
# characters are being used.
if !is_windows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    stdin_write, stdout_read, stderr_read, repl = fake_repl()

    repl.specialdisplay = Base.REPL.REPLDisplay(repl)
    repl.history_file = false

    repltask = @async begin
        Base.REPL.run_repl(repl)
    end

    sendrepl(cmd) = begin
        write(stdin_write,"$(curmod_prefix)inc || wait($(curmod_prefix)b); r = $cmd; notify($(curmod_prefix)c); r\r")
    end

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
    mktempdir() do tmpdir
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
        readuntil(stdout_read, realpath(homedir())[max(1,end-39):end])
        readuntil(stdout_read, "\n")
        readuntil(stdout_read, "\n")
        @test pwd() == realpath(homedir())
    end
    cd(origpwd)

    # Test that accepting a REPL result immediately shows up, not
    # just on the next keystroke
    write(stdin_write, "1+1\n") # populate history with a trivial input
    readline(stdout_read)
    write(stdin_write, "\e[A\n")
    t = Timer(10) do t
        isopen(t) || return
        error("Stuck waiting for the repl to write `1+1`")
    end
    # yield make sure this got processed
    readuntil(stdout_read, "1+1")
    close(t)

    # Issue #10222
    # Test ignoring insert key in standard and prefix search modes
    write(stdin_write, "\e[2h\e[2h\n") # insert (VT100-style)
    @test search(readline(stdout_read), "[2h") == 0:-1
    readline(stdout_read)
    write(stdin_write, "\e[2~\e[2~\n") # insert (VT220-style)
    @test search(readline(stdout_read), "[2~") == 0:-1
    readline(stdout_read)
    write(stdin_write, "1+1\n") # populate history with a trivial input
    readline(stdout_read)
    write(stdin_write, "\e[A\e[2h\n") # up arrow, insert (VT100-style)
    readline(stdout_read)
    readline(stdout_read)
    write(stdin_write, "\e[A\e[2~\n") # up arrow, insert (VT220-style)
    readline(stdout_read)
    readline(stdout_read)

    # Test down arrow to go back to history
    # populate history with a trivial input

    t = Timer(10) do t
        isopen(t) || return
        error("Stuck waiting for history test")
    end
    s1 = "12345678"; s2 = "23456789"
    write(stdin_write, s1, '\n')
    readuntil(stdout_read, s1)
    write(stdin_write, s2, '\n')
    readuntil(stdout_read, s2)
    # Two up arrow, enter, should get back to 1
    write(stdin_write, "\e[A\e[A\n")
    readuntil(stdout_read, s1)
    # Now, down arrow, enter, should get us back to 2
    write(stdin_write, "\e[B\n")
    readuntil(stdout_read, s2)
    close(t)

    # Close REPL ^D
    write(stdin_write, '\x04')
    wait(repltask)
end

function buffercontents(buf::IOBuffer)
    p = position(buf)
    seek(buf,0)
    c = readstring(buf)
    seek(buf,p)
    c
end

function AddCustomMode(repl)
    # Custom REPL mode tests
    foobar_mode = LineEdit.Prompt("TestΠ";
        prompt_prefix="\e[38;5;166m",
        prompt_suffix=Base.text_colors[:white],
        on_enter = s->true,
        on_done = line->true)

    main_mode = repl.interface.modes[1]
    push!(repl.interface.modes,foobar_mode)

    hp = main_mode.hist
    hp.mode_mapping[:foobar] = foobar_mode
    foobar_mode.hist = hp

    const foobar_keymap = Dict{Any,Any}(
        '<' => function (s,args...)
            if isempty(s)
                if !haskey(s.mode_state,foobar_mode)
                    s.mode_state[foobar_mode] = LineEdit.init_state(repl.t,foobar_mode)
                end
                LineEdit.transition(s,foobar_mode)
            else
                LineEdit.edit_insert(s,'<')
            end
        end
    )

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    mk = REPL.mode_keymap(main_mode)

    b = Dict{Any,Any}[skeymap, mk, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    foobar_mode.keymap_dict = LineEdit.keymap(b)

    main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, foobar_keymap)
    foobar_mode, search_prompt
end

# Note: since the \t character matters for the REPL file history,
# it is important not to have the """ code reindent this line,
# possibly converting \t to spaces.
fakehistory = """
# time: 2014-06-29 20:44:29 EDT
# mode: julia
\té
# time: 2014-06-29 21:44:29 EDT
# mode: julia
\téé
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
\t2 + 2
"""

# Test various history related issues
begin
    stdin_write, stdout_read, stdout_read, repl = fake_repl()
    # In the future if we want we can add a test that the right object
    # gets displayed by intercepting the display
    repl.specialdisplay = Base.REPL.REPLDisplay(repl)

    repl.interface = REPL.setup_interface(repl)
    repl_mode = repl.interface.modes[1]
    shell_mode = repl.interface.modes[2]
    help_mode = repl.interface.modes[3]
    histp = repl.interface.modes[4]
    prefix_mode = repl.interface.modes[5]

    hp = REPL.REPLHistoryProvider(Dict{Symbol,Any}(:julia => repl_mode,
                                                   :shell => shell_mode,
                                                   :help  => help_mode))

    REPL.hist_from_file(hp, IOBuffer(fakehistory), "fakehistorypath")
    REPL.history_reset_state(hp)

    histp.hp = repl_mode.hist = shell_mode.hist = help_mode.hist = hp

    # Some manual setup
    s = LineEdit.init_state(repl.t, repl.interface)
    LineEdit.edit_insert(s, "wip")

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
    @test LineEdit.mode(s) == repl_mode
    @test buffercontents(LineEdit.buffer(s)) == "wip"
    @test position(LineEdit.buffer(s)) == 3
    LineEdit.move_line_start(s)
    @test position(LineEdit.buffer(s)) == 0

    # Test that the same holds for prefix search
    ps = LineEdit.state(s, prefix_mode)::LineEdit.PrefixSearchState
    @test LineEdit.input_string(ps) == ""
    LineEdit.enter_prefix_search(s, prefix_mode, true)
    LineEdit.history_prev_prefix(ps, hp, "")
    @test ps.prefix == ""
    @test ps.parent == repl_mode
    @test LineEdit.input_string(ps) == "2 + 2"
    @test position(LineEdit.buffer(s)) == 5
    LineEdit.history_prev_prefix(ps, hp, "")
    @test ps.parent == shell_mode
    @test LineEdit.input_string(ps) == "ls"
    @test position(LineEdit.buffer(s)) == 2
    LineEdit.history_prev_prefix(ps, hp, "sh")
    @test ps.parent == repl_mode
    @test LineEdit.input_string(ps) == "shell"
    @test position(LineEdit.buffer(s)) == 2
    LineEdit.history_next_prefix(ps, hp, "sh")
    @test ps.parent == repl_mode
    @test LineEdit.input_string(ps) == "wip"
    @test position(LineEdit.buffer(s)) == 0
    LineEdit.move_input_end(s)
    LineEdit.history_prev_prefix(ps, hp, "é")
    @test ps.parent == repl_mode
    @test LineEdit.input_string(ps) == "éé"
    @test position(LineEdit.buffer(s)) == sizeof("é") > 1
    LineEdit.history_prev_prefix(ps, hp, "é")
    @test ps.parent == repl_mode
    @test LineEdit.input_string(ps) == "é"
    @test position(LineEdit.buffer(s)) == sizeof("é")
    LineEdit.history_next_prefix(ps, hp, "zzz")
    @test ps.parent == repl_mode
    @test LineEdit.input_string(ps) == "wip"
    @test position(LineEdit.buffer(s)) == 3
    LineEdit.accept_result(s, prefix_mode)

    # Test that searching backwards puts you into the correct mode and
    # skips invalid modes.
    LineEdit.enter_search(s, histp, true)
    ss = LineEdit.state(s, histp)
    write(ss.query_buffer, "l")
    LineEdit.update_display_buffer(ss, ss)
    LineEdit.accept_result(s, histp)
    @test LineEdit.mode(s) == shell_mode
    @test buffercontents(LineEdit.buffer(s)) == "ls"
    @test position(LineEdit.buffer(s)) == 0

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
    @test position(LineEdit.buffer(s)) == 0

    # Test that searching backwards with a one-letter query doesn't
    # return indefinitely the same match (#9352)
    LineEdit.enter_search(s, histp, true)
    write(ss.query_buffer, "l")
    LineEdit.update_display_buffer(ss, ss)
    LineEdit.history_next_result(s, ss)
    LineEdit.update_display_buffer(ss, ss)
    LineEdit.accept_result(s, histp)
    @test LineEdit.mode(s) == repl_mode
    @test buffercontents(LineEdit.buffer(s)) == "shell"
    @test position(LineEdit.buffer(s)) == 4

    # Test that searching backwards doesn't skip matches (#9352)
    # (for a search with multiple one-byte characters, or UTF-8 characters)
    LineEdit.enter_search(s, histp, true)
    write(ss.query_buffer, "é") # matches right-most "é" in "éé"
    LineEdit.update_display_buffer(ss, ss)
    @test position(ss.query_buffer) == sizeof("é")
    LineEdit.history_next_result(s, ss) # matches left-most "é" in "éé"
    LineEdit.update_display_buffer(ss, ss)
    LineEdit.accept_result(s, histp)
    @test buffercontents(LineEdit.buffer(s)) == "éé"
    @test position(LineEdit.buffer(s)) == 0

    # Issue #7551
    # Enter search mode and try accepting an empty result
    REPL.history_reset_state(hp)
    LineEdit.edit_clear(s)
    cur_mode = LineEdit.mode(s)
    LineEdit.enter_search(s, histp, true)
    LineEdit.accept_result(s, histp)
    @test LineEdit.mode(s) == cur_mode
    @test buffercontents(LineEdit.buffer(s)) == ""
    @test position(LineEdit.buffer(s)) == 0

    # Test that new modes can be dynamically added to the REPL and will
    # integrate nicely
    foobar_mode, custom_histp = AddCustomMode(repl)

    # ^R l, should now find `ls` in foobar mode
    LineEdit.enter_search(s, histp, true)
    ss = LineEdit.state(s, histp)
    write(ss.query_buffer, "l")
    LineEdit.update_display_buffer(ss, ss)
    LineEdit.accept_result(s, histp)
    @test LineEdit.mode(s) == foobar_mode
    @test buffercontents(LineEdit.buffer(s)) == "ls"
    @test position(LineEdit.buffer(s)) == 0

    # Try the same for prefix search
    LineEdit.history_next(s, hp)
    LineEdit.history_prev_prefix(ps, hp, "l")
    @test ps.parent == foobar_mode
    @test LineEdit.input_string(ps) == "ls"
    @test position(LineEdit.buffer(s)) == 1

    # Try entering search mode while in custom repl mode
    LineEdit.enter_search(s, custom_histp, true)
end

# Test removal of prompt in bracket pasting
begin
    stdin_write, stdout_read, stderr_read, repl = fake_repl()

    repl.interface = REPL.setup_interface(repl)
    repl_mode = repl.interface.modes[1]
    shell_mode = repl.interface.modes[2]
    help_mode = repl.interface.modes[3]

    repltask = @async begin
        Base.REPL.run_repl(repl)
    end

    c = Condition()
    sendrepl2(cmd) = write(stdin_write,"$cmd\n notify($(curmod_prefix)c)\n")

    # Test removal of prefix in single statement paste
    sendrepl2("\e[200~julia> A = 2\e[201~\n")
    wait(c)
    @test Main.A == 2

    # Test removal of prefix in multiple statement paste
    sendrepl2("""\e[200~
            julia> type T17599; a::Int; end

            julia> function foo(julia)
            julia> 3
                end

                    julia> A = 3\e[201~
             """)
    wait(c)
    @test Main.A == 3
    @test Main.foo(4)
    @test Main.T17599(3).a == 3
    @test !Main.foo(2)

    sendrepl2("""\e[200~
            julia> goo(x) = x + 1
            error()

            julia> A = 4
            4\e[201~
             """)
    wait(c)
    @test Main.A == 4
    @test Main.goo(4) == 5

    # Test prefix removal only active in bracket paste mode
    sendrepl2("julia = 4\n julia> 3 && (A = 1)\n")
    wait(c)
    @test Main.A == 1

    # Close repl
    write(stdin_write, '\x04')
    wait(repltask)
end

# Simple non-standard REPL tests
if !is_windows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    stdin_write, stdout_read, stdout_read, repl = fake_repl()
    panel = LineEdit.Prompt("testπ";
        prompt_prefix="\e[38;5;166m",
        prompt_suffix=Base.text_colors[:white],
        on_enter = s->true)

    hp = REPL.REPLHistoryProvider(Dict{Symbol,Any}(:parse => panel))
    search_prompt, skeymap = LineEdit.setup_prefix_keymap(hp, panel)
    REPL.history_reset_state(hp)

    panel.hist = hp
    panel.keymap_dict = LineEdit.keymap(Dict{Any,Any}[skeymap,
        LineEdit.default_keymap, LineEdit.escape_defaults])

    c = Condition()
    panel.on_done = (s,buf,ok)->begin
        if !ok
            LineEdit.transition(s,:abort)
        end
        line = strip(String(take!(buf)))
        LineEdit.reset_state(s)
        return notify(c,line)
    end

    repltask = @async Base.REPL.run_interface(repl.t, LineEdit.ModalInterface([panel,search_prompt]))

    write(stdin_write,"a\n")
    @test wait(c) == "a"
    # Up arrow enter should recall history even at the start
    write(stdin_write,"\e[A\n")
    @test wait(c) == "a"
    # And again
    write(stdin_write,"\e[A\n")
    @test wait(c) == "a"
    # Close REPL ^D
    write(stdin_write, '\x04')
    wait(repltask)
end

ccall(:jl_exit_on_sigint, Void, (Cint,), 1)

let exename = Base.julia_cmd()

# Test REPL in dumb mode
if !is_windows()
    TestHelpers.with_fake_pty() do slave, master
        nENV = copy(ENV)
        nENV["TERM"] = "dumb"
        p = spawn(setenv(`$exename --startup-file=no --quiet`,nENV),slave,slave,slave)
        output = readuntil(master,"julia> ")
        if ccall(:jl_running_on_valgrind,Cint,()) == 0
            # If --trace-children=yes is passed to valgrind, we will get a
            # valgrind banner here, not just the prompt.
            @test output == "julia> "
        end
        write(master,"1\nquit()\n")

        wait(p)
        output = readuntil(master,' ')
        @test output == "1\r\nquit()\r\n1\r\n\r\njulia> "
        @test nb_available(master) == 0
    end
end

# Test stream mode
if !is_windows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    outs, ins, p = readandwrite(`$exename --startup-file=no --quiet`)
    write(ins,"1\nquit()\n")
    @test readstring(outs) == "1\n"
end
end # let exename

# issue #19864:
type Error19864 <: Exception; end
function test19864()
    @eval current_module() Base.showerror(io::IO, e::Error19864) = print(io, "correct19864")
    buf = IOBuffer()
    REPL.print_response(buf, Error19864(), [], false, false, nothing)
    return String(take!(buf))
end
@test contains(test19864(), "correct19864")

# Test containers in error messages are limited #18726
let io = IOBuffer()
    Base.display_error(io,
        try
            [][trues(6000)]
        catch e
            e
        end, [])
    @test length(String(take!(io))) < 1500
end

function test_replinit()
    stdin_write, stdout_read, stdout_read, repl = fake_repl()
    # Relies on implementation detail to make sure we only have the single
    # replinit callback we want to test.
    saved_replinit = copy(Base.repl_hooks)
    slot = Ref(false)
    # Create a closure from a newer world to check if `_atreplinit`
    # can run it correctly
    atreplinit(@eval(repl::Base.REPL.LineEditREPL->($slot[] = true)))
    Base._atreplinit(repl)
    @test slot[]
    @test_throws MethodError Base.repl_hooks[1](repl)
    copy!(Base.repl_hooks, saved_replinit)
end
test_replinit()
