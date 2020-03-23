# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using REPL
using Random
import REPL.LineEdit
using Markdown

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :FakePTYs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "FakePTYs.jl"))
import .Main.FakePTYs: with_fake_pty

# For curmod_*
include(joinpath(BASE_TEST_PATH, "testenv.jl"))

include("FakeTerminals.jl")
import .FakeTerminals.FakeTerminal


function kill_timer(delay)
    # Give ourselves a generous timer here, just to prevent
    # this causing e.g. a CI hang when there's something unexpected in the output.
    # This is really messy and leaves the process in an undefined state.
    # the proper and correct way to do this in real code would be to destroy the
    # IO handles: `close(stdout_read); close(stdin_write)`
    test_task = current_task()
    function kill_test(t)
        # **DON'T COPY ME.**
        # The correct way to handle timeouts is to close the handle:
        # e.g. `close(stdout_read); close(stdin_write)`
        test_task.queue === nothing || Base.list_deletefirst!(test_task.queue, test_task)
        schedule(test_task, "hard kill repl test"; error=true)
        print(stderr, "WARNING: attempting hard kill of repl test after exceeding timeout\n")
    end
    return Timer(kill_test, delay)
end

# REPL tests
function fake_repl(@nospecialize(f); options::REPL.Options=REPL.Options(confirm_exit=false))
    # Use pipes so we can easily do blocking reads
    # In the future if we want we can add a test that the right object
    # gets displayed by intercepting the display
    input = Pipe()
    output = Pipe()
    err = Pipe()
    Base.link_pipe!(input, reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(output, reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(err, reader_supports_async=true, writer_supports_async=true)

    repl = REPL.LineEditREPL(FakeTerminal(input.out, output.in, err.in), true)
    repl.options = options

    hard_kill = kill_timer(900) # Your debugging session starts now. You have 15 minutes. Go.
    @eval Base old_have_color = have_color
    @eval Base have_color = false
    f(input.in, output.out, repl)
    @eval Base have_color = old_have_color
    t = @async begin
        close(input.in)
        close(output.in)
        close(err.in)
    end
    @test read(err.out, String) == ""
    #display(read(output.out, String))
    Base.wait(t)
    close(hard_kill)
    nothing
end

# Writing ^C to the repl will cause sigint, so let's not die on that
ccall(:jl_exit_on_sigint, Cvoid, (Cint,), 0)

# make sure `run_interface` can normally handle `eof`
# without any special handling by the user
fake_repl() do stdin_write, stdout_read, repl
    panel = LineEdit.Prompt("test";
        prompt_prefix = "",
        prompt_suffix = Base.text_colors[:white],
        on_enter = s -> true)
    panel.on_done = (s, buf, ok) -> begin
        @test !ok
        @test bytesavailable(buf) == position(buf) == 0
        nothing
    end
    repltask = @async REPL.run_interface(repl.t, LineEdit.ModalInterface(Any[panel]))
    close(stdin_write)
    Base.wait(repltask)
end

# These are integration tests. If you want to unit test test e.g. completion, or
# exact LineEdit behavior, put them in the appropriate test files.
# Furthermore since we are emulating an entire terminal, there may be control characters
# in the mix. If verification needs to be done, keep it to the bare minimum. Basically
# this should make sure nothing crashes without depending on how exactly the control
# characters are being used.
fake_repl() do stdin_write, stdout_read, repl
    repl.specialdisplay = REPL.REPLDisplay(repl)
    repl.history_file = false

    repltask = @async begin
        REPL.run_repl(repl)
    end

    global inc = false
    global b = Condition()
    global c = Condition()
    let cmd = "\"Hello REPL\""
        write(stdin_write, "$(curmod_prefix)inc || wait($(curmod_prefix)b); r = $cmd; notify($(curmod_prefix)c); r\r")
    end
    inc = true
    notify(b)
    wait(c)

    # Latex completions
    write(stdin_write, "\x32\\alpha\t")
    readuntil(stdout_read, "α")
    # Bracketed paste in search mode
    write(stdin_write, "\e[200~paste here ;)\e[201~")
    # Abort search (^C)
    write(stdin_write, '\x03')
    # Test basic completion in main mode
    write(stdin_write, "Base.REP\t")
    readuntil(stdout_read, "REPL")
    write(stdin_write, '\x03')
    write(stdin_write, "\\alpha\t")
    readuntil(stdout_read,"α")
    write(stdin_write, '\x03')
    # Test cd feature in shell mode.
    origpwd = pwd()
    mktempdir() do tmpdir
        try
            samefile = Base.Filesystem.samefile
            tmpdir_pwd = cd(pwd, tmpdir)
            homedir_pwd = cd(pwd, homedir())

            # Test `cd`'ing to an absolute path
            write(stdin_write, ";")
            readuntil(stdout_read, "shell> ")
            write(stdin_write, "cd $(escape_string(tmpdir))\n")
            readuntil(stdout_read, "cd $(escape_string(tmpdir))")
            readuntil(stdout_read, tmpdir_pwd)
            readuntil(stdout_read, "\n")
            readuntil(stdout_read, "\n")
            @test samefile(".", tmpdir)

            # Test using `cd` to move to the home directory
            write(stdin_write, ";")
            readuntil(stdout_read, "shell> ")
            write(stdin_write, "cd\n")
            readuntil(stdout_read, homedir_pwd)
            readuntil(stdout_read, "\n")
            readuntil(stdout_read, "\n")
            @test samefile(".", homedir_pwd)

            # Test using `-` to jump backward to tmpdir
            write(stdin_write, ";")
            readuntil(stdout_read, "shell> ")
            write(stdin_write, "cd -\n")
            readuntil(stdout_read, tmpdir_pwd)
            readuntil(stdout_read, "\n")
            readuntil(stdout_read, "\n")
            @test samefile(".", tmpdir)

            # Test using `~` (Base.expanduser) in `cd` commands
            if !Sys.iswindows()
                write(stdin_write, ";")
                readuntil(stdout_read, "shell> ")
                write(stdin_write, "cd ~\n")
                readuntil(stdout_read, homedir_pwd)
                readuntil(stdout_read, "\n")
                readuntil(stdout_read, "\n")
                @test samefile(".", homedir_pwd)
            end
        finally
            cd(origpwd)
        end
    end

    # issue #20482
    #if !Sys.iswindows()
    #    write(stdin_write, ";")
    #    readuntil(stdout_read, "shell> ")
    #    write(stdin_write, "echo hello >/dev/null\n")
    #    let s = readuntil(stdout_read, "\n", keep=true)
    #        @test occursin("shell> ", s) # make sure we echoed the prompt
    #        @test occursin("echo hello >/dev/null", s) # make sure we echoed the input
    #    end
    #    @test readuntil(stdout_read, "\n", keep=true) == "\e[0m\n"
    #end

    # issue #20771
    let s
        write(stdin_write, ";")
        readuntil(stdout_read, "shell> ")
        write(stdin_write, "'\n") # invalid input
        s = readuntil(stdout_read, "\n")
        @test occursin("shell> ", s) # check for the echo of the prompt
        @test occursin("'", s) # check for the echo of the input
        s = readuntil(stdout_read, "\n\n")
        @test startswith(s, "\e[0mERROR: unterminated single quote\nStacktrace:\n [1] ") ||
              startswith(s, "\e[0m\e[1m\e[91mERROR: \e[39m\e[22m\e[91munterminated single quote\e[39m\nStacktrace:\n [1] ")
    end

    # issue #27293
    if Sys.isunix()
        let s, old_stdout = stdout
            write(stdin_write, ";")
            readuntil(stdout_read, "shell> ")
            write(stdin_write, "echo ~")
            s = readuntil(stdout_read, "~")

            proc_stdout_read, proc_stdout = redirect_stdout()
            get_stdout = @async read(proc_stdout_read, String)
            try
                write(stdin_write, "\n")
                readuntil(stdout_read, "\n")
                s = readuntil(stdout_read, "\n")
            finally
                redirect_stdout(old_stdout)
            end
            @test s == "\e[0m" # the child has exited
            close(proc_stdout)
            # check for the correct, expanded response
            @test occursin(expanduser("~"), fetch(get_stdout))
        end
    end

    # issues #22176 & #20482
    # TODO: figure out how to test this on Windows
    #Sys.iswindows() || let tmp = tempname()
    #    try
    #        write(stdin_write, ";")
    #        readuntil(stdout_read, "shell> ")
    #        write(stdin_write, "echo \$123 >$tmp\n")
    #        let s = readuntil(stdout_read, "\n")
    #            @test occursin("shell> ", s) # make sure we echoed the prompt
    #            @test occursin("echo \$123 >$tmp", s) # make sure we echoed the input
    #        end
    #        @test readuntil(stdout_read, "\n", keep=true) == "\e[0m\n"
    #        @test read(tmp, String) == "123\n"
    #    finally
    #        rm(tmp, force=true)
    #    end
    #end

    # issue #10120
    # ensure that command quoting works correctly
    let s, old_stdout = stdout
        write(stdin_write, ";")
        readuntil(stdout_read, "shell> ")
        Base.print_shell_escaped(stdin_write, Base.julia_cmd().exec..., special=Base.shell_special)
        write(stdin_write, """ -e "println(\\"HI\\")\" """)
        readuntil(stdout_read, ")\"")
        proc_stdout_read, proc_stdout = redirect_stdout()
        get_stdout = @async read(proc_stdout_read, String)
        try
            write(stdin_write, '\n')
            s = readuntil(stdout_read, "\n", keep=true)
            if s == "\n"
                # if shell width is precisely the text width,
                # we may print some extra characters to fix the cursor state
                s = readuntil(stdout_read, "\n", keep=true)
                @test occursin("shell> ", s)
                s = readuntil(stdout_read, "\n", keep=true)
                @test s == "\r\r\n"
            else
                @test occursin("shell> ", s)
            end
            s = readuntil(stdout_read, "\n", keep=true)
            @test s == "\e[0m\n" # the child has exited
        finally
            redirect_stdout(old_stdout)
        end
        close(proc_stdout)
        @test fetch(get_stdout) == "HI\n"
    end

    # Issue #7001
    # Test ignoring '\0'
    let
        write(stdin_write, "\0\n")
        s = readuntil(stdout_read, "\n\n")
        @test !occursin("invalid character", s)
    end

    # Test that accepting a REPL result immediately shows up, not
    # just on the next keystroke
    write(stdin_write, "1+1\n") # populate history with a trivial input
    readline(stdout_read)
    write(stdin_write, "\e[A\n")
    let t = kill_timer(60)
        # yield make sure this got processed
        readuntil(stdout_read, "1+1")
        readuntil(stdout_read, "\n\n")
        close(t) # cancel timeout
    end

    # Issue #10222
    # Test ignoring insert key in standard and prefix search modes
    write(stdin_write, "\e[2h\e[2h\n") # insert (VT100-style)
    @test findfirst("[2h", readline(stdout_read)) === nothing
    readline(stdout_read)
    write(stdin_write, "\e[2~\e[2~\n") # insert (VT220-style)
    @test findfirst("[2~", readline(stdout_read)) === nothing
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

    # test that prefix history search "passes through" key bindings to parent mode
    write(stdin_write, "0x321\n")
    readuntil(stdout_read, "0x321")
    write(stdin_write, "\e[A\e[1;3C|||") # uparrow (go up history) and then Meta-rightarrow (indent right)
    s2 = readuntil(stdout_read, "|||", keep=true)
    @test endswith(s2, " 0x321\r\e[13C|||") # should have a space (from Meta-rightarrow) and not
                                            # have a spurious C before ||| (the one here is not spurious!)

    # Delete line (^U) and close REPL (^D)
    write(stdin_write, "\x15\x04")
    Base.wait(repltask)

    nothing
end

function buffercontents(buf::IOBuffer)
    p = position(buf)
    seek(buf,0)
    c = read(buf, String)
    seek(buf,p)
    c
end

function AddCustomMode(repl, prompt)
    # Custom REPL mode tests
    foobar_mode = LineEdit.Prompt(prompt;
        prompt_prefix="\e[38;5;166m",
        prompt_suffix=Base.text_colors[:white],
        on_enter = s->true,
        on_done = line->true)

    main_mode = repl.interface.modes[1]
    push!(repl.interface.modes,foobar_mode)

    hp = main_mode.hist
    hp.mode_mapping[:foobar] = foobar_mode
    foobar_mode.hist = hp

    foobar_keymap = Dict{Any,Any}(
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
# time: 2014-06-30 99:99:99 EDT
# mode: julia
\tx ΔxΔ
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
for prompt = ["TestΠ", () -> randstring(rand(1:10))]
    fake_repl() do stdin_write, stdout_read, repl
        # In the future if we want we can add a test that the right object
        # gets displayed by intercepting the display
        repl.specialdisplay = REPL.REPLDisplay(repl)

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

        # LineEdit functions related to history
        LineEdit.edit_insert_last_word(s)
        @test buffercontents(LineEdit.buffer(s)) == "wip2"
        LineEdit.edit_backspace(s) # remove the "2"

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
        LineEdit.history_next(s, hp)
        @test buffercontents(LineEdit.buffer(s)) == "wip"
        LineEdit.history_prev(s, hp, 2)
        @test LineEdit.mode(s) == shell_mode
        @test buffercontents(LineEdit.buffer(s)) == "ls"
        LineEdit.history_prev(s, hp, -2) # equivalent to history_next(s, hp, 2)
        @test LineEdit.mode(s) == repl_mode
        @test buffercontents(LineEdit.buffer(s)) == "2 + 2"
        LineEdit.history_next(s, hp, -2) # equivalent to history_prev(s, hp, 2)
        @test LineEdit.mode(s) == shell_mode
        @test buffercontents(LineEdit.buffer(s)) == "ls"
        LineEdit.history_first(s, hp)
        @test LineEdit.mode(s) == repl_mode
        @test buffercontents(LineEdit.buffer(s)) == "é"
        LineEdit.history_next(s, hp, 6)
        @test LineEdit.mode(s) == shell_mode
        @test buffercontents(LineEdit.buffer(s)) == "ls"
        LineEdit.history_last(s, hp)
        @test buffercontents(LineEdit.buffer(s)) == "wip"
        @test position(LineEdit.buffer(s)) == 3
        # test that history_first jumps to beginning of current session's history
        hp.start_idx -= 5 # temporarily alter history
        LineEdit.history_first(s, hp)
        @test hp.cur_idx == 6
        # we are at the beginning of current session's history, so history_first
        # must now jump to the beginning of all history
        LineEdit.history_first(s, hp)
        @test hp.cur_idx == 1
        LineEdit.history_last(s, hp)
        @test hp.cur_idx-1 == length(hp.history)
        hp.start_idx += 5
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
        @test buffercontents(ss.response_buffer) == "ll"
        @test position(ss.response_buffer) == 1
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
        foobar_mode, custom_histp = AddCustomMode(repl, prompt)

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

        # Some Unicode handling testing
        LineEdit.history_prev(s, hp)
        LineEdit.enter_search(s, histp, true)
        write(ss.query_buffer, "x")
        LineEdit.update_display_buffer(ss, ss)
        @test buffercontents(ss.response_buffer) == "x ΔxΔ"
        @test position(ss.response_buffer) == 4
        write(ss.query_buffer, " ")
        LineEdit.update_display_buffer(ss, ss)
        LineEdit.accept_result(s, histp)
        @test LineEdit.mode(s) == repl_mode
        @test buffercontents(LineEdit.buffer(s)) == "x ΔxΔ"
        @test position(LineEdit.buffer(s)) == 0

        LineEdit.edit_clear(s)
        LineEdit.enter_search(s, histp, true)
        ss = LineEdit.state(s, histp)
        write(ss.query_buffer, "Å") # should not be in history
        LineEdit.update_display_buffer(ss, ss)
        @test buffercontents(ss.response_buffer) == ""
        @test position(ss.response_buffer) == 0
        LineEdit.history_next_result(s, ss) # should not throw BoundsError
        LineEdit.accept_result(s, histp)

        # Try entering search mode while in custom repl mode
        LineEdit.enter_search(s, custom_histp, true)
    end
end

# Test removal of prompt in bracket pasting
fake_repl() do stdin_write, stdout_read, repl
    repl.interface = REPL.setup_interface(repl)
    repl_mode = repl.interface.modes[1]
    shell_mode = repl.interface.modes[2]
    help_mode = repl.interface.modes[3]

    repltask = @async begin
        REPL.run_repl(repl)
    end

    global c = Condition()
    sendrepl2(cmd) = write(stdin_write, "$cmd\n notify($(curmod_prefix)c)\n")

    # Test removal of prefix in single statement paste
    sendrepl2("\e[200~julia> A = 2\e[201~\n")
    wait(c)
    @test Main.A == 2

    # Test removal of prefix in multiple statement paste
    sendrepl2("""\e[200~
            julia> mutable struct T17599; a::Int; end

            julia> function foo(julia)
            julia> 3
                end

                    julia> A = 3\e[201~
             """)
    wait(c)
    @test Main.A == 3
    @test Base.invokelatest(Main.foo, 4)
    @test Base.invokelatest(Main.T17599, 3).a == 3
    @test !Base.invokelatest(Main.foo, 2)

    sendrepl2("""\e[200~
            julia> goo(x) = x + 1
            error()

            julia> A = 4
            4\e[201~
             """)
    wait(c)
    @test Main.A == 4
    @test Base.invokelatest(Main.goo, 4) == 5

    # Test prefix removal only active in bracket paste mode
    sendrepl2("julia = 4\n julia> 3 && (A = 1)\n")
    wait(c)
    @test Main.A == 1

    # Test that indentation corresponding to the prompt is removed
    sendrepl2("""\e[200~julia> begin\n           α=1\n           β=2\n       end\n\e[201~""")
    wait(c)
    readuntil(stdout_read, "begin")
    @test readuntil(stdout_read, "end", keep=true) == "\n\r\e[7C    α=1\n\r\e[7C    β=2\n\r\e[7Cend"
    # for incomplete input (`end` below is added after the end of bracket paste)
    sendrepl2("""\e[200~julia> begin\n           α=1\n           β=2\n\e[201~end""")
    wait(c)
    readuntil(stdout_read, "begin")
    readuntil(stdout_read, "begin")
    @test readuntil(stdout_read, "end", keep=true) == "\n\r\e[7C    α=1\n\r\e[7C    β=2\n\r\e[7Cend"

    # Close repl
    write(stdin_write, '\x04')
    Base.wait(repltask)
end

# Simple non-standard REPL tests
fake_repl() do stdin_write, stdout_read, repl
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
    panel.on_done = (s, buf, ok) -> begin
        if !ok
            LineEdit.transition(s, :abort)
        end
        line = strip(String(take!(buf)))
        LineEdit.reset_state(s)
        notify(c, line)
        nothing
    end

    repltask = @async REPL.run_interface(repl.t, LineEdit.ModalInterface(Any[panel, search_prompt]))

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
    Base.wait(repltask)
end

ccall(:jl_exit_on_sigint, Cvoid, (Cint,), 1)

let exename = Base.julia_cmd()
    # Test REPL in dumb mode
    with_fake_pty() do pty_slave, pty_master
        nENV = copy(ENV)
        nENV["TERM"] = "dumb"
        p = run(detach(setenv(`$exename --startup-file=no -q`, nENV)), pty_slave, pty_slave, pty_slave, wait=false)
        Base.close_stdio(pty_slave)
        output = readuntil(pty_master, "julia> ", keep=true)
        if ccall(:jl_running_on_valgrind, Cint,()) == 0
            # If --trace-children=yes is passed to valgrind, we will get a
            # valgrind banner here, not just the prompt.
            @test output == "julia> "
        end
        write(pty_master, "1\nexit()\n")

        output = readuntil(pty_master, ' ', keep=true)
        if Sys.iswindows()
	    # Our fake pty is actually a pipe, and thus lacks the input echo feature of posix
            @test output == "1\n\njulia> "
        else
            @test output == "1\r\nexit()\r\n1\r\n\r\njulia> "
        end
        @test bytesavailable(pty_master) == 0
        @test if Sys.iswindows() || Sys.isbsd()
                eof(pty_master)
            else
                # Some platforms (such as linux) report EIO instead of EOF
                # possibly consume child-exited notification
                # for example, see discussion in https://bugs.python.org/issue5380
                try
                    eof(pty_master) && !Sys.islinux()
                catch ex
                    (ex isa Base.IOError && ex.code == Base.UV_EIO) || rethrow()
                    @test_throws ex eof(pty_master) # make sure the error is sticky
                    pty_master.readerror = nothing
                    eof(pty_master)
                end
            end
        @test read(pty_master, String) == ""
        wait(p)
    end

    # Test stream mode
    p = open(`$exename --startup-file=no -q`, "r+")
    write(p, "1\nexit()\n")
    @test read(p, String) == "1\n"
end # let exename

# issue #19864
mutable struct Error19864 <: Exception; end
function test19864()
    @eval Base.showerror(io::IO, e::Error19864) = print(io, "correct19864")
    buf = IOBuffer()
    fake_response = (Any[(Error19864(), Ptr{Cvoid}[])], true)
    REPL.print_response(buf, fake_response, false, false, nothing)
    return String(take!(buf))
end
@test occursin("correct19864", test19864())

# Test containers in error messages are limited #18726
let io = IOBuffer()
    Base.display_error(io,
        try
            [][trues(6000)]
            @assert false
        catch e
            e
        end, [])
    @test length(String(take!(io))) < 1500
end

fake_repl() do stdin_write, stdout_read, repl
    # Relies on implementation detail to make sure we only have the single
    # replinit callback we want to test.
    saved_replinit = copy(Base.repl_hooks)
    slot = Ref(false)
    # Create a closure from a newer world to check if `_atreplinit`
    # can run it correctly
    atreplinit(@eval(repl::REPL.LineEditREPL -> ($slot[] = true)))
    Base._atreplinit(repl)
    @test slot[]
    @test_throws MethodError Base.repl_hooks[1](repl)
    copyto!(Base.repl_hooks, saved_replinit)
    nothing
end

let ends_with_semicolon = REPL.ends_with_semicolon
    @test !ends_with_semicolon("")
    @test ends_with_semicolon(";")
    @test !ends_with_semicolon("a")
    @test ends_with_semicolon("1;")
    @test ends_with_semicolon("1;\n")
    @test ends_with_semicolon("1;\r")
    @test ends_with_semicolon("1;\r\n   \t\f")
    @test ends_with_semicolon("1;#text\n")
    @test ends_with_semicolon("a; #=#=# =# =#\n")
    @test !ends_with_semicolon("begin\na;\nb;\nend")
    @test !ends_with_semicolon("begin\na; #=#=#\n=#b=#\nend")
    @test ends_with_semicolon("\na; #=#=#\n=#b=#\n# test\n#=\nfoobar\n=##bazbax\n")
end

# PR #20794, TTYTerminal with other kinds of streams
let term = REPL.Terminals.TTYTerminal("dumb",IOBuffer("1+2\n"),IOContext(IOBuffer(),:foo=>true),IOBuffer())
    r = REPL.BasicREPL(term)
    REPL.run_repl(r)
    @test String(take!(term.out_stream.io)) == "julia> 3\n\njulia> \n"
    @test haskey(term, :foo) == true
    @test haskey(term, :bar) == false
    @test (:foo=>true) in term
    @test (:foo=>false) ∉ term
    @test term[:foo] == get(term, :foo, nothing) == true
    @test get(term, :bar, nothing) === nothing
    @test_throws KeyError term[:bar]
end


# a small module for alternative keymap tests
module AltLE
import REPL
import REPL.LineEdit

function history_move_prefix(s::LineEdit.MIState,
                             hist::REPL.REPLHistoryProvider,
                             backwards::Bool)
    buf = LineEdit.buffer(s)
    pos = position(buf)
    prefix = REPL.beforecursor(buf)
    allbuf = String(take!(copy(buf)))
    cur_idx = hist.cur_idx
    # when searching forward, start at last_idx
    if !backwards && hist.last_idx > 0
        cur_idx = hist.last_idx
    end
    hist.last_idx = -1
    idxs = backwards ? ((cur_idx-1):-1:1) : ((cur_idx+1):length(hist.history))
    for idx in idxs
        if startswith(hist.history[idx], prefix) && hist.history[idx] != allbuf
            REPL.history_move(s, hist, idx)
            seek(LineEdit.buffer(s), pos)
            LineEdit.refresh_line(s)
            return :ok
        end
    end
    REPL.Terminals.beep(LineEdit.terminal(s))
end
history_next_prefix(s::LineEdit.MIState, hist::REPL.REPLHistoryProvider) =
    history_move_prefix(s, hist, false)
history_prev_prefix(s::LineEdit.MIState, hist::REPL.REPLHistoryProvider) =
    history_move_prefix(s, hist, true)

end # module

# Test alternative keymaps and prompt
# (Alt. keymaps may be passed as a Vector{<:Dict} or as a Dict)

const altkeys = [Dict{Any,Any}("\e[A" => (s,o...)->(LineEdit.edit_move_up(s) || LineEdit.history_prev(s, LineEdit.mode(s).hist))), # Up Arrow
                 Dict{Any,Any}("\e[B" => (s,o...)->(LineEdit.edit_move_down(s) || LineEdit.history_next(s, LineEdit.mode(s).hist))), # Down Arrow
                 Dict{Any,Any}("\e[5~" => (s,o...)->(AltLE.history_prev_prefix(s, LineEdit.mode(s).hist))), # Page Up
                 Dict{Any,Any}("\e[6~" => (s,o...)->(AltLE.history_next_prefix(s, LineEdit.mode(s).hist))), # Page Down
                ]


for keys = [altkeys, merge(altkeys...)],
        altprompt = ["julia-$(VERSION.major).$(VERSION.minor)> ",
                     () -> "julia-$(Base.GIT_VERSION_INFO.commit_short)"]
    histfile = tempname()
    try
        fake_repl() do stdin_write, stdout_read, repl
            repl.specialdisplay = REPL.REPLDisplay(repl)
            repl.history_file = true
            withenv("JULIA_HISTORY" => histfile) do
                repl.interface = REPL.setup_interface(repl, extra_repl_keymap = altkeys)
            end
            repl.interface.modes[1].prompt = altprompt

            repltask = @async begin
                REPL.run_repl(repl)
            end

            sendrepl3(cmd) = write(stdin_write,"$cmd\n")

            sendrepl3("1 + 1;")                        # a simple line
            sendrepl3("multi=2;\e\nline=2;")           # a multiline input
            sendrepl3("ignoreme\e[A\b\b3;\e[B\b\b1;")  # edit the previous multiline input
            sendrepl3("1 +\e[5~\b*")                   # use prefix search to edit the 1st input

            # Close REPL ^D
            write(stdin_write, '\x04')
            Base.wait(repltask)

            # Close the history file
            # (otherwise trying to delete it fails on Windows)
            close(repl.interface.modes[1].hist.history_file)

            # Check that the correct prompt was displayed
            output = readuntil(stdout_read, "1 * 1;", keep=true)
            @test !occursin(output, LineEdit.prompt_string(altprompt))
            @test !occursin(output, "julia> ")

            # Check the history file
            history = read(histfile, String)
            @test occursin(r"""
                           ^\#\ time:\ .*\n
                            \#\ mode:\ julia\n
                            \t1\ \+\ 1;\n
                            \#\ time:\ .*\n
                            \#\ mode:\ julia\n
                            \tmulti=2;\n
                            \tline=2;\n
                            \#\ time:\ .*\n
                            \#\ mode:\ julia\n
                            \tmulti=3;\n
                            \tline=1;\n
                            \#\ time:\ .*\n
                            \#\ mode:\ julia\n
                            \t1\ \*\ 1;\n$
                           """xm, history)
        end
    finally
        rm(histfile, force=true)
    end
end

# Test that module prefix is omitted when type is reachable from Main (PR #23806)
fake_repl() do stdin_write, stdout_read, repl
    repl.specialdisplay = REPL.REPLDisplay(repl)
    repl.history_file = false

    repltask = @async begin
        REPL.run_repl(repl)
    end

    @eval Main module TestShowTypeREPL; export TypeA; struct TypeA end; end
    write(stdin_write, "TestShowTypeREPL.TypeA\n")
    @test endswith(readline(stdout_read), "\r\e[7CTestShowTypeREPL.TypeA\r\e[29C")
    readline(stdout_read)
    readline(stdout_read)
    @eval Main using .TestShowTypeREPL
    write(stdin_write, "TypeA\n")
    @test endswith(readline(stdout_read), "\r\e[7CTypeA\r\e[12C")
    readline(stdout_read)

    # Close REPL ^D
    write(stdin_write, '\x04')
    Base.wait(repltask)
end

# Docs.helpmode tests: we test whether the correct expressions are being generated here,
# rather than complete integration with Julia's REPL mode system.
for (line, expr) in Pair[
    "sin"          => :sin,
    "Base.sin"     => :(Base.sin),
    "@time(x)"     => Expr(:macrocall, Symbol("@time"), LineNumberNode(1, :none), :x),
    "@time"        => Expr(:macrocall, Symbol("@time"), LineNumberNode(1, :none)),
    ":@time"       => Expr(:quote, (Expr(:macrocall, Symbol("@time"), LineNumberNode(1, :none)))),
    "@time()"      => Expr(:macrocall, Symbol("@time"), LineNumberNode(1, :none)),
    "Base.@time()" => Expr(:macrocall, Expr(:., :Base, QuoteNode(Symbol("@time"))), LineNumberNode(1, :none)),
    "ccall"        => :ccall, # keyword
    "while       " => :while, # keyword, trailing spaces should be stripped.
    "0"            => 0,
    "\"...\""      => "...",
    "r\"...\""     => Expr(:macrocall, Symbol("@r_str"), LineNumberNode(1, :none), "..."),
    "using Foo"    => :using,
    "import Foo"   => :import,
    ]
    @test REPL._helpmode(line).args[4] == expr
    buf = IOBuffer()
    @test Base.eval(REPL._helpmode(buf, line)) isa Union{Markdown.MD,Nothing}
end

# PR 30754, Issues #22013, #24871, #26933, #29282, #29361, #30348
for line in ["′", "abstract", "type", "|=", ".="]
    @test occursin("No documentation found.",
        sprint(show, Base.eval(REPL._helpmode(IOBuffer(), line))::Union{Markdown.MD,Nothing}))
end

# Issue #25930

# Brief and extended docs (issue #25930)
let text =
        """
            brief_extended()

        Short docs

        # Extended help

        Long docs
        """,
    md = Markdown.parse(text)
    @test md == REPL.trimdocs(md, false)
    @test !isa(md.content[end], REPL.Message)
    mdbrief = REPL.trimdocs(md, true)
    @test length(mdbrief.content) == 3
    @test isa(mdbrief.content[1], Markdown.Code)
    @test isa(mdbrief.content[2], Markdown.Paragraph)
    @test isa(mdbrief.content[3], REPL.Message)
    @test occursin("??", mdbrief.content[3].msg)
end

module BriefExtended
"""
    f()

Short docs

# Extended help

Long docs
"""
f() = nothing
end # module BriefExtended
buf = IOBuffer()
md = Base.eval(REPL._helpmode(buf, "$(@__MODULE__).BriefExtended.f"))
@test length(md.content) == 2 && isa(md.content[2], REPL.Message)
buf = IOBuffer()
md = Base.eval(REPL._helpmode(buf, "?$(@__MODULE__).BriefExtended.f"))
@test length(md.content) == 1 && length(md.content[1].content[1].content) == 4

# PR #27562
fake_repl() do stdin_write, stdout_read, repl
    repltask = @async begin
        REPL.run_repl(repl)
    end
    write(stdin_write, "Expr(:call, GlobalRef(Base.Math, :float), Core.SlotNumber(1))\n")
    readline(stdout_read)
    @test readline(stdout_read) == "\e[0m:(Base.Math.float(_1))"
    write(stdin_write, "ans\n")
    readline(stdout_read)
    readline(stdout_read)
    @test readline(stdout_read) == "\e[0m:(Base.Math.float(_1))"
    write(stdin_write, '\x04')
    Base.wait(repltask)
end

# issue #31352
fake_repl() do stdin_write, stdout_read, repl
    repltask = @async begin
        REPL.run_repl(repl)
    end
    write(stdin_write, "struct Errs end\n")
    readline(stdout_read)
    readline(stdout_read)
    write(stdin_write, "Base.show(io::IO, ::Errs) = throw(Errs())\n")
    readline(stdout_read)
    readline(stdout_read)
    write(stdin_write, "Errs()\n")
    write(stdin_write, '\x04')
    wait(repltask)
    @test istaskdone(repltask)
end

# issue #34842
fake_repl() do stdin_write, stdout_read, repl
    repltask = @async begin
        REPL.run_repl(repl)
    end
    write(stdin_write, "?;\n")
    readline(stdout_read)
    @test endswith(readline(stdout_read),";")
    write(stdin_write, '\x04')
    Base.wait(repltask)
end


fake_repl() do stdin_write, stdout_read, repl
    repltask = @async begin
        REPL.run_repl(repl)
    end
    write(stdin_write, "anything\x15\x19\x19") # ^u^y^y : kill line backwards + 2 yanks
    s1 = readuntil(stdout_read, "anything") # typed
    s2 = readuntil(stdout_read, "anything") # yanked (first ^y)
    s3 = readuntil(stdout_read, "anything") # previous yanked refreshed (from second ^y)
    s4 = readuntil(stdout_read, "anything", keep=true) # last yanked
    # necessary to read at least some part of the buffer,
    # for the "region_active" to have time to be updated

    @test LineEdit.state(repl.mistate).region_active == :off
    @test s4 == "anything" # no control characters between the last two occurrences of "anything"
    write(stdin_write, "\x15\x04")
    Base.wait(repltask)
end

# AST transformations (softscope, Revise, OhMyREPL, etc.)
repl_channel = Channel(1)
response_channel = Channel(1)
backend = REPL.start_repl_backend(repl_channel, response_channel)
put!(repl_channel, (:(1+1), false))
reply = take!(response_channel)
@test reply == (2, false)
twice(ex) = Expr(:tuple, ex, ex)
push!(backend.ast_transforms, twice)
put!(repl_channel, (:(1+1), false))
reply = take!(response_channel)
@test reply == ((2, 2), false)
put!(repl_channel, (nothing, -1))
Base.wait(backend.backend_task)
