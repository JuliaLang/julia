# This file is a part of Julia. License is MIT: https://julialang.org/license

using REPL.TerminalMenus: readevent, KeyEvent, MouseEvent, PasteEvent, FocusEvent, SchemeEvent,
    LegacyMouseEvent, SHIFT, ALT, CTRL, LineField, edit!, paste!

@testset "readevent" begin
    ev(s) = readevent(IOBuffer(s))
    @test ev("q") == KeyEvent('q')
    @test ev("é") == KeyEvent('é')
    @test ev("\r") == ev("\n") == KeyEvent(:enter)
    @test ev("\t") == KeyEvent(:tab)
    @test ev("\x7f") == ev("\b") == KeyEvent(:backspace)
    @test ev("\x01") == KeyEvent('a', CTRL)
    @test ev("\0") == KeyEvent(' ', CTRL)
    @test ev("\x1f") == KeyEvent('_', CTRL)
    @test ev("\e") == KeyEvent(:escape)
    @test ev("\e[A") == ev("\eOA") == KeyEvent(:up)
    @test ev("\e[1;5C") == KeyEvent(:right, CTRL)
    @test ev("\e[1;3D") == ev("\e\e[D") == KeyEvent(:left, ALT)
    @test ev("\e[5;2~") == KeyEvent(:pageup, SHIFT)
    @test ev("\e[3~") == KeyEvent(:delete)
    @test ev("\eOP") == ev("\e[11~") == KeyEvent(:f1)
    @test ev("\e[24~") == KeyEvent(:f12)
    @test ev("\e[Z") == KeyEvent(:tab, SHIFT)
    @test ev("\eOM") == KeyEvent(:enter)
    # Alt and a key is Escape and the key, with nothing more behind it
    @test ev("\eb") == KeyEvent('b', ALT)
    @test ev("\e\x7f") == KeyEvent(:backspace, ALT)
    @test ev("\e\x01") == KeyEvent('a', CTRL | ALT)
    @test ev("\e[") == KeyEvent('[', ALT) && ev("\eO") == KeyEvent('O', ALT)
    @test ev("\e\e") == KeyEvent(:escape, ALT)

    # mouse reports, in the SGR encoding
    @test ev("\e[<0;12;7M") == MouseEvent(:press, 1, 7, 12, 0)
    @test ev("\e[<2;12;7m") == MouseEvent(:release, 3, 7, 12, 0)
    @test ev("\e[<32;12;7M") == MouseEvent(:drag, 1, 7, 12, 0)
    @test ev("\e[<35;12;7M") == MouseEvent(:move, 0, 7, 12, 0)
    @test ev("\e[<64;12;7M") == MouseEvent(:wheelup, 0, 7, 12, 0)
    @test ev("\e[<67;12;7M") == MouseEvent(:wheelright, 0, 7, 12, 0)
    @test ev("\e[<128;12;7M") == MouseEvent(:press, 8, 7, 12, 0)
    @test ev("\e[<129;12;7m") == MouseEvent(:release, 9, 7, 12, 0)
    @test ev("\e[<16;12;7M") == MouseEvent(:press, 1, 7, 12, CTRL)

    @test ev("\e[200~some\ntext\e[201~") == PasteEvent("some\ntext")
    @test ev("\e[200~cut off") == PasteEvent("cut off")
    @test ev("\e[I") == FocusEvent(true) && ev("\e[O") == FocusEvent(false)
    @test ev("\e[?997;1n") == SchemeEvent(true, "")
    @test ev("\e[?997;2n") == SchemeEvent(false, "")
    @test ev("\e]11;rgb:1e1e/1e1e/1e1e\e\\") == ev("\e]11;rgb:1e1e/1e1e/1e1e\a") ==
        SchemeEvent(nothing, "rgb:1e1e/1e1e/1e1e")

    # unknown sequences, and ones cut off by the end of input, are read whole
    for (seq, rest) in ("\e[299~q" => "q", "\e[?62;22cq" => "q", "\e]10;rgb:0/0/0\aq" => "q",
                        # xterm.js reports a place it cannot name like this
                        "\e[<0;NaN;NaNmq" => "q",
                        # an X10 report's bytes come after its final byte, and are not keys
                        "\e[M #!q" => "q", "\e[M\e[Aq" => "\e[Aq")
        io = IOBuffer(seq)
        e = readevent(io)
        @test e === nothing || e isa LegacyMouseEvent
        @test read(io, String) == rest
    end
    @test ev("\e[M #!") == LegacyMouseEvent()
    io = IOBuffer("\e[1;5")
    @test readevent(io) === nothing && eof(io)
end

@testset "screen modes and frames" begin
    out = IOBuffer()
    term = REPL.Terminals.TTYTerminal("xterm", IOBuffer(), out, IOBuffer())
    s = TerminalMenus.enter_screen(term; mouse=true, motion=true, paste=true, scheme=true)
    @test !s.raw # an IOBuffer has no raw mode
    @test String(take!(out)) == string("\e[?1049h\e[?25l", TerminalMenus.mouse_reporting(true; motion=true),
                                       TerminalMenus.bracketed_paste(true), TerminalMenus.scheme_reports(true))
    @test occursin("\e[?1003h", TerminalMenus.mouse_reporting(true; motion=true))
    # an editor runs on the screen as it was, and the modes come back after it
    @test TerminalMenus.suspend(() -> (write(out, "edit"); 1), s) == 1
    shown = String(take!(out))
    @test occursin("\e[?1049ledit\e[?1049h", shown) && endswith(shown, TerminalMenus.scheme_reports(true))
    # leaving undoes the modes in the reverse order, and a terminal gone is no error
    TerminalMenus.leave_screen(s)
    @test String(take!(out)) == string(TerminalMenus.scheme_reports(false), TerminalMenus.bracketed_paste(false),
                                       TerminalMenus.mouse_reporting(false; motion=true), "\e[?25h\e[?1049l")
    close(out)
    @test TerminalMenus.leave_screen(s) === nothing

    # only changed rows are written, each on a line deleted and put back
    io = IOBuffer()
    prev = TerminalMenus.write_frame(io, ["one", "two"])
    @test String(take!(io)) == "\e[?2026h\e[?7l\e[r\e[1H\e[M\e[L\e[0mone\e[0m\e[2H\e[M\e[L\e[0mtwo\e[0m\e[?7h\e[?2026l"
    TerminalMenus.write_frame(io, ["one", "too"], prev)
    shown = String(take!(io))
    @test occursin("\e[2H\e[M\e[L\e[0mtoo", shown) && !occursin("one", shown)
    # an annotated row is written in its faces where there is colour
    row = Base.AnnotatedString("red", [(1:3, :face, :red)])
    TerminalMenus.write_frame(IOContext(io, :color => true), [row])
    @test occursin("\e[31mred", String(take!(io)))
end

@testset "LineField" begin
    f = LineField()
    type!(f, keys...) = foreach(k -> edit!(f, k isa KeyEvent ? k : KeyEvent(k)), keys)
    ctrl(c) = KeyEvent(c, CTRL)
    alt(k) = KeyEvent(k, ALT)
    type!(f, "Base.srt"...)
    type!(f, :left, :left, 'o')
    @test f.text == "Base.sort" && f.cursor == 7
    type!(f, :end, alt(:backspace))
    @test f.text == "Base." && f.killed == "sort"
    type!(f, ctrl('y'), ctrl('y'))
    @test f.text == "Base.sortsort"
    type!(f, alt(:left), ctrl('k'))
    @test f.text == "Base." && f.killed == "sortsort"
    type!(f, ctrl('a'), :delete, ctrl('e'), ctrl('w'))
    @test f.text == "" && f.killed == "ase."
    type!(f, "ab"..., ctrl('t'))
    @test f.text == "ba"
    type!(f, :home, alt('d'))
    @test f.text == "" && f.cursor == 0
    # a key that does not edit is the program's
    @test !edit!(f, KeyEvent(:enter)) && !edit!(f, KeyEvent(:up)) && !edit!(f, KeyEvent(:escape))
    @test !edit!(f, ctrl('c'))
    paste!(f, "one\ntwo\x01")
    @test f.text == "one two" && f.cursor == 7
    type!(f, :home, ctrl('f'), ctrl('f'), ctrl('f'), ctrl('u'))
    @test f.text == " two" && f.cursor == 0
end
