# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.LineEdit
isdefined(Main, :TestHelpers) || @eval Main include(joinpath(dirname(@__FILE__), "TestHelpers.jl"))
using TestHelpers

function run_test(d,buf)
    global a_foo, b_foo, a_bar, b_bar
    a_foo = b_foo = a_bar = b_bar = 0
    while !eof(buf)
        LineEdit.match_input(d, nothing, buf)(nothing,nothing)
    end
end


a_foo = 0

const foo_keymap = Dict(
    'a' => (o...)->(global a_foo; a_foo += 1)
)

b_foo = 0

const foo2_keymap = Dict(
    'b' => (o...)->(global b_foo; b_foo += 1)
)

a_bar = 0
b_bar = 0

const bar_keymap = Dict(
    'a' => (o...)->(global a_bar; a_bar += 1),
    'b' => (o...)->(global b_bar; b_bar += 1)
)

test1_dict = LineEdit.keymap([foo_keymap])

run_test(test1_dict,IOBuffer("aa"))
@test a_foo == 2

test2_dict = LineEdit.keymap([foo2_keymap, foo_keymap])

run_test(test2_dict,IOBuffer("aaabb"))
@test a_foo == 3
@test b_foo == 2

test3_dict = LineEdit.keymap([bar_keymap, foo_keymap])

run_test(test3_dict,IOBuffer("aab"))
@test a_bar == 2
@test b_bar == 1

# Multiple spellings in the same keymap
const test_keymap_1 = Dict(
    "^C" => (o...)->1,
    "\\C-C" => (o...)->2
)

@test_throws ErrorException LineEdit.keymap([test_keymap_1])

a_foo = a_bar = 0

const test_keymap_2 = Dict(
    "abc" => (o...)->(global a_foo = 1)
)

const test_keymap_3 = Dict(
    "a"  => (o...)->(global a_foo = 2),
    "bc" => (o...)->(global a_bar = 3)
)

function keymap_fcn(keymaps)
    d = LineEdit.keymap(keymaps)
    f = buf->(LineEdit.match_input(d, nothing, buf)(nothing,nothing))
end

let f = keymap_fcn([test_keymap_3, test_keymap_2])
    buf = IOBuffer("abc")
    f(buf); f(buf)
    @test a_foo == 2
    @test a_bar == 3
    @test eof(buf)
end

# Eager redirection when the redirected-to behavior is changed.

a_foo = 0

const test_keymap_4 = Dict(
    "a" => (o...)->(global a_foo = 1),
    "b" => "a",
    "c" => (o...)->(global a_foo = 2),
)

const test_keymap_5 = Dict(
    "a" => (o...)->(global a_foo = 3),
    "d" => "c"
)

let f = keymap_fcn([test_keymap_5, test_keymap_4])
    buf = IOBuffer("abd")
    f(buf)
    @test a_foo == 3
    f(buf)
    @test a_foo == 1
    f(buf)
    @test a_foo == 2
    @test eof(buf)
end

# Eager redirection with cycles

const test_cycle = Dict(
    "a" => "b",
    "b" => "a"
)

@test_throws ErrorException keymap([test_cycle])

# Lazy redirection with Cycles

const level1 = Dict(
    "a" => LineEdit.KeyAlias("b")
)

const level2a = Dict(
    "b" => "a"
)

const level2b = Dict(
    "b" => LineEdit.KeyAlias("a")
)

@test_throws ErrorException keymap([level2a,level1])
@test_throws ErrorException keymap([level2b,level1])

# Lazy redirection functionality test

a_foo = 0

const test_keymap_6 = Dict(
    "a" => (o...)->(global a_foo = 1),
    "b" => LineEdit.KeyAlias("a"),
    "c" => (o...)->(global a_foo = 2),
)

const test_keymap_7 = Dict(
    "a" => (o...)->(global a_foo = 3),
    "d" => "c"
)

let f = keymap_fcn([test_keymap_7, test_keymap_6])
    buf = IOBuffer("abd")
    f(buf)
    @test a_foo == 3
    a_foo = 0
    f(buf)
    @test a_foo == 3
    f(buf)
    @test a_foo == 2
    @test eof(buf)
end

# Test requiring postprocessing (see conflict fixing in LineEdit.jl )

global path1 = 0
global path2 = 0
global path3 = 0

const test_keymap_8 = Dict(
    "**" => (o...)->(global path1 += 1),
    "ab" => (o...)->(global path2 += 1),
    "cd" => (o...)->(global path3 += 1),
    "d" => (o...)->(error("This is not the key you're looking for"))
)

let f = keymap_fcn([test_keymap_8])
    buf = IOBuffer("bbabaccd")
    f(buf)
    @test path1 == 1
    f(buf)
    @test path2 == 1
    f(buf)
    @test path1 == 2
    f(buf)
    @test path3 == 1
    @test eof(buf)
end

global path1 = 0
global path2 = 0

const test_keymap_9 = Dict(
    "***" => (o...)->(global path1 += 1),
    "*a*" => (o...)->(global path2 += 1)
)

let f = keymap_fcn([test_keymap_9])
    buf = IOBuffer("abaaaa")
    f(buf)
    @test path1 == 1
    f(buf)
    @test path2 == 1
    @test eof(buf)
end


## edit_move{left,right} ##
buf = IOBuffer("a\na\na\n")
seek(buf, 0)
for i = 1:6
    LineEdit.edit_move_right(buf)
    @test position(buf) == i
end
@test eof(buf)
for i = 5:0
    LineEdit.edit_move_left(buf)
    @test position(buf) == i
end

# skip unicode combining characters
buf = IOBuffer("ŷ")
seek(buf, 0)
LineEdit.edit_move_right(buf)
@test eof(buf)
LineEdit.edit_move_left(buf)
@test position(buf) == 0

## edit_move_{up,down} ##

buf = IOBuffer("type X\n    a::Int\nend")
for i = 0:6
    seek(buf,i)
    @test !LineEdit.edit_move_up(buf)
    @test position(buf) == i
    seek(buf,i)
    @test LineEdit.edit_move_down(buf)
    @test position(buf) == i+7
end
for i = 7:17
    seek(buf,i)
    @test LineEdit.edit_move_up(buf)
    @test position(buf) == min(i-7,6)
    seek(buf,i)
    @test LineEdit.edit_move_down(buf)
    @test position(buf) == min(i+11,21)
end
for i = 18:21
    seek(buf,i)
    @test LineEdit.edit_move_up(buf)
    @test position(buf) == i-11
    seek(buf,i)
    @test !LineEdit.edit_move_down(buf)
    @test position(buf) == i
end

buf = IOBuffer("type X\n\n")
seekend(buf)
@test LineEdit.edit_move_up(buf)
@test position(buf) == 7
@test LineEdit.edit_move_up(buf)
@test position(buf) == 0
@test !LineEdit.edit_move_up(buf)
@test position(buf) == 0
seek(buf,0)
@test LineEdit.edit_move_down(buf)
@test position(buf) == 7
@test LineEdit.edit_move_down(buf)
@test position(buf) == 8
@test !LineEdit.edit_move_down(buf)
@test position(buf) == 8

## edit_delete_prev_word ##

buf = IOBuffer("type X\n ")
seekend(buf)
@test LineEdit.edit_delete_prev_word(buf)
@test position(buf) == 5
@test buf.size == 5
@test String(buf.data[1:buf.size]) == "type "

buf = IOBuffer("4 +aaa+ x")
seek(buf,8)
@test LineEdit.edit_delete_prev_word(buf)
@test position(buf) == 3
@test buf.size == 4
@test String(buf.data[1:buf.size]) == "4 +x"

buf = IOBuffer("x = func(arg1,arg2 , arg3)")
seekend(buf)
LineEdit.char_move_word_left(buf)
@test position(buf) == 21
@test LineEdit.edit_delete_prev_word(buf)
@test String(buf.data[1:buf.size]) == "x = func(arg1,arg3)"
@test LineEdit.edit_delete_prev_word(buf)
@test String(buf.data[1:buf.size]) == "x = func(arg3)"
@test LineEdit.edit_delete_prev_word(buf)
@test String(buf.data[1:buf.size]) == "x = arg3)"

# Unicode combining characters
let buf = IOBuffer()
    LineEdit.edit_insert(buf, "â")
    LineEdit.edit_move_left(buf)
    @test position(buf) == 0
    LineEdit.edit_move_right(buf)
    @test nb_available(buf) == 0
    LineEdit.edit_backspace(buf)
    @test String(buf.data[1:buf.size]) == "a"
end

## edit_transpose_chars ##
let buf = IOBuffer()
    LineEdit.edit_insert(buf, "abcde")
    seek(buf,0)
    LineEdit.edit_transpose_chars(buf)
    @test String(buf.data[1:buf.size]) == "abcde"
    LineEdit.char_move_right(buf)
    LineEdit.edit_transpose_chars(buf)
    @test String(buf.data[1:buf.size]) == "bacde"
    LineEdit.edit_transpose_chars(buf)
    @test String(buf.data[1:buf.size]) == "bcade"
    seekend(buf)
    LineEdit.edit_transpose_chars(buf)
    @test String(buf.data[1:buf.size]) == "bcaed"
    LineEdit.edit_transpose_chars(buf)
    @test String(buf.data[1:buf.size]) == "bcade"

    seek(buf, 0)
    LineEdit.edit_clear(buf)
    LineEdit.edit_insert(buf, "αβγδε")
    seek(buf,0)
    LineEdit.edit_transpose_chars(buf)
    @test String(buf.data[1:buf.size]) == "αβγδε"
    LineEdit.char_move_right(buf)
    LineEdit.edit_transpose_chars(buf)
    @test String(buf.data[1:buf.size]) == "βαγδε"
    LineEdit.edit_transpose_chars(buf)
    @test String(buf.data[1:buf.size]) == "βγαδε"
    seekend(buf)
    LineEdit.edit_transpose_chars(buf)
    @test String(buf.data[1:buf.size]) == "βγαεδ"
    LineEdit.edit_transpose_chars(buf)
    @test String(buf.data[1:buf.size]) == "βγαδε"
end

@testset "edit_word_transpose" begin
    buf = IOBuffer()
    mode = Ref{Symbol}()
    function transpose!(i) # i: char indice
        seek(buf, Base.unsafe_chr2ind(String(take!(copy(buf))), i+1)-1)
        LineEdit.edit_transpose_words(buf, mode[])
        str = String(take!(copy(buf)))
        str, Base.unsafe_ind2chr(str, position(buf)+1)-1
    end

    mode[] = :readline
    LineEdit.edit_insert(buf, "àbç def  gh ")
    @test transpose!(0) == ("àbç def  gh ", 0)
    @test transpose!(1) == ("àbç def  gh ", 1)
    @test transpose!(2) == ("àbç def  gh ", 2)
    @test transpose!(3) == ("def àbç  gh ", 7)
    @test transpose!(4) == ("àbç def  gh ", 7)
    @test transpose!(5) == ("def àbç  gh ", 7)
    @test transpose!(6) == ("àbç def  gh ", 7)
    @test transpose!(7) == ("àbç gh  def ", 11)
    @test transpose!(10) == ("àbç def  gh ", 11)
    @test transpose!(11) == ("àbç gh   def", 12)
    LineEdit.edit_insert(buf, " ")
    @test transpose!(13) == ("àbç def    gh", 13)

    take!(buf)
    mode[] = :emacs
    LineEdit.edit_insert(buf, "àbç def  gh ")
    @test transpose!(0) == ("def àbç  gh ", 7)
    @test transpose!(4) == ("àbç def  gh ", 7)
    @test transpose!(5) == ("àbç gh  def ", 11)
    @test transpose!(10) == ("àbç def   gh", 12)
    LineEdit.edit_insert(buf, " ")
    @test transpose!(13) == ("àbç gh    def", 13)
end

let
    term = TestHelpers.FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())
    s = LineEdit.init_state(term, ModalInterface([Prompt("test> ")]))
    buf = LineEdit.buffer(s)

    LineEdit.edit_insert(s,"first line\nsecond line\nthird line")
    @test String(buf.data[1:buf.size]) == "first line\nsecond line\nthird line"

    ## edit_move_line_start/end ##
    seek(buf, 0)
    LineEdit.move_line_end(s)
    @test position(buf) == sizeof("first line")
    LineEdit.move_line_end(s) # Only move to input end on repeated keypresses
    @test position(buf) == sizeof("first line")
    s.key_repeats = 1 # Manually flag a repeated keypress
    LineEdit.move_line_end(s)
    s.key_repeats = 0
    @test eof(buf)

    seekend(buf)
    LineEdit.move_line_start(s)
    @test position(buf) == sizeof("first line\nsecond line\n")
    LineEdit.move_line_start(s)
    @test position(buf) == sizeof("first line\nsecond line\n")
    s.key_repeats = 1 # Manually flag a repeated keypress
    LineEdit.move_line_start(s)
    s.key_repeats = 0
    @test position(buf) == 0

    ## edit_kill_line, edit_yank ##
    seek(buf, 0)
    LineEdit.edit_kill_line(s)
    s.key_repeats = 1 # Manually flag a repeated keypress
    LineEdit.edit_kill_line(s)
    s.key_repeats = 0
    @test String(buf.data[1:buf.size]) == "second line\nthird line"
    LineEdit.move_line_end(s)
    LineEdit.edit_move_right(s)
    LineEdit.edit_yank(s)
    @test String(buf.data[1:buf.size]) == "second line\nfirst line\nthird line"
end

# Issue 7845
# First construct a problematic string:
# julia> is 6 characters + 1 character for space,
# so the rest of the terminal is 73 characters
#########################################################################
let
    buf = IOBuffer(
    "begin\nprint(\"A very very very very very very very very very very very very ve\")\nend")
    seek(buf,4)
    outbuf = IOBuffer()
    termbuf = Base.Terminals.TerminalBuffer(outbuf)
    term = TestHelpers.FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())
    s = LineEdit.refresh_multi_line(termbuf, term, buf,
        Base.LineEdit.InputAreaState(0,0), "julia> ", indent = 7)
    @test s == Base.LineEdit.InputAreaState(3,1)
end

@testset "function prompt indentation" begin
    term = TestHelpers.FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer(), false)
    # default prompt: PromptState.indent should not be set to a final fixed value
    s = LineEdit.init_state(term, ModalInterface([Prompt("julia> ")]))
    ps::LineEdit.PromptState = s.mode_state[s.current_mode]
    @test ps.indent == -1
    # the prompt is modified afterwards to a function
    ps.p.prompt = let i = 0
        () -> ["Julia is Fun! > ", "> "][mod1(i+=1, 2)] # lengths are 16 and 2
    end
    buf = LineEdit.buffer(ps)
    write(buf, "begin\n    julia = :fun\nend")
    outbuf = IOBuffer()
    termbuf = Base.Terminals.TerminalBuffer(outbuf)
    LineEdit.refresh_multi_line(termbuf, term, ps)
    @test String(take!(outbuf)) ==
        "\r\e[0K\e[1mJulia is Fun! > \e[0m\r\e[16Cbegin\n" *
        "\r\e[16C    julia = :fun\n" *
        "\r\e[16Cend\r\e[19C"
    LineEdit.refresh_multi_line(termbuf, term, ps)
    @test String(take!(copy(outbuf))) ==
        "\r\e[0K\e[1A\r\e[0K\e[1A\r\e[0K\e[1m> \e[0m\r\e[2Cbegin\n" *
        "\r\e[2C    julia = :fun\n" *
        "\r\e[2Cend\r\e[5C"
end

@testset "tab/backspace alignment feature" begin
    term = TestHelpers.FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())
    s = LineEdit.init_state(term, ModalInterface([Prompt("test> ")]))
    function bufferdata(s)
        buf = LineEdit.buffer(s)
        String(buf.data[1:buf.size])
    end
    move_left(s, n) = for x = 1:n
        LineEdit.edit_move_left(s)
    end

    bufpos(s::Base.LineEdit.MIState) = position(LineEdit.buffer(s))

    LineEdit.edit_insert(s, "for x=1:10\n")
    LineEdit.edit_tab(s)
    @test bufferdata(s) == "for x=1:10\n    "
    LineEdit.edit_backspace(s, true, false)
    @test bufferdata(s) == "for x=1:10\n"
    LineEdit.edit_insert(s, "  ")
    @test bufpos(s) == 13
    LineEdit.edit_tab(s)
    @test bufferdata(s) == "for x=1:10\n    "
    LineEdit.edit_insert(s, "  ")
    LineEdit.edit_backspace(s, true, false)
    @test bufferdata(s) == "for x=1:10\n    "
    LineEdit.edit_insert(s, "éé=3   ")
    LineEdit.edit_tab(s)
    @test bufferdata(s) == "for x=1:10\n    éé=3    "
    LineEdit.edit_backspace(s, true, false)
    @test bufferdata(s) == "for x=1:10\n    éé=3"
    LineEdit.edit_insert(s, "\n    1∉x  ")
    LineEdit.edit_tab(s)
    @test bufferdata(s) == "for x=1:10\n    éé=3\n    1∉x     "
    LineEdit.edit_backspace(s, false, false)
    @test bufferdata(s) == "for x=1:10\n    éé=3\n    1∉x    "
    LineEdit.edit_backspace(s, true, false)
    @test bufferdata(s) == "for x=1:10\n    éé=3\n    1∉x "
    LineEdit.edit_move_word_left(s)
    LineEdit.edit_tab(s)
    @test bufferdata(s) == "for x=1:10\n    éé=3\n        1∉x "
    LineEdit.move_line_start(s)
    @test bufpos(s) == 22
    LineEdit.edit_tab(s, true)
    @test bufferdata(s) == "for x=1:10\n    éé=3\n        1∉x "
    @test bufpos(s) == 30
    LineEdit.edit_move_left(s)
    @test bufpos(s) == 29
    LineEdit.edit_backspace(s, true, true)
    @test bufferdata(s) == "for x=1:10\n    éé=3\n    1∉x "
    @test bufpos(s) == 26
    LineEdit.edit_tab(s, false) # same as edit_tab(s, true) here
    @test bufpos(s) == 30
    move_left(s, 6)
    @test bufpos(s) == 24
    LineEdit.edit_backspace(s, true, true)
    @test bufferdata(s) == "for x=1:10\n    éé=3\n    1∉x "
    @test bufpos(s) == 22
    LineEdit.edit_kill_line(s)
    LineEdit.edit_insert(s, ' '^10)
    move_left(s, 7)
    @test bufferdata(s) == "for x=1:10\n    éé=3\n          "
    @test bufpos(s) == 25
    LineEdit.edit_tab(s, true, false)
    @test bufpos(s) == 32
    move_left(s, 7)
    LineEdit.edit_tab(s, true, true)
    @test bufpos(s) == 26
    @test bufferdata(s) == "for x=1:10\n    éé=3\n    "
    # test again the same, when there is a next line
    LineEdit.edit_insert(s, "      \nend")
    move_left(s, 11)
    @test bufpos(s) == 25
    LineEdit.edit_tab(s, true, false)
    @test bufpos(s) == 32
    move_left(s, 7)
    LineEdit.edit_tab(s, true, true)
    @test bufpos(s) == 26
    @test bufferdata(s) == "for x=1:10\n    éé=3\n    \nend"
end

@testset "newline alignment feature" begin
    term = TestHelpers.FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())
    s = LineEdit.init_state(term, ModalInterface([Prompt("test> ")]))
    function bufferdata(s)
        buf = LineEdit.buffer(s)
        String(buf.data[1:buf.size])
    end

    LineEdit.edit_insert(s, "for x=1:10\n    é = 1")
    LineEdit.edit_insert_newline(s)
    @test bufferdata(s) == "for x=1:10\n    é = 1\n    "
    LineEdit.edit_insert(s, " b = 2")
    LineEdit.edit_insert_newline(s)
    @test bufferdata(s) == "for x=1:10\n    é = 1\n     b = 2\n     "
    # after an empty line, should still insert the expected number of spaces
    LineEdit.edit_insert_newline(s)
    @test bufferdata(s) == "for x=1:10\n    é = 1\n     b = 2\n     \n     "
    LineEdit.edit_insert_newline(s, 0)
    @test bufferdata(s) == "for x=1:10\n    é = 1\n     b = 2\n     \n     \n"
    LineEdit.edit_insert_newline(s, 2)
    @test bufferdata(s) == "for x=1:10\n    é = 1\n     b = 2\n     \n     \n\n  "
    # test when point before first letter of the line
    for i=6:10
        LineEdit.edit_clear(s)
        LineEdit.edit_insert(s, "begin\n    x")
        seek(LineEdit.buffer(s), i)
        LineEdit.edit_insert_newline(s)
        @test bufferdata(s) == "begin\n" * ' '^(i-6) * "\n    x"
    end
end

@testset "change case on the right" begin
    buf = IOBuffer()
    LineEdit.edit_insert(buf, "aa bb CC")
    seekstart(buf)
    LineEdit.edit_upper_case(buf)
    LineEdit.edit_title_case(buf)
    @test String(take!(copy(buf))) == "AA Bb CC"
    @test position(buf) == 5
    LineEdit.edit_lower_case(buf)
    @test String(take!(copy(buf))) == "AA Bb cc"
end
