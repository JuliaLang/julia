using Base.LineEdit
using TestHelpers

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

test1_func = LineEdit.keymap([foo_keymap])

function run_test(f,buf)
    global a_foo, a_bar, b_bar
    a_foo = a_bar = b_bar = 0
    while !eof(buf)
        f(buf,nothing)
    end
end

run_test(test1_func,IOBuffer("aa"))
@test a_foo == 2

test2_func = LineEdit.keymap([foo2_keymap, foo_keymap])

run_test(test2_func,IOBuffer("aaabb"))
@test a_foo == 3
@test b_foo == 2

test3_func = LineEdit.keymap([bar_keymap, foo_keymap])

run_test(test3_func,IOBuffer("aab"))
@test a_bar == 2
@test b_bar == 1

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
@test bytestring(buf.data[1:buf.size]) == "type "

buf = IOBuffer("4 +aaa+ x")
seek(buf,8)
@test LineEdit.edit_delete_prev_word(buf)
@test position(buf) == 3
@test buf.size == 4
@test bytestring(buf.data[1:buf.size]) == "4 +x"

buf = IOBuffer("x = func(arg1,arg2 , arg3)")
seekend(buf)
LineEdit.char_move_word_left(buf)
@test position(buf) == 21
@test LineEdit.edit_delete_prev_word(buf)
@test bytestring(buf.data[1:buf.size]) == "x = func(arg1,arg3)"
@test LineEdit.edit_delete_prev_word(buf)
@test bytestring(buf.data[1:buf.size]) == "x = func(arg3)"
@test LineEdit.edit_delete_prev_word(buf)
@test bytestring(buf.data[1:buf.size]) == "x = arg3)"

# Unicode combining characters
let buf = IOBuffer()
    LineEdit.edit_insert(buf, "â")
    LineEdit.edit_move_left(buf)
    @test position(buf) == 0
    LineEdit.edit_move_right(buf)
    @test nb_available(buf) == 0
    LineEdit.edit_backspace(buf)
    @test bytestring(buf.data[1:buf.size]) == "a"
end

## edit_transpose ##
let buf = IOBuffer()
    LineEdit.edit_insert(buf, "abcde")
    seek(buf,0)
    LineEdit.edit_transpose(buf)
    @test bytestring(buf.data[1:buf.size]) == "abcde"
    LineEdit.char_move_right(buf)
    LineEdit.edit_transpose(buf)
    @test bytestring(buf.data[1:buf.size]) == "bacde"
    LineEdit.edit_transpose(buf)
    @test bytestring(buf.data[1:buf.size]) == "bcade"
    seekend(buf)
    LineEdit.edit_transpose(buf)
    @test bytestring(buf.data[1:buf.size]) == "bcaed"
    LineEdit.edit_transpose(buf)
    @test bytestring(buf.data[1:buf.size]) == "bcade"

    seek(buf, 0)
    LineEdit.edit_clear(buf)
    LineEdit.edit_insert(buf, "αβγδε")
    seek(buf,0)
    LineEdit.edit_transpose(buf)
    @test bytestring(buf.data[1:buf.size]) == "αβγδε"
    LineEdit.char_move_right(buf)
    LineEdit.edit_transpose(buf)
    @test bytestring(buf.data[1:buf.size]) == "βαγδε"
    LineEdit.edit_transpose(buf)
    @test bytestring(buf.data[1:buf.size]) == "βγαδε"
    seekend(buf)
    LineEdit.edit_transpose(buf)
    @test bytestring(buf.data[1:buf.size]) == "βγαεδ"
    LineEdit.edit_transpose(buf)
    @test bytestring(buf.data[1:buf.size]) == "βγαδε"
end

let
    term = TestHelpers.FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())
    s = LineEdit.init_state(term, ModalInterface([Prompt("test> ")]))
    buf = LineEdit.buffer(s)

    LineEdit.edit_insert(s,"first line\nsecond line\nthird line")
    @test bytestring(buf.data[1:buf.size]) == "first line\nsecond line\nthird line"

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
    @test bytestring(buf.data[1:buf.size]) == "second line\nthird line"
    LineEdit.move_line_end(s)
    LineEdit.edit_move_right(s)
    LineEdit.edit_yank(s)
    @test bytestring(buf.data[1:buf.size]) == "second line\nfirst line\nthird line"
end

# Issue 7845
# First construct a problematic string:
# julia> is 6 characters + 1 character for space,
# so the rest of the terminal is 73 characters
#########################################################################
buf = IOBuffer(
"begin\nprint(\"A very very very very very very very very very very very very ve\")\nend")
seek(buf,4)
outbuf = IOBuffer()
termbuf = Base.Terminals.TerminalBuffer(outbuf)
term = TestHelpers.FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())
s = LineEdit.refresh_multi_line(termbuf, term, buf,
    Base.LineEdit.InputAreaState(0,0), "julia> ", indent = 7)
@test s == Base.LineEdit.InputAreaState(3,1)
