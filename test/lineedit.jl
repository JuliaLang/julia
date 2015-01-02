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

test1_dict = LineEdit.keymap([foo_keymap])

function run_test(d,buf)
    global a_foo, a_bar, b_bar
    a_foo = a_bar = b_bar = 0
    while !eof(buf)
        LineEdit.match_input(d, nothing, buf)(nothing,nothing)
    end
end

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
