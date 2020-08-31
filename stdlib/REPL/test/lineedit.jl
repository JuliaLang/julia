# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using REPL
import REPL.LineEdit
import REPL.LineEdit: edit_insert, buffer, content, setmark, getmark, region

include("FakeTerminals.jl")
import .FakeTerminals.FakeTerminal

# no need to have animation in tests
REPL.GlobalOptions.region_animation_duration=0.0001
# tests are inserting code much faster than humans
REPL.GlobalOptions.auto_indent_time_threshold = -0.0

## helper functions

function new_state()
    term = FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())
    LineEdit.init_state(term, LineEdit.ModalInterface([LineEdit.Prompt("test> ")]))
end

charseek(buf, i) = seek(buf, nextind(content(buf), 0, i+1)-1)
charpos(buf, pos=position(buf)) = length(content(buf), 1, pos)

function transform!(f, s, i = -1) # i is char-based (not bytes) buffer position
    buf = buffer(s)
    i >= 0 && charseek(buf, i)
    # simulate what happens in LineEdit.set_action!
    s isa LineEdit.MIState && (s.current_action = :unknown)
    status = f(s)
    if s isa LineEdit.MIState && status != :ignore
        # simulate what happens in LineEdit.prompt!
        s.last_action = s.current_action
    end
    content(s), charpos(buf), charpos(buf, getmark(buf))
end


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

@test_throws ErrorException LineEdit.keymap([test_cycle])

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

@test_throws ErrorException LineEdit.keymap([level2a,level1])
@test_throws ErrorException LineEdit.keymap([level2b,level1])

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
    global a_foo = 0
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
    @test LineEdit.edit_move_right(buf)
    @test position(buf) == i
end
@test eof(buf)
@test !LineEdit.edit_move_right(buf)
for i = 5:-1:0
    @test @inferred LineEdit.edit_move_left(buf)
    @test position(buf) == i
end

# skip unicode combining characters
buf = IOBuffer("ŷ")
seek(buf, 0)
@test LineEdit.edit_move_right(buf)
@test eof(buf)
@test LineEdit.edit_move_left(buf)
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
@test !isempty(@inferred(LineEdit.edit_delete_prev_word(buf)))
@test position(buf) == 5
@test buf.size == 5
@test content(buf) == "type "

buf = IOBuffer("4 +aaa+ x")
seek(buf,8)
@test !isempty(LineEdit.edit_delete_prev_word(buf))
@test position(buf) == 3
@test buf.size == 4
@test content(buf) == "4 +x"

buf = IOBuffer("x = func(arg1,arg2 , arg3)")
seekend(buf)
LineEdit.char_move_word_left(buf)
@test position(buf) == 21
@test !isempty(@inferred(LineEdit.edit_delete_prev_word(buf)))
@test content(buf) == "x = func(arg1,arg3)"
@test !isempty(LineEdit.edit_delete_prev_word(buf))
@test content(buf) == "x = func(arg3)"
@test !isempty(LineEdit.edit_delete_prev_word(buf))
@test content(buf) == "x = arg3)"

# Unicode combining characters
let buf = IOBuffer()
    edit_insert(buf, "â")
    LineEdit.edit_move_left(buf)
    @test position(buf) == 0
    LineEdit.edit_move_right(buf)
    @test @inferred(bytesavailable(buf)) == 0
    @inferred(LineEdit.edit_backspace(buf, false, false))
    @test content(buf) == "a"
end

## edit_transpose_chars ##
let buf = IOBuffer()
    @inferred(edit_insert(buf, "abcde"))
    seek(buf,0)
    @inferred(LineEdit.edit_transpose_chars(buf))
    @test content(buf) == "abcde"
    @inferred Union{Char,Bool} LineEdit.char_move_right(buf)
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "bacde"
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "bcade"
    seekend(buf)
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "bcaed"
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "bcade"

    seek(buf, 0)
    @inferred(LineEdit.edit_clear(buf))
    edit_insert(buf, "αβγδε")
    seek(buf,0)
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "αβγδε"
    LineEdit.char_move_right(buf)
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "βαγδε"
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "βγαδε"
    seekend(buf)
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "βγαεδ"
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "βγαδε"
end

@testset "edit_word_transpose" begin
    local buf, mode
    buf = IOBuffer()
    mode = Ref{Symbol}()
    transpose!(i) = transform!(buf -> LineEdit.edit_transpose_words(buf, mode[]),
                               buf, i)[1:2]

    mode[] = :readline
    edit_insert(buf, "àbç def  gh ")
    @test @inferred(transpose!(0)) == ("àbç def  gh ", 0)
    @test transpose!(1) == ("àbç def  gh ", 1)
    @test transpose!(2) == ("àbç def  gh ", 2)
    @test transpose!(3) == ("def àbç  gh ", 7)
    @test transpose!(4) == ("àbç def  gh ", 7)
    @test transpose!(5) == ("def àbç  gh ", 7)
    @test transpose!(6) == ("àbç def  gh ", 7)
    @test transpose!(7) == ("àbç gh  def ", 11)
    @test transpose!(10) == ("àbç def  gh ", 11)
    @test transpose!(11) == ("àbç gh   def", 12)
    @inferred(edit_insert(buf, " "))
    @test transpose!(13) == ("àbç def    gh", 13)

    take!(buf)
    mode[] = :emacs
    edit_insert(buf, "àbç def  gh ")
    @test transpose!(0) == ("def àbç  gh ", 7)
    @test transpose!(4) == ("àbç def  gh ", 7)
    @test transpose!(5) == ("àbç gh  def ", 11)
    @test transpose!(10) == ("àbç def   gh", 12)
    edit_insert(buf, " ")
    @test transpose!(13) == ("àbç gh    def", 13)
end

let s = new_state(),
    buf = buffer(s)

    @inferred Union{Int,LineEdit.InputAreaState} edit_insert(s,"first line\nsecond line\nthird line")
    @test content(buf) == "first line\nsecond line\nthird line"

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
    @inferred Union{Symbol,LineEdit.InputAreaState} LineEdit.edit_kill_line(s)
    s.key_repeats = 1 # Manually flag a repeated keypress
    LineEdit.edit_kill_line(s)
    s.key_repeats = 0
    @test content(buf) == "second line\nthird line"
    LineEdit.move_line_end(s)
    LineEdit.edit_move_right(s)
    @inferred Union{Symbol,LineEdit.InputAreaState} LineEdit.edit_yank(s)
    @test content(buf) == "second line\nfirst line\nthird line"
end

# Issue 7845
# First construct a problematic string:
# julia> is 6 characters + 1 character for space,
# so the rest of the terminal is 73 characters
#########################################################################
let buf = IOBuffer(
        "begin\nprint(\"A very very very very very very very very very very very very ve\")\nend")
    seek(buf, 4)
    outbuf = IOBuffer()
    termbuf = REPL.Terminals.TerminalBuffer(outbuf)
    term = FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())
    s = @inferred LineEdit.refresh_multi_line(termbuf, term, buf,
        REPL.LineEdit.InputAreaState(0,0), "julia> ", indent = 7)
    @test s == REPL.LineEdit.InputAreaState(3,1)
end

@testset "function prompt indentation" begin
    local s, term, ps, buf, outbuf, termbuf
    s = new_state()
    term = @inferred REPL.AbstractTerminal REPL.LineEdit.terminal(s)
    # default prompt: PromptState.indent should not be set to a final fixed value
    ps::LineEdit.PromptState = @inferred LineEdit.ModeState s.mode_state[s.current_mode]
    @test ps.indent == -1
    # the prompt is modified afterwards to a function
    ps.p.prompt = let i = 0
        () -> ["Julia is Fun! > ", "> "][mod1(i+=1, 2)] # lengths are 16 and 2
    end
    buf = buffer(ps)
    write(buf, "begin\n    julia = :fun\nend")
    outbuf = IOBuffer()
    termbuf = REPL.Terminals.TerminalBuffer(outbuf)
    @inferred(LineEdit.refresh_multi_line(termbuf, term, ps))
    @test String(take!(outbuf)) ==
        "\r\e[0K\e[1mJulia is Fun! > \e[0m\r\e[16Cbegin\n" *
        "\r\e[16C    julia = :fun\n" *
        "\r\e[16Cend\r\e[19C"
    LineEdit.refresh_multi_line(termbuf, term, ps)
    @test String(take!(outbuf)) ==
        "\r\e[0K\e[1A\r\e[0K\e[1A\r\e[0K\e[1m> \e[0m\r\e[2Cbegin\n" *
        "\r\e[2C    julia = :fun\n" *
        "\r\e[2Cend\r\e[5C"
end

@testset "shift selection" begin
    s = new_state()
    edit_insert(s, "αä🐨") # for issue #28183
    s.current_action = :unknown
    @inferred Union{Bool, LineEdit.InputAreaState} LineEdit.edit_shift_move(s, LineEdit.edit_move_left)
    @test LineEdit.region(s) == (5=>9)
    LineEdit.edit_shift_move(s, LineEdit.edit_move_left)
    @test LineEdit.region(s) == (2=>9)
    LineEdit.edit_shift_move(s, LineEdit.edit_move_left)
    @test LineEdit.region(s) == (0=>9)
    @inferred Union{Bool, LineEdit.InputAreaState} LineEdit.edit_shift_move(s, LineEdit.edit_move_right)
    @test LineEdit.region(s) == (2=>9)
end

@testset "tab/backspace alignment feature" begin
    s = new_state()
    move_left(s, n) = for x = 1:n
        LineEdit.edit_move_left(s)
    end

    edit_insert(s, "for x=1:10\n")
    @inferred Union{Symbol,LineEdit.InputAreaState} LineEdit.edit_tab(s)
    @test content(s) == "for x=1:10\n    "
    @inferred Union{Nothing,LineEdit.InputAreaState} LineEdit.edit_backspace(s, true, false)
    @test content(s) == "for x=1:10\n"
    edit_insert(s, "  ")
    @test position(s) == 13
    LineEdit.edit_tab(s)
    @test content(s) == "for x=1:10\n    "
    edit_insert(s, "  ")
    LineEdit.edit_backspace(s, true, false)
    @test content(s) == "for x=1:10\n    "
    edit_insert(s, "éé=3   ")
    LineEdit.edit_tab(s)
    @test content(s) == "for x=1:10\n    éé=3    "
    LineEdit.edit_backspace(s, true, false)
    @test content(s) == "for x=1:10\n    éé=3"
    edit_insert(s, "\n    1∉x  ")
    LineEdit.edit_tab(s)
    @test content(s) == "for x=1:10\n    éé=3\n    1∉x     "
    LineEdit.edit_backspace(s, false, false)
    @test content(s) == "for x=1:10\n    éé=3\n    1∉x    "
    LineEdit.edit_backspace(s, true, false)
    @test content(s) == "for x=1:10\n    éé=3\n    1∉x "
    LineEdit.edit_move_word_left(s)
    LineEdit.edit_tab(s)
    @test content(s) == "for x=1:10\n    éé=3\n        1∉x "
    LineEdit.move_line_start(s)
    @test position(s) == 22
    LineEdit.edit_tab(s, true)
    @test content(s) == "for x=1:10\n    éé=3\n        1∉x "
    @test position(s) == 30
    LineEdit.edit_move_left(s)
    @test position(s) == 29
    LineEdit.edit_backspace(s, true, true)
    @test content(s) == "for x=1:10\n    éé=3\n    1∉x "
    @test position(s) == 26
    LineEdit.edit_tab(s, false) # same as edit_tab(s, true) here
    @test position(s) == 30
    move_left(s, 6)
    @test position(s) == 24
    LineEdit.edit_backspace(s, true, true)
    @test content(s) == "for x=1:10\n    éé=3\n    1∉x "
    @test position(s) == 22
    LineEdit.edit_kill_line(s)
    edit_insert(s, ' '^10)
    move_left(s, 7)
    @test content(s) == "for x=1:10\n    éé=3\n          "
    @test position(s) == 25
    LineEdit.edit_tab(s, true, false)
    @test position(s) == 32
    move_left(s, 7)
    LineEdit.edit_tab(s, true, true)
    @test position(s) == 26
    @test content(s) == "for x=1:10\n    éé=3\n    "
    # test again the same, when there is a next line
    edit_insert(s, "      \nend")
    move_left(s, 11)
    @test position(s) == 25
    LineEdit.edit_tab(s, true, false)
    @test position(s) == 32
    move_left(s, 7)
    LineEdit.edit_tab(s, true, true)
    @test position(s) == 26
    @test content(s) == "for x=1:10\n    éé=3\n    \nend"
end

@testset "newline alignment feature" begin
    s = new_state()
    edit_insert(s, "for x=1:10\n    é = 1")
    LineEdit.edit_insert_newline(s)
    @test content(s) == "for x=1:10\n    é = 1\n    "
    edit_insert(s, " b = 2")
    LineEdit.edit_insert_newline(s)
    @test content(s) == "for x=1:10\n    é = 1\n     b = 2\n     "
    # after an empty line, should still insert the expected number of spaces
    LineEdit.edit_insert_newline(s)
    @test content(s) == "for x=1:10\n    é = 1\n     b = 2\n     \n     "
    LineEdit.edit_insert_newline(s, 0)
    @test content(s) == "for x=1:10\n    é = 1\n     b = 2\n     \n     \n"
    LineEdit.edit_insert_newline(s, 2)
    @test content(s) == "for x=1:10\n    é = 1\n     b = 2\n     \n     \n\n  "
    # test when point before first letter of the line
    for i=6:10
        LineEdit.edit_clear(s)
        edit_insert(s, "begin\n    x")
        seek(LineEdit.buffer(s), i)
        LineEdit.edit_insert_newline(s)
        @test content(s) == "begin\n" * ' '^(i-6) * "\n    x"
    end
end

@testset "change case on the right" begin
    local buf = IOBuffer()
    edit_insert(buf, "aa bB CC")
    seekstart(buf)
    @inferred LineEdit.edit_upper_case(buf)
    @inferred LineEdit.edit_title_case(buf)
    @test String(take!(copy(buf))) == "AA Bb CC"
    @test position(buf) == 5
    @inferred LineEdit.edit_lower_case(buf)
    @test String(take!(copy(buf))) == "AA Bb cc"
end

@testset "kill ring" begin
    local buf
    s = new_state()
    buf = buffer(s)
    edit_insert(s, "ça ≡ nothing")
    @test @inferred transform!(LineEdit.edit_copy_region, s) == ("ça ≡ nothing", 12, 0)
    @test s.kill_ring[end] == "ça ≡ nothing"
    @test @inferred transform!(LineEdit.edit_exchange_point_and_mark, s)[2:3] == (0, 12)
    charseek(buf, 8); setmark(s)
    charseek(buf, 1)
    @test @inferred transform!(LineEdit.edit_kill_region, s) == ("çhing", 1, 1)
    @test s.kill_ring[end] == "a ≡ not"
    charseek(buf, 0)
    @test @inferred transform!(LineEdit.edit_yank, s) == ("a ≡ notçhing", 7, 0)
    s.last_action = :unknown
    # next action will fail, as yank-pop doesn't know a yank was just issued
    @test @inferred transform!(LineEdit.edit_yank_pop, s) == ("a ≡ notçhing", 7, 0)
    s.last_action = :edit_yank
    # now this should work:
    @test transform!(LineEdit.edit_yank_pop, s) == ("ça ≡ nothingçhing", 12, 0)
    @test s.kill_idx == 1
    LineEdit.edit_kill_line(s)
    @test s.kill_ring[end] == "çhing"
    @test s.kill_idx == 3
    # check that edit_yank_pop works when passing require_previous_yank=false (#23635)
    s.last_action = :unknown
    @test transform!(s->LineEdit.edit_yank_pop(s, false), s) == ("ça ≡ nothinga ≡ not", 19, 12)

    # repetition (concatenation of killed strings
    edit_insert(s, "A B  C")
    LineEdit.edit_delete_prev_word(s)
    s.key_repeats = 1
    LineEdit.edit_delete_prev_word(s)
    s.key_repeats = 0
    @test s.kill_ring[end] == "B  C"
    LineEdit.edit_yank(s)
    LineEdit.edit_werase(s)
    @test s.kill_ring[end] == "C"
    s.key_repeats = 1
    LineEdit.edit_werase(s)
    s.key_repeats = 0
    @test s.kill_ring[end] == "B  C"
    LineEdit.edit_yank(s)
    LineEdit.edit_move_word_left(s)
    LineEdit.edit_move_word_left(s)
    LineEdit.edit_delete_next_word(s)
    @test s.kill_ring[end] == "B"
    s.key_repeats = 1
    LineEdit.edit_delete_next_word(s)
    s.key_repeats = 0
    @test s.kill_ring[end] == "B  C"

    # edit_kill_line_backwards
    LineEdit.edit_clear(s)
    edit_insert(s, "begin\n  a=1\n  b=2")
    LineEdit.edit_kill_line_backwards(s)
    @test s.kill_ring[end] == "  b=2"
    s.key_repeats = 1
    LineEdit.edit_kill_line_backwards(s)
    @test s.kill_ring[end] == "\n  b=2"
    LineEdit.edit_kill_line_backwards(s)
    @test s.kill_ring[end] == "  a=1\n  b=2"
    s.key_repeats = 0
end

@testset "undo" begin
    s = new_state()
    edit!(f) = transform!(f, s)[1]
    edit_undo! = LineEdit.edit_undo!
    edit_redo! = LineEdit.edit_redo!

    edit_insert(s, "one two three")

    @test @inferred edit!(LineEdit.edit_delete_prev_word) == "one two "
    @test @inferred edit!(edit_undo!) == "one two three"
    @test edit!(edit_redo!) == "one two "
    @test edit!(edit_undo!) == "one two three"

    edit_insert(s, " four")
    @test @inferred edit!(s->edit_insert(s, " five")) == "one two three four five"
    @test edit!(edit_undo!) == "one two three four"
    @test edit!(edit_undo!) == "one two three"
    @test edit!(edit_redo!) == "one two three four"
    @test edit!(edit_redo!) == "one two three four five"
    @test edit!(edit_undo!) == "one two three four"
    @test edit!(edit_undo!) == "one two three"

    @test @inferred edit!(LineEdit.edit_clear) == ""
    @test edit!(LineEdit.edit_clear) == "" # should not be saved twice
    @test edit!(edit_undo!) == "one two three"

    @test @inferred edit!(LineEdit.edit_insert_newline) == "one two three\n"
    @test edit!(edit_undo!) == "one two three"

    LineEdit.edit_move_left(s)
    LineEdit.edit_move_left(s)
    @test @inferred edit!(LineEdit.edit_transpose_chars) == "one two there"
    @test edit!(edit_undo!) == "one two three"
    @test edit!(LineEdit.edit_transpose_words) == "one three two"
    @test edit!(edit_undo!) == "one two three"

    LineEdit.move_line_start(s)
    @test edit!(LineEdit.edit_kill_line) == ""
    @test edit!(edit_undo!) == "one two three"
    # undo stack not updated if killing nothing:
    LineEdit.move_line_start(s)
    LineEdit.edit_kill_line(s)
    LineEdit.edit_kill_line(s) # no effect
    @test edit!(edit_undo!) == "one two three"

    LineEdit.move_line_end(s)
    @test edit!(LineEdit.edit_kill_line_backwards) == ""
    @test edit!(edit_undo!) == "one two three"

    LineEdit.move_line_start(s)
    LineEdit.edit_kill_line(s)
    LineEdit.edit_yank(s)
    @test edit!(LineEdit.edit_yank) == "one two threeone two three"
    @test edit!(edit_undo!) == "one two three"
    @test edit!(edit_undo!) == ""
    @test edit!(edit_undo!) == "one two three"

    LineEdit.setmark(s)
    LineEdit.edit_move_word_right(s)
    @test edit!(LineEdit.edit_kill_region) == " two three"
    @test edit!(LineEdit.edit_yank) == "one two three"
    @test edit!(LineEdit.edit_yank_pop) == "one two three two three"
    @test edit!(edit_undo!) == "one two three"
    @test edit!(edit_undo!) == " two three"
    @test edit!(edit_undo!) == "one two three"

    LineEdit.move_line_end(s)
    LineEdit.edit_backspace(s, false, false)
    LineEdit.edit_backspace(s, false, false)
    @test edit!(s->LineEdit.edit_backspace(s, false, false)) == "one two th"
    @test edit!(edit_undo!) == "one two thr"
    @test edit!(edit_undo!) == "one two thre"
    @test edit!(edit_undo!) == "one two three"

    LineEdit.push_undo(s) # TODO: incorporate push_undo into edit_splice! ?
    LineEdit.edit_splice!(s, 4 => 7, "stott")
    @test content(s) == "one stott three"
    s.last_action = :not_undo
    @test edit!(edit_undo!) == "one two three"

    LineEdit.edit_move_left(s)
    LineEdit.edit_move_left(s)
    LineEdit.edit_move_left(s)
    @test edit!(LineEdit.edit_delete) == "one two thee"
    @test edit!(edit_undo!) == "one two three"

    LineEdit.edit_move_word_left(s)
    LineEdit.edit_werase(s)
    @test edit!(LineEdit.edit_delete_next_word) == "one "
    @test edit!(edit_undo!) == "one three"
    @test edit!(edit_undo!) == "one two three"
    @test edit!(edit_redo!) == "one three"
    @test edit!(edit_redo!) == "one "
    @test edit!(edit_redo!) == "one " # nothing more to redo (this "beeps")
    @test edit!(edit_undo!) == "one three"
    @test edit!(edit_undo!) == "one two three"

    LineEdit.move_line_start(s)
    @test edit!(LineEdit.edit_upper_case) == "ONE two three"
    LineEdit.move_line_start(s)
    @test edit!(LineEdit.edit_lower_case) == "one two three"
    @test edit!(LineEdit.edit_title_case) == "one Two three"
    @test edit!(edit_undo!) == "one two three"
    @test edit!(edit_undo!) == "ONE two three"
    @test edit!(edit_undo!) == "one two three"

    LineEdit.move_line_end(s)
    edit_insert(s, "  ")
    @test edit!(LineEdit.edit_tab) == "one two three   "
    @test edit!(edit_undo!) == "one two three  "
    @test edit!(edit_undo!) == "one two three"
    LineEdit.move_line_start(s)
    edit_insert(s, "  ")
    LineEdit.move_line_start(s)
    @test edit!(s->LineEdit.edit_tab(s, true, true)) == "  one two three" # tab moves cursor to position 2
    @test edit!(edit_undo!) == "one two three" # undo didn't record cursor movement
    # TODO: add tests for complete_line, which don't work directly

    # pop initial insert of "one two three"
    @test edit!(edit_undo!) == ""
    @test edit!(edit_undo!) == "" # nothing more to undo (this "beeps")
end

@testset "edit_indent_{left,right}" begin
    local buf = IOBuffer()
    write(buf, "1\n22\n333")
    seek(buf, 0)
    @test @inferred LineEdit.edit_indent(buf, -1, false) == false
    @test @inferred transform!(buf->LineEdit.edit_indent(buf, -1, false), buf) == ("1\n22\n333", 0, 0)
    @test transform!(buf->LineEdit.edit_indent(buf, +1, false), buf) == (" 1\n22\n333", 1, 0)
    @test transform!(buf->LineEdit.edit_indent(buf, +2, false), buf) == ("   1\n22\n333", 3, 0)
    @test transform!(buf->LineEdit.edit_indent(buf, -2, false), buf) == (" 1\n22\n333", 1, 0)
    seek(buf, 0) # if the cursor is already on the left column, it stays there
    @test transform!(buf->LineEdit.edit_indent(buf, -2, false), buf) == ("1\n22\n333", 0, 0)
    seek(buf, 3) # between the two "2"
    @test transform!(buf->LineEdit.edit_indent(buf, +3, false), buf) == ("1\n   22\n333", 6, 0)
    @test transform!(buf->LineEdit.edit_indent(buf, -9, false), buf) == ("1\n22\n333", 3, 0)
    seekend(buf) # position 8
    @test transform!(buf->LineEdit.edit_indent(buf, +3, false), buf) == ("1\n22\n   333", 11, 0)
    @test transform!(buf->LineEdit.edit_indent(buf, -1, false), buf) == ("1\n22\n  333", 10, 0)
    @test transform!(buf->LineEdit.edit_indent(buf, -2, false), buf) == ("1\n22\n333", 8, 0)
    @test transform!(buf->LineEdit.edit_indent(buf, -1, false), buf) == ("1\n22\n333", 8, 0)
    @test transform!(buf->LineEdit.edit_indent(buf, +3, false), buf) == ("1\n22\n   333", 11, 0)
    seek(buf, 5) # left column
    @test transform!(buf->LineEdit.edit_indent(buf, -2, false), buf) == ("1\n22\n 333", 5, 0)
    # multiline tests
    @test transform!(buf->LineEdit.edit_indent(buf, -2, true), buf) == ("1\n22\n 333", 5, 0)
    @test transform!(buf->LineEdit.edit_indent(buf, +2, true), buf) == ("  1\n  22\n   333", 11, 0)
    @test transform!(buf->LineEdit.edit_indent(buf, -1, true), buf) == (" 1\n 22\n  333", 8, 0)
    REPL.LineEdit.edit_exchange_point_and_mark(buf)
    seek(buf, 5)
    @test transform!(buf->LineEdit.edit_indent(buf, -1, true), buf) == (" 1\n22\n 333", 4, 6)

    # check that if the mark at the beginning of the line, it is moved when right-indenting,
    # which is more natural when the region is active
    seek(buf, 0)
    buf.mark = 0
#    @test transform!(buf->LineEdit.edit_indent(buf, +1, false), buf) == ("  1\n22\n 333", 1, 1)
end

@testset "edit_transpose_lines_{up,down}!" begin
    transpose_lines_up!(buf) = @inferred LineEdit.edit_transpose_lines_up!(buf, position(buf)=>position(buf))
    transpose_lines_up_reg!(buf) = @inferred LineEdit.edit_transpose_lines_up!(buf, region(buf))
    transpose_lines_down!(buf) = @inferred LineEdit.edit_transpose_lines_down!(buf, position(buf)=>position(buf))
    transpose_lines_down_reg!(buf) = @inferred LineEdit.edit_transpose_lines_down!(buf, region(buf))

    local buf
    buf = IOBuffer()

    write(buf, "l1\nl2\nl3")
    seek(buf, 0)
    @test transpose_lines_up!(buf) == false
    @test transform!(transpose_lines_up!, buf) == ("l1\nl2\nl3", 0, 0)
    @test transform!(transpose_lines_down!, buf) == ("l2\nl1\nl3", 3, 0)
    @test transpose_lines_down!(buf) == true
    @test String(take!(copy(buf))) == "l2\nl3\nl1"
    @test transpose_lines_down!(buf) == false
    @test String(take!(copy(buf))) == "l2\nl3\nl1" # no change
    LineEdit.edit_move_right(buf)
    @test transform!(transpose_lines_up!, buf) == ("l2\nl1\nl3", 4, 0)
    LineEdit.edit_move_right(buf)
    @test transform!(transpose_lines_up!, buf) == ("l1\nl2\nl3", 2, 0)

    # multiline
    @test transpose_lines_up_reg!(buf) == false
    @test transform!(transpose_lines_down_reg!, buf) == ("l2\nl1\nl3", 5, 0)
    REPL.LineEdit.edit_exchange_point_and_mark(buf)
    seek(buf, 1)
    @test transpose_lines_up_reg!(buf) == false
    @test transform!(transpose_lines_down_reg!, buf) == ("l3\nl2\nl1", 4, 8)

    # check that if the mark is at the beginning of the line, it is moved when transposing down,
    # which is necessary when the region is active: otherwise, the line which is moved up becomes
    # included in the region
    buf.mark = 0
    seek(buf, 1)
    @test transform!(transpose_lines_down_reg!, buf) == ("l2\nl3\nl1", 4, 3)
end

@testset "edit_insert_last_word" begin
    get_last_word(str::String) = @inferred LineEdit.get_last_word(IOBuffer(str))
    @test get_last_word("1+2") == "2"
    @test get_last_word("1+23") == "23"
    @test get_last_word("1+2") == "2"
    @test get_last_word(""" "a" * "b" """) == "b"
    @test get_last_word(""" "a" * 'b' """) == "b"
    @test get_last_word(""" "a" * `b` """) == "b"
    @test get_last_word("g()") == "g()"
    @test get_last_word("g(1, 2)") == "2"
    @test get_last_word("g(1, f())") == "f"
    @test get_last_word("a[1]") == "1"
    @test get_last_word("a[b[]]") == "b"
    @test get_last_word("a[]") == "a[]"
end
