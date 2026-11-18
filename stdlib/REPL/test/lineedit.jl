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

# Run `keys` through the real input loop of a prompt. The final `\r` ends the prompt.
function press(keys; prompt=LineEdit.Prompt("test> "))
    term = FakeTerminal(IOBuffer(keys * "\r"), IOBuffer(), IOBuffer(), false)
    s = LineEdit.init_state(term, LineEdit.ModalInterface(LineEdit.TextInterface[prompt]))
    LineEdit.prompt!(term, s.interface, s)
    return s
end

# Normal and prefix history search prompts emit one marker pair without changing their width.
@testset "semantic prompt rendering" begin
    for markers in (REPL.OSC_133_MARKERS, REPL.OSC_633_MARKERS),
            enabled in (false, true), color in (false, true)
        output = IOBuffer()
        term = FakeTerminal(IOBuffer(), output, IOBuffer(), color)
        repl = REPL.LineEditREPL(term, color)
        repl.semantic_prompt_markers = markers
        repl.options.semantic_prompts = enabled
        prompt = LineEdit.Prompt("julia> "; repl)
        prefix_prompt = LineEdit.PrefixHistoryPrompt(prompt.hist, prompt)
        rendered = color ? Base.text_colors[:bold] * "julia> " * Base.text_colors[:normal] : "julia> "
        expected = enabled ? markers.prompt_start * rendered * markers.prompt_end : rendered
        for state in (prompt, LineEdit.init_state(term, prompt), LineEdit.init_state(term, prefix_prompt))
            @test LineEdit.write_prompt(term, state, color) == 7
            @test String(take!(output)) == expected
        end
        prompt.repl = nothing
        @test LineEdit.write_prompt(term, prompt, color) == 7
        @test String(take!(output)) == rendered
    end
end

# History search initializes prompt modes added after MIState creation (#61584).
module HistorySearchDynamicMode

using Test
using REPL
import REPL.LineEdit
import ..FakeTerminals: FakeTerminal

struct MockHistoryFile end

mutable struct MockHistoryProvider <: LineEdit.HistoryProvider
    history::MockHistoryFile
    last_buffer::IOBuffer
    last_mode::Union{Nothing,LineEdit.Prompt}
    mode_mapping::Dict{Symbol,LineEdit.Prompt}
end

REPL.histsearch(::MockHistoryFile, args...) = (mode = :pkg, text = "status")
LineEdit.add_history(::MockHistoryProvider, s) = nothing

@testset "history search initializes dynamic modes" begin
    term = FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())
    main_mode = LineEdit.Prompt("julia> ")
    dummy_pkg_mode = LineEdit.Prompt("pkg> ")
    history = MockHistoryProvider(MockHistoryFile(), IOBuffer(), nothing,
                                  Dict(:julia => main_mode, :pkg => dummy_pkg_mode))
    main_mode.hist = dummy_pkg_mode.hist = history
    interface = LineEdit.ModalInterface(LineEdit.TextInterface[main_mode, dummy_pkg_mode])
    mistate = LineEdit.init_state(term, interface)
    mistate.terminal_properties.da1 = Int[]

    pkg_mode = LineEdit.Prompt("(project) pkg> ")
    pkg_mode.hist = history
    history.mode_mapping[:pkg] = pkg_mode
    push!(interface.modes, pkg_mode)
    @test !haskey(mistate.mode_state, pkg_mode)

    LineEdit.history_search(mistate)
    @test mistate.current_mode === pkg_mode
    @test String(take!(copy(LineEdit.buffer(mistate)))) == "status"

    # Both search keys start the search from the keymap and leave the result in the buffer.
    prompt_keymap = LineEdit.keymap([LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults])
    for key in ("\x12", "\x13")
        main_mode = LineEdit.Prompt("julia> "; keymap_dict=prompt_keymap)
        pkg_mode = LineEdit.Prompt("pkg> "; keymap_dict=prompt_keymap)
        main_mode.hist = pkg_mode.hist = MockHistoryProvider(MockHistoryFile(), IOBuffer(), nothing,
            Dict(:julia => main_mode, :pkg => pkg_mode))
        term = FakeTerminal(IOBuffer(key * "\r"), IOBuffer(), IOBuffer(), false)
        mistate = LineEdit.init_state(term, LineEdit.ModalInterface(LineEdit.TextInterface[main_mode, pkg_mode]))
        mistate.terminal_properties.da1 = Int[]
        LineEdit.prompt!(term, mistate.interface, mistate)
        @test mistate.current_mode === pkg_mode
        @test LineEdit.input_string(LineEdit.state(mistate)) == "status"
    end
end

end # module HistorySearchDynamicMode

charseek(buf, i) = seek(buf, nextind(content(buf), 0, i+1)-1)
charpos(buf, pos=position(buf)) = length(content(buf), 1, pos)

function transform!(f, s, i = -1) # i is char-based (not bytes) buffer position
    buf = buffer(s)
    i >= 0 && charseek(buf, i)
    # simulate what happens in LineEdit.set_action!
    s isa LineEdit.MIState && (s.current_action = :unknown)
    status = f(s)
    if s isa LineEdit.MIState && status !== :ignore
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

const foo_keymap = Dict{Char,Any}(
    'a' => (o...)->(global a_foo; a_foo += 1)
)

b_foo = 0

const foo2_keymap = Dict{Char,Any}(
    'b' => (o...)->(global b_foo; b_foo += 1)
)

a_bar = 0
b_bar = 0

const bar_keymap = Dict{Char,Any}(
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
const test_keymap_1 = Dict{Any,Any}(
    "^C" => (o...)->1,
    "\\C-C" => (o...)->2
)

@test_throws ErrorException LineEdit.keymap([test_keymap_1])

a_foo = a_bar = 0

const test_keymap_2 = Dict{Any,Any}(
    "abc" => (o...)->(global a_foo = 1)
)

const test_keymap_3 = Dict{Any,Any}(
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

const test_keymap_4 = Dict{Any,Any}(
    "a" => (o...)->(global a_foo = 1),
    "b" => "a",
    "c" => (o...)->(global a_foo = 2),
)

const test_keymap_5 = Dict{Any,Any}(
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

const test_cycle = Dict{Any,Any}(
    "a" => "b",
    "b" => "a"
)

@test_throws ErrorException LineEdit.keymap([test_cycle])

# Lazy redirection with Cycles

const level1 = Dict{Any,Any}(
    "a" => LineEdit.KeyAlias("b")
)

const level2a = Dict{Any,Any}(
    "b" => "a"
)

const level2b = Dict{Any,Any}(
    "b" => LineEdit.KeyAlias("a")
)

@test_throws ErrorException LineEdit.keymap([level2a,level1])
@test_throws ErrorException LineEdit.keymap([level2b,level1])

# Lazy redirection functionality test

a_foo = 0

const test_keymap_6 = Dict{Any,Any}(
    "a" => (o...)->(global a_foo = 1),
    "b" => LineEdit.KeyAlias("a"),
    "c" => (o...)->(global a_foo = 2),
)

const test_keymap_7 = Dict{Any,Any}(
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

const test_keymap_8 = Dict{Any,Any}(
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

const test_keymap_9 = Dict{Any,Any}(
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

buf = IOBuffer(Vector{UInt8}("type X\n "), read=true, write=true)
seekend(buf)
@test !isempty(@inferred(LineEdit.edit_delete_prev_word(buf)))
@test position(buf) == 5
@test buf.size == 5
@test content(buf) == "type "

buf = IOBuffer(Vector{UInt8}("4 +aaa+ x"), read=true, write=true)
seek(buf,8)
@test !isempty(LineEdit.edit_delete_prev_word(buf))
@test position(buf) == 3
@test buf.size == 4
@test content(buf) == "4 +x"

buf = IOBuffer(Vector{UInt8}("x = func(arg1,arg2 , arg3)"), read=true, write=true)
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


    # Transposing a one-char buffer should behave like Emacs
    seek(buf, 0)
    @inferred(LineEdit.edit_clear(buf))
    edit_insert(buf, "a")
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "a"
    seekend(buf)
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == "a"
    @test position(buf) == 0

    # Transposing an empty buffer shouldn't implode
    seek(buf, 0)
    LineEdit.edit_clear(buf)
    LineEdit.edit_transpose_chars(buf)
    @test content(buf) == ""
    @test position(buf) == 0
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
withenv("COLUMNS"=>"80") do
    buf = IOBuffer(
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

    # issue #61377
    for edit_move in [LineEdit.edit_insert_newline, LineEdit.edit_backspace,
            LineEdit.edit_move_left, LineEdit.edit_move_right,
            LineEdit.edit_move_word_left, LineEdit.edit_move_word_right,
            LineEdit.edit_move_up, LineEdit.edit_move_down]
        s = new_state()
        edit_insert(s, "abcd\nefgh\nijkl")
        s.current_action = :unknown
        LineEdit.edit_move_up(s)
        s.current_action = :unknown
        LineEdit.edit_shift_move(s, LineEdit.edit_move_left)
        s.current_action = :unknown
        LineEdit.edit_shift_move(s, LineEdit.edit_move_left)
        s.current_action = :unknown
        edit_move(s)
        @test !LineEdit.is_region_active(s)
    end
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

    # repetition (concatenation of killed strings)
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

@testset "show_completions" begin
    term = FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())

    function getcompletion(completions)
        promptstate = REPL.LineEdit.init_state(term, REPL.LineEdit.mode(new_state()))
        REPL.LineEdit.show_completions(promptstate, completions)
        return String(take!(term.out_stream))
    end

    # When the number of completions is less than
    # LineEdit.MULTICOLUMN_THRESHOLD, they should be in a single column.
    strings = ["abcdef", "123456", "ijklmn"]
    @assert length(strings) < LineEdit.MULTICOLUMN_THRESHOLD
    @test getcompletion(strings) == "\033[0B\n\rabcdef\n\r123456\n\rijklmn\n"

    # But with more than the threshold there should be multiple columns
    strings2 = repeat(["foo"], LineEdit.MULTICOLUMN_THRESHOLD + 1)
    @test getcompletion(strings2) == "\033[0B\n\rfoo\r\e[5Cfoo\n\rfoo\r\e[5Cfoo\n\rfoo\r\e[5Cfoo\n"

    # Check that newlines in completions are handled correctly (issue #45836)
    strings3 = ["abcdef", "123456\nijklmn"]
    @test getcompletion(strings3) == "\033[0B\nabcdef\n123456\nijklmn\n"

    # Internal names come after the public ones, under a header and in their own columns.
    # Lists without internal names get no header, as checked above.
    named = [LineEdit.NamedCompletion("b", "b", true), LineEdit.NamedCompletion("a"),
             LineEdit.NamedCompletion("long_internal", "long_internal", true)]
    @test getcompletion(named) == "\033[0B\n\ra\n\rInternal:\n\rb\n\rlong_internal\n"
    named2 = vcat(map(LineEdit.NamedCompletion, strings2),
                  [LineEdit.NamedCompletion("internal", "internal", true) for _ in strings2])
    @test getcompletion(named2) ==
        "\033[0B\n" * "\rfoo\r\e[5Cfoo\n" ^ 3 * "\rInternal:\n" * "\rinternal\r\e[10Cinternal\n" ^ 3
    @test getcompletion([LineEdit.NamedCompletion("x", "x", true)]) == "\033[0B\n\rInternal:\n\rx\n"
end

# Each binding of the default keymap, with `|` typed at the end to show where the cursor is.
@testset "default keymap" begin
    left, right, up, down = "\e[D", "\e[C", "\e[A", "\e[B"
    newline = "\e\r"
    for (keys, expected) in Pair{String,String}[
            # cursor movement
            "abc$left|" => "ab|c",
            "abc$left$left$right|" => "ab|c",
            "abc\x02|" => "ab|c", # ^B
            "abc\x02\x02\x06|" => "ab|c", # ^F
            "ab\e[H|" => "|ab",
            "ab\e[H\e[F|" => "ab|",
            "ab cd\x01\x05|" => "ab cd|", # ^A, ^E
            "a$(newline)bc$up|" => "a|\nbc",
            "a$(newline)bc$up$down|" => "a\nb|c",
            "a$(newline)bc\x10|" => "a|\nbc", # ^P
            "a$(newline)bc\x10\x0e|" => "a\nb|c", # ^N
            "ab cd\eb|" => "ab |cd",
            "ab cd\eb\ef|" => "ab cd|",
            "ab cd\e[1;5D|" => "ab |cd",
            "ab cd\e[1;5D\e[1;5C|" => "ab cd|",
            "ab cd\eOd|" => "ab |cd",
            "ab cd\eOd\eOc|" => "ab cd|",
            "ab\x01\eb|" => "|ab",
            "ab\ef|" => "ab|",
            # moving past the first or last line asks the history, and this prompt has none
            "ab$up$down\x10\x0e|" => "ab|",
            # deleting and killing
            "ab cd\e\b" => "ab ",
            "ab cd\e\x7f" => "ab ",
            "ab\e[H\x04" => "b", # ^D on a non-empty buffer
            "ab\e[H\e[3~" => "b",
            "ab cd\x17" => "ab ", # ^W
            "ab cd\x01\ed" => " cd",
            "ab cd\x02\x02\x0b" => "ab ", # ^K
            "ab cd\x02\x02\x15" => "cd", # ^U
            "ab cd\x02\x02\x0b\x01\x19" => "cdab ", # ^Y
            "ab cd\x17\x19\x19" => "ab cdcd",
            "ab\x17 cd\x17\x19\ey" => " ab", # alt-y cycles through the kill ring
            "ab\x19" => "ab", # the kill ring is empty
            # the region: ^space twice activates it
            "abc\0\x02\x02\x7f" => "bc",
            "abc\0\0\x02\x02\x7f" => "a",
            "abc\0\0\x02\x02\x07\x7f" => "bc", # ^G cancels it
            "ab\0\x01\x18\x18|" => "ab|", # ^X^X
            "abc\0\x02\ew\x05\x19" => "abcc",
            "abc\0\x02\eW|" => "ab|",
            "abc\e[1;2D\e[1;2D\x7f" => "a",
            "abc\x01\e[1;2C\x7f" => "bc",
            "a$(newline)b\e[1;2A\x7f" => "a",
            "a$(newline)b\x01\x10\e[1;2B\x7f" => "b",
            # moving and indenting lines
            "l1$(newline)l2\e[1;3A" => "l2\nl1",
            "l1$(newline)l2\e[1;3A\e[1;3B" => "l1\nl2",
            "l1$(newline)l2\e[1;3B" => "l1\nl2",
            "l1$(newline)l2\e[A\e[1;3A" => "l1\nl2",
            "l1$(newline)l2$(newline)l3\0\0\x02\x02\x10\e[1;3A" => "l2\nl3\nl1", # the whole region moves
            "l1$(newline)l2$(newline)l3\e[A\0\0\x02\x02\x10\e[1;3B" => "l3\nl1\nl2",
            "ab\e[1;3C" => " ab",
            "    ab\e[1;3D" => "   ab",
            "ab\e[1;3D" => "ab",
            # other edits
            "ab\x14" => "ba", # ^T
            "ab cd\x01\et" => "cd ab",
            "abc\x01\eu" => "ABC",
            "ABC\x01\el" => "abc",
            "abc\x01\ec" => "Abc",
            "\e[200~ a\r  b\e[201~" => "a\n b",
            # alt-. needs a history, and this prompt has none
            "ab\e." => "ab",
            "\x0c" => "",
            "a\x0cb" => "ab",
            "a\x03b" => "b",
        ]
        @test (keys => content(press(keys))) == (keys => expected)
    end

    term_output(s) = String(take!(LineEdit.terminal(s).out_stream))
    @test occursin("\e[H\e[2J", term_output(press("\x0c")))
    @test occursin("^C\n\n", term_output(press("a\x03")))
    @test occursin("press ^C again to cancel all in-flight work", term_output(press("\x03\x03")))

    # ^Z asks the caller to suspend and keeps the input.
    if Sys.isunix()
        term = FakeTerminal(IOBuffer("ab\x1a"), IOBuffer(), IOBuffer(), false)
        s = LineEdit.init_state(term, LineEdit.ModalInterface(LineEdit.TextInterface[LineEdit.Prompt("test> ")]))
        buf, ok, suspend = LineEdit.prompt!(term, s.interface, s)
        @test ok && suspend
        @test content(buf) == "ab"
    end

    # ^D on an empty prompt asks for confirmation on the terminal when that is enabled.
    let repl = REPL.LineEditREPL(FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer(), false), false)
        repl.options = REPL.Options(confirm_exit=true)
        prompt = LineEdit.Prompt("test> "; repl)
        term = FakeTerminal(IOBuffer("\x04\x04"), IOBuffer(), IOBuffer(), false)
        s = LineEdit.init_state(term, LineEdit.ModalInterface(LineEdit.TextInterface[prompt]))
        buf, ok, suspend = LineEdit.prompt!(term, s.interface, s)
        @test !ok && s.aborted
        @test occursin("Type ^D again to exit.", term_output(s))
    end

    # A key that changes nothing must not leave a step in the undo history.
    for noop in ("\x7f", "\e\b", "\x17", "\x15", "\e[1;3D", "\eW")
        @test content(press("ab\x01$noop\x1f")) == "a"
    end
    for noop in ("\x04", "\e[3~", "\ed", "\x0b")
        @test content(press("ab$noop\x1f")) == "a"
    end
    @test content(press("ab\x1f")) == "a"
    @test content(press("abc\x1f\x1f\e_")) == "ab"
    @test content(press("abc\x1f\x1f\e_\e_\e_")) == "abc" # redo stops at the newest edit
    @test content(press("ab\x1fc\e_")) == "ac" # redo needs an undo right before it
    @test content(press("\x1f")) == ""
end

@testset "show" begin
    prompt = LineEdit.Prompt(() -> "test> ")
    @test occursin("test> ", repr(prompt))
    s = LineEdit.init_state(FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer()), LineEdit.ModalInterface([prompt]))
    @test repr(s) == "MI State ($(repr(prompt)) active)"
end

struct StaticCompletions <: LineEdit.CompletionProvider
    completions::Any
    should_complete::Bool
end
# Hints run on another task and would race with the key presses.
function LineEdit.complete_line(c::StaticCompletions, s; hint::Bool=false)
    hint && return LineEdit.NamedCompletion[], "", false
    word = last(split("x " * LineEdit.input_string(s)[1:position(s)], ' '))
    return c.completions, String(word), c.should_complete
end

@testset "completion keys" begin
    function complete(keys, completions; should_complete=true)
        s = press(keys; prompt=LineEdit.Prompt("test> "; complete=StaticCompletions(completions, should_complete)))
        return content(s), String(take!(LineEdit.terminal(s).out_stream))
    end
    listed(out) = occursin("\n\rfoo", out)

    # one choice replaces the word, and undo brings it back
    @test complete("f\t", ["foo"])[1] == "foo"
    @test complete("x f\t|", ["foo"])[1] == "x foo|"
    @test complete("f\t\x1f", ["foo"])[1] == "f"
    @test complete("f\e[Z", ["foo"])[1] == "foo"

    # several choices complete their common prefix, then list on the next press
    named = LineEdit.NamedCompletion.(["foo1", "foo2"])
    for completions in (["foo1", "foo2"], named)
        text, out = complete("f\t", completions)
        @test text == "foo"
        @test !listed(out)
        text, out = complete("f\t\t", completions)
        @test text == "foo"
        @test listed(out)
    end
    @test complete("f\t", ["αβγ", "αβδ"])[1] == "αβ"
    @test complete("f\t", ["αβ", "αβγ"])[1] == "αβ"
    @test complete("f\t", ["", "foo"])[1] == "f"
    @test complete("f\t", ["abc", "xyz"])[1] == "f"

    # should_complete only lists, even for one choice
    text, out = complete("f\t", ["foo"]; should_complete=false)
    @test text == "f"
    @test occursin("foo", out)

    # no choice leaves the input alone
    @test complete("f\t", String[])[1] == "f"

    # right arrow at the end of the input accepts a single completion, in either return format
    @test complete("fo\e[C", ["foo"])[1] == "foo"
    @test complete("fo\e[C", LineEdit.NamedCompletion.(["foo"]))[1] == "foo"
    @test complete("fo\e[C", ["foo", "fob"])[1] == "fo"
    @test complete("f\e[C", ["foo"])[1] == "f"
    @test complete("fo\e[C", ["foo"]; should_complete=false)[1] == "fo"
    @test complete("fo\x01\e[C|", ["foo"])[1] == "f|o"
end

@testset "beep" begin
    term = FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer(), true)
    prompt = LineEdit.Prompt("test> "; prompt_prefix="\e[1m")
    s = LineEdit.init_state(term, prompt)
    # disabled when not interactive
    LineEdit.beep(s, 0.01, 0.001, 0.01; colors=["\e[41m"], use_current=true)
    @test s.beeping == 0.0
    @test isempty(take!(term.out_stream))

    old = isinteractive()
    @eval Base is_interactive = true
    try
        LineEdit.beep(s, 100.0, 0.001, 0.005; colors=["\e[41m"], use_current=true)
        @test s.beeping == 0.005 # capped
        yield() # let it start, so that the wait below sees it running
        LineEdit.cancel_beep(s)
    finally
        @eval Base is_interactive = $old
    end
    @test s.beeping <= 0.0
    @test prompt.prompt_prefix == "\e[1m"
    out = String(take!(term.out_stream))
    @test occursin("\e[41m", out)
    @test endswith(first(out, findlast('>', out)), "\e[0K\e[1m\e[1mtest>") # the last redraw restored the prefix
end

@testset "Hints at the terminal edge" begin
    # Hints must leave room for trailing brackets and keep the cursor on its original row.
    for color in (false, true),
        (prefix, trailing, visible) in ((" "^57, ")", true), (" "^58, ")", false),
                                        (" "^60, ")", false), (" "^80, ")", true),
                                        (" "^56, "))", true), (" "^57, "))", false),
                                        ("界"^29, ")", false), ("e\u0301"^58, ")", false),
                                        ("x\n" * " "^58, ")", false),
                                        (" "^58, "", false))
        output = IOBuffer()
        term = FakeTerminal(IOBuffer(), output, IOBuffer(), color)
        s = LineEdit.init_state(term, LineEdit.Prompt("julia> "))
        input = prefix * "f(reada" * trailing
        LineEdit.replace_line(s, input)
        cursor = sizeof(prefix * "f(reada")
        seek(buffer(s), cursor)
        s.hint = "vailable"
        LineEdit.refresh_line(s)
        rendered = String(take!(output))
        @test occursin("vailable", rendered) == visible
        @test occursin("\e[8@", rendered) == (visible && !isempty(trailing))
        @test occursin(Base.text_colors[:light_black], rendered) == (visible && color)
        @test content(s) == input
        @test position(buffer(s)) == cursor
    end
end

# Test bracket insertion functionality
@testset "Bracket insertion" begin
    # Test bracket insertion with a fake REPL that has bracket completion enabled
    term = FakeTerminal(IOBuffer(), IOBuffer(), IOBuffer())
    prompt = LineEdit.Prompt("test> ")

    # Build keymap with bracket insertion enabled (as it would be in practice)
    base_keymaps = Dict{Any,Any}[LineEdit.bracket_insert_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    prompt.keymap_dict = LineEdit.keymap(base_keymaps)

    interface = LineEdit.ModalInterface([prompt])
    s = LineEdit.init_state(term, interface)

    # Helper to write characters as stdin input
    write_input(s, str) = for c in str
        buf = IOBuffer(string(c))
        LineEdit.match_input(prompt.keymap_dict, s, buf)(s, buf)
    end

    # Test left bracket at EOF triggers auto-complete
    write_input(s, "(")
    @test content(s) == "()"
    @test position(buffer(s)) == 1

    # Test right bracket skips over matching bracket
    write_input(s, ")")
    @test content(s) == "()"
    @test position(buffer(s)) == 2

    # Test backspace removes both brackets
    s = LineEdit.init_state(term, interface)
    write_input(s, "(")
    write_input(s, "\b")
    @test content(s) == ""
    @test position(buffer(s)) == 0

    # Test quote insertion at EOF
    s = LineEdit.init_state(term, interface)
    write_input(s, "\"")
    @test content(s) == "\"\""
    @test position(buffer(s)) == 1

    # Test quote skip over
    write_input(s, "\"")
    @test content(s) == "\"\""
    @test position(buffer(s)) == 2

    # Test transpose detection - single quote after letter shouldn't auto-complete
    s = LineEdit.init_state(term, interface)
    write_input(s, "A")
    write_input(s, "'")
    @test content(s) == "A'"
    @test position(buffer(s)) == 2

    # Test single quote after space should auto-complete
    s = LineEdit.init_state(term, interface)
    write_input(s, " ")
    write_input(s, "'")
    @test content(s) == " ''"
    @test position(buffer(s)) == 2

    # Test bracket not inserted when next char is not whitespace
    s = LineEdit.init_state(term, interface)
    write_input(s, "x")
    charseek(buffer(s), 0)
    write_input(s, "(")
    @test content(s) == "(x"
    @test position(buffer(s)) == 1

    # Test all bracket types
    for (left, right) in (('[', ']'), ('{', '}'))
        s = LineEdit.init_state(term, interface)
        write_input(s, string(left))
        @test content(s) == string(left, right)
        @test position(buffer(s)) == 1
        write_input(s, string(right))
        @test position(buffer(s)) == 2
        write_input(s, "\b")
        @test content(s) == string(left)
        @test position(buffer(s)) == 1
        write_input(s, "\b")
        @test content(s) == ""
        @test position(buffer(s)) == 0
    end

    # Test all quote types
    for quote_char in ('`', '"', '\'')
        s = LineEdit.init_state(term, interface)
        write_input(s, string(quote_char))
        @test content(s) == string(quote_char, quote_char)
        @test position(buffer(s)) == 1
    end

    # Test nested brackets
    s = LineEdit.init_state(term, interface)
    write_input(s, "(")
    write_input(s, "[")
    @test content(s) == "([])"
    @test position(buffer(s)) == 2
    write_input(s, "]")
    @test position(buffer(s)) == 3
    write_input(s, ")")
    @test position(buffer(s)) == 4

    # Test backspace in middle of nested brackets
    s = LineEdit.init_state(term, interface)
    write_input(s, "(")
    write_input(s, "{")
    @test content(s) == "({})"
    @test position(buffer(s)) == 2
    write_input(s, "\b")
    @test content(s) == "()"
    @test position(buffer(s)) == 1

    # Test triple quotes don't auto-complete
    s = LineEdit.init_state(term, interface)
    write_input(s, "\"")
    @test content(s) == "\"\""
    @test position(buffer(s)) == 1
    write_input(s, "\"")
    @test content(s) == "\"\""
    @test position(buffer(s)) == 2
    write_input(s, "\"")
    @test content(s) == "\"\"\""
    @test position(buffer(s)) == 3

    # Test transpose detection for various cases
    s = LineEdit.init_state(term, interface)
    write_input(s, "x123")
    write_input(s, "'")
    @test content(s) == "x123'"
    @test position(buffer(s)) == 5

    s = LineEdit.init_state(term, interface)
    write_input(s, "arr]")
    write_input(s, "'")
    @test content(s) == "arr]'"
    @test position(buffer(s)) == 5

    # Test right bracket insert when not matching
    s = LineEdit.init_state(term, interface)
    write_input(s, ")")
    @test content(s) == ")"
    @test position(buffer(s)) == 1

    # Test backspace doesn't remove mismatched brackets
    s = LineEdit.init_state(term, interface)
    write_input(s, "(")
    write_input(s, "]")
    charseek(buffer(s), 1)
    write_input(s, "\b")
    @test content(s) == "])"
    @test position(buffer(s)) == 0

    # Test bracket insertion followed by whitespace
    s = LineEdit.init_state(term, interface)
    write_input(s, " ")
    charseek(buffer(s), 0)
    write_input(s, "(")
    @test content(s) == "() "
    @test position(buffer(s)) == 1

    # Test quote behavior: |foo" + " -> "foo" (not ""foo")
    s = LineEdit.init_state(term, interface)
    write_input(s, "foo\"")
    charseek(buffer(s), 0)
    write_input(s, "\"")
    @test content(s) == "\"foo\""
    @test position(buffer(s)) == 1

    # Test quote behavior: foo| + " -> foo" (not foo"")
    s = LineEdit.init_state(term, interface)
    write_input(s, "foo")
    write_input(s, "\"")
    @test content(s) == "foo\""
    @test position(buffer(s)) == 4

    # Test quote behavior: foo | + " -> foo ""
    s = LineEdit.init_state(term, interface)
    write_input(s, "foo ")
    write_input(s, "\"")
    @test content(s) == "foo \"\""
    @test position(buffer(s)) == 5

    # Test quote behavior: | foo + " -> "" foo (space before foo means double quotes)
    s = LineEdit.init_state(term, interface)
    write_input(s, " foo")
    charseek(buffer(s), 0)
    write_input(s, "\"")
    @test content(s) == "\"\" foo"
    @test position(buffer(s)) == 1

    # Test quote behavior:  | + " -> ""
    s = LineEdit.init_state(term, interface)
    write_input(s, " ")
    write_input(s, "\"")
    @test content(s) == " \"\""
    @test position(buffer(s)) == 2

    # Test quote behavior: (|) + " -> ("")
    s = LineEdit.init_state(term, interface)
    write_input(s, ")")
    charseek(buffer(s), 0)
    write_input(s, "(")
    # Buffer is now () with cursor at 1 (not paired since there was an unmatched `)`)
    @test content(s) == "()"
    write_input(s, "\"")
    @test content(s) == "(\"\")"
    @test position(buffer(s)) == 2

    # Test quote behavior: (|bar) + " -> ("bar)
    s = LineEdit.init_state(term, interface)
    write_input(s, "(bar)")
    charseek(buffer(s), 1)
    write_input(s, "\"")
    @test content(s) == "(\"bar)"
    @test position(buffer(s)) == 2

    # Brackets are not paired inside literals and comments: "|" + ( -> "(|"
    s = LineEdit.init_state(term, interface)
    write_input(s, "\"\"")
    charseek(buffer(s), 1)
    write_input(s, "(")
    @test content(s) == "\"(\""
    @test position(buffer(s)) == 2

    # Typing out code gives that code, also when literals contain brackets or quotes
    for str in ("split(s, \"(\")", "c = '('", "c = '['", "f(x, \"a (b\")", "\"a\\\"b\\\"\"",
                "# don't \"quote", "r\"[a-z]+\"", "x = \"\"\"a\"b\"\"\"", "`echo \"(\"`",
                "println(\"\$x[1]\")", "\"\$f(\$x)\"", "Regex(\"^\$v[a-z]\")",
                "print(\"\"\"x = \$(f(1))\"\"\")", "f(`a \$(g(1)) b`)")
        s = LineEdit.init_state(term, interface)
        write_input(s, str)
        @test content(s) == str
        @test position(buffer(s)) == sizeof(str)
    end

    # An escaped newline and its indentation are still inside the string literal.
    for prefix in ("\"abc\\\n", "\"abc\\\n    ")
        s = LineEdit.init_state(term, interface)
        LineEdit.replace_line(LineEdit.state(s), prefix)
        write_input(s, "\"")
        @test content(s) == prefix * "\""
        @test position(buffer(s)) == sizeof(prefix) + 1

        s = LineEdit.init_state(term, interface)
        LineEdit.replace_line(LineEdit.state(s), prefix * ")\"")
        seek(buffer(s), sizeof(prefix))
        write_input(s, ")")
        @test content(s) == prefix * "))\""
        @test position(buffer(s)) == sizeof(prefix) + 1
    end

    # Interpolation in a string is code
    s = LineEdit.init_state(term, interface)
    write_input(s, "\"\$(")
    @test content(s) == "\"\$()\""

    # Don't pair a bracket in front of a string, char or command literal
    for str in ("\"a\"", "'a'", "`a`")
        s = LineEdit.init_state(term, interface)
        write_input(s, str)
        charseek(buffer(s), 0)
        write_input(s, "f(")
        @test content(s) == "f(" * str
        @test position(buffer(s)) == 2
    end

    # Don't move over a closing bracket that is needed by another opening one
    s = LineEdit.init_state(term, interface)
    write_input(s, "f(g(x")
    @test content(s) == "f(g(x))"
    LineEdit.edit_delete(buffer(s))
    write_input(s, ")")
    @test content(s) == "f(g(x))"
    @test position(buffer(s)) == 6
    write_input(s, ")")
    @test content(s) == "f(g(x))"
    @test position(buffer(s)) == 7

    # Don't remove a closing bracket that is needed by another opening one
    s = LineEdit.init_state(term, interface)
    write_input(s, "f((a)")
    @test content(s) == "f((a))"
    write_input(s, "\b\b")
    @test content(s) == "f(()"
    write_input(s, "\b")
    @test content(s) == "f()"
    @test position(buffer(s)) == 2
    write_input(s, "\b")
    @test content(s) == "f"

    # Quotes: don't remove two adjacent strings' quotes; do move over a closing quote
    # that isn't escaped
    s = LineEdit.init_state(term, interface)
    write_input(s, "\"a\"\"b\"")
    charseek(buffer(s), 3)
    write_input(s, "\b")
    @test content(s) == "\"a\"b\""
    s = LineEdit.init_state(term, interface)
    write_input(s, "\"a\\")
    write_input(s, "\"")
    @test content(s) == "\"a\\\"\""
    @test position(buffer(s)) == 4
    write_input(s, "\"")
    @test content(s) == "\"a\\\"\""
    @test position(buffer(s)) == 5

    # Inserting and removing a pair are undo steps of their own
    s = LineEdit.init_state(term, interface)
    write_input(s, "ab(\"")
    @test content(s) == "ab(\"\")"
    LineEdit.edit_undo!(s)
    @test content(s) == "ab()"
    write_input(s, "\b")
    @test content(s) == "ab"
    LineEdit.edit_undo!(s)
    @test content(s) == "ab()"
    LineEdit.edit_undo!(s)
    @test content(s) == "ab"

    # Enter continues on a new line when the input is only complete thanks to automatically
    # inserted closing brackets, but not for an input that wasn't typed (e.g. from history)
    # Typing or moving over an automatically inserted closing bracket commits it
    prompt.on_enter = REPL.return_callback
    function type_keys(s, keys)
        done = false
        for k in keys
            buf = IOBuffer(k)
            done = LineEdit.match_input(prompt.keymap_dict, s, buf)(s, buf) === :done
            LineEdit.auto_closed_after_cursor(s) # as the prompt does after each key
        end
        return done
    end
    for (str, expected) in (("[1, 2\r3]\r", "[1, 2\n3]"), ("f(\r1)\r", "f(\n1)"),
                            ("x = [\rx\rfor x in 1:2\r]\r", "x = [\nx\nfor x in 1:2\n]"),
                            ("([]\r)\r", "([]\n)"), ("([]\e[C\r", "([])"),
                            ("()\e[D\r", "()"), ("f(x\e[D\r", "f(x)"),
                            ("print(\"\"\"\rx = \$(f(1))\r\"\"\")\r", "print(\"\"\"\nx = \$(f(1))\n\"\"\")"))
        s = LineEdit.init_state(term, interface)
        @test type_keys(s, [String(m.match) for m in eachmatch(r"\e\[.|.", str)])
        @test content(s) == expected
    end
    s = LineEdit.init_state(term, interface)
    type_keys(s, ["f", "(", "x"])
    @test LineEdit.at_input_end(LineEdit.state(s))
    type_keys(s, ["\e[D"])
    @test !LineEdit.at_input_end(LineEdit.state(s))
    s = LineEdit.init_state(term, interface)
    LineEdit.replace_line(LineEdit.state(s), "f(x)")
    charseek(buffer(s), 3)
    buf = IOBuffer("\r")
    @test LineEdit.match_input(prompt.keymap_dict, s, buf)(s, buf) === :done
    @test content(s) == "f(x)"
end

@testset "Conflicting definitions for keyseq" begin
    @testset "string" begin
        keymap = Dict{Char, Any}()
        REPL.LineEdit.add_nested_key!(keymap, "a", "abc")
        @test keymap == Dict('a' => "abc")
        expected_msg = "Conflicting definitions for keyseq a within one keymap"
        @test_throws ErrorException(expected_msg) REPL.LineEdit.add_nested_key!(keymap, "a", "abdef")
        @test keymap == Dict('a' => "abc")
    end
    @testset "char" begin
        keymap = Dict{Char, Any}()
        REPL.LineEdit.add_nested_key!(keymap, 'a', "abc")
        @test keymap == Dict('a' => "abc")
        expected_msg = "Conflicting definitions for keyseq a within one keymap"
        @test_throws ErrorException(expected_msg) REPL.LineEdit.add_nested_key!(keymap, 'a', "abdef")
        @test keymap == Dict('a' => "abc")
   end
end

@testset "key specifiers" begin
    @test LineEdit.normalize_key("\\M-x") == "\ex"
    @test LineEdit.normalize_key("\\C-a") == "\x01"
    @test LineEdit.normalize_key("\\^") == "^"
    @test_throws "must start with \"\\\\M-\"" LineEdit.normalize_key("\\Mx")
    @test_throws "Could not find redirected value \"a\"" LineEdit.keymap([Dict{Any,Any}("a" => "zz")])
    # a redirect to a key bound in a later keymap
    km = LineEdit.keymap([Dict{Any,Any}("a" => "b"), Dict{Any,Any}("b" => (o...) -> :b)])
    @test km['a']() === :b
end

@testset "edit_input" begin
    s = new_state()
    edit_insert(s, "l1\nl2\nl3")
    seek(buffer(s), 4)
    seen = Ref{Any}()
    LineEdit.edit_input(s, (file, line, col) -> (seen[] = (read(file, String), line, col)))
    @test seen[] == ("l1\nl2\nl3", 2, 2)
    # unchanged input keeps the cursor where it was
    @test content(s) == "l1\nl2\nl3"
    @test position(buffer(s)) == 4
    LineEdit.edit_input(s, (file, line, col) -> write(file, "new\n"))
    @test content(s) == "new"
    @test position(buffer(s)) == 3
end

# Test TerminalProperties and private CSI (`\e[?`) parsing
@testset "TerminalProperties" begin
    # Feed `input` (after `\e[?` has been consumed) to a fresh set of properties.
    function receive_csi(input; awaiting = false)
        props = LineEdit.TerminalProperties()
        props.awaiting_colors = awaiting
        term = FakeTerminal(IOBuffer(input), IOBuffer(), IOBuffer())
        LineEdit.receive_private_csi!(props, term)
        props, term
    end

    @testset "DA1 responses" begin
        @test receive_csi("64;1;2;6;22c")[1].da1 == [64, 1, 2, 6, 22]
        @test receive_csi("1c")[1].da1 == [1]
        # An empty response is still a response, but a truncated one is not.
        @test receive_csi("c")[1].da1 == Int[]
        @test receive_csi("64;1\x03")[1].da1 === nothing
        @test receive_csi("64;1")[1].da1 === nothing
    end

    @testset "palette update notifications" begin
        props, term = receive_csi("997;1nabc")
        @test props.awaiting_colors
        @test read(term.in_stream, String) == "abc"
        @test startswith(String(take!(term.out_stream)), "\e]4;0;?\e\\")
        # Not while a query is already being answered, nor for other reports (here DECRPM).
        for (input, awaiting) in (("997;2n", true), ("2031;1\$yabc", false))
            props, term = receive_csi(input; awaiting)
            @test isempty(take!(term.out_stream))
            @test props.da1 === nothing
        end
        @test read(term.in_stream, String) == "abc"
    end

    @testset "TerminalProperties default initialization" begin
        props = LineEdit.TerminalProperties()
        @test props.da1 === nothing
    end

    @testset "MIState has terminal_properties" begin
        s = new_state()
        @test s.terminal_properties isa LineEdit.TerminalProperties
        @test s.terminal_properties.da1 === nothing
    end
end

# Test OSC colour response parsing (see `query_colors`)
# The sentinel reply applies the palette to StyledStrings' global colour state, which other
# tests on the same worker would otherwise see, so restore it afterwards.
osc_saved_colors = copy(REPL.StyledStrings.FACES.basecolors)
osc_saved_faces = copy(REPL.StyledStrings.FACES.current[])
@testset "OSC colour responses" begin
    RGB(r, g, b) = (; r=UInt8(r), g=UInt8(g), b=UInt8(b))
    # `awaiting` mirrors a pending `query_colors`, so the sentinel applies the palette.
    function receive(responses...; awaiting = true)
        props = LineEdit.TerminalProperties()
        props.awaiting_colors = awaiting
        for r in responses
            LineEdit.receive_osc!(props, IOBuffer(r))
        end
        props
    end

    @testset "interpret_color" begin
        @test LineEdit.interpret_color("rgb:24/27/30") == RGB(0x24, 0x27, 0x30)
        @test LineEdit.interpret_color("rgb:1/2/3") == RGB(0x11, 0x22, 0x33)
        @test LineEdit.interpret_color("rgb:242/272/303") == RGB(0x24, 0x27, 0x30)
        @test LineEdit.interpret_color("rgb:2424/2727/3030") == RGB(0x24, 0x27, 0x30)
        @test LineEdit.interpret_color("rgb:24242/2/3") === nothing
        @test LineEdit.interpret_color("rgba:24/27/30/ff") == RGB(0x24, 0x27, 0x30)
        @test LineEdit.interpret_color("#242730") === nothing
        @test LineEdit.interpret_color("rgb:24/27") === nothing
        @test LineEdit.interpret_color("rgb:24/27/30/ff") === nothing
        @test LineEdit.interpret_color("rgba:24/27/30") === nothing
        @test LineEdit.interpret_color("rgb:zz/27/30") === nothing
    end

    @testset "read_osc_response terminators" begin
        for term in ("\a", "\e\\", "\x9c", "\x03")
            @test LineEdit.read_osc_response(IOBuffer("10;rgb:24/27/30" * term)) == "10;rgb:24/27/30"
        end
        # An unterminated response ends at EOF rather than blocking.
        @test LineEdit.read_osc_response(IOBuffer("10;rgb:24/27/30")) == "10;rgb:24/27/30"
    end

    @testset "colour accumulation" begin
        props = receive("4;1;rgb:ff/00/00\a", "4;2;rgb:00/ff/00\a")
        @test props.colors == [:red => RGB(0xff, 0, 0), :green => RGB(0, 0xff, 0)]
        @test receive("4;15;rgb:ff/ff/ff\a").colors == [:bright_white => RGB(0xff, 0xff, 0xff)]
        # Out-of-range indices and malformed specs are dropped, not stored.
        @test isempty(receive("4;16;rgb:ff/ff/ff\a").colors)
        @test isempty(receive("4;0;not-a-color\a").colors)
        @test isempty(receive("nonsense\a").colors)
    end

    @testset "foreground/background sentinel" begin
        props = receive("4;0;rgb:00/00/00\a", "10;rgb:bb/c2/cf\a", "11;rgb:24/27/30\a")
        @test props.colors == [:black => RGB(0, 0, 0), :foreground => RGB(0xbb, 0xc2, 0xcf),
                               :background => RGB(0x24, 0x27, 0x30)]
        props = receive("4;0;rgb:00/00/00\a", "10;rgb:bb/c2/cf\a")
        @test props.awaiting_colors
        LineEdit.receive_osc!(props, IOBuffer("11;rgb:24/27/30\a"))
        @test !props.awaiting_colors
    end

    @testset "partial response" begin
        # A terminal that answers only fg/bg still applies its colours.
        props = receive("10;rgb:bb/c2/cf\a", "11;rgb:24/27/30\a")
        @test props.colors == [:foreground => RGB(0xbb, 0xc2, 0xcf), :background => RGB(0x24, 0x27, 0x30)]
        @test !props.awaiting_colors
        # One that answers only the palette never reaches the sentinel.
        props = receive("4;0;rgb:00/00/00\a")
        @test props.awaiting_colors
        # A malformed sentinel reply still ends the query.
        props = receive("10;garbage\a", "11;rgb:24/27/30\a")
        @test props.colors == [:background => RGB(0x24, 0x27, 0x30)]
        @test !props.awaiting_colors
        # Responses outside a query are ignored.
        props = receive("4;1;rgb:ff/00/00\a", awaiting = false)
        @test isempty(props.colors)
    end
end
merge!(empty!(REPL.StyledStrings.FACES.basecolors), osc_saved_colors)
merge!(empty!(REPL.StyledStrings.FACES.current[]), osc_saved_faces)
