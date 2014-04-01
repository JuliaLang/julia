using Base.LineEdit

a_foo = 0

const foo_keymap = {
    'a' => :( global a_foo; a_foo += 1)
}

b_foo = 0

const foo2_keymap = {
    'b' => :( global b_foo; b_foo += 1)
}

a_bar = 0 
b_bar = 0

const bar_keymap = {
    'a' => :( global a_bar; a_bar += 1),
    'b' => :( global b_bar; b_bar += 1)
}

@eval @LineEdit.keymap test1_func $foo_keymap

function run_test(f,buf)
    global a_foo, a_bar, b_bar
    a_foo = a_bar = b_bar = 0
    while !eof(buf)
        f(buf,nothing)
    end
end

run_test(test1_func,IOBuffer("aa"))
@test a_foo == 2

@eval @LineEdit.keymap test2_func $([foo2_keymap, foo_keymap])

run_test(test2_func,IOBuffer("aaabb"))
@test a_foo == 3
@test b_foo == 2

@eval @LineEdit.keymap test3_func $([bar_keymap, foo_keymap])

run_test(test3_func,IOBuffer("aab"))
@test a_bar == 2
@test b_bar == 1

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
