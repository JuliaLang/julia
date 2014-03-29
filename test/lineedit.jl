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
