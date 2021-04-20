# This file is a part of Julia. License is MIT: https://julialang.org/license

const Stack = Main.TestGCExt.Stack

function make()
    ccall(:stk_make, Stack, ())
end

function push(stack :: Stack, val :: String)
    ccall(:stk_push, Nothing, (Stack, String), stack, val)
end

function top(stack :: Stack)
    return ccall(:stk_top, String, (Stack,), stack)
end

function pop(stack :: Stack)
    return ccall(:stk_pop, String, (Stack,), stack)
end

function size(stack :: Stack)
    return ccall(:stk_size, UInt, (Stack,), stack)
end

function empty(stack :: Stack)
    return size(stack) == 0
end

function blob(stack :: Stack)
    return ccall(:stk_blob, Any, (Stack,), stack)
end

function gc_counter_full()
    return ccall(:get_gc_counter, UInt, (Cint,), 1)
end

function gc_counter_inc()
    return ccall(:get_gc_counter, UInt, (Cint,), 0)
end

function gc_counter()
    return gc_counter_full() + gc_counter_inc()
end

function num_obj_sweeps()
    return ccall(:get_obj_sweeps, UInt, ())
end

function get_aux_root(n :: Int)
    return ccall(:get_aux_root, String, (UInt,), n)
end

function set_aux_root(n :: Int, x :: String)
    return ccall(:set_aux_root, Nothing, (UInt, String), n, x)
end

function internal_obj_scan(p :: Any)
    if ccall(:internal_obj_scan, Cint, (Any,), p) == 0
        global internal_obj_scan_failures += 1
    end
end

global internal_obj_scan_failures = 0

for i in 0:1000
    set_aux_root(i, string(i))
end

function test()
    local stack = make()
    for i in 1:100000
        push(stack, string(i, base=2))
        internal_obj_scan(top(stack))
    end
    for i in 1:1000
        local stack2 = make()
        internal_obj_scan(stack2)
        internal_obj_scan(blob(stack2))
        while !empty(stack)
            push(stack2, pop(stack))
        end
        stack = stack2
        if i % 100 == 0
            GC.gc()
        end
    end
end

@time test()

global corrupted_roots = 0
for i in 0:1000
    if get_aux_root(i) != string(i)
        global corrupted_roots += 1
    end
end

print(gc_counter_full(), " full collections.\n")
print(gc_counter_inc(), " partial collections.\n")
print(num_obj_sweeps(), " object sweeps.\n")
print(internal_obj_scan_failures, " internal object scan failures.\n")
print(corrupted_roots, " corrupted auxiliary roots.\n")
