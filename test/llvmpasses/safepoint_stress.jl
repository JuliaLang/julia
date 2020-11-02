# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s | opt -load libjulia%shlibext -LateLowerGCFrame -FinalLowerGC -S - | FileCheck %s

println("""
declare {} addrspace(10)* @alloc()
declare void @one_arg_boxed({} addrspace(10)*)
declare {}*** @julia.ptls_states()

define void @stress(i64 %a, i64 %b) {
    %ptls = call {}*** @julia.ptls_states()
""")

# CHECK: %gcframe = alloca {} addrspace(10)*, i32 10002
for i = 1:10000
    println("\t%arg$i = call {} addrspace(10)* @alloc()")
end

for i = 1:10000
    println("\tcall void @one_arg_boxed({} addrspace(10)* %arg$i)")
end

println("""
    ret void
}
""")
