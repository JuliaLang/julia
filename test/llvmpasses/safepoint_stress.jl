# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s | opt -enable-new-pm=0 -load libjulia-codegen%shlibext -LateLowerGCFrame -FinalLowerGC -S - | FileCheck %s
# RUN: julia --startup-file=no %s | opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LateLowerGCFrame),FinalLowerGC' -S - | FileCheck %s


println("""
declare {} addrspace(10)* @alloc()
declare void @one_arg_boxed({} addrspace(10)*)
declare {}*** @julia.ptls_states()
declare {}*** @julia.get_pgcstack()

define void @stress(i64 %a, i64 %b) {
    %pgcstack = call {}*** @julia.get_pgcstack()
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
