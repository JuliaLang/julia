# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck --check-prefix=ATTR %s
# RUN: julia --startup-file=no %s %t -O && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck --check-prefix=ROOT %s

# Guards JuliaLang/julia#62613: `add_fn_attrs_for_effects` gives calls to
# `:consistent`+`:effect_free` functions optimistic memory effects for the
# pre-GC pipeline. The unoptimized run checks the attributes are really
# emitted (so this test cannot silently go stale); the optimized run checks
# that a tracked value live across such a call still gets a GC frame root
# after the full pipeline.

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

@noinline pureadd(a::Int, b::Int) = a + b
@noinline makeref(n::Int) = Ref(n)

function rootacross(n::Int)
    y = makeref(n)
    s = pureadd(n, n)
    return s + y[]
end

# ATTR: define {{.*}}i64 @julia_rootacross
# ATTR: call swiftcc i64 @j_pureadd{{[^(]*}}({{[^)]*}}) [[PUREATTRS:#[0-9]+]]
# ATTR: attributes [[PUREATTRS]] = { {{.*}}memory(argmem: read){{.*}}"julia.safepoint"{{.*}} }

# ROOT: define {{.*}}i64 @julia_rootacross
# ROOT: {{%gcframe[0-9]*}} = alloca
# ROOT: [[Y:%.*]] = call swiftcc nonnull ptr @j_makeref
# ROOT-NEXT: store ptr [[Y]], ptr [[SLOT:%.*]],
# ROOT-NEXT: {{.*}}call swiftcc i64 @j_pureadd
emit(rootacross, Int)
