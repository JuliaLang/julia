# This file is a part of Julia. License is MIT: https://julialang.org/license

# REQUIRES: x86_64, linux
# COM: software ticks for rr (src/llvm-softticks.cpp), enabled with -julia-softticks so that
# COM: this also runs in a runtime that is not built for them: the pipeline ticks at the end,
# COM: JIT code ticks (and runs), and a package image gets the ticks, the initializer and the note.
# RUN: rm -rf %t && mkdir -p %t
# RUN: env JULIA_LLVM_ARGS=-julia-softticks julia --startup-file=no -O2 %s %t/ir -O on %t/depot | FileCheck %s --check-prefix=EXEC
# RUN: llvm-link -S %t/ir/* | FileCheck %s
# RUN: env JULIA_LLVM_ARGS=-julia-softticks=0 julia --startup-file=no -O2 %s %t/ir -O off %t/depot
# RUN: llvm-link -S %t/ir/* | FileCheck %s --check-prefix=OFF

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

const enabled = ARGS[3] == "on"

# CHECK-LABEL: @julia_spin
# CHECK: top:
# CHECK: call void asm sideeffect "testb $$0x80, 0x70001807\0A\09jnz 1f\0A\09decq 0x70001800\0A\09jnz 1f\0A\09int3\0A1:"
# CHECK: L{{[0-9]+}}:
# CHECK-NEXT: = phi
# CHECK-NEXT: = phi
# CHECK-NEXT: call void asm sideeffect "testb $$0x80, 0x70001807\0A\09jnz 1f\0A\09decq 0x70001800\0A\09jnz 1f\0A\09int3\0A1:"
# CHECK: call {{.*}}@j_step
# OFF-NOT: asm sideeffect "testb $$0x80, 0x70001807
@noinline step(s, i) = (s ⊻ i) * 3
function spin(n)
    s = 0
    i = 0
    while i < n
        s = step(s, i)
        i += 1
    end
    return s
end
emit(spin, Int)

const countdown = Ptr{UInt64}(UInt(0x70001800))
const armed = UInt64(1) << 62
# The ticks that spin(n) executes. Outside rr the countdown is disarmed (its top bit is set) and
# the ticks only read it; arm it, as rr would, to count them, and disarm it again.
function ticks(n)
    unsafe_store!(countdown, armed)
    spin(n)
    c = unsafe_load(countdown)
    unsafe_store!(countdown, typemax(UInt64))
    return Int(armed - c)
end
# The ticks of spin(n) leave a disarmed countdown alone.
function disarmed(n)
    unsafe_store!(countdown, typemax(UInt64))
    spin(n)
    return unsafe_load(countdown) == typemax(UInt64)
end

if enabled
    # The countdown counts down by (at least) a tick at each call of step.
    ticks(10)
    # EXEC: JIT ticks: true
    println("JIT ticks: ", ticks(1000) - ticks(0) >= 1000)
    # EXEC: JIT ticks disarmed: true
    println("JIT ticks disarmed: ", disarmed(1000))

    # A package image: ticks, and one initializer and one note (one COMDAT group per image).
    depot = ARGS[4]
    pkg = joinpath(depot, "dev", "SoftTicksImage")
    mkpath(joinpath(pkg, "src"))
    write(joinpath(pkg, "Project.toml"), """
        name = "SoftTicksImage"
        uuid = "4b9a3d2e-2c4e-4c35-9a3e-6a7f5f3e1c11"
        """)
    write(joinpath(pkg, "src", "SoftTicksImage.jl"), """
        module SoftTicksImage
        f(n) = (s = 0; for i in 1:n; s = s * 31 + hash(i); end; s)
        g(x) = x isa Int ? f(x) : 0
        precompile(f, (Int,)); precompile(g, (Any,))
        end
        """)
    cmd = `$(Base.julia_cmd()) --startup-file=no -e "using SoftTicksImage; SoftTicksImage.f(100)"`
    env = ("JULIA_DEPOT_PATH" => depot * ":", "JULIA_LOAD_PATH" => "@stdlib:" * pkg,
           "JULIA_IMAGE_THREADS" => "4")
    run(addenv(cmd, env...))
    images = String[]
    for (root, _, files) in walkdir(joinpath(depot, "compiled"))
        append!(images, joinpath.(root, filter(endswith(".so"), files)))
    end
    image = read(only(images))
    count_bytes(pat) = length(findall(pat, image))
    tick = UInt8[0xf6, 0x04, 0x25, 0x07, 0x18, 0x00, 0x70, 0x80, 0x75, 0x0b,
                 0x48, 0xff, 0x0c, 0x25, 0x00, 0x18, 0x00, 0x70, 0x75, 0x01, 0xcc]
    # namesz 3, descsz 4, type 1, "rr", ABI version 1
    note = UInt8[3, 0, 0, 0, 4, 0, 0, 0, 1, 0, 0, 0, 'r', 'r', 0, 0, 1, 0, 0, 0]
    # EXEC: image ticks: true
    println("image ticks: ", count_bytes(tick) > 0)
    # EXEC: image notes: 1
    println("image notes: ", count_bytes(note))
    # movl $9, %eax; movl $0x70001000, %edi
    init = UInt8[0xb8, 0x09, 0x00, 0x00, 0x00, 0xbf, 0x00, 0x10, 0x00, 0x70]
    # EXEC: image initializers: 1
    println("image initializers: ", count_bytes(init))
end
