# Demonstrator B: a parallel path tracer, regions vs the stock collector, on
# the SAME code. Embarrassingly parallel: one sibling leaf per worker thread.
# Each pixel's samples allocate per-bounce hit records (the natural allocating
# form -- a mutable Hit per intersection, dynamic dispatch on an abstract
# Material); in region mode the pixel's whole bounce garbage dies at one
# unsafe_region_reset. The surviving output is a Vec3 colour (isbits) stored into the
# region-0 image buffer -- a legal store. In stock mode the same garbage falls
# to the collector.
#
# The per-pixel RNG is seeded by the pixel index, so the image is identical
# across region/stock and across thread scheduling: a bit-exact A == B check.
#   run with: julia -t4 pathtrace.jl
include(joinpath(@__DIR__, "demo_common.jl"))
using .DemoCommon
using Printf

@noinline region_set(n)        = ccall(:jl_gc_region_set, Cint, (Cint,), n)
@noinline unsafe_region_reset(n)      = UInt64(ccall(:jl_gc_region_unsafe_reset, UInt64, (Cint,), n))
@noinline region_parent!(c, p) = ccall(:jl_gc_region_declare_parent, Cint, (Cint, Cint), c, p)
@noinline quarantined(n)       = Int(ccall(:jl_gc_region_quarantined, Cint, (Cint,), n)) != 0

struct Vec3; x::Float64; y::Float64; z::Float64; end
@inline Base.:+(a::Vec3, b::Vec3) = Vec3(a.x+b.x, a.y+b.y, a.z+b.z)
@inline Base.:-(a::Vec3, b::Vec3) = Vec3(a.x-b.x, a.y-b.y, a.z-b.z)
@inline Base.:*(a::Vec3, s::Float64) = Vec3(a.x*s, a.y*s, a.z*s)
@inline mul(a::Vec3, b::Vec3) = Vec3(a.x*b.x, a.y*b.y, a.z*b.z)
@inline dot(a::Vec3, b::Vec3) = a.x*b.x + a.y*b.y + a.z*b.z
@inline norm(a::Vec3) = sqrt(dot(a, a))
@inline unit(a::Vec3) = a * (1.0 / norm(a))

abstract type Material end
struct Lambertian <: Material; albedo::Vec3; end
struct Metal <: Material; albedo::Vec3; end
struct Sphere; center::Vec3; radius::Float64; mat::Material; end   # mat abstract: a boxed ref

mutable struct Hit; t::Float64; p::Vec3; n::Vec3; mat::Material; end   # allocated per intersection

# A tiny deterministic RNG (xorshift), seeded per pixel.
mutable struct RNG; s::UInt64; end
@inline function nextf(r::RNG)
    x = r.s; x ⊻= x << 13; x ⊻= x >> 7; x ⊻= x << 17; r.s = x
    (x >> 11) * (1.0 / 2.0^53)
end
@inline rand_unit(r) = begin
    while true
        v = Vec3(2*nextf(r)-1, 2*nextf(r)-1, 2*nextf(r)-1)
        dot(v, v) < 1.0 && return unit(v)
    end
end

@inline function hit_sphere(s::Sphere, o::Vec3, d::Vec3, tmax::Float64)
    oc = o - s.center
    a = dot(d, d); b = dot(oc, d); c = dot(oc, oc) - s.radius*s.radius
    disc = b*b - a*c
    disc < 0 && return nothing
    t = (-b - sqrt(disc)) / a
    (t < 1e-3 || t > tmax) && return nothing
    p = o + d*t
    Hit(t, p, unit(p - s.center), s.mat)     # allocates a Hit per candidate
end

function hit_world(spheres, o::Vec3, d::Vec3)
    best = nothing; tmax = 1e30
    for s in spheres
        h = hit_sphere(s, o, d, tmax)
        if h !== nothing; best = h; tmax = h.t; end
    end
    best
end

scatter(m::Lambertian, d::Vec3, h::Hit, r::RNG) = (unit(h.n + rand_unit(r)), m.albedo)
scatter(m::Metal, d::Vec3, h::Hit, r::RNG) = (unit(d) - h.n * (2*dot(unit(d), h.n)), m.albedo)

function ray_color(spheres, o::Vec3, d::Vec3, depth::Int, r::RNG)
    depth <= 0 && return Vec3(0.0, 0.0, 0.0)
    h = hit_world(spheres, o, d)
    if h === nothing
        t = 0.5*(unit(d).y + 1.0)
        return Vec3(1.0,1.0,1.0)*(1.0-t) + Vec3(0.5,0.7,1.0)*t
    end
    sdir, atten = scatter(h.mat, d, h, r)
    mul(atten, ray_color(spheres, h.p, sdir, depth-1, r))
end

const W = 160; const H = 100; const DEPTH = 8
scene() = Sphere[
    Sphere(Vec3(0,0,-1), 0.5, Lambertian(Vec3(0.7,0.3,0.3))),
    Sphere(Vec3(0,-100.5,-1), 100.0, Lambertian(Vec3(0.8,0.8,0.0))),
    Sphere(Vec3(1,0,-1), 0.5, Metal(Vec3(0.8,0.6,0.2))),
    Sphere(Vec3(-1,0,-1), 0.5, Metal(Vec3(0.8,0.8,0.8))),
]

# `leaf == 0` is stock mode. Each pixel resets its worker's leaf after its
# samples; the pixel colour (isbits Vec3) is stored into the region-0 image.
function render!(image, spheres, samples, leaf_for)
    lower = Vec3(-2.0,-1.0,-1.0); horiz = Vec3(4.0,0,0); vert = Vec3(0,2.0,0); origin = Vec3(0,0,0)
    Threads.@threads for j in 1:H
        leaf = leaf_for()
        for i in 1:W
            leaf != 0 && region_set(leaf)
            r = RNG(UInt64(0x2545F4914F6CDD1D) * UInt64((j-1)*W + i) | 1)
            col = Vec3(0.0,0.0,0.0)
            for _ in 1:samples
                u = (i - 1 + nextf(r)) / W; v = (j - 1 + nextf(r)) / H
                d = lower + horiz*u + vert*v - origin
                col = col + ray_color(spheres, origin, d, DEPTH, r)   # Hits land in the leaf
            end
            leaf != 0 && region_set(0)
            leaf != 0 && unsafe_region_reset(leaf)
            @inbounds image[j, i] = col * (1.0 / samples)             # isbits store into region 0
        end
    end
end

checksum(image) = sum(p.x + p.y + p.z for p in image)

function drive(samples, tree::Bool)
    image = Matrix{Vec3}(undef, H, W)
    spheres = scene()
    leaf_for = tree ? (() -> Threads.threadid()) : (() -> 0)
    render!(image, spheres, samples, leaf_for)
    checksum(image)
end

# Declare one sibling leaf per thread (children of region 0, mutually isolated).
for t in 1:Threads.nthreads()
    region_parent!(t, 0)
end

function run_scale(label, samples)
    reg, sto, equal = DemoCommon.ab(() -> drive(samples, true), () -> drive(samples, false); reps = 3)
    report_table("B", "$label  ($(W)x$(H), $samples spp, depth $DEPTH, $(Threads.nthreads()) threads)", reg, sto)
    anyq = any(quarantined(t) for t in 1:Threads.nthreads())
    println("  same image: ", equal, "   quarantine: ", anyq)
    equal && !anyq
end

# Warm in STOCK mode FIRST so render!/ray_color/hit_* compile at region 0.
# Compiling inside a leaf window would allocate JIT method-table entries in the
# leaf and store them into the global cache -- an escape the barrier quarantines.
drive(2, false); drive(2, true)
println("Demonstrator B -- parallel path tracer, regions vs stock\n")
allok = true
allok &= run_scale("small ", 8)
allok &= run_scale("medium", 24)
allok &= run_scale("large ", 64)
println()
println(allok ? "DEMO B: consistent (bit-identical image, no escape)" : "DEMO B: CHECK (image or escape)")
