# Delaunay mesh refinement -- the sequential core, tested on its own before the
# optimistic-parallel/region layer is added. Bowyer-Watson point insertion:
# to refine a bad triangle, insert its circumcenter; the cavity is every
# triangle whose circumcircle contains the new point (found by BFS over edge
# adjacency); the cavity is removed and re-fanned from the new point.
# Float64 predicates, perturbed points -- demonstrator grade, not production.
module DMRCore
export Mesh, build_square, refine_sequential!, count_bad, ntris, checksum

struct P; x::Float64; y::Float64; end

mutable struct Tri
    a::Int; b::Int; c::Int      # point indices, counter-clockwise
    alive::Bool
    id::Int
end

mutable struct Mesh
    pts::Vector{P}
    tris::Vector{Tri}
    edge::Dict{Tuple{Int,Int},Vector{Int}}   # undirected edge -> triangle ids sharing it
    area_thresh::Float64
    next_id::Int
end

@inline ekey(u, v) = u < v ? (u, v) : (v, u)

@inline function tri_area(m, t)
    a = m.pts[t.a]; b = m.pts[t.b]; c = m.pts[t.c]
    abs((b.x-a.x)*(c.y-a.y) - (c.x-a.x)*(b.y-a.y)) / 2
end

# In-circle: is point p inside the circumcircle of triangle (a,b,c)? Sign of the
# standard 3x3 determinant, assuming CCW orientation.
@inline function in_circumcircle(m, t, p::P)
    a = m.pts[t.a]; b = m.pts[t.b]; c = m.pts[t.c]
    ax = a.x-p.x; ay = a.y-p.y; bx = b.x-p.x; by = b.y-p.y; cx = c.x-p.x; cy = c.y-p.y
    d = (ax*ax+ay*ay) * (bx*cy-cx*by) -
        (bx*bx+by*by) * (ax*cy-cx*ay) +
        (cx*cx+cy*cy) * (ax*by-bx*ay)
    d > 0
end

# The centroid is always strictly inside the triangle (hence inside the
# domain), so refining by it always terminates -- each split shrinks the area.
@inline function centroid(m, t)
    a = m.pts[t.a]; b = m.pts[t.b]; c = m.pts[t.c]
    P((a.x+b.x+c.x)/3, (a.y+b.y+c.y)/3)
end

@inline function circumcenter(m, t)
    a = m.pts[t.a]; b = m.pts[t.b]; c = m.pts[t.c]
    d = 2*(a.x*(b.y-c.y) + b.x*(c.y-a.y) + c.x*(a.y-b.y))
    ux = ((a.x^2+a.y^2)*(b.y-c.y) + (b.x^2+b.y^2)*(c.y-a.y) + (c.x^2+c.y^2)*(a.y-b.y)) / d
    uy = ((a.x^2+a.y^2)*(c.x-b.x) + (b.x^2+b.y^2)*(a.x-c.x) + (c.x^2+c.y^2)*(b.x-a.x)) / d
    P(ux, uy)
end

function add_edge!(m, u, v, tid)
    k = ekey(u, v)
    lst = get!(m.edge, k, Int[])
    push!(lst, tid)
end
function del_edge!(m, u, v, tid)
    k = ekey(u, v); lst = m.edge[k]
    deleteat!(lst, findfirst(==(tid), lst))
    isempty(lst) && delete!(m.edge, k)
end
function newtri!(m, a, b, c)
    t = Tri(a, b, c, true, m.next_id); m.next_id += 1
    push!(m.tris, t)
    add_edge!(m, a, b, t.id); add_edge!(m, b, c, t.id); add_edge!(m, c, a, t.id)
    t
end

byid(m, id) = m.tris[id]   # ids are 1:1 with the tris index while alive; compacted lazily

# Build a triangulated square [0,1]^2 with `g`x`g` grid points (two tris/cell),
# then perturb interior points to avoid degeneracies.
function build_square(g::Int, area_thresh::Float64)
    m = Mesh(P[], Tri[], Dict{Tuple{Int,Int},Vector{Int}}(), area_thresh, 1)
    st = UInt64(0x1234567)
    rnd() = (st = st*6364136223846793005 + 1442695040888963407; ((st>>33)/2.0^31 - 1) )
    idx(i, j) = (i-1)*(g) + j
    for i in 1:g, j in 1:g
        x = (j-1)/(g-1); y = (i-1)/(g-1)
        interior = (i>1 && i<g && j>1 && j<g)
        px = interior ? x + 0.15*rnd()/(g-1) : x
        py = interior ? y + 0.15*rnd()/(g-1) : y
        push!(m.pts, P(px, py))
    end
    for i in 1:g-1, j in 1:g-1
        newtri!(m, idx(i,j), idx(i,j+1), idx(i+1,j))
        newtri!(m, idx(i+1,j), idx(i,j+1), idx(i+1,j+1))
    end
    m
end

ntris(m) = count(t -> t.alive, m.tris)
count_bad(m) = count(t -> t.alive && tri_area(m, t) > m.area_thresh, m.tris)
checksum(m) = sum(t.alive ? (t.a + 3*t.b + 7*t.c) : 0 for t in m.tris)

# The cavity of point p seeded at bad triangle `seed`: BFS over edge adjacency,
# including a triangle if p is in its circumcircle. Returns (cavity ids,
# boundary edges as (u,v) with the cavity on the p side).
function cavity(m, seed::Int, p::P)
    cav = Int[seed]; incav = Set{Int}(seed)
    stack = Int[seed]
    while !isempty(stack)
        t = byid(m, pop!(stack))
        for (u, v) in ((t.a,t.b),(t.b,t.c),(t.c,t.a))
            for nid in get(m.edge, ekey(u,v), Int[])
                (nid == t.id || nid in incav) && continue
                nb = byid(m, nid)
                if nb.alive && in_circumcircle(m, nb, p)
                    push!(incav, nid); push!(cav, nid); push!(stack, nid)
                end
            end
        end
    end
    # boundary edges: edges of cavity tris whose other triangle is outside
    bnd = Tuple{Int,Int}[]
    for id in cav
        t = byid(m, id)
        for (u, v) in ((t.a,t.b),(t.b,t.c),(t.c,t.a))
            others = get(m.edge, ekey(u,v), Int[])
            outside = all(o -> o == id || !(o in incav), others)
            outside && push!(bnd, (u, v))
        end
    end
    (cav, incav, bnd)
end

# Insert p by removing the cavity and fanning from p to each boundary edge.
function insert_point!(m, seed::Int, p::P)
    cav, incav, bnd = cavity(m, seed, p)
    pid = (push!(m.pts, p); length(m.pts))
    for id in cav
        t = byid(m, id); t.alive = false
        del_edge!(m, t.a, t.b, id); del_edge!(m, t.b, t.c, id); del_edge!(m, t.c, t.a, id)
    end
    for (u, v) in bnd
        newtri!(m, u, v, pid)   # orientation may flip; area test is abs, fine for the demo
    end
    length(bnd)
end

# Refine every bad triangle by inserting its circumcenter, up to `maxsteps`.
function refine_sequential!(m; maxsteps=10^6)
    steps = 0
    while steps < maxsteps
        seed = findfirst(t -> t.alive && tri_area(m, t) > m.area_thresh, m.tris)
        seed === nothing && break
        cc = circumcenter(m, m.tris[seed])
        (0.0 < cc.x < 1.0 && 0.0 < cc.y < 1.0) || (m.tris[seed].alive = false; steps += 1; continue)
        insert_point!(m, m.tris[seed].id, cc)
        steps += 1
    end
    steps
end

# Exposed for the optimistic-parallel layer: the cavity computation (the heavy
# read-only speculation) and the plan application (the commit).
export P, Tri, cavity, tri_area, circumcenter, centroid, newtri!, del_edge!, byid

end # module DMRCore
