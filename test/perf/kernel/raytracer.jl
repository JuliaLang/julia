# raytracer.jl
# This is a translation from Java/C++ of the raytracer located at
#   http://www.ffconsultancy.com/languages/ray_tracer/
# It mostly keeps the structure of the code of these other languages, but is
# still fairly Julian.
# This code was discussed at
#   https://groups.google.com/forum/#!topic/julia-users/UkeV1H9fct4
# It is most likely GC-bound.

const delta = sqrt(eps(Float64))

immutable Vec
    x::Float64
    y::Float64
    z::Float64
end
+(a::Vec, b::Vec) = Vec(a.x+b.x, a.y+b.y, a.z+b.z)
-(a::Vec, b::Vec) = Vec(a.x-b.x, a.y-b.y, a.z-b.z)
*(a::Float64, b::Vec) = Vec(a*b.x, a*b.y, a*b.z)
*(a::Int, b::Vec) = Vec(a*b.x, a*b.y, a*b.z)
*(a::Vec, b::Float64) = *(b,a)
dot(a::Vec, b::Vec) = (a.x*b.x + a.y*b.y + a.z*b.z)
unitize(a::Vec) = (1. / sqrt(dot(a, a)) * a)

type Ray
    orig::Vec
    dir::Vec
end

type Hit
    lambda::Float64
    normal::Vec
end

abstract Scene

immutable Sphere <: Scene
    center::Vec
    radius::Float64
end

function ray_sphere(s::Sphere, ray::Ray)
    v = s.center - ray.orig
    b = dot(v, ray.dir)
    disc = b*b - dot(v, v) + s.radius*s.radius
    if disc >= 0
        d = sqrt(disc)
        t2 = b + d
        if t2 >= 0
            t1 = b - d
            return t1 > 0 ? t1 : t2
        end
    end
    return Inf
end

function intersect(s::Sphere, i::Hit, ray::Ray)
    l = ray_sphere(s, ray)
    if l >= i.lambda
        return i
    else
        n = ray.orig + l * ray.dir - s.center
        return Hit(l, unitize(n))
    end
end

immutable Group <: Scene
    bound::Sphere
    objs::Array{Scene}
end

Group(b::Sphere) = Group(b, Scene[])

function intersect(g::Group, i::Hit, ray::Ray)
    l = ray_sphere(g.bound, ray)
    if l >= i.lambda
        return i
    else
        for j in g.objs
            i = intersect(j, i, ray)
        end
        return i
    end
end

function ray_trace(light::Vec, ray::Ray, scene::Scene)
    i = intersect(scene, Hit(Inf, Vec(0.,0.,0.)), ray)
    if i.lambda == Inf
        return 0
    end
    o = ray.orig + i.lambda*ray.dir + delta*i.normal
    g = dot(i.normal, light)
    if g >= 0
        return 0
    end
    sray = Ray(o, -1*light)
    si = intersect(scene, Hit(Inf, Vec(0.,0.,0.)), sray)
    return si.lambda == Inf ? -g : 0
end

function create(level, c::Vec, r)
    sphere = Sphere(c, r)
    if level == 1
        return sphere
    end
    group = Group(Sphere(c, 3*r))
    push!(group.objs, sphere)
    rn = 3*r/sqrt(12)
    for dz = -1:2:1
        for dx = -1:2:1
            c2 = c + Vec(dx*rn, rn, dz*rn)
            push!(group.objs, create(level-1, c2, r/2))
        end
    end
    return group
end


function Raytracer(levels, n, ss)
    scene = create(levels, Vec(0., -1., 0.), 1.)
    light = unitize(Vec(-1., -3., 2.))
    # f = open("output.pgm", "w")
    # write(f,string("P5\n",n," ",n,"\n",255,"\n"))
    for y in (n-1):-1:0
        for x in 0:1:(n-1)
            g = 0.
            for dx in 0:1:(ss-1)
                for dy in 0:1:(ss-1)
                    d = Vec(x+dx*1./ss-n/2., y+dy*1./ss-n/2., n*1.0)
                    ray = Ray(Vec(0., 0., -4.0), unitize(d))
                    g += ray_trace(light, ray, scene);
                end
            end
            # write(f, int8(0.5 + 255. * g / (ss*ss)))
        end
    end
    # close(f)
end
