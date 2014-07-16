#Molecular dynamics of gas of randomly charged particles

type Particle{T<:Real}
    x :: Vector{T} #position
    p :: Vector{T} #momentum
    a :: Vector{T} #acceleration
    q :: T         #charge
end

function Particle{T<:Real}(x::Vector{T}, p::Vector{T}, q::T)
    @assert length(x)==length(p)==3
    Particle{T}(x,p,q)
end

function initrand!(n::Integer)
    #Initialize
    particles = Particle{Float64}[]
    for i=1:n
        push!(particles,Particle(randn(3), randn(3), randn(3), randn()))
    end
    particles
end

#Compute potential at a point using naive computation
function potential{T<:Real}(r::Vector{T}, particles::Vector{Particle{T}})
    n=length(particles)
    F=zeros(T,3)
    for i=1:n
	p1=particles[i]
	r==p1.x && continue
	F-=p1.q/(norm(p1.x-r)^3)*(p1.x-r)
    end
    F
end

abstract integrator <: Base.Algorithm
immutable velocityverlet <: integrator end
function update!{T<:Real}(particles::Vector{Particle{T}}, dt::T, ::Type{velocityverlet})
    for p in particles
        p.p += p.a * dt/2
        p.x += p.p * dt
	p.a  = p.q * potential(p.x, particles)
        p.p += p.a * dt/2
    end
end

function run{T<:Real}(particles::Vector{Particle{T}}, tend::T=10.0, dt::T=0.01)
    n=length(particles)
    forces = Array(T, 3, n)
    for (i,t) in enumerate(0:dt:tend)
	#println("Timestep: %i (time = %t)")
        update!(particles, dt, velocityverlet)
	draw(SVG("traj-$i.svg", 4inch, 4inch), compose(
	    context(units=UnitBox(-5,-5,10,10)),
	    [compose(context(), circle(p.x[1], p.x[2], 0.01),
	        fill(p.q>0? RGB(p.q,0,0) : RGB(0,0,-p.q))) for p in particles]...
	    ))
    end
end

using Color, Compose
particles=initrand!(1000)
run(particles,1.e-10,1.e-10)
@time run(particles)

cmd(`for a in *.svg; do convert \$a -depth 8 \${a%svg}png; done`)
cmd(`ffmpeg -i traj-%d.png -r 1/4 -c:v libx264 -r 30 -pix_fmt yuv420p -y traj.mp4`)
