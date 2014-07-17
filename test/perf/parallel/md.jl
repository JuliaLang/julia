#Molecular dynamics of gas of randomly charged particles
#Extremely simplistic microcanonical (NVE) dynamics

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

rendernone(args...)=nothing
function renderparticles{T}(eventname::String, particles::Vector{Particle{T}})
    if eventname == "finalize"
        #Use ImageMagick to convert SVG to PNG
	run(`sh -c "for a in traj-*.svg; do convert \$a -depth 8 -resize 1024x1024 \${a%svg}png; done"`)
	#Use ffmpeg to generate video
        run(`ffmpeg -i traj-%d.png -r 1/4 -c:v libx264 -r 30 -pix_fmt yuv420p -y traj.mp4`)
	#Cleanup all the intermediate frames
	run(`sh -c "rm -f traj-*.svg traj-*.png"`)
    else #Render current particle positions
	draw(
            SVG(string(eventname,".svg"), 4inch, 4inch),
	    compose(context(units=UnitBox(-5,-5,10,10)),
	        [compose(context(), circle(p.x[1], p.x[2], 0.01),
	         fill(p.q>0? RGB(p.q,0,0) : RGB(0,0,-p.q))) for p in particles]...
    ))
    end
end

function rundynamics{T<:Real}(particles::Vector{Particle{T}}, tend::T=1.0, dt::T=0.001;
	render::Function=rendernone)
    n=length(particles)
    forces = Array(T, 3, n)
    for (i,t) in enumerate(0:dt:tend)
	#println("Timestep: %i (time = %t)")
        update!(particles, dt, velocityverlet)
	render("traj-$i", particles)
    end
    render("finalize", particles)
end

renderer=renderparticles
renderer==rendernone || using Color, Compose

particles=initrand!(10)
rundynamics(particles,1.e-10,1.e-10)

@time rundynamics(particles;render=renderer)

