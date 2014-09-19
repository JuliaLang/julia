#
# The Computer Language Benchmarks Game
# nbody benchmark
# http://shootout.alioth.debian.org/u32/performance.php?test=nbody
#
# A straight port from the Java version
#

module NBody

# Constants
const solar_mass = 4 * pi * pi
const days_per_year = 365.24

# A heavenly body in the system
type Body
    x::Float64
    y::Float64
    z::Float64
    vx::Float64
    vy::Float64
    vz::Float64
    mass::Float64
end

function offset_momentum(b::Body, px, py, pz)
    b.vx = -px / solar_mass
    b.vy = -py / solar_mass
    b.vz = -pz / solar_mass
end

function init_sun(bodies)
    local px::Float64 = 0.0
    local py::Float64 = 0.0
    local pz::Float64 = 0.0
    for b in bodies
        px += b.vx * b.mass
        py += b.vy * b.mass
        pz += b.vz * b.mass
    end
    offset_momentum(bodies[1], px, py, pz)
end

function advance(bodies, dt)
    for i = 1:length(bodies)
        for j = i+1:length(bodies)
            dx = bodies[i].x - bodies[j].x
            dy = bodies[i].y - bodies[j].y
            dz = bodies[i].z - bodies[j].z
            dsq = dx^2 + dy^2 + dz^2
            distance = sqrt(dsq)
            mag = dt / (dsq * distance)

            bodies[i].vx -= dx * bodies[j].mass * mag
            bodies[i].vy -= dy * bodies[j].mass * mag
            bodies[i].vz -= dz * bodies[j].mass * mag

            bodies[j].vx += dx * bodies[i].mass * mag
            bodies[j].vy += dy * bodies[i].mass * mag
            bodies[j].vz += dz * bodies[i].mass * mag
        end
    end

    for b in bodies
        b.x += dt * b.vx
        b.y += dt * b.vy
        b.z += dt * b.vz
    end
end

function energy(bodies)
    local e::Float64 = 0.0
    for i = 1:length(bodies)
        e += 0.5 * bodies[i].mass *
             (bodies[i].vx^2 + bodies[i].vy^2 + bodies[i].vz^2)
        for j = i+1:length(bodies)
            dx = bodies[i].x - bodies[j].x
            dy = bodies[i].y - bodies[j].y
            dz = bodies[i].z - bodies[j].z
            distance = sqrt(dx^2 + dy^2 + dz^2)
            e -= (bodies[i].mass * bodies[j].mass) / distance
        end
    end
    e
end


function nbody(N::Int=1000)
    jupiter = Body( 4.84143144246472090e+00,                   # x
                   -1.16032004402742839e+00,                   # y
                   -1.03622044471123109e-01,                   # z
                   1.66007664274403694e-03 * days_per_year,   # vx
                   7.69901118419740425e-03 * days_per_year,   # vy
                   -6.90460016972063023e-05 * days_per_year,   # vz
                   9.54791938424326609e-04 * solar_mass)      # mass

    saturn = Body( 8.34336671824457987e+00,
                  4.12479856412430479e+00,
                  -4.03523417114321381e-01,
                  -2.76742510726862411e-03 * days_per_year,
                  4.99852801234917238e-03 * days_per_year,
                  2.30417297573763929e-05 * days_per_year,
                  2.85885980666130812e-04 * solar_mass)

    uranus = Body( 1.28943695621391310e+01,
                  -1.51111514016986312e+01,
                  -2.23307578892655734e-01,
                  2.96460137564761618e-03 * days_per_year,
                  2.37847173959480950e-03 * days_per_year,
                  -2.96589568540237556e-05 * days_per_year,
                  4.36624404335156298e-05 * solar_mass)

    neptune = Body( 1.53796971148509165e+01,
                   -2.59193146099879641e+01,
                   1.79258772950371181e-01,
                   2.68067772490389322e-03 * days_per_year,
                   1.62824170038242295e-03 * days_per_year,
                   -9.51592254519715870e-05 * days_per_year,
                   5.15138902046611451e-05 * solar_mass)

    sun = Body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solar_mass)

    bodies = [sun, jupiter, saturn, uranus, neptune]

    init_sun(bodies)
#    @printf("%.9f\n", energy(bodies))
    for i = 1:N
        advance(bodies, 0.01)
    end
#    @printf("%.9f\n", energy(bodies))
end

end # module
