module Graphics

import Base: fill, norm, scale

export
    # Part 1. 2D Geometry
    Vec2, Point, BoundingBox,
    # limits in world coordinates
    isinside, xmin, xmax, ymin, ymax, center, xrange, yrange,
    aspect_ratio, with_aspect_ratio, diagonal, shift, deform,
    # TODO: more

    # TODO: 3D geometry

    # Part 2. 2D Drawing
    # device and context
    GraphicsDevice, GraphicsContext, creategc, getgc,
    # width, height are in user (world) coordinates for geometric objects,
    # but in device coordinates for GraphicsDevice, GraphicsContext, and
    # other concrete things like windows and widgets.
    width, height,

    # drawing attribute manipulation
    save, restore, set_line_width, set_dash, set_source_rgb, set_source_rgba,
    set_source,

    # coordinate systems
    reset_transform, set_coords, rotate, scale, translate, user_to_device!,
    device_to_user!, user_to_device_distance!, device_to_user_distance!,
    user_to_device, device_to_user,

    # clipping
    clip, clip_preserve, reset_clip,

    # path primitives
    move_to, line_to, rel_line_to, rel_move_to, new_path, new_sub_path,
    close_path, arc,

    # fill and stroke
    fill, fill_preserve, paint, stroke, stroke_preserve,
    stroke_transformed, stroke_transformed_preserve,

    # derived path operations
    rectangle, circle, polygon

    # TODO: text drawing API

    # TODO: rendering pipeline API


# Part 1. geometric primitives

immutable Vec2
    x::Float64
    y::Float64
end

typealias Point Vec2

(+)(a::Vec2, b::Vec2) = Vec2(a.x + b.x, a.y + b.y)
(-)(a::Vec2, b::Vec2) = Vec2(a.x - b.x, a.y - b.y)
(*)(p::Vec2, s::Real) = Vec2(p.x*s, p.y*s)
(/)(p::Vec2, s::Real) = Vec2(p.x/s, p.y/s)
(*)(s::Real, p::Vec2) = p*s

# rotate p around o by angle
function rotate(p::Vec2, angle::Real, o::Vec2)
    c = cos(angle)
    s = sin(angle)
    d = p - o
    Vec2(o.x + c*d.x - s*d.y, o.y + s*d.x + c*d.y)
end
rotate(p::Vec2, angle::Real) = rotate(p, angle, Vec2(0.,0.))

norm(p::Vec2) = hypot(p.x, p.y)

immutable BoundingBox
    xmin::Float64
    xmax::Float64
    ymin::Float64
    ymax::Float64
end

BoundingBox() = BoundingBox(NaN, NaN, NaN, NaN)

function BoundingBox(points::Point...)
    xmin, xmax, ymin, ymax = NaN, NaN, NaN, NaN
    for p in points
        xmin = min(xmin, p.x)
        xmax = max(xmax, p.x)
        ymin = min(ymin, p.y)
        ymax = max(ymax, p.y)
    end
    return BoundingBox(xmin, xmax, ymin, ymax)
end

function BoundingBox(bboxes::BoundingBox...)
    xmin, xmax, ymin, ymax = NaN, NaN, NaN, NaN
    for bb in bboxes
        xmin = min(xmin, bb.xmin)
        xmax = max(xmax, bb.xmax)
        ymin = min(ymin, bb.ymin)
        ymax = max(ymax, bb.ymax)
    end
    return BoundingBox(xmin, xmax, ymin, ymax)
end

width(bb::BoundingBox) = bb.xmax - bb.xmin
height(bb::BoundingBox) = bb.ymax - bb.ymin
diagonal(bb) = hypot(width(bb), height(bb))
aspect_ratio(bb) = height(bb)/width(bb)

xmin(bb::BoundingBox) = bb.xmin
xmax(bb::BoundingBox) = bb.xmax
ymin(bb::BoundingBox) = bb.ymin
ymax(bb::BoundingBox) = bb.ymax

center(x) = Point((xmin(x)+xmax(x))/2, (ymin(x)+ymax(x))/2)
xrange(x) = xmin(x), xmax(x)
yrange(x) = ymin(x), ymax(x)

function (+)(bb1::BoundingBox, bb2::BoundingBox)
    BoundingBox(min(bb1.xmin, bb2.xmin),
                max(bb1.xmax, bb2.xmax),
                min(bb1.ymin, bb2.ymin),
                max(bb1.ymax, bb2.ymax))
end

function (&)(bb1::BoundingBox, bb2::BoundingBox)
    BoundingBox(max(bb1.xmin, bb2.xmin),
                min(bb1.xmax, bb2.xmax),
                max(bb1.ymin, bb2.ymin),
                min(bb1.ymax, bb2.ymax))
end

function deform(bb::BoundingBox, dl, dr, dt, db)
    BoundingBox(bb.xmin + dl, bb.xmax + dr, bb.ymin + dt, bb.ymax + db)
end

# shift center by (dx,dy), keeping width & height fixed
function shift(bb::BoundingBox, dx, dy)
    BoundingBox(bb.xmin + dx, bb.xmax + dx, bb.ymin + dy, bb.ymax + dy)
end

# scale width & height, keeping center fixed
function (*)(bb::BoundingBox, s::Real)
    dw = 0.5*(s - 1)*width(bb)
    dh = 0.5*(s - 1)*height(bb)
    deform(bb, -dw, dw, -dh, dh)
end
(*)(s::Real, bb::BoundingBox) = bb*s

function rotate(bb::BoundingBox, angle::Real, p::Point)
    a = rotate(Point(bb.xmin,bb.ymin), angle, p)
    b = rotate(Point(bb.xmax,bb.ymin), angle, p)
    c = rotate(Point(bb.xmin,bb.ymax), angle, p)
    d = rotate(Point(bb.xmax,bb.ymax), angle, p)
    BoundingBox(a, b, c, d)
end

function with_aspect_ratio(bb::BoundingBox, ratio::Real)
    if ratio < aspect_ratio(bb)
        dh = height(bb) - ratio * width(bb)
        return BoundingBox(bb.xmin, bb.xmax, bb.ymin + dh/2, bb.ymax - dh/2)
    else
        dw = width(bb) - height(bb) / ratio
        return BoundingBox(bb.xmin + dw/2, bb.xmax - dw/2, bb.ymin, bb.ymax)
    end
end

isinside(bb::BoundingBox, x, y) = (bb.xmin <= x <= bb.xmax) && (bb.ymin <= y <= bb.ymax)
isinside(bb::BoundingBox, p::Point) = isinside(bb, p.x, p.y)


# Part 2. Drawing

macro mustimplement(sig)
    fname = sig.args[1]
    arg1 = sig.args[2]
    if isa(arg1,Expr)
        arg1 = arg1.args[1]
    end
    :($(esc(sig)) = error(typeof($(esc(arg1))),
                          " must implement ", $(Expr(:quote,fname))))
end

# a graphics output device; can create GraphicsContexts
abstract GraphicsDevice

@mustimplement width(gd::GraphicsDevice)
@mustimplement height(gd::GraphicsDevice)
@mustimplement creategc(gd::GraphicsDevice)
xmin(g::GraphicsDevice) = 0
xmax(g::GraphicsDevice) = width(g)
ymin(g::GraphicsDevice) = 0
ymax(g::GraphicsDevice) = height(g)

# an object that can actually be drawn to
abstract GraphicsContext

@mustimplement width(gc::GraphicsContext)
@mustimplement height(gc::GraphicsContext)
# getgc() - get a GraphicsContext from something that might be drawable
getgc(gc::GraphicsContext) = gc


# transformations

# set coordinates in terms of left, right, top, bottom
function set_coords(c::GraphicsContext, x, y, w, h, l, r, t, b)
    reset_transform(c)
    reset_clip(c)
    rectangle(c, x, y, w, h)
    clip(c)
    if (r-l) != w || (b-t) != h || l != x || t != y
        # note: Cairo assigns integer pixel-space coordinates to the grid
        # points between sample locations, not to the centers of pixels.
        xs = w/(r-l)
        ys = h/(b-t)
        scale(c, xs, ys)

        xcent = (l+r)/2
        ycent = (t+b)/2
        translate(c, -xcent + (w/2 + x)/xs, -ycent + (h/2 + y)/ys)
    end
    c
end
set_coords(c::GraphicsContext, device::BoundingBox, user::BoundingBox) = set_coords(c, device.xmin, device.ymin, width(device), height(device), user.xmin, user.xmax, user.ymin, user.ymax)

@mustimplement save(gc::GraphicsContext)
@mustimplement restore(gc::GraphicsContext)
@mustimplement reset_transform(gc::GraphicsContext)
@mustimplement rotate(gc::GraphicsContext, ::Real)
@mustimplement scale(gc::GraphicsContext, ::Real, ::Real)
@mustimplement translate(gc::GraphicsContext, ::Real, ::Real)

user_to_device!(gc::GraphicsContext, c::Vector{Float64}) = c
device_to_user!(gc::GraphicsContext, c::Vector{Float64}) = c
user_to_device_distance!(gc::GraphicsContext, c::Vector{Float64}) = c
device_to_user_distance!(gc::GraphicsContext, c::Vector{Float64}) = c

const d2ubuf = zeros(2)
function device_to_user(gc::GraphicsContext, x::Real, y::Real)
    d2ubuf[1] = x
    d2ubuf[2] = y
    device_to_user!(gc, d2ubuf)
    d2ubuf[1], d2ubuf[2]
end
function user_to_device(gc::GraphicsContext, x::Real, y::Real)
    d2ubuf[1] = x
    d2ubuf[2] = y
    user_to_device!(gc, d2ubuf)
    d2ubuf[1], d2ubuf[2]
end

# drawing and properties

@mustimplement set_line_width(gc::GraphicsContext, ::Real)
@mustimplement set_dash(gc::GraphicsContext, ::Vector{Float64}, ::Real)
@mustimplement set_source_rgb(gc::GraphicsContext, ::Real, ::Real, ::Real)
@mustimplement set_source_rgba(gc::GraphicsContext, ::Real, ::Real, ::Real, ::Real)
@mustimplement set_source(gc::GraphicsContext, src)

@mustimplement clip(gc::GraphicsContext)
@mustimplement clip_preserve(gc::GraphicsContext)
@mustimplement reset_clip(gc::GraphicsContext)

@mustimplement move_to(gc::GraphicsContext, ::Real, ::Real)
@mustimplement line_to(gc::GraphicsContext, ::Real, ::Real)
@mustimplement rel_move_to(gc::GraphicsContext, ::Real, ::Real)
@mustimplement rel_line_to(gc::GraphicsContext, ::Real, ::Real)
@mustimplement arc(gc::GraphicsContext, ::Real, ::Real, ::Real, ::Real, ::Real)
@mustimplement close_path(gc::GraphicsContext)
@mustimplement new_path(gc::GraphicsContext)
@mustimplement new_sub_path(gc::GraphicsContext)

@mustimplement fill(gc::GraphicsContext)
@mustimplement fill_preserve(gc::GraphicsContext)
@mustimplement stroke(gc::GraphicsContext)
@mustimplement stroke_preserve(gc::GraphicsContext)
@mustimplement paint(gc::GraphicsContext)
stroke_transformed(gc::GraphicsContext) = stroke(gc)
stroke_transformed_preserve(gc::GraphicsContext) = stroke_preserve(gc)

# generic path functions

function rectangle(gc::GraphicsContext, x::Real, y::Real, width::Real, height::Real)
    move_to(gc, x, y)
    rel_line_to(gc, width, 0)
    rel_line_to(gc, 0, height)
    rel_line_to(gc, -width, 0)
    close_path(gc)
end
rectangle(gc::GraphicsContext, user::BoundingBox) = rectangle(gc, user.xmin, user.ymin, width(user), height(user))

circle(ctx::GraphicsContext, x::Real, y::Real, r::Real) =
    arc(ctx, x, y, r, 0., 2pi)

function polygon(gc::GraphicsContext, verts::Matrix, idx::Vector)
    move_to(gc, verts[1,idx[1]], verts[2,idx[1]])
    for i=2:length(idx)
        n = idx[i]
        line_to(gc, verts[1,n], verts[2,n])
    end
    close_path(gc)
end

function polygon(self::GraphicsContext, points::AbstractVector)
    move_to(self, points[1].x, points[1].y)
    for i in 2:length(points)
        line_to(self, points[i].x, points[i].y)
    end
    close_path(self)
end

end # module
