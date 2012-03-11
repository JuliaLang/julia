
_jl_libcairo = dlopen("libcairo")

type CairoSurface
    ptr::Ptr{Void}
    kind::Symbol
    width::Float64
    height::Float64

    function CairoSurface(ptr::Ptr{Void}, kind::Symbol)
        self = new(ptr, kind)
        finalizer(self, destroy)
        self
    end
end

function finish(surface::CairoSurface)
    ccall(dlsym(_jl_libcairo,:cairo_surface_finish),
        Void, (Ptr{Void},), surface.ptr)
end

function destroy(surface::CairoSurface)
    ccall(dlsym(_jl_libcairo,:cairo_surface_destroy),
        Void, (Ptr{Void},), surface.ptr)
end

function status(surface::CairoSurface)
    ccall(dlsym(_jl_libcairo,:cairo_surface_status),
        Int32, (Ptr{Void},), surface.ptr)
end

const CAIRO_FORMAT_ARGB32 = 0
const CAIRO_FORMAT_RGB24 = 1
const CAIRO_FORMAT_A8 = 2
const CAIRO_FORMAT_A1 = 3
const CAIRO_FORMAT_RGB16_565 = 4

function CairoRGBSurface(w::Integer, h::Integer)
    ptr = ccall(dlsym(_jl_libcairo,:cairo_image_surface_create),
        Ptr{Void}, (Int32,Int32,Int32), CAIRO_FORMAT_RGB24, w, h)
    surface = CairoSurface(ptr, :rgb)
    @assert status(surface) == 0
    surface.width = w
    surface.height = h
    surface
end

function CairoPDFSurface(filename::String, w_pts::Real, h_pts::Real)
    ptr = ccall(dlsym(_jl_libcairo,:cairo_pdf_surface_create), Ptr{Void},
        (Ptr{Uint8},Float64,Float64), cstring(filename), w_pts, h_pts)
    surface = CairoSurface(ptr, :pdf)
    surface.width = w_pts
    surface.height = h_pts
    surface
end

function write_to_png(surface::CairoSurface, filename::String)
    ccall(dlsym(_jl_libcairo,:cairo_surface_write_to_png), Void,
        (Ptr{Uint8},Ptr{Uint8}), surface.ptr, cstring(filename))
end

# -----------------------------------------------------------------------------

type CairoContext
    ptr::Ptr{Void}
    surface::CairoSurface

    function CairoContext(surface::CairoSurface)
        ptr = ccall(dlsym(_jl_libcairo,:cairo_create),
            Ptr{Void}, (Ptr{Void},), surface.ptr)
        self = new(ptr, surface)
        finalizer(self, destroy)
        self
    end
end

macro _CTX_FUNC_V(NAME, FUNCTION)
    quote
        ($NAME)(ctx::CairoContext) =
            ccall(dlsym(_jl_libcairo,$string(FUNCTION)),
                Void, (Ptr{Void},), ctx.ptr)
    end
end

@_CTX_FUNC_V destroy cairo_destroy
@_CTX_FUNC_V save cairo_save
@_CTX_FUNC_V restore cairo_restore
@_CTX_FUNC_V show_page cairo_show_page
@_CTX_FUNC_V clip cairo_clip
@_CTX_FUNC_V clip_preserve cairo_clip_preserve
@_CTX_FUNC_V fill cairo_fill
@_CTX_FUNC_V fill_preserve cairo_fill_preserve
@_CTX_FUNC_V new_path cairo_new_path
@_CTX_FUNC_V close_path cairo_close_path
@_CTX_FUNC_V paint cairo_paint
@_CTX_FUNC_V stroke cairo_stroke
@_CTX_FUNC_V stroke_preserve cairo_stroke_preserve

macro _CTX_FUNC_I(NAME, FUNCTION)
    quote
        ($NAME)(ctx::CairoContext, i0::Integer) =
            ccall(dlsym(_jl_libcairo,$string(FUNCTION)),
                Void, (Ptr{Void},Int32), ctx.ptr, i0)
    end
end

@_CTX_FUNC_I set_fill_type cairo_set_fill_rule

macro _CTX_FUNC_D(NAME, FUNCTION)
    quote
        ($NAME)(ctx::CairoContext, d0::Real) =
            ccall(dlsym(_jl_libcairo,$string(FUNCTION)),
                Void, (Ptr{Void},Float64), ctx.ptr, d0)
    end
end

@_CTX_FUNC_D set_line_width cairo_set_line_width
@_CTX_FUNC_D set_font_size cairo_set_font_size
@_CTX_FUNC_D rotate cairo_rotate

macro _CTX_FUNC_DD(NAME, FUNCTION)
    quote
        ($NAME)(ctx::CairoContext, d0::Real, d1::Real) =
            ccall(dlsym(_jl_libcairo,$string(FUNCTION)),
                Void, (Ptr{Void},Float64,Float64), ctx.ptr, d0, d1)
    end
end

@_CTX_FUNC_DD _line_to cairo_line_to
@_CTX_FUNC_DD _move_to cairo_move_to
@_CTX_FUNC_DD _rel_line_to cairo_rel_line_to
@_CTX_FUNC_DD _rel_move_to cairo_rel_move_to

move_to(ctx::CairoContext, x, y) = _move_to(ctx, x, ctx.surface.height-y)
line_to(ctx::CairoContext, x, y) = _line_to(ctx, x, ctx.surface.height-y)
rel_line_to(ctx::CairoContext, x, y) = _rel_line_to(ctx, x, -y)
rel_move_to(ctx::CairoContext, x, y) = _rel_move_to(ctx, x, -y)

macro _CTX_FUNC_DDD(NAME, FUNCTION)
    quote
        ($NAME)(ctx::CairoContext, d0::Real, d1::Real, d2::Real) =
            ccall(dlsym(_jl_libcairo,$string(FUNCTION)),
                Void, (Ptr{Void},Float64,Float64,Float64), ctx.ptr, d0, d1, d2)
    end
end

@_CTX_FUNC_DDD set_source_rgb cairo_set_source_rgb

macro _CTX_FUNC_DDDD(NAME, FUNCTION)
    quote
        ($NAME)(ctx::CairoContext, d0::Real, d1::Real, d2::Real, d3::Real) =
            ccall(dlsym(_jl_libcairo,$string(FUNCTION)), Void,
                (Ptr{Void},Float64,Float64,Float64,Float64),
                ctx.ptr, d0, d1, d2, d3)
    end
end

@_CTX_FUNC_DDDD set_source_rgba cairo_set_source_rgba
@_CTX_FUNC_DDDD rectangle cairo_rectangle

macro _CTX_FUNC_DDDDD(NAME, FUNCTION)
    quote
        ($NAME)(ctx::CairoContext, d0::Real, d1::Real, d2::Real, d3::Real, d4::Real) =
            ccall(dlsym(_jl_libcairo,$string(FUNCTION)), Void,
                (Ptr{Void},Float64,Float64,Float64,Float64,Float64),
                ctx.ptr, d0, d1, d2, d3, d4)
    end
end

@_CTX_FUNC_DDDDD _arc cairo_arc

circle(ctx::CairoContext, x::Real, y::Real, r::Real) =
    _arc(ctx, x, ctx.surface.height-y, r, 0., 2pi)

function set_font_face(ctx::CairoContext, name::String)
    ccall(dlsym(_jl_libcairo,:cairo_select_font_face),
        Void, (Ptr{Void},Ptr{Uint8},Int32,Int32), ctx.ptr, cstring(name), 0, 0);
end

function set_dash(ctx::CairoContext, dashes::Vector{Float64})
    ccall(dlsym(_jl_libcairo,:cairo_set_dash), Void,
        (Ptr{Void},Ptr{Float64},Int32,Float64), ctx.ptr, dashes, length(dashes), 0.)
end

function text_extents(ctx::CairoContext, s::String)
    extents = zeros(Float64, 6)
    ccall(dlsym(_jl_libcairo,:cairo_text_extents), Void,
        (Ptr{Void},Ptr{Uint8},Ptr{Float64}), ctx.ptr, cstring(s), extents)
    extents
end

function label(ctx::CairoContext, halign::String, valign::String, s::String, angle::Real)
    extents = text_extents(ctx, s)

    _xxx = {
        "center"    => 0.5,
        "left"      => 0.,
        "right"     => 1.,
        "top"       => 1.,
        "bottom"    => 0.,
    }
    dx = -_xxx[halign]*extents[3]
    dy = -_xxx[valign]*extents[4]
    save(ctx)
    rotate(ctx, -angle*pi/180.)
    rel_move_to(ctx, dx, dy)
    ccall(dlsym(_jl_libcairo,:cairo_show_text), Void,
        (Ptr{Void},Ptr{Uint8}), ctx.ptr, cstring(s))
    restore(ctx)
end

function set_clip_rect(ctx::CairoContext, cr)
    x = cr[1]
    y = ctx.surface.height - cr[4]
    width = cr[2] - cr[1]
    height = cr[4] - cr[3]
    rectangle(ctx, x, y, width, height)
    clip(ctx)
    new_path(ctx)
end

# -----------------------------------------------------------------------------


type RendererState
    current::HashTable
    saved::Vector{HashTable}

    RendererState() = new(HashTable(),HashTable[])
end

function set( self::RendererState, name, value )
    self.current[name] = value
end

get(self::RendererState, name) = get(self, name, nothing)
function get( self::RendererState, name, notfound )
    if has(self.current, name)
        return self.current[name]
    end
    for d = self.saved
        if has(d,name)
            return d[name]
        end
    end
    return notfound
end

function save( self::RendererState )
    enqueue( self.saved, self.current )
    self.current = HashTable()
end

function restore( self::RendererState )
    self.current = self.saved[1]
    del(self.saved, 1)
end

function color_to_rgb( hextriplet::Integer )
    s = 1. / 0xff
    r = s * ((hextriplet >> 16) & 0xff) 
    g = s * ((hextriplet >>  8) & 0xff)
    b = s * ((hextriplet >>  0) & 0xff)
    return (r, g, b)
end

function color_to_rgb(color::String)
    # XXX:TODO
    if color == "red"
        return (1.,0.,0.)
    elseif color == "blue"
        return (0.,0.,1.)
    end
end

function _set_color( ctx::CairoContext, color )
    (r,g,b) = color_to_rgb( color )
    set_source_rgb( ctx, r, g, b )
end

const _set_fill_color = _set_color
const _set_pen_color = _set_color

function _set_line_type(ctx::CairoContext, nick::String)
    const nick2name = {
       "dot"       => "dotted",
       "dash"      => "shortdashed",
       "dashed"    => "shortdashed",
    }
    # XXX:should be scaled by linewidth
    const name2dashes = {
        "dotted"          => [1.,3.],
        "dotdashed"       => [1.,3.,4.,4.],
        "longdashed"      => [6.,6.],
        "shortdashed"     => [4.,4.],
        "dotdotdashed"    => [1.,3.,1.,3.,4.,4.],
        "dotdotdotdashed" => [1.,3.,1.,3.,1.,3.,4.,4.],
    }
    name = get(nick2name, nick, nick)
    if has(name2dashes, name)
        set_dash(ctx, name2dashes[name])
    end
end

abstract Renderer

type CairoRenderer <: Renderer
    ctx::CairoContext
    state::RendererState
    on_close::Function
    lowerleft
    upperright
    bbox

    function CairoRenderer(surface)
        ctx = CairoContext(surface)
        self = new(ctx)
        self.on_close = () -> nothing
        self.lowerleft = (0,0)
        self.bbox = nothing
        self
    end
end

function PNGRenderer(filename::String, width::Integer, height::Integer)
    surface = CairoRGBSurface(width, height)
    r = CairoRenderer(surface)
    r.upperright = (width,height)
    r.on_close = () -> write_to_png(surface, filename)
    set_source_rgb(r.ctx, 1.,1.,1.)
    paint(r.ctx)
    set_source_rgb(r.ctx, 0.,0.,0.)
    r
end

function _str_size_to_pts( str )
    m = match(r"([\d.]+)([^\s]+)", str)
    num_xx = float64(m.captures[1])
    units = m.captures[2]
    # convert to postscipt pt = in/72
    xx2pt = { "in"=>72., "pt"=>1., "mm"=>2.835, "cm"=>28.35 }
    num_pt = num_xx*xx2pt[units]
    return num_pt
end

PDFRenderer(filename::String, w_str::String, h_str::String) =
    PDFRenderer(filename, _str_size_to_pts(w_str), _str_size_to_pts(h_str))

function PDFRenderer(filename::String, w_pts::Float64, h_pts::Float64)
    surface = CairoPDFSurface(filename, w_pts, h_pts)
    r = CairoRenderer(surface)
    r.upperright = (w_pts,h_pts)
    r.on_close = () -> show_page(r.ctx)
    r
end

function open( self::CairoRenderer )
    self.state = RendererState()
end

function close( self::CairoRenderer )
    self.on_close()
    finish(self.ctx.surface)
end

## state commands

__pl_style_func = {
    "color"     => _set_color,
    "linecolor" => _set_pen_color,
    "fillcolor" => _set_fill_color,
    "linetype"  => _set_line_type,
    "linewidth" => set_line_width,
    "filltype"  => set_fill_type,
    "fontface"  => set_font_face,
    "fontsize"  => set_font_size,
    "cliprect"  => set_clip_rect,
}

function set( self::CairoRenderer, key, value )
    set(self.state, key, value )
    if has(__pl_style_func, key)
        __pl_style_func[key](self.ctx, value)
    end
end

function get(self::CairoRenderer, parameter, notfound)
    return get(self.state, parameter, notfound)
end

function get(self::CairoRenderer, parameter)
    get(self, parameter, nothing)
end

function save_state( self::CairoRenderer )
    save(self.state)
    save(self.ctx)
end

function restore_state( self::CairoRenderer )
    restore(self.state)
    restore(self.ctx)
end

## drawing commands

function move(self::CairoRenderer, p)
    move_to( self.ctx, p[1], p[2] )
end

function lineto( self::CairoRenderer, p )
    line_to( self.ctx, p[1], p[2] )
end

function linetorel( self::CairoRenderer, p )
    rel_line_to( self.ctx, p[1], p[2] )
end

function line( self::CairoRenderer, p, q )
    move_to(self.ctx, p[1], p[2])
    line_to(self.ctx, q[1], q[2])
    stroke(self.ctx)
end

function rect( self::CairoRenderer, p, q )
    rect( self.ctx, p[1], p[2], q[1], q[2] )
end

function circle( self::CairoRenderer, p, r )
    circle( self.ctx, p[1], p[2], r )
end

function ellipse( self::CairoRenderer, p, rx, ry, angle )
    ellipse( self.ctx, p[1], p[2], rx, ry, angle )
end

function arc( self::CairoRenderer, c, p, q )
    arc( self.ctx, c[1], c[2], p[1], p[2], q[1], q[2] )
end

__pl_symbol_type = {
    "none"              => 0,
    "dot"               => 1,
    "plus"              => 2,
    "asterisk"          => 3,
    "circle"            => 4,
    "cross"             => 5,
    "square"            => 6,
    "triangle"          => 7,
    "diamond"           => 8,
    "star"              => 9,
    "inverted triangle"     => 10,
    "starburst"         => 11,
    "fancy plus"            => 12,
    "fancy cross"           => 13,
    "fancy square"          => 14,
    "fancy diamond"         => 15,
    "filled circle"         => 16,
    "filled square"         => 17,
    "filled triangle"       => 18,
    "filled diamond"        => 19,
    "filled inverted triangle"  => 20,
    "filled fancy square"       => 21,
    "filled fancy diamond"      => 22,
    "half filled circle"        => 23,
    "half filled square"        => 24,
    "half filled triangle"      => 25,
    "half filled diamond"       => 26,
    "half filled inverted triangle" => 27,
    "half filled fancy square"  => 28,
    "half filled fancy diamond" => 29,
    "octagon"           => 30,
    "filled octagon"        => 31,
}

function symbol( self::CairoRenderer, p )
    symbols( self, [p[1]], [p[2]] )
end

function symbols( self::CairoRenderer, x, y )
    DEFAULT_SYMBOL_TYPE = "square"
    DEFAULT_SYMBOL_SIZE = 0.01
    type_str = get(self.state, "symboltype", DEFAULT_SYMBOL_TYPE )
    size = get(self.state, "symbolsize", DEFAULT_SYMBOL_SIZE )

    new_path(self.ctx)
    for i = 1:min(length(x),length(y))
        # XXX:TODO
        circle(self.ctx, x[i], y[i], 0.4*size)
        stroke(self.ctx)
    end
end

function curve( self::CairoRenderer, x::Vector, y::Vector )
    n = min(length(x), length(y))
    if n <= 0
        return
    end
    new_path(self.ctx)
    move_to(self.ctx, x[1], y[1])
    for i = 2:n
        line_to( self.ctx, x[i], y[i] )
    end
    stroke(self.ctx)
end

function polygon( self::CairoRenderer, points::Vector )
    move(self, points[1])
    for i in 2:length(points)
        lineto(self, points[i])
    end
    close_path(self.ctx)
    fill(self.ctx)
end

# text commands

function text( self::CairoRenderer, p, str )
    hstr = get( self.state, "texthalign", "center" )
    vstr = get( self.state, "textvalign", "center" )
    angle = get( self.state, "textangle", 0. )
    move_to( self.ctx, p[1], p[2] )
    label( self.ctx, hstr, vstr, str, angle )
end

function textwidth( self::CairoRenderer, str )
    extents = text_extents(self.ctx, str)
    extents[3]
end

function textheight( self::CairoRenderer, str )
    #extents = text_extents(self.ctx, str)
    #extents[4] # XXX:plot layout doesn't converge
    get( self.state, "fontsize" ) ## XXX: kludge?
end

