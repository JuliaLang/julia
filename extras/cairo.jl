module Cairo
import Base.*

export CairoSurface, finish, destroy, status,
    CAIRO_FORMAT_ARGB32,
    CAIRO_FORMAT_RGB24,
    CAIRO_FORMAT_A8,
    CAIRO_FORMAT_A1,
    CAIRO_FORMAT_RGB16_565,
    CAIRO_CONTENT_COLOR,
    CAIRO_CONTENT_ALPHA,
    CAIRO_CONTENT_COLOR_ALPHA,
    CairoRGBSurface, CairoPDFSurface, CairoEPSSurface, CairoXlibSurface,
    CairoARGBSurface, CairoSVGSurface, surface_create_similar,
    write_to_png, CairoContext, save, restore, show_page, clip, clip_preserve,
    fill, fill_preserve, new_path, new_sub_path, close_path, paint, stroke,
    stroke_preserve, set_fill_type, set_line_width, rotate, set_source_rgb,
    set_source_surface,
    move_to, line_to, rel_line_to, rel_move_to, set_source_rgba, rectangle,
    circle, arc, set_dash, set_clip_rect, set_font_from_string, set_markup,
    get_layout_size, update_layout, show_layout, image, read_from_png,
    RendererState, color_to_rgb, Renderer, CairoRenderer, PNGRenderer,
    PDFRenderer, EPSRenderer, save_state, restore_state, move, lineto,
    linetorel, line, rect, ellipse, symbol, symbols, set, get,
    open, close, curve, polygon, layout_text, text, textwidth, textheight,
    TeXLexer, tex2pango, SVGRenderer

load("color.jl")

load("openlib.jl")

try
    global _jl_libcairo = openlib("libcairo")
    global _jl_libpangocairo = openlib("libpangocairo-1.0")
    global _jl_libgobject = openlib("libgobject-2.0")
    global libcairo_wrapper = dlopen("libcairo_wrapper")
catch err
    println("Oops, could not load cairo or pango libraries. Are they installed?")
    if OS_NAME == :Darwin
        println(E"
  homebrew:
    $ brew install cairo pango

  macports:
    $ port install cairo pango
    $ export LD_LIBRARY_PATH=/opt/local/lib
"       )
    end
    throw(err)
end

type CairoSurface
    ptr::Ptr{Void}
    width::Float64
    height::Float64

    function CairoSurface(ptr::Ptr{Void}, w, h)
        self = new(ptr, w, h)
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
const CAIRO_CONTENT_COLOR = int(0x1000)
const CAIRO_CONTENT_ALPHA = int(0x2000)
const CAIRO_CONTENT_COLOR_ALPHA = int(0x3000)

function CairoRGBSurface(w::Integer, h::Integer)
    ptr = ccall(dlsym(_jl_libcairo,:cairo_image_surface_create),
        Ptr{Void}, (Int32,Int32,Int32), CAIRO_FORMAT_RGB24, w, h)
    CairoSurface(ptr, w, h)
end

function CairoARGBSurface(w::Integer, h::Integer)
    ptr = ccall(dlsym(_jl_libcairo,:cairo_image_surface_create),
        Ptr{Void}, (Int32,Int32,Int32), CAIRO_FORMAT_ARGB32, w, h)
    CairoSurface(ptr, w, h)
end

function CairoPDFSurface(filename::String, w_pts::Real, h_pts::Real)
    ptr = ccall(dlsym(_jl_libcairo,:cairo_pdf_surface_create), Ptr{Void},
        (Ptr{Uint8},Float64,Float64), bytestring(filename), w_pts, h_pts)
    CairoSurface(ptr, w_pts, h_pts)
end

function CairoEPSSurface(filename::String, w_pts::Real, h_pts::Real)
    ptr = ccall(dlsym(_jl_libcairo,:cairo_ps_surface_create), Ptr{Void},
        (Ptr{Uint8},Float64,Float64), bytestring(filename), w_pts, h_pts)
    ccall(dlsym(_jl_libcairo,:cairo_ps_surface_set_eps), Void,
        (Ptr{Void},Int32), ptr, 1)
    CairoSurface(ptr, w_pts, h_pts)
end

function CairoXlibSurface(display, drawable, visual, w, h)
    ptr = ccall(dlsym(_jl_libcairo,:cairo_xlib_surface_create), Ptr{Void},
                (Ptr{Void}, Int32, Ptr{Void}, Int32, Int32),
                display, drawable, visual, w, h)
    CairoSurface(ptr, w, h)
end

function CairoSVGSurface(stream::IOStream, w, h)
    ptr = ccall(dlsym(_jl_libcairo,:cairo_svg_surface_create_for_stream), Ptr{Void},
                (Ptr{Void}, Ptr{Void}, Float64, Float64),
                dlsym(libcairo_wrapper,:cairo_write_to_ios_callback), stream, w, h)
    CairoSurface(ptr, w, h)
end

function read_from_png(filename::String)
    ptr = ccall(dlsym(_jl_libcairo,:cairo_image_surface_create_from_png),
        Ptr{Void}, (Ptr{Uint8},), bytestring(filename))
    w = ccall(dlsym(_jl_libcairo,:cairo_image_surface_get_width),
        Int32, (Ptr{Void},), ptr)
    h = ccall(dlsym(_jl_libcairo,:cairo_image_surface_get_height),
        Int32, (Ptr{Void},), ptr)
    CairoSurface(ptr, w, h)
end

function write_to_png(surface::CairoSurface, filename::String)
    ccall(dlsym(_jl_libcairo,:cairo_surface_write_to_png), Void,
        (Ptr{Uint8},Ptr{Uint8}), surface.ptr, bytestring(filename))
end

function surface_create_similar(s::CairoSurface, w, h)
    ptr = ccall(dlsym(_jl_libcairo,:cairo_surface_create_similar), Ptr{Void},
                (Ptr{Void}, Int32, Int32, Int32),
                s.ptr, CAIRO_CONTENT_COLOR_ALPHA, w, h)
    CairoSurface(ptr, w, h)
end

# -----------------------------------------------------------------------------

type CairoContext
    ptr::Ptr{Void}
    surface::CairoSurface
    layout::Ptr{Void} # cache PangoLayout

    function CairoContext(surface::CairoSurface)
        ptr = ccall(dlsym(_jl_libcairo,:cairo_create),
            Ptr{Void}, (Ptr{Void},), surface.ptr)
        layout = ccall(dlsym(_jl_libpangocairo,:pango_cairo_create_layout),
            Ptr{Void}, (Ptr{Void},), ptr)
        self = new(ptr, surface, layout)
        finalizer(self, destroy)
        self
    end
end

function destroy(ctx::CairoContext)
    ccall(dlsym(_jl_libgobject,:g_object_unref), Void, (Ptr{Void},), ctx.layout)
    _destroy(ctx)
end

macro _CTX_FUNC_V(NAME, FUNCTION)
    quote
        $(esc(NAME))(ctx::CairoContext) =
            ccall(dlsym(_jl_libcairo,$(string(FUNCTION))),
                Void, (Ptr{Void},), ctx.ptr)
    end
end

@_CTX_FUNC_V _destroy cairo_destroy
@_CTX_FUNC_V save cairo_save
@_CTX_FUNC_V restore cairo_restore
@_CTX_FUNC_V show_page cairo_show_page
@_CTX_FUNC_V clip cairo_clip
@_CTX_FUNC_V clip_preserve cairo_clip_preserve
@_CTX_FUNC_V fill cairo_fill
@_CTX_FUNC_V fill_preserve cairo_fill_preserve
@_CTX_FUNC_V new_path cairo_new_path
@_CTX_FUNC_V new_sub_path cairo_new_sub_path
@_CTX_FUNC_V close_path cairo_close_path
@_CTX_FUNC_V paint cairo_paint
@_CTX_FUNC_V stroke cairo_stroke
@_CTX_FUNC_V stroke_preserve cairo_stroke_preserve

macro _CTX_FUNC_I(NAME, FUNCTION)
    quote
        $(esc(NAME))(ctx::CairoContext, i0::Integer) =
            ccall(dlsym(_jl_libcairo,$(string(FUNCTION))),
                Void, (Ptr{Void},Int32), ctx.ptr, i0)
    end
end

@_CTX_FUNC_I set_fill_type cairo_set_fill_rule

macro _CTX_FUNC_D(NAME, FUNCTION)
    quote
        $(esc(NAME))(ctx::CairoContext, d0::Real) =
            ccall(dlsym(_jl_libcairo,$(string(FUNCTION))),
                Void, (Ptr{Void},Float64), ctx.ptr, d0)
    end
end

@_CTX_FUNC_D set_line_width cairo_set_line_width
@_CTX_FUNC_D rotate cairo_rotate

macro _CTX_FUNC_DD(NAME, FUNCTION)
    quote
        $(esc(NAME))(ctx::CairoContext, d0::Real, d1::Real) =
            ccall(dlsym(_jl_libcairo,$(string(FUNCTION))),
                Void, (Ptr{Void},Float64,Float64), ctx.ptr, d0, d1)
    end
end

@_CTX_FUNC_DD line_to cairo_line_to
@_CTX_FUNC_DD move_to cairo_move_to
@_CTX_FUNC_DD rel_line_to cairo_rel_line_to
@_CTX_FUNC_DD rel_move_to cairo_rel_move_to
@_CTX_FUNC_DD scale cairo_scale
@_CTX_FUNC_DD translate cairo_translate

macro _CTX_FUNC_DDD(NAME, FUNCTION)
    quote
        $(esc(NAME))(ctx::CairoContext, d0::Real, d1::Real, d2::Real) =
            ccall(dlsym(_jl_libcairo,$(string(FUNCTION))),
                Void, (Ptr{Void},Float64,Float64,Float64), ctx.ptr, d0, d1, d2)
    end
end

@_CTX_FUNC_DDD set_source_rgb cairo_set_source_rgb

macro _CTX_FUNC_DDDD(NAME, FUNCTION)
    quote
        $(esc(NAME))(ctx::CairoContext, d0::Real, d1::Real, d2::Real, d3::Real) =
            ccall(dlsym(_jl_libcairo,$(string(FUNCTION))), Void,
                (Ptr{Void},Float64,Float64,Float64,Float64),
                ctx.ptr, d0, d1, d2, d3)
    end
end

@_CTX_FUNC_DDDD set_source_rgba cairo_set_source_rgba
@_CTX_FUNC_DDDD rectangle cairo_rectangle

macro _CTX_FUNC_DDDDD(NAME, FUNCTION)
    quote
        $(esc(NAME))(ctx::CairoContext, d0::Real, d1::Real, d2::Real, d3::Real, d4::Real) =
            ccall(dlsym(_jl_libcairo,$(string(FUNCTION))), Void,
                (Ptr{Void},Float64,Float64,Float64,Float64,Float64),
                ctx.ptr, d0, d1, d2, d3, d4)
    end
end

@_CTX_FUNC_DDDDD arc cairo_arc

function set_dash(ctx::CairoContext, dashes::Vector{Float64})
    ccall(dlsym(_jl_libcairo,:cairo_set_dash), Void,
        (Ptr{Void},Ptr{Float64},Int32,Float64), ctx.ptr, dashes, length(dashes), 0.)
end

function set_source_surface(ctx::CairoContext, s::CairoSurface, x::Real, y::Real)
    ccall(dlsym(_jl_libcairo,:cairo_set_source_surface), Void,
        (Ptr{Void},Ptr{Void},Float64,Float64), ctx.ptr, s.ptr, x, y)
end

function set_font_from_string(ctx::CairoContext, str::String)
    fontdesc = ccall(dlsym(_jl_libpangocairo,:pango_font_description_from_string),
        Ptr{Void}, (Ptr{Uint8},), bytestring(str))
    ccall(dlsym(_jl_libpangocairo,:pango_layout_set_font_description), Void,
        (Ptr{Void},Ptr{Void}), ctx.layout, fontdesc)
    ccall(dlsym(_jl_libpangocairo,:pango_font_description_free), Void,
        (Ptr{Void},), fontdesc)
end

function set_markup(ctx::CairoContext, markup::String)
    ccall(dlsym(_jl_libpangocairo,:pango_layout_set_markup), Void,
        (Ptr{Void},Ptr{Uint8},Int32), ctx.layout, bytestring(markup), -1)
end

function get_layout_size(ctx::CairoContext)
    w = Array(Int32,2)
    ccall(dlsym(_jl_libpangocairo,:pango_layout_get_pixel_size), Void,
        (Ptr{Void},Ptr{Int32},Ptr{Int32}), ctx.layout, pointer(w,1), pointer(w,2))
    w
end

function update_layout(ctx::CairoContext)
    ccall(dlsym(_jl_libpangocairo,:pango_cairo_update_layout), Void,
        (Ptr{Void},Ptr{Void}), ctx.ptr, ctx.layout)
end

function show_layout(ctx::CairoContext)
    ccall(dlsym(_jl_libpangocairo,:pango_cairo_show_layout), Void,
        (Ptr{Void},Ptr{Void}), ctx.ptr, ctx.layout)
end

# -----------------------------------------------------------------------------

_circle(ctx::CairoContext, x::Real, y::Real, r::Real) =
    arc(ctx, x, ctx.surface.height-y, r, 0., 2pi)
_move_to(ctx::CairoContext, x, y) = move_to(ctx, x, ctx.surface.height-y)
_line_to(ctx::CairoContext, x, y) = line_to(ctx, x, ctx.surface.height-y)
_rectangle(ctx::CairoContext, x::Real, y::Real, w::Real, h::Real) =
    rectangle(ctx, x, ctx.surface.height-y-h, w, h)
_rel_line_to(ctx::CairoContext, x, y) = rel_line_to(ctx, x, -y)
_rel_move_to(ctx::CairoContext, x, y) = rel_move_to(ctx, x, -y)
_translate(ctx::CairoContext, x, y) = translate(ctx, x, ctx.surface.height-y)

function set_clip_rect(ctx::CairoContext, cr)
    x = cr[1]
    y = cr[3]
    width = cr[2] - cr[1]
    height = cr[4] - cr[3]
    _rectangle(ctx, x, y, width, height)
    clip(ctx)
    new_path(ctx)
end

type RendererState
    current::Dict
    saved::Vector{Dict}

    RendererState() = new(Dict(),Dict[])
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
    self.current = Dict()
end

function restore( self::RendererState )
    self.current = self.saved[1]
    del(self.saved, 1)
end

color_to_rgb(i::Integer) = hex2rgb(i)
color_to_rgb(s::String) = name2rgb(s)

function _set_color( ctx::CairoContext, color )
    (r,g,b) = color_to_rgb( color )
    set_source_rgb( ctx, r, g, b )
end

function _set_line_type(ctx::CairoContext, nick::String)
    const nick2name = {
       "dot"       => "dotted",
       "dash"      => "shortdashed",
       "dashed"    => "shortdashed",
    }
    # XXX:should be scaled by linewidth
    const name2dashes = {
        "solid"           => Float64[],
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
    const xx2pt = { "in"=>72., "pt"=>1., "mm"=>2.835, "cm"=>28.35 }
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

EPSRenderer(filename::String, w_str::String, h_str::String) =
    EPSRenderer(filename, _str_size_to_pts(w_str), _str_size_to_pts(h_str))

function EPSRenderer(filename::String, w_pts::Float64, h_pts::Float64)
    surface = CairoEPSSurface(filename, w_pts, h_pts)
    r = CairoRenderer(surface)
    r.upperright = (w_pts,h_pts)
    r.on_close = () -> show_page(r.ctx)
    r
end

function SVGRenderer(stream::IOStream, w::Real, h::Real)
    surface = CairoSVGSurface(stream, w, h)
    r = CairoRenderer(surface)
    r.upperright = (w,h)
    #r.on_close = () -> show_page(r.ctx)
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

const __pl_style_func = {
    "color"     => _set_color,
    "linecolor" => _set_color,
    "fillcolor" => _set_color,
    "linestyle" => _set_line_type,
    "linetype"  => _set_line_type,
    "linewidth" => set_line_width,
    "filltype"  => set_fill_type,
    "cliprect"  => set_clip_rect,
}

function set( self::CairoRenderer, key::String, value )
    set(self.state, key, value )
    if key == "fontface"
        fontsize = get(self, "fontsize", 12)
        set_font_from_string(self.ctx, "$value $(fontsize)px")
    elseif key == "fontsize"
        fontface = get(self, "fontface", "sans-serif")
        set_font_from_string(self.ctx, "$fontface $(value)px")
    elseif has(__pl_style_func, key)
        __pl_style_func[key](self.ctx, value)
    end
end

function get(self::CairoRenderer, parameter::String, notfound)
    return get(self.state, parameter, notfound)
end

function get(self::CairoRenderer, parameter::String)
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

stroke(cr::CairoRenderer) = stroke(cr.ctx)

function move(self::CairoRenderer, p)
    _move_to( self.ctx, p[1], p[2] )
end

function lineto( self::CairoRenderer, p )
    _line_to( self.ctx, p[1], p[2] )
end

function linetorel( self::CairoRenderer, p )
    _rel_line_to( self.ctx, p[1], p[2] )
end

function line( self::CairoRenderer, p, q )
    _move_to(self.ctx, p[1], p[2])
    _line_to(self.ctx, q[1], q[2])
    stroke(self.ctx)
end

function rect( self::CairoRenderer, p, q )
    _rectangle( self.ctx, p[1], p[2], q[1]-p[1], q[2]-p[2] )
end

function circle( self::CairoRenderer, p, r )
    _circle( self.ctx, p[1], p[2], r )
end

function ellipse( self::CairoRenderer, p, rx, ry, angle )
    ellipse( self.ctx, p[1], p[2], rx, ry, angle )
end

function arc( self::CairoRenderer, c, p, q )
    arc( self.ctx, c[1], c[2], p[1], p[2], q[1], q[2] )
end

function symbol( self::CairoRenderer, p )
    symbols( self, [p[1]], [p[2]] )
end

function symbols( self::CairoRenderer, x, y )
    fullname = get(self.state, "symboltype", "square")
    size = get(self.state, "symbolsize", 0.01)

    splitname = split(fullname)
    name = pop(splitname)
    filled = contains(splitname, "solid") || contains(splitname, "filled")

    const symbol_funcs = {
        "asterisk" => (c, x, y, r) -> (
            _move_to(c, x, y+r);
            _line_to(c, x, y-r);
            _move_to(c, x+0.866r, y-0.5r);
            _line_to(c, x-0.866r, y+0.5r);
            _move_to(c, x+0.866r, y+0.5r);
            _line_to(c, x-0.866r, y-0.5r)
        ),
        "cross" => (c, x, y, r) -> (
            _move_to(c, x+r, y+r);
            _line_to(c, x-r, y-r);
            _move_to(c, x+r, y-r);
            _line_to(c, x-r, y+r)
        ),
        "diamond" => (c, x, y, r) -> (
            _move_to(c, x, y+r);
            _line_to(c, x+r, y);
            _line_to(c, x, y-r);
            _line_to(c, x-r, y);
            close_path(c)
        ),
        "dot" => (c, x, y, r) -> (
            new_sub_path(c);
            _rectangle(c, x, y, 1., 1.)
        ),
        "plus" => (c, x, y, r) -> (
            _move_to(c, x+r, y);
            _line_to(c, x-r, y);
            _move_to(c, x, y+r);
            _line_to(c, x, y-r)
        ),
        "square" => (c, x, y, r) -> (
            new_sub_path(c);
            _rectangle(c, x-0.866r, y-0.866r, 1.732r, 1.732r)
        ),
        "triangle" => (c, x, y, r) -> (
            _move_to(c, x, y+r);
            _line_to(c, x+0.866r, y-0.5r);
            _line_to(c, x-0.866r, y-0.5r);
            close_path(c)
        ),
        "down-triangle" => (c, x, y, r) -> (
            _move_to(c, x, y-r);
            _line_to(c, x+0.866r, y+0.5r);
            _line_to(c, x-0.866r, y+0.5r);
            close_path(c)
        ),
        "right-triangle" => (c, x, y, r) -> (
            _move_to(c, x+r, y);
            _line_to(c, x-0.5r, y+0.866r);
            _line_to(c, x-0.5r, y-0.866r);
            close_path(c)
        ),
        "left-triangle" => (c, x, y, r) -> (
            _move_to(c, x-r, y);
            _line_to(c, x+0.5r, y+0.866r);
            _line_to(c, x+0.5r, y-0.866r);
            close_path(c)
        ),
    }
    default_symbol_func = (ctx,x,y,r) -> (
        new_sub_path(ctx);
        _circle(ctx,x,y,r)
    )
    symbol_func = get(symbol_funcs, name, default_symbol_func)

    save(self.ctx)
    set_dash(self.ctx, Float64[])
    new_path(self.ctx)
    for i = 1:min(length(x),length(y))
        symbol_func(self.ctx, x[i], y[i], 0.5*size)
    end
    if filled
        fill_preserve(self.ctx)
    end
    stroke(self.ctx)
    restore(self.ctx)
end

function curve( self::CairoRenderer, x::Vector, y::Vector )
    n = min(length(x), length(y))
    if n <= 0
        return
    end
    new_path(self.ctx)
    _move_to(self.ctx, x[1], y[1])
    for i = 2:n
        _line_to( self.ctx, x[i], y[i] )
    end
    stroke(self.ctx)
end

function image(r::CairoRenderer, s::CairoSurface, x, y, w, h)
    _rectangle(r.ctx, x, y, w, h)
    save(r.ctx)
    _translate(r.ctx, x, y+h)
    scale(r.ctx, w/s.width, h/s.height)
    set_source_surface(r.ctx, s, 0, 0)
    fill(r.ctx)
    restore(r.ctx)
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

function layout_text(self::CairoRenderer, text::String)
    markup = tex2pango(text, get(self,"fontsize"))
    set_markup(self.ctx, markup)
end

function text( self::CairoRenderer, p, text )
    halign = get( self.state, "texthalign", "center" )
    valign = get( self.state, "textvalign", "center" )
    angle = get( self.state, "textangle", 0. )

    _move_to( self.ctx, p[1], p[2] )
    save(self.ctx)
    rotate(self.ctx, -angle*pi/180.)

    layout_text(self, text)
    update_layout(self.ctx)

    const _xxx = {
        "center"    => 0.5,
        "left"      => 0.,
        "right"     => 1.,
        "top"       => 0.,
        "bottom"    => 1.,
    }
    extents = get_layout_size(self.ctx)
    dx = -_xxx[halign]*extents[1]
    dy = _xxx[valign]*extents[2]
    _rel_move_to(self.ctx, dx, dy)

    show_layout(self.ctx)
    restore(self.ctx)
end

function textwidth( self::CairoRenderer, str )
    layout_text(self, str)
    extents = get_layout_size(self.ctx)
    extents[1]
end

function textheight( self::CairoRenderer, str )
    get( self.state, "fontsize" ) ## XXX: kludge?
end

type TeXLexer
    str::String
    len::Int
    pos::Int
    token_stack::Array{String,1}
    re_control_sequence::Regex

    function TeXLexer( str::String )
        self = new()
        self.str = str
        self.len = strlen(str)
        self.pos = 1
        self.token_stack = String[]
        self.re_control_sequence = r"^\\[a-zA-Z]+[ ]?|^\\[^a-zA-Z][ ]?"
        self
    end
end

function get_token( self::TeXLexer )
    if self.pos == self.len+1
        return nothing
    end

    if length(self.token_stack) > 0
        return pop(self.token_stack)
    end

    str = self.str[self.pos:end]
    m = match(self.re_control_sequence, str)
    if m != nothing
        token = m.match
        self.pos = self.pos + strlen(token)
        # consume trailing space
        if strlen(token) > 2 && token[end] == ' '
            token = token[1:end-1]
        end
    else
        token = str[1:1]
        self.pos = self.pos + 1
    end

    return token
end

function put_token( self::TeXLexer, token )
    push( self.token_stack, token )
end

function peek( self::TeXLexer )
    token = get_token(self)
    put_token( self, token )
    return token
end

const _common_token_dict = {
    L"\{"               => L"{",
    L"\}"               => L"}",
    L"\_"               => L"_",
    L"\^"               => L"^",
    L"\-"               => L"-",

    ## ignore stray brackets
    L"{"                => L"",
    L"}"                => L"",
}

const _text_token_dict = {
    ## non-math symbols (p438)
    L"\S"               => E"\ua7",
    L"\P"               => E"\ub6",
    L"\dag"             => E"\u2020",
    L"\ddag"            => E"\u2021",
}

const _math_token_dict = {

    L"-"                => E"\u2212", # minus sign

    ## spacing
    L"\quad"            => E"\u2003", # 1 em
    L"\qquad"           => E"\u2003\u2003", # 2 em
    L"\,"               => E"\u2006", # 3/18 em
    L"\>"               => E"\u2005", # 4/18 em
    L"\;"               => E"\u2004", # 5/18 em

    ## lowercase greek
    L"\alpha"           => E"\u03b1",
    L"\beta"            => E"\u03b2",
    L"\gamma"           => E"\u03b3",
    L"\delta"           => E"\u03b4",
    L"\epsilon"         => E"\u03b5",
    L"\varepsilon"      => E"\u03f5",
    L"\zeta"            => E"\u03b6",
    L"\eta"             => E"\u03b7",
    L"\theta"           => E"\u03b8",
    L"\vartheta"        => E"\u03d1",
    L"\iota"            => E"\u03b9",
    L"\kappa"           => E"\u03ba",
    L"\lambda"          => E"\u03bb",
    L"\mu"              => E"\u03bc",
    L"\nu"              => E"\u03bd",
    L"\xi"              => E"\u03be",
    L"\omicron"         => E"\u03bf",
    L"\pi"              => E"\u03c0",
    L"\varpi"           => E"\u03d6",
    L"\rho"             => E"\u03c1",
    L"\varrho"          => E"\u03f1",
    L"\sigma"           => E"\u03c3",
    L"\varsigma"        => E"\u03c2",
    L"\tau"             => E"\u03c4",
    L"\upsilon"         => E"\u03c5",
    L"\phi"             => E"\u03d5",
    L"\varphi"          => E"\u03c6",
    L"\chi"             => E"\u03c7",
    L"\psi"             => E"\u03c8",
    L"\omega"           => E"\u03c9",

    ## uppercase greek
    L"\Alpha"           => E"\u0391",
    L"\Beta"            => E"\u0392",
    L"\Gamma"           => E"\u0393",
    L"\Delta"           => E"\u0394",
    L"\Epsilon"         => E"\u0395",
    L"\Zeta"            => E"\u0396",
    L"\Eta"             => E"\u0397",
    L"\Theta"           => E"\u0398",
    L"\Iota"            => E"\u0399",
    L"\Kappa"           => E"\u039a",
    L"\Lambda"          => E"\u039b",
    L"\Mu"              => E"\u039c",
    L"\Nu"              => E"\u039d",
    L"\Xi"              => E"\u039e",
    L"\Pi"              => E"\u03a0",
    L"\Rho"             => E"\u03a1",
    L"\Sigma"           => E"\u03a3",
    L"\Tau"             => E"\u03a4",
    L"\Upsilon"         => E"\u03a5",
    L"\Phi"             => E"\u03a6",
    L"\Chi"             => E"\u03a7",
    L"\Psi"             => E"\u03a8",
    L"\Omega"           => E"\u03a9",

    ## miscellaneous
    L"\aleph"           => E"\u2135",
    L"\hbar"            => E"\u210f",
    L"\ell"             => E"\u2113",
    L"\wp"              => E"\u2118",
    L"\Re"              => E"\u211c",
    L"\Im"              => E"\u2111",
    L"\partial"         => E"\u2202",
    L"\infty"           => E"\u221e",
    L"\prime"           => E"\u2032",
    L"\emptyset"        => E"\u2205",
    L"\nabla"           => E"\u2206",
    L"\surd"            => E"\u221a",
    L"\top"             => E"\u22a4",
    L"\bot"             => E"\u22a5",
    L"\|"               => E"\u2225",
    L"\angle"           => E"\u2220",
    L"\triangle"        => E"\u25b3", # == \bigtriangleup
    L"\backslash"       => E"\u2216",
    L"\forall"          => E"\u2200",
    L"\exists"          => E"\u2203",
    L"\neg"             => E"\uac",
    L"\flat"            => E"\u266d",
    L"\natural"         => E"\u266e",
    L"\sharp"           => E"\u266f",
    L"\clubsuit"        => E"\u2663",
    L"\diamondsuit"     => E"\u2662",
    L"\heartsuit"       => E"\u2661",
    L"\spadesuit"       => E"\u2660",

    ## large operators
    L"\sum"             => E"\u2211",
    L"\prod"            => E"\u220f",
    L"\coprod"          => E"\u2210",
    L"\int"             => E"\u222b",
    L"\oint"            => E"\u222e",
    L"\bigcap"          => E"\u22c2",
    L"\bigcup"          => E"\u22c3",
    L"\bigscup"         => E"\u2a06",
    L"\bigvee"          => E"\u22c1",
    L"\bigwedge"        => E"\u22c0",
    L"\bigodot"         => E"\u2a00",
    L"\bigotimes"       => E"\u2a02",
    L"\bigoplus"        => E"\u2a01",
    L"\biguplus"        => E"\u2a04",

    ## binary operations
    L"\pm"              => E"\ub1",
    L"\mp"              => E"\u2213",
    L"\setminus"        => E"\u2216",
    L"\cdot"            => E"\u22c5",
    L"\times"           => E"\ud7",
    L"\ast"             => E"\u2217",
    L"\star"            => E"\u22c6",
    L"\diamond"         => E"\u22c4",
    L"\circ"            => E"\u2218",
    L"\bullet"          => E"\u2219",
    L"\div"             => E"\uf7",
    L"\cap"             => E"\u2229",
    L"\cup"             => E"\u222a",
    L"\uplus"           => E"\u228c", # 228e?
    L"\sqcap"           => E"\u2293",
    L"\sqcup"           => E"\u2294",
    L"\triangleleft"    => E"\u22b2",
    L"\triangleright"   => E"\u22b3",
    L"\wr"              => E"\u2240",
    L"\bigcirc"         => E"\u25cb",
    L"\bigtriangleup"   => E"\u25b3", # == \triangle
    L"\bigtriangledown" => E"\u25bd",
    L"\vee"             => E"\u2228",
    L"\wedge"           => E"\u2227",
    L"\oplus"           => E"\u2295",
    L"\ominus"          => E"\u2296",
    L"\otimes"          => E"\u2297",
    L"\oslash"          => E"\u2298",
    L"\odot"            => E"\u2299",
    L"\dagger"          => E"\u2020",
    L"\ddagger"         => E"\u2021",
    L"\amalg"           => E"\u2210",

    ## relations
    L"\leq"             => E"\u2264",
    L"\prec"            => E"\u227a",
    L"\preceq"          => E"\u227c",
    L"\ll"              => E"\u226a",
    L"\subset"          => E"\u2282",
    L"\subseteq"        => E"\u2286",
    L"\sqsubseteq"      => E"\u2291",
    L"\in"              => E"\u2208",
    L"\vdash"           => E"\u22a2",
    L"\smile"           => E"\u2323",
    L"\frown"           => E"\u2322",
    L"\geq"             => E"\u2265",
    L"\succ"            => E"\u227b",
    L"\succeq"          => E"\u227d",
    L"\gg"              => E"\u226b",
    L"\supset"          => E"\u2283",
    L"\supseteq"        => E"\u2287",
    L"\sqsupseteq"      => E"\u2292",
    L"\ni"              => E"\u220b",
    L"\dashv"           => E"\u22a3",
    L"\mid"             => E"\u2223",
    L"\parallel"        => E"\u2225",
    L"\equiv"           => E"\u2261",
    L"\sim"             => E"\u223c",
    L"\simeq"           => E"\u2243",
    L"\asymp"           => E"\u224d",
    L"\approx"          => E"\u2248",
    L"\cong"            => E"\u2245",
    L"\bowtie"          => E"\u22c8",
    L"\propto"          => E"\u221d",
    L"\models"          => E"\u22a7", # 22a8?
    L"\doteq"           => E"\u2250",
    L"\perp"            => E"\u27c2",

    ## arrows
    L"\leftarrow"       => E"\u2190",
    L"\Leftarrow"       => E"\u21d0",
    L"\rightarrow"      => E"\u2192",
    L"\Rightarrow"      => E"\u21d2",
    L"\leftrightarrow"  => E"\u2194",
    L"\Leftrightarrow"  => E"\u21d4",
    L"\mapsto"          => E"\u21a6",
    L"\hookleftarrow"   => E"\u21a9",
    L"\leftharpoonup"   => E"\u21bc",
    L"\leftharpoondown" => E"\u21bd",
    L"\rightleftharpoons" => E"\u21cc",
    L"\longleftarrow"   => E"\u27f5",
    L"\Longleftarrow"   => E"\u27f8",
    L"\longrightarrow"  => E"\u27f6",
    L"\Longrightarrow"  => E"\u27f9",
    L"\longleftrightarrow" => E"\u27f7",
    L"\Longleftrightarrow" => E"\u27fa",
    L"\hookrightarrow"  => E"\u21aa",
    L"\rightharpoonup"  => E"\u21c0",
    L"\rightharpoondown" => E"\u21c1",
    L"\uparrow"         => E"\u2191",
    L"\Uparrow"         => E"\u21d1",
    L"\downarrow"       => E"\u2193",
    L"\Downarrow"       => E"\u21d3",
    L"\updownarrow"     => E"\u2195",
    L"\Updownarrow"     => E"\u21d5",
    L"\nearrow"         => E"\u2197",
    L"\searrow"         => E"\u2198",
    L"\swarrow"         => E"\u2199",
    L"\nwarrow"         => E"\u2196",

    ## openings
#    L"\lbrack"          => E"[",
#    L"\lbrace"          => E"{",
    L"\langle"          => E"\u27e8",
    L"\lfloor"          => E"\u230a",
    L"\lceil"           => E"\u2308",

    ## closings
#    L"\rbrack"          => E"]",
#    L"\rbrace"          => E"}",
    L"\rangle"          => E"\u27e9",
    L"\rfloor"          => E"\u230b",
    L"\rceil"           => E"\u2309",

    ## alternate names
    L"\ne"              => E"\u2260",
    L"\neq"             => E"\u2260",
    L"\le"              => E"\u2264",
    L"\ge"              => E"\u2265",
    L"\to"              => E"\u2192",
    L"\gets"            => E"\u2192",
    L"\owns"            => E"\u220b",
    L"\land"            => E"\u2227",
    L"\lor"             => E"\u2228",
    L"\lnot"            => E"\uac",
    L"\vert"            => E"\u2223",
    L"\Vert"            => E"\u2225",

    ## extensions
    L"\deg"             => E"\ub0",
    L"\degr"            => E"\ub0",
    L"\degree"          => E"\ub0",
    L"\degrees"         => E"\ub0",
    L"\arcdeg"          => E"\ub0",
    L"\arcmin"          => E"\u2032",
    L"\arcsec"          => E"\u2033",
}

function map_text_token(token::String)
    if has(_text_token_dict, token)
        return _text_token_dict[token]
    else
        return get(_common_token_dict, token, token )
    end
end

function map_math_token(token::String)
    if has(_math_token_dict, token)
        return _math_token_dict[token]
    else
        return get(_common_token_dict, token, token )
    end
end

function math_group(lexer::TeXLexer)
    output = ""
    bracketmode = false
    while true
        token = get_token(lexer)
        if token == nothing
            break
        end

        if token == L"{"
            bracketmode = true
        elseif token == L"}"
            break
        else
            output = strcat(output, map_math_token(token))
            if !bracketmode
                break
            end
        end
    end
    return output
end

#font_code = [ L"\f0", L"\f1", L"\f2", L"\f3" ]

function tex2pango( str::String, fontsize::Real )
    output = ""
    mathmode = true
    font_stack = {}
    font = 1
    script_size = fontsize/1.618034

    lexer = TeXLexer(str)
    while true
        token = get_token(lexer)
        if token == nothing
            break
        end

        more_output = ""

        if token == L"$"
#            mathmode = !mathmode
            more_output = L"$"
        elseif token == L"{"
            push(font_stack, font)
        elseif token == L"}"
            old_font = pop(font_stack)
            if old_font != font
                font = old_font
#                more_output = font_code[font]
            end
        elseif token == L"\rm"
            font = 1
#            more_output = font_code[font]
        elseif token == L"\it"
            font = 2
#            more_output = font_code[font]
        elseif token == L"\bf"
            font = 3
#            more_output = font_code[font]
        elseif !mathmode
            more_output = map_text_token(token)
        elseif token == L"_"
            more_output = strcat("<sub><span font=\"$script_size\">", math_group(lexer), L"</span></sub>")
            #if peek(lexer) == L"^"
            #    more_output = strcat(L"\mk", more_output, L"\rt")
            #end
        elseif token == L"^"
            more_output = strcat("<sup><span font=\"$script_size\">", math_group(lexer), L"</span></sup>")
            #if peek(lexer) == L"_"
            #    more_output = strcat(L"\mk", more_output, L"\rt")
            #end
        else
            more_output = map_math_token(token)
        end

        output = strcat(output, more_output)
    end

    return output
end

end  # module
