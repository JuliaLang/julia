load("plot")
load("tk")

using Tk, Cairo, Winston, Plot

function TkRenderer(name, w, h)
    win = Window(name, w, h)
    c = Canvas(win)
    pack(c)
    cr = cairo_context(c)
    set_source_rgb(cr, 1, 1, 1)
    paint(cr)
    r = CairoRenderer(cairo_surface(c))
    r.upperright = (w,h)
    r.on_close = () -> reveal(c)
    r
end

function tk(self::PlotContainer, w, h)
    dev = TkRenderer("plot", w, h)
    Winston.page_compose(self, dev, false)
    dev.on_close()
end

function imagesc(I)
    h, w = size(I)
    pl = Plot.imagesc((0,w),(0,h),I)
    tk(pl, w, h)
end
