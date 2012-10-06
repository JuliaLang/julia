import Tk.*
import Cairo.*

function sketch_window()
    w = Window("drawing", 400, 300)
    c = Canvas(w)
    pack(c)
    lastx = 0
    lasty = 0
    cr = cairo_context(c)
    set_source_rgb(cr, 1, 1, 1)
    paint(cr)
    reveal(c)
    set_source_rgb(cr, 0, 0, 0.85)
    c.mouse.button1press = function (c, x, y)
        lastx = x; lasty = y
    end
    c.mouse.button1motion = function (c, x, y)
        move_to(cr, lastx, lasty)
        line_to(cr, x, y)
        stroke(cr)
        reveal(c)
        lastx = x; lasty = y
    end
    c
end
