module GUI

using Trees

import Trees: destroy!

export # types
    AbstractButton,
    AbstractCanvas,
    AbstractCanvas3D,
    AbstractFrame,
    AbstractWidget,
    AbstractWindow,
    # functions
    button,
    draw,
    frame,
    resize,
    height,
    width,
    window

abstract GraphicsObject

typealias GraphicsHandle{T<:GraphicsObject} AryNode{T}  # I _love_ that this is possible!

graphicsobject(p::GraphicsHandle, obj::GraphicsObject) = AryNode(p, obj)

type GraphicsRoot <: GraphicsObject; end

const graphicsroot = AryNode{GraphicsRoot}()

destroy!() = destroy!(graphicsroot)

function draw(obj::GraphicsHandle)
    for c in obj.children
        draw(c)
    end
    draw(get(obj))
end

draw(n::Nothing) = nothing

function resize(obj::GraphicsHandle)
    for c in obj.children
        resize(c)
    end
    resize(get(obj))
end

resize(n::Nothing) = nothing

width(obj::GraphicsHandle) = width(get(obj))
height(obj::GraphicsHandle) = height(get(obj))

abstract AbstractWindow <: GraphicsObject
abstract AbstractFrame <: GraphicsObject
abstract AbstractCanvas <: GraphicsObject
abstract AbstractCanvas3D <: GraphicsObject
abstract AbstractWidget <: GraphicsObject
abstract AbstractButton <: GraphicsObject

# This is predicated on a constructor Window() in a specific graphics toolkit wrapper
# Will this mod trick work??
window(title::String, w::Integer, h::Integer; mod::Module = current_module()) = AryNode(graphicsroot, mod.Window(title, int(w), int(h)))

function frame{T<:Union(AbstractWindow,AbstractFrame)}(win::AryNode{T}; col::Integer = 1, row::Integer = 1, justify = "nsew", color = (0.85,0.85,0.85), mod::Module = current_module())
    f = AryNode(win, mod.Frame(color))
    mod.grid(f, col, row, justify)
    f
end

function button{F<:AbstractFrame}(f::AryNode{F}, msg::String, callback::Function; col::Integer = 1, row::Integer = 1, justify = "nsew", mod::Module = current_module())
    b = AryNode(f, mod.Button(msg, callback))
    mod.grid(f, col, row, justify)
    b
end

type MouseHandler
    button1press
    button1release
    button2press
    button2release
    button3press
    button3release
    motion
    button1motion

    MouseHandler() = new(default_mouse_cb, default_mouse_cb, default_mouse_cb,
                         default_mouse_cb, default_mouse_cb, default_mouse_cb,
                         default_mouse_cb, default_mouse_cb)
end

end
