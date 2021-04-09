mutable struct Pager{C} <: _ConfiguredMenu{C}
    lines::Vector{String}
    pagesize::Int
    pageoffset::Int
    selected::Nothing
    config::C
end

function Pager(text::AbstractString; pagesize::Int=10, kwargs...)
    lines = readlines(IOBuffer(text))
    return Pager(lines, pagesize, 0, nothing, Config(; kwargs...))
end

function header(p::Pager)
    total = length(p.lines)
    current = min(p.pageoffset + p.pagesize, total)
    percent = round(Int, (current / total) * 100)
    return "($(lpad(current, ndigits(total))) / $total) $(lpad(percent, 3))%"
end

options(p::Pager) = p.lines

cancel(::Pager) = nothing

pick(::Pager, ::Int) = true

function writeline(buf::IOBuffer, pager::Pager{Config}, idx::Int, iscursor::Bool)
    print(buf, pager.lines[idx])
end

function pager(terminal, object)
    lines, columns = displaysize(terminal)::Tuple{Int,Int}
    columns -= 3
    buffer = IOBuffer()
    ctx = IOContext(buffer, :color => REPL.Terminals.hascolor(terminal), :displaysize => (lines, columns))
    show(ctx, "text/plain", object)
    pager = Pager(String(take!(buffer)); pagesize = div(lines, 2))
    return request(terminal, pager)
end
pager(object) = pager(terminal, object)
