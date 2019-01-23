"""
    trace_map(p::Plot, axis::Symbol=:x)

Return an array of `length(p.data)` that maps each element of `p.data` into an
integer for which number axis of kind `axis` that trace belogs to. `axis` can
either be `x` or `y`. If `x` is given, return the integer for which x-axis the
trace belongs to. Similar for `y`.
"""
function trace_map(p::Plot, axis=:x)
    out = fill(1, length(p.data))
    ax_key = axis == :x ? :xaxis : :yaxis
    for (i, t) in enumerate(p.data)
        if haskey(t, ax_key)
            out[i] = parse(Int, t[ax_key][2:end])
        end
    end
    out
end
