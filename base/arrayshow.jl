# This file is a part of Julia. License is MIT: https://julialang.org/license

# methods related to array printing

# Printing a value requires to take into account the :typeinfo property
# from the IO context; this property encodes (as a type) the type information
# that is supposed to have already been displayed concerning this value,
# so that redundancy can be avoided. For example, when printing an array of
# `Float16` values, the header "Float16" will be printed, and the values
# can simply be printed with the decimal representations:
# show(Float16(1)) -> "Float16(1.0)"
# show([Float16(1)]) -> "Float16[1.0]" (instead of "Float16[Float16(1.0)]")
# Similarly:
# show([[Float16(1)]]) -> "Array{Float16}[[1.0]]" (instead of "Array{Float16}[Float16[1.0]]")
#
# The array printing methods here can be grouped into two categories (and are annotated as such):
# 1) "typeinfo aware" : these are "API boundaries" functions, which will read the typeinfo
#    property from the context, and pass down to their value an updated property
#    according to its eltype; at each layer of nesting, only one "typeinfo aware"
#    function must be called;
# 2) "typeinfo agnostic": these are helper functions used by the first category; hence
#    they don't manipulate the typeinfo property, and let the printing routines
#    for their elements read directly the property set by their callers
#
# Non-annotated functions are even lower level (e.g. print_matrix_row), so they fall
# by default into category 2.
#
# The basic organization of this file is
# 1) printing with `display` (docs: "best visualization")
# 2) printing with 3-argument `show` ("verbose pretty-print")
# 3) printing with 2-argument `show` ("repr string")
# 4) Logic for displaying type information


## printing with `display`

function show(io::IO, ::MIME"text/plain", @nospecialize(X::AbstractArray))
    get(io, :limit, false)::Bool || return array_show(io, MIME"text/plain"(), X)
    sz = displaysize(io)::Tuple{Int,Int}
    sh, sw = sz[1] - 4, sz[2] + 1
    dims = ndims(X)

    print(io, type_abbreviation(summary(X), sw))
    io = IOContext(io, :typeinfo => eltype(X))
    isempty(X) && return
    if all(x->length(x) == 1, axes(X))
        print(io, ":\n ")
        _display_capped(io, X, firstindex.(axes(X)))
        return
    end

    X = view(X, ntuple(i->axes(X, i), Val(6))..., firstindex.(axes(X, i) for i in 7:dims)...)
    h0, v0, h1, v1, h2, v2 = ax = axes(X)

    if !haskey(io, :compact) && any(length(i) > 1 for i=(v0, v1, v2))
        io = IOContext(io, :compact => true)
    end

    h0, v0, h1, v1, h2, v2 = _trim_axes(sh, sw, h0, v0, h1, v1, h2, v2)

    align = _alignment(io, X, h0, v0, h1, v1, h2, v2)

    h0, v0, h1, v1, h2, v2 = _trim_cols(align, sw, h0, v0, h1, v1, h2, v2)

    if (h0, v0, h1, v1, h2, v2) == ax && dims <= 6
        println(io, ':')
    elseif all(!isnothing, [h0; v0])
        print(io, " (showing [:, :")
        dims > 2 && begin
            print(io, ax[3] == h1 ? ", :" : ", $(length(h1) > 1 ? h1[1:end] : h1[])")
        dims > 3 end && begin
            print(io, ax[4] == v1 ? ", :" : ", $(length(v1) > 1 ? v1[1:end] : v1[])")
        dims > 4 end && begin
            print(io, ax[5] == h2 ? ", :" : ", $(length(h2) > 1 ? h2[1:end] : h2[])")
        dims > 5 end && begin
            print(io, ax[6] == v2 ? ", :" : ", $(length(v2) > 1 ? v2[1:end] : v2[])")
        dims > 6 end && begin
            print(io, ", 1"^(dims-6))
        end
        println(io, "]):")
    elseif (length(h0) > 2 || all(!isnothing, h0)) && (length(v0) > 2 || all(!isnothing, v0))
        print(io, " (eliding ")
        h0 != ax[1] && print(io, "$(length(ax[1])-length(h0)+1) rows")
        h0 != ax[1] && v0 != ax[2] && print(io, " and ")
        v0 != ax[2] && print(io, "$(length(ax[2])-length(v0)+1) cols")
        println(io, "):")
    else
        print(io, ": …")
        return
    end

    if all(x->length(x) == 1, ax)
        print(io, ' ')
        _display_capped(io, X, firstindex.(ax))
    else
        _display_matrix(io, X, align, h0, v0, h1, v1, h2, v2)
    end
end

"limit axis values to things that could possibly fit on the screen"
function _trim_axes(sh, sw, h0, v0, h1, v1, h2, v2)
    if (res = (sw+2) ÷ (3length(v0) * length(v1) + length(v1) + 2)) > 0
        res < length(v2) && (v2 = v2[1:res])
    elseif (res = sw ÷ (3length(v0) + 1)) > 0
        v2 = v2[1:1]
        res < length(v1) && (v1 = v1[1:res])
    else
        v2 = v2[1:1]
        v1 = v1[1:1]
        if sw < length(v0)
            v0 = [v0[1:~-sw÷3÷2 - (~-sw÷3-1)%2]; nothing; v0[end - ~-sw÷3÷2 + 1:end]]
            h1, h2 = h1[1:1], h2[1:1]
        end
    end

    if (res = (sh+2) ÷ (length(h0) * length(h1) + length(h1) + 1)) > 0
        res < length(h2) && (h2 = h2[1:res])
    elseif (res = (sh+1) ÷ (length(h0) + 1)) > 0
        h2 = h2[1:1]
        res < length(h1) && (h1 = h1[1:res])
    else
        h2 = h2[1:1]
        h1 = h1[1:1]
        if sh < length(h0)
            h0 = [h0[1:sh÷2 - ~-sh%2]; nothing; h0[end-sh÷2+1:end]]
            v1, v2 = v1[1:1], v2[1:1]
        end
    end
    h0, v0, h1, v1, h2, v2
end

_alignment(io::IO, X, h0, v0, h1, v1, h2, v2) = Dict(
    col => max.((0, 0, 0), (
        _display_alignment(io, X, row, col)
        for row in Iterators.product(h0, h1, h2)
        if !isnothing(row[1])
    )...)
    for col in Iterators.product(v0, v1, v2)
    if !isnothing(col[1])
)

function _trim_cols(align, sw, h0, v0, h1, v1, h2, v2)
    elided = any(isnothing, v0)
    width = sum(ali[3]+2 for ali in values(align); init=0) +
        length(v1)*length(v2) + 2length(v2) - 2 + 3elided

    while width > sw
        if length(v2) > 1
            v2 = v2[1:end-1]
        elseif length(v1) > 1
            v1 = v1[1:end-1]
        else
            v0 = [v0[1:-~end÷2-end%2-1]; nothing; v0[-~end÷2-~end%2+1:end]]
            elided || ((h1, h2, elided) = (h1[1:1], h2[1:1], true))
        end
        width = sum(align[ind][3]+2 for ind in Iterators.product(v0, v1, v2) if !isnothing(ind[1]); init=0) +
            length(v1)*length(v2) + 2length(v2) - 2 + 3elided
    end

    h0, v0, h1, v1, h2, v2
end

"print out the matrix at provided indices"
function _display_matrix(io::IO, X, align, h0, v0, h1, v1, h2, v2)
    for hk in h2
        for hj in h1
            for hi in h0
                if !isnothing(hi)
                    _print_matrix_row(io, X, align, hk, hj, hi, v0, v1, v2)
                else
                    _print_ellipsis_row(io, align, v0, v1, v2)
                end
                hi == h0[end] && hj == h1[end] && hk == h2[end] || println(io)
            end
            if hj != h1[end]
                _print_matrix_floor(io, align, v0, v1, v2, "─", "─┼─", "─┨ ┠─")
            end
        end
        if hk != h2[end]
            _print_matrix_floor(io, align, v0, v1, v2, "━", "━┷━", "━┛ ┗━")
            _print_matrix_floor(io, align, v0, v1, v2, "━", "━┯━", "━┓ ┏━")
        end
    end
end

"print out one row of the matrix"
function _print_matrix_row(io::IO, X, align, hk, hj, hi, v0, v1, v2)
    for vk in v2
        for vj in v1
            for vi in v0
                if !isnothing(vi)
                    offset = _offsets(io, X, (hi, hj, hk), (vi, vj, vk), align[vi, vj, vk])
                    print(io, " " ^ offset[1])
                    _display_capped(io, X, (hi, vi, hj, vj, hk, vk))
                    (vk,vj,vi) == (v2[end],v1[end],v0[end]) || print(io, " " ^ offset[2])
                else
                    print(io, " ⋯ ")
                end
            end
            if vj != v1[end]
                printstyled(io, "│", color=:yellow)
            end
        end
        if vk != v2[end]
            printstyled(io, "┃ ┃", color=:yellow)
        end
    end
end

"print out a divider to separate the higher dimensions"
function _print_matrix_floor(io::IO, align, v0, v1, v2, line, inter1, inter2)
    print(io, ' ')
    for vk in v2
        for vj in v1
            for vi in v0
                printstyled(io, line^align[vi, vj, vk][3], color=:yellow)
            end
            printstyled(io, line^(2v0[end] - 2), color=:yellow)
            if vj != v1[end]
                printstyled(io, inter1, color=:yellow)
            end
        end
        if vk != v2[end]
            printstyled(io, inter2, color=:yellow)
        end
    end
    println(io)
end

"print horizontal row of ellipsis"
function _print_ellipsis_row(io::IO, align, v0, v1, v2)
    for vi in v0
        if !isnothing(vi)
            buff = " "^(2+align[vi, v1[], v2[]][3])
            print(io, buff[1:~-end÷2] * "⋮" * (vi==v0[end] ? "" : buff[-~end÷2+1:end]))
        else
            print(io, " ⋱ ")
        end
    end
end

"calculate buffer needed on each side for proper alignment"
function _offsets(io::IO, X, row, col, align)
    ali = _display_alignment(io, X, row, col)
    offset = (align .- ali) .+ (1, 1, 2+ali[3])
    if ali[1] == 0
        offset = 1, offset[3] - ali[2] - 1
    else
        offset = offset[1], offset[3] - offset[1] - sum(ali[1:2])
    end
end

"element print function that promises to stay within a limit"
function _display_capped(io::IO, X, inds, limit=0)
    if limit == 0  # set default limit based on screen size and :compact
        width = displaysize(io)[2]
        limit = get(io, :compact, false)::Bool ? min(40, width÷2) : width
    end
    if isassigned(X, inds...)
        elm = X[inds...]
        if elm isa String && limit > 16
            x = sprint((io,elm)->show(io, MIME"text/plain"(), elm; limit), elm; context=io, sizehint=0)
        else
            x = dash(show, MIME"text/plain"(), elm; context=io, sizehint=0, limit=2limit)
            (occursin('\n', x) || textwidth(x |> ANSIIterator) >= limit) && (x = dash(show, elm, context=io, sizehint=0, limit=2limit))
            (occursin('\n', x) || textwidth(x |> ANSIIterator) >= limit) && (x = '<' * summary(elm) * '>')
            (occursin('\n', x) || textwidth(x |> ANSIIterator) >= limit) && (x = split(x, '\n')[1] * '>')
            (                     textwidth(x |> ANSIIterator) >= limit) && (x = type_abbreviation(x, limit))
            (                     textwidth(x |> ANSIIterator) >= limit) && (x = x[1:limit-2] * "…>")
        end
    else
        x = undef_ref_str
    end
    x = try replace_in_print_matrix(parent(parent(X)), inds[1], inds[2], x) catch e x end
    print(io, x)
end

function type_abbreviation(s::AbstractString, limit)
    nest = length(s) ÷ 2
    while textwidth(s |> ANSIIterator) > limit
        io = IOBuffer(s)
        d, s = colapse_braces(io, nest)
        nest = min(d, nest) - 1
        nest == 0 && break
    end
    s
end

function colapse_braces(io::IO,nest)
    let s="", depth
        while !eof(io)
            c = read(io, Char)
            if c == '{'
                d, inner = colapse_braces(io, nest-1)
                depth = try max(d, depth) catch e d end
                s *= '{' * inner * '}'
            elseif c == '}'
                depth = try depth catch e 0 end
                break
            else
                s *= c
            end
        end
        try depth+1 catch e 0 end, nest <= 0 ? '…' : s
    end
end

struct LimitedIOBuffer <: IO
    buffer::IOBuffer
    limit::Int
end

function Base.write(io::LimitedIOBuffer, b::UInt8)
    try
        position(io.buffer) >= io.limit && throw(ErrorException("Buffer limit exceeded"))
    finally
        write(io.buffer, b)
    end
end

"a limited sprint which feeds an exception to its printing function if it gets too long"
function dash(f::Function, args...; limit::Int=0, context=nothing, sizehint=0)
    s = LimitedIOBuffer(IOBuffer(;sizehint), limit)
    try
        if context isa Tuple
            f(IOContext(s, context...), args...)
        elseif context !== nothing
            f(IOContext(s, context), args...)
        else
            f(s, args...)
        end
    catch e
        contains(string(e), "Buffer limit exceeded") || rethrow(e)
    end
    takestring!(s.buffer)
end

function _display_alignment(io::IO, X, row, col, limit=0)
    if limit == 0  # set default limit based on screen size and :compact
        width = displaysize(io)[2]
        limit = get(io, :compact, false)::Bool ? min(40, width÷2) : width
    end
    inds = Iterators.flatten(zip(row, col))
    if isassigned(X, inds...)
        align = alignment(io, X[inds...])
    else
        align = (0, 6)
    end
    if sum(align) < limit
        (align..., sum(align))
    else
        width = textwidth(sprint(_display_capped, X, inds, limit, context=io) |> ANSIIterator)
        0, width, width
    end
end

# printing with 3-arg show

const undef_ref_alignment = (3,3)

function _show_matrix(io, @nospecialize(X::AbstractVecOrMat), rows::AbstractVector, cols::AbstractVector)
    align = [isassigned(X, row, col) ? alignment(io, X[row, col]) : (3, 3) for row in rows, col in cols]
    A = [max.((0, 0), align[:,i]...) for i in cols]

    for row in rows
        for col in cols
            if isassigned(X, row, col)
                a = A[col] .- alignment(io, X[row, col]) .+ 1

                # First try 3-arg show
                sx = sprint(show, "text/plain", X[row, col], context=io, sizehint=0)

                # If the output contains line breaks, try 2-arg show instead.
                if occursin('\n', sx)
                    sx = sprint(show, X[row, col], context=io, sizehint=0)
                end
            else
                a = (3,3)
                sx = "#undef"
            end
            print(io, " "^a[1])
            print(io, replace_in_print_matrix(X,row,col,sx))
            col == cols[end] || print(io, " "^a[2])
        end
        row == rows[end] || println(io)
    end
end


# typeinfo agnostic
"""
Unexported convenience function used in body of `replace_in_print_matrix`
methods. By default returns a string of the same width as original with a
centered cdot, used in printing of structural zeros of structured matrices.
Accept keyword args `c` for alternate single character marker.
"""
function replace_with_centered_mark(s::AbstractString;c::AbstractChar = '⋅')
    N = textwidth(ANSIIterator(s))
    return N == 0 ? string(c) : join(setindex!([" " for i=1:N],string(c),ceil(Int,N/2)))
end


"""
    print_matrix(io::IO, mat, pre, sep, post)

Prints a 2d matrix with elements vertically aligned.
Optional arguments are string pre (printed before the matrix, e.g. an opening bracket)
which will cause a corresponding same-size indent on following rows, and
string post (printed at the end of the last row of the matrix).
"""
function print_matrix(io::IO, X::AbstractVecOrMat,
                      pre::AbstractString = " ",  # pre-matrix string
                      sep::AbstractString = "  ", # separator between elements
                      post::AbstractString = "")  # post-matrix string
    _show_matrix(io, inferencebarrier(X), unitrange(axes(X,1)), unitrange(axes(X,2)))
end

# typeinfo agnostic
# n-dimensional arrays
show_nd(io::IO, a::AbstractArray, print_matrix::Function, show_indices::Bool) =
    _show_nd(io, inferencebarrier(a), print_matrix, show_indices, map(unitrange, axes(a)))

function _show_nd(io::IO, @nospecialize(a::AbstractArray), print_matrix::Function,
                  show_indices::Bool, axs::Tuple{Vararg{AbstractUnitRange}})
    if isempty(a)
        return
    end
    tailinds = tail(tail(axs))
    nd = ndims(a)-2
    show_indices || print(io, "[")
    Is = CartesianIndices(tailinds)
    lastidxs = first(Is).I
    reached_last_d = false
    for I in Is
        idxs = I.I
        if show_indices
            print(io, "[:, :, ")
            for i = 1:length(idxs)-1
                print(io, idxs[i], ", ")
            end
            println(io, idxs[end], "] =")
        end
        slice = view(a, axs[1], axs[2], idxs...)
        if show_indices
            print_matrix(io, slice)
            print(io, idxs == map(last,tailinds) ? "" : "\n\n")
        else
            idxdiff = lastidxs .- idxs .< 0
            if any(idxdiff)
                lastchangeindex = 2 + findlast(idxdiff)
                print(io, ";"^lastchangeindex)
                lastchangeindex == ndims(a) && (reached_last_d = true)
                print(io, " ")
            end
            print_matrix(io, slice)
        end
        lastidxs = idxs
    end
    if !show_indices
        reached_last_d || print(io, ";"^(nd+2))
        print(io, "]")
    end
end

# print_array: main helper functions for show(io, text/plain, array)
# typeinfo agnostic
# Note that this is for showing the content inside the array, and for `MIME"text/plain".
# There are `show(::IO, ::A) where A<:AbstractArray` methods that don't use this
# e.g. show_vector, show_zero_dim
print_array(io::IO, X::AbstractArray{<:Any, 0}) =
    isassigned(X) ? show(io, X[]) : print(io, undef_ref_str)
print_array(io::IO, X::AbstractVecOrMat) = print_matrix(io, X)
print_array(io::IO, X::AbstractArray) = show_nd(io, X, print_matrix, true)

# typeinfo aware
# implements: show(io::IO, ::MIME"text/plain", X::AbstractArray)
function array_show(io::IO, ::MIME"text/plain", X::AbstractArray)
    if isempty(X) && (get(io, :compact, false)::Bool || X isa AbstractVector)
        return show(io, X)
    end
    # 1) show summary before setting :compact
    summary(io, X)
    isempty(X) && return
    print(io, ":")
    show_circular(io, X) && return
    println(io)

    # 2) compute new IOContext
    if !haskey(io, :compact) && length(axes(X, 2)) > 1
        io = IOContext(io, :compact => true)
    end

    # 3) update typeinfo
    #
    # it must come after printing the summary, which can exploit :typeinfo itself
    # (e.g. views)
    # we assume this function is always called from top-level, i.e. that it's not nested
    # within another "show" method; hence we always print the summary, without
    # checking for current :typeinfo (this could be changed in the future)
    io = IOContext(io, :typeinfo => eltype(X))

    # 4) show actual content
    recur_io = IOContext(io, :SHOWN_SET => X)
    print_array(recur_io, X)
end

## printing with `show`

### non-Vector arrays

# _show_nonempty & _show_empty: main helper functions for show(io, X)
# typeinfo agnostic

"""
`_show_nonempty(io, X::AbstractMatrix, prefix)` prints matrix X with opening and closing square brackets,
preceded by `prefix`, supposed to encode the type of the elements.
"""
_show_nonempty(io::IO, X::AbstractMatrix, prefix::String) =
    _show_nonempty(io, inferencebarrier(X), prefix, false, axes(X))

function _show_nonempty(io::IO, X::AbstractArray, prefix::String)
    print(io, prefix)
    show_nd(io, X, (io, slice) -> _show_nonempty(io, inferencebarrier(slice), prefix, true, axes(slice)), false)
end

# a specific call path is used to show vectors (show_vector)
_show_nonempty(::IO, ::AbstractVector, ::String) =
    error("_show_nonempty(::IO, ::AbstractVector, ::String) is not implemented")

_show_nonempty(io::IO, X::AbstractArray{T,0} where T, prefix::String) = print_array(io, X)

function _show_nonempty(io::IO, @nospecialize(X::AbstractMatrix), prefix::String, drop_brackets::Bool, axs::Tuple{AbstractUnitRange,AbstractUnitRange})
    @assert !isempty(X) "X should be non-empty"
    limit = get(io, :limit, false)::Bool
    indr, indc = axs
    nr, nc = length(indr), length(indc)
    rdots, cdots = false, false
    rr1, rr2 = unitrange(indr), 1:0
    cr1 = unitrange(indc)
    cr2 = first(cr1) .+ (0:-1)
    if limit
        if nr > 4
            rr1, rr2 = rr1[1:2], rr1[nr-1:nr]
            rdots = true
        end
        if nc > 4
            cr1, cr2 = cr1[1:2], cr1[nc-1:nc]
            cdots = true
        end
    end
    drop_brackets || print(io, prefix, "[")
    for rr in (rr1, rr2)
        for i in rr
            for cr in (cr1, cr2)
                for j in cr
                    j > first(cr) && print(io, " ")
                    if !isassigned(X,i,j)
                        print(io, undef_ref_str)
                    else
                        el = X[i,j]
                        show(io, el)
                    end
                end
                if last(cr) == last(indc)
                    i < last(indr) && print(io, "; ")
                elseif cdots
                    print(io, " \u2026 ")
                end
            end
        end
        last(rr) != last(indr) && rdots && print(io, "\u2026 ; ")
    end
    if !drop_brackets
        nc > 1 || print(io, ";;")
        print(io, "]")
    end
    return nothing
end

# NOTE: it's not clear how this method could use the :typeinfo attribute
function _show_empty(io::IO, X::Array)
    show(io, typeof(X))
    print(io, "(undef, ", join(size(X),", "), ')')
end
_show_empty(io, X::AbstractArray) = summary(io, X)

# typeinfo aware (necessarily)
function show(io::IO, X::AbstractArray)
    ndims(X) == 0 && return show_zero_dim(io, X)
    ndims(X) == 1 && return show_vector(io, X)
    prefix, implicit = typeinfo_prefix(io, X)
    if !implicit
        io = IOContext(io, :typeinfo => eltype(X))
    end
    if isempty(X)
        return _show_empty(io, X)
    end
    show_circular(io, X) && return
    recur_io = IOContext(io, :SHOWN_SET => X)
    _show_nonempty(recur_io, X, prefix)
end

### 0-dimensional arrays (#31481)
show_zero_dim(io::IO, X::BitArray{0}) = print(io, "BitArray(", Int(X[]), ")")
function show_zero_dim(io::IO, X::AbstractArray{T, 0}) where T
    if isassigned(X)
        print(io, "fill(")
        show(io, X[])
    else
        print(io, "Array{", T, ", 0}(")
        show(io, undef)
    end
    print(io, ")")
end

### Vector arrays

# typeinfo aware
# NOTE: v is not constrained to be a vector, as this function can work with iterables
# in general (it's used e.g. by show(::IO, ::Set))
function show_vector(io::IO, v, opn='[', cls=']')
    prefix, implicit = typeinfo_prefix(io, v)
    print(io, prefix)
    # directly or indirectly, the context now knows about eltype(v)
    if !implicit
        io = IOContext(io, :typeinfo => eltype(v))
    end
    limited = get(io, :limit, false)::Bool

    if limited && length(v) > 20
        axs1 = axes1(v)
        f, l = first(axs1), last(axs1)
        show_delim_array(io, v, opn, ",", "", false, f, f+9)
        print(io, "  …  ")
        show_delim_array(io, v, "", ",", cls, false, l-9, l)
    else
        show_delim_array(io, v, opn, ",", cls, false)
    end
end


## Logic for displaying type information

# given type `typeinfo` extracted from context, assuming a collection
# is being displayed, deduce the elements type; in spirit this is
# similar to `eltype` (except that we don't want a default fall-back
# returning Any, as this would cause incorrect printing in e.g. `Vector[Any[1]]`,
# because eltype(Vector) == Any so `Any` wouldn't be printed in `Any[1]`)
typeinfo_eltype(typeinfo) = nothing # element type not precisely known
typeinfo_eltype(typeinfo::Type{Union{}}, slurp...) = nothing
typeinfo_eltype(typeinfo::Type{<:AbstractArray{T}}) where {T} = eltype(typeinfo)
typeinfo_eltype(typeinfo::Type{<:AbstractDict{K,V}}) where {K,V} = eltype(typeinfo)
typeinfo_eltype(typeinfo::Type{<:AbstractSet{T}}) where {T} = eltype(typeinfo)

# This is a fancy way to make de-specialize a call to `typeinfo_implicit(T)`
# which is unfortunately invalidated by Dates
#  (https://github.com/JuliaLang/julia/issues/56080)
#
# This makes the call less efficient, but avoids being invalidated by Dates.
_typeinfo_implicit(@nospecialize(T)) = Base.invoke_in_world(Base.tls_world_age(), typeinfo_implicit, T)::Bool

# types that can be parsed back accurately from their un-decorated representations
function typeinfo_implicit(@nospecialize(T))
    if T === Float64 || T === Int || T === Char || T === String || T === Symbol ||
        issingletontype(T)
        return true
    end
    return isconcretetype(T) &&
        ((T <: Array && _typeinfo_implicit(eltype(T))) ||
         ((T <: Tuple || T <: NamedTuple || T <: Pair) && all(_typeinfo_implicit, fieldtypes(T))) ||
         (T <: AbstractDict && _typeinfo_implicit(keytype(T)) && _typeinfo_implicit(valtype(T))))
end

# X not constrained, can be any iterable (cf. show_vector)
function typeinfo_prefix(io::IO, X)
    typeinfo = get(io, :typeinfo, Any)::Type

    if !(X isa typeinfo)
        typeinfo = Any
    end

    # what the context already knows about the eltype of X:
    eltype_ctx = typeinfo_eltype(typeinfo)
    eltype_X = eltype(X)

    if X isa AbstractDict
        if eltype_X == eltype_ctx
            sprint(show_type_name, typeof(X).name; context=io), false
        elseif !isempty(X) && _typeinfo_implicit(keytype(X)) && _typeinfo_implicit(valtype(X))
            sprint(show_type_name, typeof(X).name; context=io), true
        else
            sprint(print, typeof(X); context=io), false
        end
    else
        # Types hard-coded here are those which are created by default for a given syntax
        if eltype_X == eltype_ctx
            "", false
        elseif !isempty(X) && _typeinfo_implicit(eltype_X)
            "", true
        elseif print_without_params(eltype_X)
            sprint(show_type_name, unwrap_unionall(eltype_X).name; context=io), false # Print "Array" rather than "Array{T,N}"
        else
            sprint(print, eltype_X; context=io), false
        end
    end
end
