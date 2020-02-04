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
# 1) printing with `display`
# 2) printing with `show`
# 3) Logic for displaying type information


## printing with `display`

"""
Unexported convenience function used in body of `replace_in_print_matrix`
methods. By default returns a string of the same width as original with a
centered cdot, used in printing of structural zeros of structured matrices.
Accept keyword args `c` for alternate single character marker.
"""
function replace_with_centered_mark(s::AbstractString;c::AbstractChar = '⋅')
    N = length(s)
    return join(setindex!([" " for i=1:N],string(c),ceil(Int,N/2)))
end

const undef_ref_alignment = (3,3)

"""
`alignment(X, rows, cols, cols_if_complete, cols_otherwise, sep)` returns the
alignment for specified parts of array `X`, returning the (left,right) info.
It will look in X's `rows`, `cols` (both lists of indices)
and figure out what's needed to be fully aligned, for example looking all
the way down a column and finding out the maximum size of each element.
Parameter `sep::Integer` is number of spaces to put between elements.
`cols_if_complete` and `cols_otherwise` indicate screen width to use.
Alignment is reported as a vector of (left,right) tuples, one for each
column going across the screen.
"""
function alignment(io::IO, X::AbstractVecOrMat,
        rows::AbstractVector, cols::AbstractVector,
        cols_if_complete::Integer, cols_otherwise::Integer, sep::Integer)
    a = Tuple{Int, Int}[]
    for j in cols # need to go down each column one at a time
        l = r = 0
        for i in rows # plumb down and see what largest element sizes are
            if isassigned(X,i,j)
                aij = alignment(io, X[i,j])
            else
                aij = undef_ref_alignment
            end
            l = max(l, aij[1]) # left characters
            r = max(r, aij[2]) # right characters
        end
        push!(a, (l, r)) # one tuple per column of X, pruned to screen width
        if length(a) > 1 && sum(map(sum,a)) + sep*length(a) >= cols_if_complete
            pop!(a) # remove this latest tuple if we're already beyond screen width
            break
        end
    end
    if 1 < length(a) < length(axes(X,2))
        while sum(map(sum,a)) + sep*length(a) >= cols_otherwise
            pop!(a)
        end
    end
    return a
end

"""
`print_matrix_row(io, X, A, i, cols, sep)` produces the aligned output for
a single matrix row X[i, cols] where the desired list of columns is given.
The corresponding alignment A is used, and the separation between elements
is specified as string sep.
`print_matrix_row` will also respect compact output for elements.
"""
function print_matrix_row(io::IO,
        X::AbstractVecOrMat, A::Vector,
        i::Integer, cols::AbstractVector, sep::AbstractString)
    for (k, j) = enumerate(cols)
        k > length(A) && break
        if isassigned(X,Int(i),Int(j)) # isassigned accepts only `Int` indices
            x = X[i,j]
            a = alignment(io, x)

            # First try 3-arg show
            sx = sprint(show, "text/plain", x, context=io, sizehint=0)

            # If the output contains line breaks, try 2-arg show instead.
            if occursin('\n', sx)
                sx = sprint(show, x, context=io, sizehint=0)
            end
        else
            a = undef_ref_alignment
            sx = undef_ref_str
        end
        l = repeat(" ", A[k][1]-a[1]) # pad on left and right as needed
        r = j == axes(X, 2)[end] ? "" : repeat(" ", A[k][2]-a[2])
        prettysx = replace_in_print_matrix(X,i,j,sx)
        print(io, l, prettysx, r)
        if k < length(A); print(io, sep); end
    end
end


"""
`print_matrix_vdots` is used to show a series of vertical ellipsis instead
of a bunch of rows for long matrices. Not only is the string vdots shown
but it also repeated every M elements if desired.
"""
function print_matrix_vdots(io::IO, vdots::AbstractString,
                            A::Vector, sep::AbstractString, M::Integer, m::Integer,
                            pad_right::Bool = true)
    for k = 1:length(A)
        w = A[k][1] + A[k][2]
        if k % M == m
            l = repeat(" ", max(0, A[k][1]-length(vdots)))
            r = k == length(A) && !pad_right ?
                "" :
                repeat(" ", max(0, w-length(vdots)-length(l)))
            print(io, l, vdots, r)
        else
            (k != length(A) || pad_right) && print(io, repeat(" ", w))
        end
        if k < length(A); print(io, sep); end
    end
end

# typeinfo agnostic
"""
    print_matrix(io::IO, mat, pre, sep, post, hdots, vdots, ddots, hmod, vmod)

Prints a matrix with limited output size. If `io` sets `:limit` to true,
then only the corners of the matrix are printed, separated with vertical,
horizontal, and diagonal ellipses as appropriate.
Optional arguments are string pre (printed before the matrix, e.g. an opening bracket)
which will cause a corresponding same-size indent on following rows, and
string post (printed at the end of the last row of the matrix).
Also options to use different ellipsis characters hdots, vdots, ddots.
These are repeated every hmod or vmod elements.
"""
function print_matrix(io::IO, X::AbstractVecOrMat,
                      pre::AbstractString = " ",  # pre-matrix string
                      sep::AbstractString = "  ", # separator between elements
                      post::AbstractString = "",  # post-matrix string
                      hdots::AbstractString = "  \u2026  ",
                      vdots::AbstractString = "\u22ee",
                      ddots::AbstractString = "  \u22f1  ",
                      hmod::Integer = 5, vmod::Integer = 5)
    if !get(io, :limit, false)
        screenheight = screenwidth = typemax(Int)
    else
        sz = displaysize(io)
        screenheight, screenwidth = sz[1] - 4, sz[2]
    end
    screenwidth -= length(pre) + length(post)
    presp = repeat(" ", length(pre))  # indent each row to match pre string
    postsp = ""
    @assert textwidth(hdots) == textwidth(ddots)
    sepsize = length(sep)
    rowsA, colsA = UnitRange(axes(X,1)), UnitRange(axes(X,2))
    m, n = length(rowsA), length(colsA)
    # To figure out alignments, only need to look at as many rows as could
    # fit down screen. If screen has at least as many rows as A, look at A.
    # If not, then we only need to look at the first and last chunks of A,
    # each half a screen height in size.
    halfheight = div(screenheight,2)
    if m > screenheight
        rowsA = [rowsA[(0:halfheight-1) .+ firstindex(rowsA)]; rowsA[(end-div(screenheight-1,2)+1):end]]
    end
    # Similarly for columns, only necessary to get alignments for as many
    # columns as could conceivably fit across the screen
    maxpossiblecols = div(screenwidth, 1+sepsize)
    if n > maxpossiblecols
        colsA = [colsA[(0:maxpossiblecols-1) .+ firstindex(colsA)]; colsA[(end-maxpossiblecols+1):end]]
    end
    A = alignment(io, X, rowsA, colsA, screenwidth, screenwidth, sepsize)
    # Nine-slicing is accomplished using print_matrix_row repeatedly
    if m <= screenheight # rows fit vertically on screen
        if n <= length(A) # rows and cols fit so just print whole matrix in one piece
            for i in rowsA
                print(io, i == first(rowsA) ? pre : presp)
                print_matrix_row(io, X,A,i,colsA,sep)
                print(io, i == last(rowsA) ? post : postsp)
                if i != last(rowsA); println(io); end
            end
        else # rows fit down screen but cols don't, so need horizontal ellipsis
            c = div(screenwidth-length(hdots)+1,2)+1  # what goes to right of ellipsis
            Ralign = reverse(alignment(io, X, rowsA, reverse(colsA), c, c, sepsize)) # alignments for right
            c = screenwidth - sum(map(sum,Ralign)) - (length(Ralign)-1)*sepsize - length(hdots)
            Lalign = alignment(io, X, rowsA, colsA, c, c, sepsize) # alignments for left of ellipsis
            for i in rowsA
                print(io, i == first(rowsA) ? pre : presp)
                print_matrix_row(io, X,Lalign,i,colsA[1:length(Lalign)],sep)
                print(io, (i - first(rowsA)) % hmod == 0 ? hdots : repeat(" ", length(hdots)))
                print_matrix_row(io, X, Ralign, i, (n - length(Ralign)) .+ colsA, sep)
                print(io, i == last(rowsA) ? post : postsp)
                if i != last(rowsA); println(io); end
            end
        end
    else # rows don't fit so will need vertical ellipsis
        if n <= length(A) # rows don't fit, cols do, so only vertical ellipsis
            for i in rowsA
                print(io, i == first(rowsA) ? pre : presp)
                print_matrix_row(io, X,A,i,colsA,sep)
                print(io, i == last(rowsA) ? post : postsp)
                if i != rowsA[end] || i == rowsA[halfheight]; println(io); end
                if i == rowsA[halfheight]
                    print(io, i == first(rowsA) ? pre : presp)
                    print_matrix_vdots(io, vdots, A, sep, vmod, 1, false)
                    print(io, i == last(rowsA) ? post : postsp * '\n')
                end
            end
        else # neither rows nor cols fit, so use all 3 kinds of dots
            c = div(screenwidth-length(hdots)+1,2)+1
            Ralign = reverse(alignment(io, X, rowsA, reverse(colsA), c, c, sepsize))
            c = screenwidth - sum(map(sum,Ralign)) - (length(Ralign)-1)*sepsize - length(hdots)
            Lalign = alignment(io, X, rowsA, colsA, c, c, sepsize)
            r = mod((length(Ralign)-n+1),vmod) # where to put dots on right half
            for i in rowsA
                print(io, i == first(rowsA) ? pre : presp)
                print_matrix_row(io, X,Lalign,i,colsA[1:length(Lalign)],sep)
                print(io, (i - first(rowsA)) % hmod == 0 ? hdots : repeat(" ", length(hdots)))
                print_matrix_row(io, X,Ralign,i,(n-length(Ralign)).+colsA,sep)
                print(io, i == last(rowsA) ? post : postsp)
                if i != rowsA[end] || i == rowsA[halfheight]; println(io); end
                if i == rowsA[halfheight]
                    print(io, i == first(rowsA) ? pre : presp)
                    print_matrix_vdots(io, vdots, Lalign, sep, vmod, 1, true)
                    print(io, ddots)
                    print_matrix_vdots(io, vdots, Ralign, sep, vmod, r, false)
                    print(io, i == last(rowsA) ? post : postsp * '\n')
                end
            end
        end
        if isempty(rowsA)
            print(io, pre)
            print(io, vdots)
            length(colsA) > 1 && print(io, "    ", ddots)
            print(io, post)
        end
    end
end

# typeinfo agnostic
# n-dimensional arrays
function show_nd(io::IO, a::AbstractArray, print_matrix::Function, label_slices::Bool)
    limit::Bool = get(io, :limit, false)
    if isempty(a)
        return
    end
    tailinds = tail(tail(axes(a)))
    nd = ndims(a)-2
    for I in CartesianIndices(tailinds)
        idxs = I.I
        if limit
            for i = 1:nd
                ii = idxs[i]
                ind = tailinds[i]
                if length(ind) > 10
                    if ii == ind[firstindex(ind)+3] && all(d->idxs[d]==first(tailinds[d]),1:i-1)
                        for j=i+1:nd
                            szj = length(axes(a, j+2))
                            indj = tailinds[j]
                            if szj>10 && first(indj)+2 < idxs[j] <= last(indj)-3
                                @goto skip
                            end
                        end
                        #println(io, idxs)
                        print(io, "...\n\n")
                        @goto skip
                    end
                    if ind[firstindex(ind)+2] < ii <= ind[end-3]
                        @goto skip
                    end
                end
            end
        end
        if label_slices
            print(io, "[:, :, ")
            for i = 1:(nd-1); print(io, "$(idxs[i]), "); end
            println(io, idxs[end], "] =")
        end
        slice = view(a, axes(a,1), axes(a,2), idxs...)
        print_matrix(io, slice)
        print(io, idxs == map(last,tailinds) ? "" : "\n\n")
        @label skip
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
function show(io::IO, ::MIME"text/plain", X::AbstractArray)
    # 0) show summary before setting :compact
    summary(io, X)
    isempty(X) && return
    print(io, ":")
    show_circular(io, X) && return

    # 1) compute new IOContext
    if !haskey(io, :compact) && length(axes(X, 2)) > 1
        io = IOContext(io, :compact => true)
    end
    if get(io, :limit, false) && eltype(X) === Method
        # override usual show method for Vector{Method}: don't abbreviate long lists
        io = IOContext(io, :limit => false)
    end

    if get(io, :limit, false) && displaysize(io)[1]-4 <= 0
        return print(io, " …")
    else
        println(io)
    end

    # 2) update typeinfo
    #
    # it must come after printing the summary, which can exploit :typeinfo itself
    # (e.g. views)
    # we assume this function is always called from top-level, i.e. that it's not nested
    # within another "show" method; hence we always print the summary, without
    # checking for current :typeinfo (this could be changed in the future)
    io = IOContext(io, :typeinfo => eltype(X))

    # 2) show actual content
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
function _show_nonempty(io::IO, X::AbstractMatrix, prefix::String)
    @assert !isempty(X)
    limit = get(io, :limit, false)::Bool
    indr, indc = axes(X,1), axes(X,2)
    nr, nc = length(indr), length(indc)
    rdots, cdots = false, false
    rr1, rr2 = UnitRange{Int}(indr), 1:0
    cr1, cr2 = UnitRange{Int}(indc), 1:0
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
    print(io, prefix, "[")
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
        last(rr) != nr && rdots && print(io, "\u2026 ; ")
    end
    print(io, "]")
end


_show_nonempty(io::IO, X::AbstractArray, prefix::String) =
    show_nd(io, X, (io, slice) -> _show_nonempty(io, slice, prefix), false)

# a specific call path is used to show vectors (show_vector)
_show_nonempty(::IO, ::AbstractVector, ::String) =
    error("_show_nonempty(::IO, ::AbstractVector, ::String) is not implemented")

_show_nonempty(io::IO, X::AbstractArray{T,0} where T, prefix::String) = print_array(io, X)

# NOTE: it's not clear how this method could use the :typeinfo attribute
_show_empty(io::IO, X::Array{T}) where {T} = print(io, "Array{", T, "}(undef,", join(size(X),','), ')')
_show_empty(io, X) = nothing # by default, we don't know this constructor

# typeinfo aware (necessarily)
function show(io::IO, X::AbstractArray)
    ndims(X) == 0 && return show_zero_dim(io, X)
    ndims(X) == 1 && return show_vector(io, X)
    prefix, implicit = typeinfo_prefix(io, X)
    if !implicit
        io = IOContext(io, :typeinfo => eltype(X))
    end
    isempty(X) ?
        _show_empty(io, X) :
        _show_nonempty(io, X, prefix)
end

### 0-dimensional arrays (#31481)
show_zero_dim(io::IO, X::BitArray{0}) = print(io, "BitArray(", Int(X[]), ")")
function show_zero_dim(io::IO, X::AbstractArray{T, 0}) where T
    if isassigned(X)
        print(io, "fill(")
        show(io, X[])
    else
        print(io, "Array{$T,0}(")
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
    limited = get(io, :limit, false)

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
typeinfo_eltype(typeinfo::Type{<:AbstractArray{T}}) where {T} = eltype(typeinfo)
typeinfo_eltype(typeinfo::Type{<:AbstractDict{K,V}}) where {K,V} = eltype(typeinfo)
typeinfo_eltype(typeinfo::Type{<:AbstractSet{T}}) where {T} = eltype(typeinfo)

# types that can be parsed back accurately from their un-decorated representations
function typeinfo_implicit(@nospecialize(T))
    if T === Float64 || T === Int || T === Char || T === String || T === Symbol ||
        issingletontype(T)
        return true
    end
    return isconcretetype(T) &&
        ((T <: Array && typeinfo_implicit(eltype(T))) ||
         ((T <: Tuple || T <: Pair) && all(typeinfo_implicit, fieldtypes(T))) ||
         (T <: AbstractDict && typeinfo_implicit(keytype(T)) && typeinfo_implicit(valtype(T))))
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
            string(typeof(X).name), false
        elseif !isempty(X) && typeinfo_implicit(keytype(X)) && typeinfo_implicit(valtype(X))
            string(typeof(X).name), true
        else
            string(typeof(X)), false
        end
    else
        # Types hard-coded here are those which are created by default for a given syntax
        if eltype_X == eltype_ctx
            "", false
        elseif !isempty(X) && typeinfo_implicit(eltype_X)
            "", true
        elseif print_without_params(eltype_X)
            string(unwrap_unionall(eltype_X).name), false # Print "Array" rather than "Array{T,N}"
        else
            string(eltype_X), false
        end
    end
end
