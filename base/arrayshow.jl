"""
Unexported convenience function used in body of `replace_in_print_matrix`
methods. By default returns a string of the same width as original with a
centered cdot, used in printing of structural zeros of structured matrices.
Accept keyword args `c` for alternate single character marker.
"""
function replace_with_centered_mark(s::AbstractString;c::Char = '⋅')
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
    if 1 < length(a) < length(indices(X,2))
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
    isempty(A) || first(indices(cols,1)) == 1 || throw(DimensionMismatch("indices of cols ($(indices(cols,1))) must start at 1"))
    for k = 1:length(A)
        j = cols[k]
        if isassigned(X,Int(i),Int(j)) # isassigned accepts only `Int` indices
            x = X[i,j]
            a = alignment(io, x)
            sx = sprint(0, show, x, env=io)
        else
            a = undef_ref_alignment
            sx = undef_ref_str
        end
        l = repeat(" ", A[k][1]-a[1]) # pad on left and right as needed
        r = repeat(" ", A[k][2]-a[2])
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
        A::Vector, sep::AbstractString, M::Integer, m::Integer)
    for k = 1:length(A)
        w = A[k][1] + A[k][2]
        if k % M == m
            l = repeat(" ", max(0, A[k][1]-length(vdots)))
            r = repeat(" ", max(0, w-length(vdots)-length(l)))
            print(io, l, vdots, r)
        else
            print(io, repeat(" ", w))
        end
        if k < length(A); print(io, sep); end
    end
end

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
    rowsA, colsA = indices(X,1), indices(X,2)
    m, n = length(rowsA), length(colsA)
    # To figure out alignments, only need to look at as many rows as could
    # fit down screen. If screen has at least as many rows as A, look at A.
    # If not, then we only need to look at the first and last chunks of A,
    # each half a screen height in size.
    halfheight = div(screenheight,2)
    if m > screenheight
        rowsA = [rowsA[1:halfheight]; rowsA[m-div(screenheight-1,2)+1:m]]
    end
    # Similarly for columns, only necessary to get alignments for as many
    # columns as could conceivably fit across the screen
    maxpossiblecols = div(screenwidth, 1+sepsize)
    if n > maxpossiblecols
        colsA = [colsA[1:maxpossiblecols]; colsA[(n-maxpossiblecols+1):n]]
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
                    print_matrix_vdots(io, vdots,A,sep,vmod,1)
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
                    print_matrix_vdots(io, vdots,Lalign,sep,vmod,1)
                    print(io, ddots)
                    print_matrix_vdots(io, vdots,Ralign,sep,vmod,r)
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

# n-dimensional arrays
function show_nd(io::IO, a::AbstractArray, print_matrix, label_slices)
    limit::Bool = get(io, :limit, false)
    if isempty(a)
        return
    end
    tailinds = tail(tail(indices(a)))
    nd = ndims(a)-2
    for I in CartesianRange(tailinds)
        idxs = I.I
        if limit
            for i = 1:nd
                ii = idxs[i]
                ind = tailinds[i]
                if length(ind) > 10
                    if ii == ind[4] && all(d->idxs[d]==first(tailinds[d]),1:i-1)
                        for j=i+1:nd
                            szj = length(indices(a, j+2))
                            indj = tailinds[j]
                            if szj>10 && first(indj)+2 < idxs[j] <= last(indj)-3
                                @goto skip
                            end
                        end
                        #println(io, idxs)
                        print(io, "...\n\n")
                        @goto skip
                    end
                    if ind[3] < ii <= ind[end-3]
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
        slice = view(a, indices(a,1), indices(a,2), idxs...)
        print_matrix(io, slice)
        print(io, idxs == map(last,tailinds) ? "" : "\n\n")
        @label skip
    end
end

"""
`print_matrix_repr(io, X)` prints matrix X with opening and closing square brackets.
"""
function print_matrix_repr(io, X::AbstractArray)
    limit = get(io, :limit, false)::Bool
    compact, prefix = array_eltype_show_how(X)
    if compact && !haskey(io, :compact)
        io = IOContext(io, :compact => compact)
    end
    indr, indc = indices(X,1), indices(X,2)
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

show(io::IO, X::AbstractArray) = showarray(io, X, true)

repremptyarray(io::IO, X::Array{T}) where {T} = print(io, "Array{$T}(", join(size(X),','), ')')
repremptyarray(io, X) = nothing # by default, we don't know this constructor

function showarray(io::IO, X::AbstractArray, repr::Bool = true; header = true)
    if repr && ndims(X) == 1
        return show_vector(io, X, "[", "]")
    end
    if !haskey(io, :compact) && length(indices(X, 2)) > 1
        io = IOContext(io, :compact => true)
    end
    if !repr && get(io, :limit, false) && eltype(X) === Method
        # override usual show method for Vector{Method}: don't abbreviate long lists
        io = IOContext(io, :limit => false)
    end
    (!repr && header) && summary(io, X)
    if !isempty(X)
        if !repr && header
            print(io, ":")
            if get(io, :limit, false) && displaysize(io)[1]-4 <= 0
                return print(io, " …")
            else
                println(io)
            end
        end
        if ndims(X) == 0
            if isassigned(X)
                return show(io, X[])
            else
                return print(io, undef_ref_str)
            end
        end
        if repr
            if ndims(X) <= 2
                print_matrix_repr(io, X)
            else
                show_nd(io, X, print_matrix_repr, false)
            end
        else
            punct = (" ", "  ", "")
            if ndims(X) <= 2
                print_matrix(io, X, punct...)
            else
                show_nd(io, X,
                        (io, slice) -> print_matrix(io, slice, punct...),
                        !repr)
            end
        end
    elseif repr
        repremptyarray(io, X)
    end
end

# returns compact, prefix
function array_eltype_show_how(X)
    e = eltype(X)
    if print_without_params(e)
        str = string(unwrap_unionall(e).name) # Print "Array" rather than "Array{T,N}"
    else
        str = string(e)
    end
    # Types hard-coded here are those which are created by default for a given syntax
    (_isleaftype(e),
     (!isempty(X) && (e===Float64 || e===Int || e===Char || e===String) ? "" : str))
end

function show_vector(io::IO, v, opn, cls)
    compact, prefix = array_eltype_show_how(v)
    limited = get(io, :limit, false)
    if compact && !haskey(io, :compact)
        io = IOContext(io, :compact => compact)
    end
    print(io, prefix)
    if limited && _length(v) > 20
        inds = indices1(v)
        show_delim_array(io, v, opn, ",", "", false, inds[1], inds[1]+9)
        print(io, "  \u2026  ")
        show_delim_array(io, v, "", ",", cls, false, inds[end-9], inds[end])
    else
        show_delim_array(io, v, opn, ",", cls, false)
    end
end
