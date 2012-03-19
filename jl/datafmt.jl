## file formats ##

function _jl_dlm_readrow(f, dlm)
    row = split(readline(f), dlm, true)
    row[end] = chomp(row[end])
    row
end

# all strings
function _jl_dlmread(a, f, dlm, nr, nc, row)
    for i=1:nr
        a[i,:] = row
        if i < nr
            row = _jl_dlm_readrow(f, dlm)
        end
    end
    a
end

# all numeric, with NaN for invalid data
function _jl_dlmread{T<:Number}(a::Array{T}, f, dlm, nr, nc, row)
    tmp = Array(Float64,1)
    for i=1:nr
        for j=1:nc
            if float64_isvalid(row[j], tmp)
                a[i,j] = tmp[1]
            else
                a[i,j] = NaN
            end
        end
        if i < nr
            row = _jl_dlm_readrow(f, dlm)
        end
    end
end

# float64 or string
_jl_dlmread(a::Array{Any}, f, dlm, nr, nc, row) =
    _jl_dlmread(a, f, dlm, nr, nc, row, 1, 1)
function _jl_dlmread(a::Array{Any}, f, dlm, nr, nc, row, i0, j0)
    tmp = Array(Float64,1)
    j = j0
    for i=i0:nr
        while j <= nc
            el = row[j]
            if float64_isvalid(el, tmp)
                a[i,j] = tmp[1]
            else
                a[i,j] = el
            end
            j += 1
        end
        j = 1
        if i < nr
            row = _jl_dlm_readrow(f, dlm)
        end
    end
    a
end

# float64 or cell depending on data
function _jl_dlmread_auto(a, f, dlm, nr, nc, row)
    tmp = Array(Float64, 1)
    for i=1:nr
        for j=1:nc
            el = row[j]
            if !float64_isvalid(el, tmp)
                a = convert(Array{Any,2}, a)
                _jl_dlmread(a, f, dlm, nr, nc, row, i, j)
                return a
            else
                a[i,j] = tmp[1]
            end
        end
        if i < nr
            row = _jl_dlm_readrow(f, dlm)
        end
    end
    a
end

function _jl_dlmread_setup(fname::String, dlm::(Char...))
    if length(dlm) == 0
        error("dlmread: no separator characters specified")
    end
    nr = integer(split(readall(`wc -l $fname`),' ',false)[1])
    f = open(fname)
    row = _jl_dlm_readrow(f, dlm)
    nc = length(row)
    return (f, nr, nc, row)
end

const _jl_dlmread_default_delimiters = (' ', ',', ';', '\t', '\v')

dlmread(fname::String, T::Type) = dlmread(fname, _jl_dlmread_default_delimiters, T)

dlmread(fname::String) = dlmread(fname, _jl_dlmread_default_delimiters)

dlmread(fname::String, dlm::Char, T::Type) = dlmread(fname, (dlm,), T)

dlmread(fname::String, dlm::Union(Vector{Char}, ASCIIString), T::Type) =
    dlmread(fname, ntuple(length(dlm), i->dlm[i]), T)

function dlmread(fname::String, dlm::(Char...), T::Type)
    (f, nr, nc, row) = _jl_dlmread_setup(fname, dlm)
    a = Array(T, nr, nc)
    _jl_dlmread(a, f, dlm, nr, nc, row)
    close(f)
    return a
end

dlmread(fname::String, dlm::Char) = dlmread(fname, (dlm,))

dlmread(fname::String, dlm::Union(Vector{Char}, ASCIIString)) =
    dlmread(fname, ntuple(length(dlm), i->dlm[i]))

function dlmread(fname::String, dlm::(Char...))
    (f, nr, nc, row) = _jl_dlmread_setup(fname, dlm)
    a = Array(Float64, nr, nc)
    a = _jl_dlmread_auto(a, f, dlm, nr, nc, row)
    close(f)
    return a
end

csvread(f)          = dlmread(f, ',')
csvread(f, T::Type) = dlmread(f, ',', T)

# todo: keyword argument for # of digits to print
function dlmwrite(f, a, dlm::Char)
    nr, nc = size(a)
    try
        set_current_output_stream(f)
        for i=1:nr
            for j=1:nc
                elt = a[i,j]
                if isa(elt,Float)
                    print_shortest(elt)
                else
                    print(elt)
                end
                if j < nc
                    write(f, dlm)
                end
            end
            write(f, '\n')
        end
    catch e
        throw(e)
    end
    nothing
end

function dlmwrite(fname::String, a, dlm::Char)
    f = open(fname, "w")
    dlmwrite(f, a, dlm)
    close(f)
    nothing
end

dlmwrite(f, a) = dlmwrite(f, a, ',')
csvwrite(f, a) = dlmwrite(f, a, ',')
