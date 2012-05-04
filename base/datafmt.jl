## file formats ##

function _jl_dlm_readrow(io, dlm, eol)
    row = split(readuntil(io, eol), dlm, true)
    row[end] = chomp(row[end])
    row
end

# all strings
function _jl_dlmread(a, io, dlm, nr, nc, row, eol)
    for i=1:nr
        a[i,:] = row
        if i < nr
            row = _jl_dlm_readrow(io, dlm, eol)
        end
    end
    a
end

# all numeric, with NaN for invalid data
function _jl_dlmread{T<:Number}(a::Array{T}, io, dlm, nr, nc, row, eol)
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
            row = _jl_dlm_readrow(io, dlm, eol)
        end
    end
end

# float64 or string
_jl_dlmread(a::Array{Any}, io, dlm, nr, nc, row, eol) =
    _jl_dlmread(a, io, dlm, nr, nc, row, eol, 1, 1)
function _jl_dlmread(a::Array{Any}, io, dlm, nr, nc, row, eol, i0, j0)
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
            row = _jl_dlm_readrow(io, dlm, eol)
        end
    end
    a
end

# float64 or cell depending on data
function _jl_dlmread_auto(a, io, dlm, nr, nc, row, eol)
    tmp = Array(Float64, 1)
    for i=1:nr
        for j=1:nc
            el = row[j]
            if !float64_isvalid(el, tmp)
                a = convert(Array{Any,2}, a)
                _jl_dlmread(a, io, dlm, nr, nc, row, eol, i, j)
                return a
            else
                a[i,j] = tmp[1]
            end
        end
        if i < nr
            row = _jl_dlm_readrow(io, dlm, eol)
        end
    end
    a
end

countlines(io) = countlines(io, '\n')
function countlines(io::String, eol::Char)
    fh = open(io)
    n = countlines(fh, eol)
    close(fh)
    return n
end
function countlines(io::IOStream, eol::Char)
    if !iswascii(eol)
        error("countlines: only ASCII line terminators supported")
    end
    a = Array(Uint8, 8192)
    nl = 0
    while !eof(io)
        fill!(a, uint8(eol)+1)  # fill with byte we're not looking for
        try
            read(io, a)
        end
        for i=1:length(a)
            if a[i] == eol
                nl+=1
            end
        end
    end
    skip(io,-1)
    if read(io,Uint8) != eol
        nl+=1
    end
    nl
end

function _jl_dlmread_setup(fname::String, dlm, eol)
    if length(dlm) == 0
        error("dlmread: no separator characters specified")
    end
    nr = countlines(fname,eol)
    io = open(fname)
    row = _jl_dlm_readrow(io, dlm, eol)
    nc = length(row)
    return (io, nr, nc, row)
end

const _jl_dlmread_default_delimiters = [' ', ',', ';', '\t', '\v']

dlmread(fname::String, T::Type) = dlmread(fname, _jl_dlmread_default_delimiters, T, '\n')

dlmread(fname::String, dlm, T::Type) = dlmread(fname, dlm, T, '\n')

function dlmread(fname::String, dlm, T::Type, eol::Char)
    (io, nr, nc, row) = _jl_dlmread_setup(fname, dlm, eol)
    a = Array(T, nr, nc)
    _jl_dlmread(a, io, dlm, nr, nc, row, eol)
    close(io)
    return a
end

dlmread(fname::String) = dlmread(fname, _jl_dlmread_default_delimiters, '\n')
dlmread(fname::String, dlm) = dlmread(fname, dlm, '\n')

function dlmread(fname::String, dlm, eol::Char)
    (io, nr, nc, row) = _jl_dlmread_setup(fname, dlm, eol)
    a = Array(Float64, nr, nc)
    a = _jl_dlmread_auto(a, io, dlm, nr, nc, row, eol)
    close(io)
    return a
end

csvread(io)          = dlmread(io, ',')
csvread(io, T::Type) = dlmread(io, ',', T)

# todo: keyword argument for # of digits to print
function dlmwrite(io, a, dlm::Char)
    nr, nc = size(a)
    for i = 1:nr
        for j = 1:nc
            elt = a[i,j]
            if isa(elt,Float)
                print_shortest(io, elt)
            else
                print(io, elt)
            end
            if j < nc
                write(io, dlm)
            end
        end
        write(io, '\n')
    end
    nothing
end

function dlmwrite(fname::String, a, dlm::Char)
    io = open(fname, "w")
    dlmwrite(io, a, dlm)
    close(io)
    nothing
end

dlmwrite(io, a) = dlmwrite(io, a, ',')
csvwrite(io, a) = dlmwrite(io, a, ',')
