## file formats ##

function _jl_dlm_readrow(f, dlm, eol)
    row = split(readuntil(f, eol), dlm, true)
    row[end] = chomp(row[end])
    row
end

# all strings
function _jl_dlmread(a, f, dlm, nr, nc, row, eol)
    for i=1:nr
        a[i,:] = row
        if i < nr
            row = _jl_dlm_readrow(f, dlm, eol)
        end
    end
    a
end

# all numeric, with NaN for invalid data
function _jl_dlmread{T<:Number}(a::Array{T}, f, dlm, nr, nc, row, eol)
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
            row = _jl_dlm_readrow(f, dlm, eol)
        end
    end
end

# float64 or string
_jl_dlmread(a::Array{Any}, f, dlm, nr, nc, row, eol) =
    _jl_dlmread(a, f, dlm, nr, nc, row, eol, 1, 1)
function _jl_dlmread(a::Array{Any}, f, dlm, nr, nc, row, eol, i0, j0)
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
            row = _jl_dlm_readrow(f, dlm, eol)
        end
    end
    a
end

# float64 or cell depending on data
function _jl_dlmread_auto(a, f, dlm, nr, nc, row, eol)
    tmp = Array(Float64, 1)
    for i=1:nr
        for j=1:nc
            el = row[j]
            if !float64_isvalid(el, tmp)
                a = convert(Array{Any,2}, a)
                _jl_dlmread(a, f, dlm, nr, nc, row, eol, i, j)
                return a
            else
                a[i,j] = tmp[1]
            end
        end
        if i < nr
            row = _jl_dlm_readrow(f, dlm, eol)
        end
    end
    a
end

countlines(f) = countlines(f, '\n')
function countlines(f::String, eol::Char)
    fh = open(f)
    n = countlines(fh, eol)
    close(fh)
    return n
end
function countlines(f::IOStream, eol::Char)
    if !iswascii(eol)
        error("countlines: only ASCII line terminators supported")
    end
    a = Array(Uint8, 8192)
    nl = 0
    while !eof(f)
        fill!(a, uint8(eol)+1)  # fill with byte we're not looking for
        try
            read(f, a)
        end
        for i=1:length(a)
            if a[i] == eol
                nl+=1
            end
        end
    end
    skip(f,-1)
    if read(f,Uint8) != eol
        nl+=1
    end
    nl
end

function _jl_dlmread_setup(fname::String, dlm, eol)
    if length(dlm) == 0
        error("dlmread: no separator characters specified")
    end
    nr = countlines(fname,eol)
    f = open(fname)
    row = _jl_dlm_readrow(f, dlm, eol)
    nc = length(row)
    return (f, nr, nc, row)
end

const _jl_dlmread_default_delimiters = [' ', ',', ';', '\t', '\v']

dlmread(fname::String, T::Type) = dlmread(fname, _jl_dlmread_default_delimiters, T, '\n')

dlmread(fname::String, dlm, T::Type) = dlmread(fname, dlm, T, '\n')

function dlmread(fname::String, dlm, T::Type, eol::Char)
    (f, nr, nc, row) = _jl_dlmread_setup(fname, dlm, eol)
    a = Array(T, nr, nc)
    _jl_dlmread(a, f, dlm, nr, nc, row, eol)
    close(f)
    return a
end

dlmread(fname::String) = dlmread(fname, _jl_dlmread_default_delimiters, '\n')

dlmread(fname::String, dlm) = dlmread(fname, dlm, '\n')

function dlmread(fname::String, dlm, eol::Char)
    (f, nr, nc, row) = _jl_dlmread_setup(fname, dlm, eol)
    a = Array(Float64, nr, nc)
    a = _jl_dlmread_auto(a, f, dlm, nr, nc, row, eol)
    close(f)
    return a
end

csvread(f)          = dlmread(f, ',')
csvread(f, T::Type) = dlmread(f, ',', T)

# todo: keyword argument for # of digits to print
function dlmwrite(f, a, dlm::Char)
    nr, nc = size(a)
    for i = 1:nr
        for j = 1:nc
            elt = a[i,j]
            if isa(elt,Float)
                print_shortest(f, elt)
            else
                print(f, elt)
            end
            if j < nc
                write(f, dlm)
            end
        end
        write(f, '\n')
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
