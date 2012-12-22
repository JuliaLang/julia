## file formats ##

const _jl_invalid_dlm = char(0xfffffffe)

function _jl_dlm_readrow(io::IO, dlm, eol::Char)
    row_string = readuntil(io, eol)
    while length(row_string)==1 && row_string[1] == eol
        row_string = readuntil(io, eol)
    end
    if dlm == _jl_invalid_dlm
        row = split(row_string)
    else
        row = split(row_string, dlm, true)
    end
    if ends_with(row[end], eol)
        row[end] = chop(row[end])
    end
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
function countlines(filename::String, eol::Char)
    open(filename) do io
        countlines(io, eol)
    end
end
function countlines(io::IOStream, eol::Char)
    if !iswascii(eol)
        error("countlines: only ASCII line terminators supported")
    end
    a = Array(Uint8, 8192)
    nl = 0
    preceded_by_eol = true
    while !eof(io)
        fill!(a, uint8(eol))
        try
            read(io, a)
        end
        for i=1:length(a)
            if a[i] == eol
                preceded_by_eol = true
            elseif preceded_by_eol
                preceded_by_eol = false
                nl+=1
            end
        end
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

dlmread(fname::String, T::Type) = dlmread(fname, _jl_invalid_dlm, T, '\n')

dlmread(fname::String, dlm, T::Type) = dlmread(fname, dlm, T, '\n')

function dlmread(fname::String, dlm, T::Type, eol::Char)
    (io, nr, nc, row) = _jl_dlmread_setup(fname, dlm, eol)
    a = Array(T, nr, nc)
    _jl_dlmread(a, io, dlm, nr, nc, row, eol)
    close(io)
    return a
end

dlmread(fname::String) = dlmread(fname, _jl_invalid_dlm, '\n')
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
function dlmwrite(io, a::Matrix, dlm::Char)
    nr, nc = size(a)
    for i = 1:nr
        for j = 1:nc
            elt = a[i,j]
            if isa(elt,FloatingPoint)
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

dlmwrite(io, a::Vector, dlm::Char) = dlmwrite(io, reshape(a,length(a),1), dlm)

function dlmwrite(fname::String, a::Matrix, dlm::Char)
    open(fname, "w") do io
        dlmwrite(io, a, dlm)
    end
end

dlmwrite(io, a) = dlmwrite(io, a, ',')
csvwrite(io, a) = dlmwrite(io, a, ',')
