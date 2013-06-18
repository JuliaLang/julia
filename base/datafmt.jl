## file formats ##

const invalid_dlm = char(0xfffffffe)

countlines(nameorfile) = countlines(nameorfile, '\n')
function countlines(filename::String, eol::Char)
    open(filename) do io
        countlines(io, eol)
    end
end
function countlines(io::IO, eol::Char)
    if !isascii(eol)
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

readdlm(input, T::Type) = readdlm(input, invalid_dlm, T, '\n')
readdlm(input, dlm::Char, T::Type) = readdlm(input, dlm, T, '\n')

readdlm(input) = readdlm(input, invalid_dlm, '\n')
readdlm(input, dlm::Char) = readdlm(input, dlm, '\n')

readdlm(input, dlm::Char, eol::Char) = readdlm_auto(input, dlm, Float64, eol, true)
readdlm(input, dlm::Char, T::Type, eol::Char) = readdlm_auto(input, dlm, T, eol, false)

readdlm_auto(input, dlm::Char, T::Type, eol::Char, auto::Bool=false) = readdlm_string(readall(input), dlm, T, eol, auto)
readdlm_auto(input::Vector{Uint8}, dlm::Char, T::Type, eol::Char, auto::Bool=false) = readdlm_string(bytestring(input), dlm, T, eol, auto)

function readdlm_string(sbuff::String, dlm::Char, T::Type, eol::Char, auto::Bool=false)
    nrows,ncols = dlm_dims(sbuff, eol, dlm)
    offsets = zeros(Int, nrows, ncols)
    cells = Array(T, nrows, ncols)
    dlm_offsets(sbuff, dlm, eol, offsets)
    dlm_fill(cells, offsets, sbuff, auto)
end

function dlm_col_begin(ncols::Int, offsets::Array{Int,2}, row::Int, col::Int)
    (row == 1) && (col == 1) && return 1
    pp_row = (1 == col) ? (row-1) : row
    pp_col = (1 == col) ? ncols : (col-1)

    ret = offsets[pp_row, pp_col]
    (ret == 0) ? dlm_col_begin(ncols, offsets, pp_row, pp_col) : (ret+2)
end

function dlm_fill{T}(cells::Array{T,2}, offsets::Array{Int,2}, sbuff::String, auto::Bool)
    maxrow,maxcol = size(cells)
    tmp64 = Array(Float64,1)
    for row in 1:maxrow
        for col in 1:maxcol
            start_pos = dlm_col_begin(maxcol, offsets, row, col)
            end_pos = offsets[row,col]
            sval = SubString(sbuff, start_pos, end_pos)

            if T <: Char
                (length(sval) != 1) && error("file entry \"$(sval)\" is not a Char")
                cells[row,col] = sval
            elseif T <: Number
                if(float64_isvalid(sval, tmp64))
                    cells[row,col] = tmp64[1]
                elseif auto
                    return dlm_fill(Array(Any,maxrow,maxcol), offsets, sbuff, false)
                else
                    cells[row,col] = NaN
                end
            elseif T <: String
                cells[row,col] = sval
            elseif T == Any
                cells[row,col] = float64_isvalid(sval, tmp64) ? tmp64[1] : sval
            else
                error("file entry \"$(sval)\" cannot be converted to $T")
            end
        end
    end
    cells
end


function dlm_offsets(sbuff::UTF8String, dlm, eol, offsets::Array{Int,2})
    col = 0
    row = 1
    maxrow,maxcol = size(offsets)
    idx = 1
    while(idx < length(sbuff.data))
        val,idx = next(sbuff, idx)
        (val != eol) && ((dlm == invalid_dlm) ? !contains(_default_delims, val) : (val != dlm)) && continue
        col += 1
        offsets[row,col] = idx-2
        (row >= maxrow) && (col == maxcol) && break
        (val == eol) && (row += 1; col = 0)
    end
end
function dlm_offsets(sbuff::ASCIIString, dlmc, eolc, offsets::Array{Int,2})
    dbuff = sbuff.data
    dlm = uint8(dlmc)
    eol = uint8(eolc)
    col = 0
    row = 1
    maxrow,maxcol = size(offsets)
    for idx in 1:length(dbuff)
        val = dbuff[idx]
        (val != eol) && ((dlm == invalid_dlm) ? !contains(_default_delims, val) : (val != dlm)) && continue
        col += 1
        offsets[row,col] = idx-1
        (row >= maxrow) && (col == maxcol) && break
        (val == eol) && (row += 1; col = 0)
    end
end

dlm_dims(s::ASCIIString, eol, dlm) = dlm_dims(s.data, uint8(eol), uint8(dlm))
function dlm_dims(dbuff, eol, dlm)
    ncols = nrows = col = 0
    for val in dbuff
        (val != eol) && ((dlm == invalid_dlm) ? !contains(_default_delims, val) : (val != dlm)) && continue
        col += 1
        (val == eol) && (nrows += 1; ncols = max(ncols, col); col = 0)
    end
    (col > 0) && (nrow += 1) 
    ncols = max(ncols, col, 1)
    nrows = max(nrows, 1)
    return (nrows, ncols)
end

readcsv(io)          = readdlm(io, ',')
readcsv(io, T::Type) = readdlm(io, ',', T)

# todo: keyword argument for # of digits to print
function writedlm(io::IO, a::Matrix, dlm::Char)
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

writedlm(io::IO, a::Vector, dlm::Char) = writedlm(io, reshape(a,length(a),1), dlm)

function writedlm(fname::String, a::Union(Vector,Matrix), dlm::Char)
    open(fname, "w") do io
        writedlm(io, a, dlm)
    end
end

writedlm(io, a) = writedlm(io, a, '\t')
writecsv(io, a) = writedlm(io, a, ',')
