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
        error("only ASCII line terminators are supported")
    end
    a = Array(Uint8, 8192)
    nl = 0
    preceded_by_eol = true
    while !eof(io)
        nb = readbytes!(io, a)
        for i=1:nb
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

readdlm(input, T::Type; opts...) = readdlm(input, invalid_dlm, T, '\n'; opts...)
readdlm(input, dlm::Char, T::Type; opts...) = readdlm(input, dlm, T, '\n'; opts...)

readdlm(input; opts...) = readdlm(input, invalid_dlm, '\n'; opts...)
readdlm(input, dlm::Char; opts...) = readdlm(input, dlm, '\n'; opts...)

readdlm(input, dlm::Char, eol::Char; opts...) = readdlm_auto(input, dlm, Float64, eol, true; opts...)
readdlm(input, dlm::Char, T::Type, eol::Char; opts...) = readdlm_auto(input, dlm, T, eol, false; opts...)

function readdlm_auto(input, dlm::Char, T::Type, eol::Char, auto::Bool; opts...)
    optsd = val_opts(opts)
    isa(input, String) && (fsz = filesize(input); input = get(optsd, :use_mmap, true) && (fsz > 0) && fsz < typemax(Int) ? as_mmap(input,fsz) : readall(input))
    sinp = isa(input, Vector{Uint8}) ? ccall(:jl_array_to_string, ByteString, (Array{Uint8,1},), input) :
           isa(input, IO) ? readall(input) :
           input
    readdlm_string(sinp, dlm, T, eol, auto, optsd)
end

function as_mmap(fname::String, fsz::Int64)
    open(fname) do io
        mmap_array(Uint8, (int(fsz),), io)
    end
end

function ascii_if_possible(sbuff::String)
    isa(sbuff, ASCIIString) && return sbuff

    asci = true
    d = sbuff.data
    for idx in 1:length(d)
        (d[idx] < 0x80) ? continue : (asci = false; break)
    end
    asci ? ASCIIString(sbuff.data) : sbuff
end

function readdlm_string(sbuff::String, dlm::Char, T::Type, eol::Char, auto::Bool, optsd::Dict)
    ign_empty = (dlm == invalid_dlm)
    quotes = get(optsd, :quotes, true)

    nrows,ncols,offsets = try
            dlm_dims(sbuff, eol, dlm, '"', ign_empty, quotes)
        catch ex
            !get(optsd, :ignore_invalid_chars, false) && throw(ex)
            sbuff = ascii_if_possible(convert(typeof(sbuff), sbuff.data, ""))
            dlm_dims(sbuff, eol, dlm, '"', ign_empty, quotes)
        end
    has_header = get(optsd, :has_header, false)
    cells = Array(T, has_header ? nrows-1 : nrows, ncols)
    has_header ? (dlm_fill(cells, offsets, sbuff, auto, 1, eol), dlm_fill(Array(String, 1, ncols), offsets, sbuff, auto, 0, eol)) : dlm_fill(cells, offsets, sbuff, auto, 0, eol)
end

const valid_opts = [:has_header, :ignore_invalid_chars, :use_mmap, :quotes]
function val_opts(opts)
    d = Dict{Symbol,Bool}()
    for opt in opts
        !in(opt[1], valid_opts) && error("unknown option $(opt[1])")
        !isa(opt[2], Bool) && error("$(opt[1]) can only be boolean")
        d[opt[1]] = opt[2]
    end
    d
end

const offs_chunk_size = 5000

function dlm_fill{T}(cells::Array{T,2}, offarr::Vector{Vector{Int}}, sbuff::String, auto::Bool, row_offset::Int, eol::Char)
    maxrow,maxcol = size(cells)
    const tmp64 = Array(Float64,1)

    idx = 1
    lastrow = 1
    lastcol = 0
    offidx = 1
    offsets = offarr[1]
    fail = false
    
    while idx <= length(offsets)
        row = offsets[idx] - row_offset
        col = offsets[idx+1]
        quoted = bool(offsets[idx+2])
        startpos = offsets[idx+3]
        endpos = offsets[idx+4]
        ((idx += 5) > offs_chunk_size) && (offidx < length(offarr)) && (idx = 1; offsets = offarr[offidx += 1])

        (row < 1) && continue
        (row > maxrow) && break

        while lastrow < row
            (lastcol == maxcol) && (lastcol = 0; lastrow += 1)
            for cidx in (lastcol+1):maxcol
                if (T <: String) || (T == Any)
                    cells[lastrow,cidx] = SubString(sbuff, 1, 0)
                elseif ((T <: Number) || (T <: Char)) && auto
                    return dlm_fill(Array(Any,maxrow,maxcol), offarr, sbuff, false, row_offset, eol)
                else
                    error("missing value at row $lastrow column $cidx")
                end
            end
            lastcol = maxcol
        (lastrow == row) && break
        end

        endpos = prevind(sbuff, nextind(sbuff,endpos))
        (endpos > 0) && ('\n' == eol) && ('\r' == sbuff[endpos]) && (endpos = prevind(sbuff, endpos))
        if quoted 
            sval = SubString(sbuff, startpos+1, endpos-1)
            fail = ('"' in sval) ? _colval(replace(sval, r"\"\"", "\""), cells, row, col, tmp64) : _colval(sval, cells, row, col, tmp64)
        else
            sval = SubString(sbuff, startpos, endpos)
            fail = _colval(sval, cells, row, col, tmp64)
        end

        if fail
            ((T <: Number) && auto) ? (return dlm_fill(Array(Any,maxrow,maxcol), offarr, sbuff, false, row_offset, eol)) : error("file entry \"$(sval)\" cannot be converted to $T")
        end

        lastrow = row
        lastcol = col
    end

    if (lastcol < maxcol) || (lastrow < maxrow)
        while lastrow <= maxrow
            (lastcol == maxcol) && (lastcol = 0; lastrow += 1)
            for cidx in (lastcol+1):maxcol
                if (T <: String) || (T == Any)
                    cells[lastrow,cidx] = SubString(sbuff, 1, 0)
                elseif ((T <: Number) || (T <: Char)) && auto
                    return dlm_fill(Array(Any,maxrow,maxcol), offarr, sbuff, false, row_offset, eol)
                else
                    error("missing value at row $lastrow column $cidx")
                end
            end
            lastcol = maxcol
            (lastrow == maxrow) && break
        end
    end
    cells
end

_colval{T<:Number, S<:String}(sval::S, cells::Array{T,2}, row::Int, col::Int, tmp64::Array{Float64,1}) = (float64_isvalid(sval, tmp64) ? ((cells[row,col] = tmp64[1]); false) : true)
_colval{T<:String, S<:String}(sval::S, cells::Array{T,2}, row::Int, col::Int, tmp64::Array{Float64,1}) = ((cells[row,col] = sval); false)
_colval{S<:String}(sval::S, cells::Array{Any,2}, row::Int, col::Int, tmp64::Array{Float64,1}) = ((cells[row,col] = float64_isvalid(sval, tmp64) ? tmp64[1] : sval); false)
_colval{T<:Char, S<:String}(sval::S, cells::Array{T,2}, row::Int, col::Int, tmp64::Array{Float64,1}) = ((length(sval) == 1) ? ((cells[row,col] = next(sval,1)[1]); false) : true)
_colval{S<:String}(sval::S, cells::Array, row::Int, col::Int, tmp64::Array{Float64,1}) = true

function store_column(oarr::Vector{Vector{Int}}, row::Int, col::Int, quoted::Bool, startpos::Int, endpos::Int, offidx::Int)
    offsets = oarr[end]
    if length(offsets) < offidx
        offsets = Array(Int, offs_chunk_size)
        push!(oarr, offsets)
        offidx = 1
    end
    offsets[offidx] = row
    offsets[offidx+1] = col
    offsets[offidx+2] = int(quoted)
    offsets[offidx+3] = startpos
    offsets[offidx+4] = endpos
    offidx + 5
end

dlm_dims(s::ASCIIString, eol::Char, dlm::Char, qchar::Char, ign_adj_dlm::Bool, allow_quote::Bool) = dlm_dims(s.data, uint8(eol), uint8(dlm), uint8(qchar), ign_adj_dlm, allow_quote)
function dlm_dims{T,D}(dbuff::T, eol::D, dlm::D, qchar::D, ign_adj_dlm::Bool, allow_quote::Bool)
    qascii = !allow_quote || (D <: Uint8) || isascii(qchar)
    (T <: UTF8String) && isascii(eol) && isascii(dlm) && qascii && (return dlm_dims(dbuff.data, uint8(eol), uint8(dlm), uint8(qchar), ign_adj_dlm, allow_quote))
    ncols = nrows = col = 0
    is_default_dlm = (dlm == convert(D, invalid_dlm))
    error_str = ""
    state = 0   # 0: begin field, 1: quoted field, 2: unquoted field, 3: second quote (could either be end of field or escape character)
    is_eol = is_dlm = is_quote = expct_col = false
    idx = 1
    offsets = Array(Array{Int,1}, 1)
    offsets[1] = Array(Int, offs_chunk_size)
    offidx = 1
    try
        slen = sizeof(dbuff)
        col_start_idx = 1
        while idx <= slen
            val,idx = next(dbuff, idx)
            is_eol = (val == eol)
            is_dlm = is_eol ? false : is_default_dlm ? in(val, _default_delims) : (val == dlm)
            is_quote = (val == qchar)

            if 2 == state   # unquoted field
                if is_dlm
                    state = 0
                    col += 1
                    offidx = store_column(offsets, nrows+1, col, false, col_start_idx, idx-2, offidx)
                    col_start_idx = idx
                    !ign_adj_dlm && (expct_col = true)
                elseif is_eol
                    nrows += 1
                    col += 1
                    offidx = store_column(offsets, nrows, col, false, col_start_idx, idx-2, offidx)
                    col_start_idx = idx
                    ncols = max(ncols, col)
                    col = 0
                    state = 0
                end
            elseif 1 == state   # quoted field
                is_quote && (state = 3)
            elseif 0 == state   # begin field
                if is_quote
                    state = allow_quote ? 1 : 2
                    expct_col = false
                elseif is_dlm
                    if !ign_adj_dlm 
                        expct_col = true
                        col += 1
                        offidx = store_column(offsets, nrows+1, col, false, col_start_idx, idx-2, offidx)
                    end
                    col_start_idx = idx
                elseif is_eol
                    nrows += 1
                    if expct_col 
                        col += 1
                        offidx = store_column(offsets, nrows, col, false, col_start_idx, idx-2, offidx)
                    end
                    col_start_idx = idx
                    ncols = max(ncols, col)
                    col = 0
                    expct_col = false
                else
                    state = 2
                    expct_col = false
                end
            elseif 3 == state   # second quote
                if is_quote
                    state = 1
                elseif is_dlm
                    state = 0
                    col += 1
                    offidx = store_column(offsets, nrows+1, col, true, col_start_idx, idx-2, offidx)
                    col_start_idx = idx
                    !ign_adj_dlm && (expct_col = true)
                elseif is_eol
                    nrows += 1
                    col += 1
                    offidx = store_column(offsets, nrows, col, true, col_start_idx, idx-2, offidx)
                    col_start_idx = idx
                    ncols = max(ncols, col)
                    col = 0
                    state = 0
                else
                    error_str = "unexpected character '$(char(val))' after quoted field at row $(nrows+1) column $(col+1)"
                    break
                end
            end
        end

        if isempty(error_str)
            if 1 == state       # quoted field
                error_str = "truncated column at row $(nrows+1) column $(col+1)"
            elseif (2 == state) || (3 == state) || ((0 == state) && is_dlm)   # unquoted field, second quote, or begin field with last character as delimiter
                col += 1
                nrows += 1
                offidx = store_column(offsets, nrows, col, (3 == state), col_start_idx, idx-1, offidx)
                ncols = max(ncols, col)
            end
        end
    catch ex
        error("at row $(nrows+1), column $col : $ex)")
    end
    !isempty(error_str) && error(error_str)
    
    ncols = max(ncols, 1)
    nrows = max(nrows, 1)
    trimsz = (offidx-1)%offs_chunk_size
    (trimsz > 0) && resize!(offsets[end], trimsz)
    return (nrows, ncols, offsets)
end

readcsv(io; opts...)          = readdlm(io, ','; opts...)
readcsv(io, T::Type; opts...) = readdlm(io, ',', T; opts...)

# todo: keyword argument for # of digits to print
writedlm_cell(io::IO, elt::FloatingPoint, dlm, quotes) = print_shortest(io, elt)
function writedlm_cell{T}(io::IO, elt::String, dlm::T, quotes::Bool)
    if quotes && !isempty(elt) && (('"' in elt) || ('\n' in elt) || ((T <: Char) ? (dlm in elt) : contains(elt, dlm)))
        print(io, '"', replace(elt, r"\"", "\"\""), '"')
    else
        print(io, elt)
    end
end
writedlm_cell(io::IO, elt, dlm, quotes) = print(io, elt)
function writedlm(io::IO, a::AbstractVecOrMat, dlm; opts...)
    optsd = val_opts(opts)
    quotes = get(optsd, :quotes, true)
    pb = PipeBuffer()
    nr = size(a,1)
    nc = size(a,2)
    for i = 1:nr
        for j = 1:nc
            writedlm_cell(pb, a[i,j], dlm, quotes)
            j == nc ? write(pb,'\n') : print(pb,dlm)
        end
        (nb_available(pb) > (16*1024)) && write(io, takebuf_array(pb))
    end
    write(io, takebuf_array(pb))
    nothing
end

writedlm{T}(io::IO, a::AbstractArray{T,0}, dlm; opts...) = writedlm(io, reshape(a,1), dlm; opts...)

function writedlm(io::IO, a::AbstractArray, dlm; opts...)
    tail = size(a)[3:end]
    function print_slice(idxs...)
        writedlm(io, sub(a, 1:size(a,1), 1:size(a,2), idxs...), dlm; opts...)
        if idxs != tail
            print("\n")
        end
    end
    cartesianmap(print_slice, tail)
end

function writedlm(io::IO, itr, dlm; opts...)
    optsd = val_opts(opts)
    quotes = get(optsd, :quotes, true)
    pb = PipeBuffer()
    for row in itr
        state = start(row)
        while !done(row, state)
            (x, state) = next(row, state)
            writedlm_cell(pb, x, dlm, quotes)
            done(row, state) ? write(pb,'\n') : print(pb,dlm)
        end
        (nb_available(pb) > (16*1024)) && write(io, takebuf_array(pb))
    end
    write(io, takebuf_array(pb))
    nothing
end

function writedlm(fname::String, a, dlm; opts...)
    open(fname, "w") do io
        writedlm(io, a, dlm; opts...)
    end
end

writedlm(io, a; opts...) = writedlm(io, a, '\t'; opts...)
writecsv(io, a; opts...) = writedlm(io, a, ','; opts...)

writemime(io::IO, ::MIME"text/csv", a::AbstractVecOrMat) = writedlm(io, a, ',')
writemime(io::IO, ::MIME"text/tab-separated-values", a::AbstractVecOrMat) = writedlm(io, a, '\t')
