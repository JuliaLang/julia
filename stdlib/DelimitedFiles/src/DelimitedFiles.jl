# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

"""
Utilities for reading and writing delimited files, for example ".csv".
See [`readdlm`](@ref) and [`writedlm`](@ref).
"""
module DelimitedFiles

using Mmap

import Base: _default_delims, tryparse_internal, show

export readdlm, writedlm

Base.@deprecate readcsv(io; opts...) readdlm(io, ','; opts...)
Base.@deprecate readcsv(io, T::Type; opts...) readdlm(io, ',', T; opts...)
Base.@deprecate writecsv(io, a; opts...) writedlm(io, a, ','; opts...)

invalid_dlm(::Type{Char})   = reinterpret(Char, 0xfffffffe)
invalid_dlm(::Type{UInt8})  = 0xfe
invalid_dlm(::Type{UInt16}) = 0xfffe
invalid_dlm(::Type{UInt32}) = 0xfffffffe

const offs_chunk_size = 5000

"""
    readdlm(source, T::Type; options...)

The columns are assumed to be separated by one or more whitespaces. The end of line
delimiter is taken as `\\n`.

# Examples
```jldoctest
julia> using DelimitedFiles

julia> x = [1; 2; 3; 4];

julia> y = [5; 6; 7; 8];

julia> open("delim_file.txt", "w") do io
           writedlm(io, [x y])
       end;

julia> readdlm("delim_file.txt", Int64)
4×2 Array{Int64,2}:
 1  5
 2  6
 3  7
 4  8

julia> readdlm("delim_file.txt", Float64)
4×2 Array{Float64,2}:
 1.0  5.0
 2.0  6.0
 3.0  7.0
 4.0  8.0

julia> rm("delim_file.txt")
```
"""
readdlm(input, T::Type; opts...) = readdlm(input, invalid_dlm(Char), T, '\n'; opts...)

"""
    readdlm(source, delim::AbstractChar, T::Type; options...)

The end of line delimiter is taken as `\\n`.

# Examples
```jldoctest
julia> using DelimitedFiles

julia> x = [1; 2; 3; 4];

julia> y = [1.1; 2.2; 3.3; 4.4];

julia> open("delim_file.txt", "w") do io
           writedlm(io, [x y], ',')
       end;

julia> readdlm("delim_file.txt", ',', Float64)
4×2 Array{Float64,2}:
 1.0  1.1
 2.0  2.2
 3.0  3.3
 4.0  4.4

julia> rm("delim_file.txt")
```
"""
readdlm(input, dlm::AbstractChar, T::Type; opts...) = readdlm(input, dlm, T, '\n'; opts...)

"""
    readdlm(source; options...)

The columns are assumed to be separated by one or more whitespaces. The end of line
delimiter is taken as `\\n`. If all data is numeric, the result will be a numeric array. If
some elements cannot be parsed as numbers, a heterogeneous array of numbers and strings
is returned.

# Examples
```jldoctest
julia> using DelimitedFiles

julia> x = [1; 2; 3; 4];

julia> y = ["a"; "b"; "c"; "d"];

julia> open("delim_file.txt", "w") do io
           writedlm(io, [x y])
       end;

julia> readdlm("delim_file.txt")
4×2 Array{Any,2}:
 1  "a"
 2  "b"
 3  "c"
 4  "d"

julia> rm("delim_file.txt")
```
"""
readdlm(input; opts...) = readdlm(input, invalid_dlm(Char), '\n'; opts...)

"""
    readdlm(source, delim::AbstractChar; options...)

The end of line delimiter is taken as `\\n`. If all data is numeric, the result will be a
numeric array. If some elements cannot be parsed as numbers, a heterogeneous array of
numbers and strings is returned.

# Examples
```jldoctest
julia> using DelimitedFiles

julia> x = [1; 2; 3; 4];

julia> y = [1.1; 2.2; 3.3; 4.4];

julia> open("delim_file.txt", "w") do io
           writedlm(io, [x y], ',')
       end;

julia> readdlm("delim_file.txt", ',')
4×2 Array{Float64,2}:
 1.0  1.1
 2.0  2.2
 3.0  3.3
 4.0  4.4

julia> rm("delim_file.txt")

julia> z = ["a"; "b"; "c"; "d"];

julia> open("delim_file.txt", "w") do io
           writedlm(io, [x z], ',')
       end;

julia> readdlm("delim_file.txt", ',')
4×2 Array{Any,2}:
 1  "a"
 2  "b"
 3  "c"
 4  "d"

julia> rm("delim_file.txt")
```
"""
readdlm(input, dlm::AbstractChar; opts...) = readdlm(input, dlm, '\n'; opts...)

"""
    readdlm(source, delim::AbstractChar, eol::AbstractChar; options...)

If all data is numeric, the result will be a numeric array. If some elements cannot be
parsed as numbers, a heterogeneous array of numbers and strings is returned.
"""
readdlm(input, dlm::AbstractChar, eol::AbstractChar; opts...) =
    readdlm_auto(input, dlm, Float64, eol, true; opts...)

"""
    readdlm(source, delim::AbstractChar, T::Type, eol::AbstractChar; header=false, skipstart=0, skipblanks=true, use_mmap, quotes=true, dims, comments=false, comment_char='#')

Read a matrix from the source where each line (separated by `eol`) gives one row, with
elements separated by the given delimiter. The source can be a text file, stream or byte
array. Memory mapped files can be used by passing the byte array representation of the
mapped segment as source.

If `T` is a numeric type, the result is an array of that type, with any non-numeric elements
as `NaN` for floating-point types, or zero. Other useful values of `T` include
`String`, `AbstractString`, and `Any`.

If `header` is `true`, the first row of data will be read as header and the tuple
`(data_cells, header_cells)` is returned instead of only `data_cells`.

Specifying `skipstart` will ignore the corresponding number of initial lines from the input.

If `skipblanks` is `true`, blank lines in the input will be ignored.

If `use_mmap` is `true`, the file specified by `source` is memory mapped for potential
speedups. Default is `true` except on Windows. On Windows, you may want to specify `true` if
the file is large, and is only read once and not written to.

If `quotes` is `true`, columns enclosed within double-quote (\") characters are allowed to
contain new lines and column delimiters. Double-quote characters within a quoted field must
be escaped with another double-quote.  Specifying `dims` as a tuple of the expected rows and
columns (including header, if any) may speed up reading of large files.  If `comments` is
`true`, lines beginning with `comment_char` and text following `comment_char` in any line
are ignored.

# Examples
```jldoctest
julia> using DelimitedFiles

julia> x = [1; 2; 3; 4];

julia> y = [5; 6; 7; 8];

julia> open("delim_file.txt", "w") do io
           writedlm(io, [x y])
       end

julia> readdlm("delim_file.txt", '\\t', Int, '\\n')
4×2 Array{Int64,2}:
 1  5
 2  6
 3  7
 4  8
```
"""
readdlm(input, dlm::AbstractChar, T::Type, eol::AbstractChar; opts...) =
    readdlm_auto(input, dlm, T, eol, false; opts...)

readdlm_auto(input::Vector{UInt8}, dlm::AbstractChar, T::Type, eol::AbstractChar, auto::Bool; opts...) =
    readdlm_string(String(input), dlm, T, eol, auto, val_opts(opts))
readdlm_auto(input::IO, dlm::AbstractChar, T::Type, eol::AbstractChar, auto::Bool; opts...) =
    readdlm_string(read(input, String), dlm, T, eol, auto, val_opts(opts))
function readdlm_auto(input::AbstractString, dlm::AbstractChar, T::Type, eol::AbstractChar, auto::Bool; opts...)
    isfile(input) || throw(ArgumentError("Cannot open \'$input\': not a file"))
    optsd = val_opts(opts)
    use_mmap = get(optsd, :use_mmap, Sys.iswindows() ? false : true)
    fsz = filesize(input)
    if use_mmap && fsz > 0 && fsz < typemax(Int)
        a = open(input, "r") do f
            Mmap.mmap(f, Vector{UInt8}, (Int(fsz),))
        end
        # TODO: It would be nicer to use String(a) without making a copy,
        # but because the mmap'ed array is not NUL-terminated this causes
        # jl_try_substrtod to segfault below.
        return readdlm_string(GC.@preserve(a, unsafe_string(pointer(a),length(a))), dlm, T, eol, auto, optsd)
    else
        return readdlm_string(read(input, String), dlm, T, eol, auto, optsd)
    end
end

#
# Handlers act on events generated by the parser.
# Parser calls store_cell on the handler to pass events.
#
# DLMOffsets: Keep offsets (when result dimensions are not known)
# DLMStore: Store values directly into a result store (when result dimensions are known)
abstract type DLMHandler end

mutable struct DLMOffsets <: DLMHandler
    oarr::Vector{Vector{Int}}
    offidx::Int
    thresh::Int
    bufflen::Int

    function DLMOffsets(sbuff::String)
        offsets = Vector{Vector{Int}}(undef, 1)
        offsets[1] = Vector{Int}(undef, offs_chunk_size)
        thresh = ceil(min(typemax(UInt), Base.Sys.total_memory()) / sizeof(Int) / 5)
        new(offsets, 1, thresh, sizeof(sbuff))
    end
end

function store_cell(dlmoffsets::DLMOffsets, row::Int, col::Int,
        quoted::Bool, startpos::Int, endpos::Int)
    offidx = dlmoffsets.offidx
    (offidx == 0) && return     # offset collection stopped to avoid choking on memory

    oarr = dlmoffsets.oarr
    offsets = oarr[end]
    if length(offsets) < offidx
        offlen = offs_chunk_size * length(oarr)
        if (offlen + offs_chunk_size) > dlmoffsets.thresh
            est_tot = round(Int, offlen * dlmoffsets.bufflen / endpos)
            if (est_tot - offlen) > offs_chunk_size    # allow another chunk
                # abandon offset collection
                dlmoffsets.oarr = Vector{Int}[]
                dlmoffsets.offidx = 0
                return
            end
        end
        offsets = Vector{Int}(undef, offs_chunk_size)
        push!(oarr, offsets)
        offidx = 1
    end
    offsets[offidx] = row
    offsets[offidx+1] = col
    offsets[offidx+2] = Int(quoted)
    offsets[offidx+3] = startpos
    offsets[offidx+4] = endpos
    dlmoffsets.offidx = offidx + 5
    nothing
end

function result(dlmoffsets::DLMOffsets)
    trimsz = (dlmoffsets.offidx-1) % offs_chunk_size
    ((trimsz > 0) || (dlmoffsets.offidx == 1)) && resize!(dlmoffsets.oarr[end], trimsz)
    dlmoffsets.oarr
end

mutable struct DLMStore{T} <: DLMHandler
    hdr::Array{AbstractString, 2}
    data::Array{T, 2}

    nrows::Int
    ncols::Int
    lastrow::Int
    lastcol::Int
    hdr_offset::Int
    sbuff::String
    auto::Bool
    eol::Char
end

function DLMStore(::Type{T}, dims::NTuple{2,Integer},
                  has_header::Bool, sbuff::String, auto::Bool, eol::AbstractChar) where T
    (nrows,ncols) = dims
    nrows <= 0 && throw(ArgumentError("number of rows in dims must be > 0, got $nrows"))
    ncols <= 0 && throw(ArgumentError("number of columns in dims must be > 0, got $ncols"))
    hdr_offset = has_header ? 1 : 0
    DLMStore{T}(fill(SubString(sbuff,1,0), 1, ncols), Matrix{T}(undef, nrows-hdr_offset, ncols),
        nrows, ncols, 0, 0, hdr_offset, sbuff, auto, Char(eol))
end

_chrinstr(sbuff::String, chr::UInt8, startpos::Int, endpos::Int) =
    GC.@preserve sbuff (endpos >= startpos) && (C_NULL != ccall(:memchr, Ptr{UInt8},
    (Ptr{UInt8}, Int32, Csize_t), pointer(sbuff)+startpos-1, chr, endpos-startpos+1))

function store_cell(dlmstore::DLMStore{T}, row::Int, col::Int,
                    quoted::Bool, startpos::Int, endpos::Int) where T
    drow = row - dlmstore.hdr_offset

    ncols = dlmstore.ncols
    lastcol = dlmstore.lastcol
    lastrow = dlmstore.lastrow
    cells::Matrix{T} = dlmstore.data
    sbuff = dlmstore.sbuff

    endpos = prevind(sbuff, nextind(sbuff,endpos))
    if (endpos > 0) && ('\n' == dlmstore.eol) && ('\r' == Char(sbuff[endpos]))
        endpos = prevind(sbuff, endpos)
    end
    if quoted
        startpos += 1
        endpos = prevind(sbuff, endpos)
    end

    if drow > 0
        # fill missing elements
        while ((drow - lastrow) > 1) || ((drow > lastrow > 0) && (lastcol < ncols))
            if (lastcol == ncols) || (lastrow == 0)
                lastcol = 0
                lastrow += 1
            end
            for cidx in (lastcol+1):ncols
                if (T <: AbstractString) || (T == Any)
                    cells[lastrow, cidx] = SubString(sbuff, 1, 0)
                elseif ((T <: Number) || (T <: AbstractChar)) && dlmstore.auto
                    throw(TypeError(:store_cell, "", Any, T))
                else
                    error("missing value at row $lastrow column $cidx")
                end
            end
            lastcol = ncols
        end

        # fill data
        if quoted && _chrinstr(sbuff, UInt8('"'), startpos, endpos)
            unescaped = replace(SubString(sbuff, startpos, endpos), r"\"\"" => "\"")
            fail = colval(unescaped, 1, lastindex(unescaped), cells, drow, col)
        else
            fail = colval(sbuff, startpos, endpos, cells, drow, col)
        end
        if fail
            sval = SubString(sbuff, startpos, endpos)
            if (T <: Number) && dlmstore.auto
                throw(TypeError(:store_cell, "", Any, T))
            else
                error("file entry \"$(sval)\" cannot be converted to $T")
            end
        end

        dlmstore.lastrow = drow
        dlmstore.lastcol = col
    else
        # fill header
        if quoted && _chrinstr(sbuff, UInt8('"'), startpos, endpos)
            unescaped = replace(SubString(sbuff, startpos, endpos), r"\"\"" => "\"")
            colval(unescaped, 1, lastindex(unescaped), dlmstore.hdr, 1, col)
        else
            colval(sbuff, startpos, endpos, dlmstore.hdr, 1, col)
        end
    end

    nothing
end

function result(dlmstore::DLMStore{T}) where T
    nrows = dlmstore.nrows - dlmstore.hdr_offset
    ncols = dlmstore.ncols
    lastcol = dlmstore.lastcol
    lastrow = dlmstore.lastrow
    cells = dlmstore.data
    sbuff = dlmstore.sbuff

    if (nrows > 0) && ((lastcol < ncols) || (lastrow < nrows))
        while lastrow <= nrows
            (lastcol == ncols) && (lastcol = 0; lastrow += 1)
            for cidx in (lastcol+1):ncols
                if (T <: AbstractString) || (T == Any)
                    cells[lastrow, cidx] = SubString(sbuff, 1, 0)
                elseif ((T <: Number) || (T <: AbstractChar)) && dlmstore.auto
                    throw(TypeError(:store_cell, "", Any, T))
                else
                    error("missing value at row $lastrow column $cidx")
                end
            end
            lastcol = ncols
            (lastrow == nrows) && break
        end
        dlmstore.lastrow = lastrow
        dlmstore.lastcol = ncols
    end
    (dlmstore.hdr_offset > 0) ? (dlmstore.data, dlmstore.hdr) : dlmstore.data
end


function readdlm_string(sbuff::String, dlm::AbstractChar, T::Type, eol::AbstractChar, auto::Bool, optsd::Dict)
    ign_empty = (dlm == invalid_dlm(Char))
    quotes = get(optsd, :quotes, true)
    comments = get(optsd, :comments, false)
    comment_char = get(optsd, :comment_char, '#')
    dims = get(optsd, :dims, nothing)

    has_header = get(optsd, :header, get(optsd, :has_header, false))
    haskey(optsd, :has_header) && (optsd[:has_header] != has_header) && throw(ArgumentError("conflicting values for header and has_header"))

    skipstart = get(optsd, :skipstart, 0)
    (skipstart >= 0) || throw(ArgumentError("skipstart must be ≥ 0, got $skipstart"))

    skipblanks = get(optsd, :skipblanks, true)

    offset_handler = (dims === nothing) ? DLMOffsets(sbuff) : DLMStore(T, dims, has_header, sbuff, auto, eol)

    for retry in 1:2
        try
            dims = dlm_parse(sbuff, eol, dlm, '"', comment_char, ign_empty, quotes, comments, skipstart, skipblanks, offset_handler)
            break
        catch ex
            if isa(ex, TypeError) && (ex.func == :store_cell)
                T = ex.expected
            else
                rethrow(ex)
            end
            offset_handler = (dims === nothing) ? DLMOffsets(sbuff) : DLMStore(T, dims, has_header, sbuff, auto, eol)
        end
    end

    isa(offset_handler, DLMStore) && (return result(offset_handler))

    offsets = result(offset_handler)
    !isempty(offsets) && (return dlm_fill(T, offsets, dims, has_header, sbuff, auto, eol))

    optsd[:dims] = dims
    return readdlm_string(sbuff, dlm, T, eol, auto, optsd)
end

const valid_opts = [:header, :has_header, :use_mmap, :quotes, :comments, :dims, :comment_char, :skipstart, :skipblanks]
const valid_opt_types = [Bool, Bool, Bool, Bool, Bool, NTuple{2,Integer}, Char, Integer, Bool]

function val_opts(opts)
    d = Dict{Symbol, Union{Bool, NTuple{2, Integer}, Char, Integer}}()
    for (opt_name, opt_val) in opts
        in(opt_name, valid_opts) ||
            throw(ArgumentError("unknown option $opt_name"))
        opt_typ = valid_opt_types[findfirst(isequal(opt_name), valid_opts)::Int]
        isa(opt_val, opt_typ) ||
            throw(ArgumentError("$opt_name should be of type $opt_typ, got $(typeof(opt_val))"))
        d[opt_name] = opt_val
    end
    return d
end

function dlm_fill(T::DataType, offarr::Vector{Vector{Int}}, dims::NTuple{2,Integer}, has_header::Bool, sbuff::String, auto::Bool, eol::AbstractChar)
    idx = 1
    offidx = 1
    offsets = offarr[1]
    row = 0
    col = 0
    try
        dh = DLMStore(T, dims, has_header, sbuff, auto, eol)
        while idx <= length(offsets)
            row = offsets[idx]
            col = offsets[idx+1]
            quoted = offsets[idx+2] != 0
            startpos = offsets[idx+3]
            endpos = offsets[idx+4]

            ((idx += 5) > offs_chunk_size) && (offidx < length(offarr)) && (idx = 1; offsets = offarr[offidx += 1])

            store_cell(dh, row, col, quoted, startpos, endpos)
        end
        return result(dh)
    catch ex
        isa(ex, TypeError) && (ex.func == :store_cell) && (return dlm_fill(ex.expected, offarr, dims, has_header, sbuff, auto, eol))
        error("at row $row, column $col : $ex")
    end
end

function colval(sbuff::String, startpos::Int, endpos::Int, cells::Array{Bool,2}, row::Int, col::Int)
    n = tryparse_internal(Bool, sbuff, startpos, endpos, 0, false)
    n === nothing || (cells[row, col] = n)
    n === nothing
end
function colval(sbuff::String, startpos::Int, endpos::Int, cells::Array{T,2}, row::Int, col::Int) where T<:Integer
    n = tryparse_internal(T, sbuff, startpos, endpos, 0, false)
    n === nothing || (cells[row, col] = n)
    n === nothing
end
function colval(sbuff::String, startpos::Int, endpos::Int, cells::Array{T,2}, row::Int, col::Int) where T<:Union{Real,Complex}
    n = tryparse_internal(T, sbuff, startpos, endpos, false)
    n === nothing || (cells[row, col] = n)
    n === nothing
end
function colval(sbuff::String, startpos::Int, endpos::Int, cells::Array{<:AbstractString,2}, row::Int, col::Int)
    cells[row, col] = SubString(sbuff, startpos, endpos)
    return false
end
function colval(sbuff::String, startpos::Int, endpos::Int, cells::Array{Any,2}, row::Int, col::Int)
    # if array is of Any type, attempt parsing only the most common types: Int, Bool, Float64 and fallback to SubString
    len = endpos-startpos+1
    if len > 0
        # check Inteter
        ni64 = tryparse_internal(Int, sbuff, startpos, endpos, 0, false)
        ni64 === nothing || (cells[row, col] = ni64; return false)

        # check Bool
        nb = tryparse_internal(Bool, sbuff, startpos, endpos, 0, false)
        nb === nothing || (cells[row, col] = nb; return false)

        # check float64
        hasvalue, valf64 = ccall(:jl_try_substrtod, Tuple{Bool, Float64},
                                 (Ptr{UInt8}, Csize_t, Csize_t), sbuff, startpos-1, endpos-startpos+1)
        hasvalue && (cells[row, col] = valf64; return false)
    end
    cells[row, col] = SubString(sbuff, startpos, endpos)
    false
end
function colval(sbuff::String, startpos::Int, endpos::Int, cells::Array{<:AbstractChar,2}, row::Int, col::Int)
    if startpos == endpos
        cells[row, col] = next(sbuff, startpos)[1]
        return false
    else
        return true
    end
end
colval(sbuff::String, startpos::Int, endpos::Int, cells::Array, row::Int, col::Int) = true

function dlm_parse(dbuff::String, eol::D, dlm::D, qchar::D, cchar::D,
                   ign_adj_dlm::Bool, allow_quote::Bool, allow_comments::Bool,
                   skipstart::Int, skipblanks::Bool, dh::DLMHandler) where D
    ncols = nrows = col = 0
    is_default_dlm = (dlm == invalid_dlm(D))
    error_str = ""
    # 0: begin field, 1: quoted field, 2: unquoted field,
    # 3: second quote (could either be end of field or escape character),
    # 4: comment, 5: skipstart
    state = (skipstart > 0) ? 5 : 0
    is_eol = is_dlm = is_cr = is_quote = is_comment = expct_col = false
    idx = 1
    try
        slen = sizeof(dbuff)
        col_start_idx = 1
        was_cr = false
        while idx <= slen
            val,idx = next(dbuff, idx)
            if (is_eol = (Char(val) == Char(eol)))
                is_dlm = is_comment = is_cr = is_quote = false
            elseif (is_dlm = (is_default_dlm ? in(Char(val), _default_delims) : (Char(val) == Char(dlm))))
                is_comment = is_cr = is_quote = false
            elseif (is_quote = (Char(val) == Char(qchar)))
                is_comment = is_cr = false
            elseif (is_comment = (Char(val) == Char(cchar)))
                is_cr = false
            else
                is_cr = (Char(eol) == '\n') && (Char(val) == '\r')
            end

            if 2 == state   # unquoted field
                if is_dlm
                    state = 0
                    col += 1
                    store_cell(dh, nrows+1, col, false, col_start_idx, idx-2)
                    col_start_idx = idx
                    !ign_adj_dlm && (expct_col = true)
                elseif is_eol
                    nrows += 1
                    col += 1
                    store_cell(dh, nrows, col, false, col_start_idx, idx - (was_cr ? 3 : 2))
                    col_start_idx = idx
                    ncols = max(ncols, col)
                    col = 0
                    state = 0
                elseif (is_comment && allow_comments)
                    nrows += 1
                    col += 1
                    store_cell(dh, nrows, col, false, col_start_idx, idx - 2)
                    ncols = max(ncols, col)
                    col = 0
                    state = 4
                end
            elseif 1 == state   # quoted field
                is_quote && (state = 3)
            elseif 4 == state   # comment line
                if is_eol
                    col_start_idx = idx
                    state = 0
                end
            elseif 0 == state   # begin field
                if is_quote
                    state = (allow_quote && !was_cr) ? 1 : 2
                    expct_col = false
                elseif is_dlm
                    if !ign_adj_dlm
                        expct_col = true
                        col += 1
                        store_cell(dh, nrows+1, col, false, col_start_idx, idx-2)
                    end
                    col_start_idx = idx
                elseif is_eol
                    if (col > 0) || !skipblanks
                        nrows += 1
                        if expct_col
                            col += 1
                            store_cell(dh, nrows, col, false, col_start_idx, idx - (was_cr ? 3 : 2))
                        end
                        ncols = max(ncols, col)
                        col = 0
                    end
                    col_start_idx = idx
                    expct_col = false
                elseif is_comment && allow_comments
                    if col > 0
                        nrows += 1
                        if expct_col
                            col += 1
                            store_cell(dh, nrows, col, false, col_start_idx, idx - 2)
                        end
                        ncols = max(ncols, col)
                        col = 0
                    end
                    expct_col = false
                    state = 4
                elseif !is_cr
                    state = 2
                    expct_col = false
                end
            elseif 3 == state   # second quote
                if is_quote && !was_cr
                    state = 1
                elseif is_dlm && !was_cr
                    state = 0
                    col += 1
                    store_cell(dh, nrows+1, col, true, col_start_idx, idx-2)
                    col_start_idx = idx
                    !ign_adj_dlm && (expct_col = true)
                elseif is_eol
                    nrows += 1
                    col += 1
                    store_cell(dh, nrows, col, true, col_start_idx, idx - (was_cr ? 3 : 2))
                    col_start_idx = idx
                    ncols = max(ncols, col)
                    col = 0
                    state = 0
                elseif is_comment && allow_comments && !was_cr
                    nrows += 1
                    col += 1
                    store_cell(dh, nrows, col, true, col_start_idx, idx - 2)
                    ncols = max(ncols, col)
                    col = 0
                    state = 4
                elseif (is_cr && was_cr) || !is_cr
                    error_str = escape_string("unexpected character '$(Char(val))' after quoted field at row $(nrows+1) column $(col+1)")
                    break
                end
            elseif 5 == state # skip start
                if is_eol
                    col_start_idx = idx
                    skipstart -= 1
                    (0 == skipstart) && (state = 0)
                end
            end
            was_cr = is_cr
        end

        if isempty(error_str)
            if 1 == state       # quoted field
                error_str = "truncated column at row $(nrows+1) column $(col+1)"
            elseif (2 == state) || (3 == state) || ((0 == state) && is_dlm)   # unquoted field, second quote, or begin field with last character as delimiter
                col += 1
                nrows += 1
                store_cell(dh, nrows, col, (3 == state), col_start_idx, idx-1)
                ncols = max(ncols, col)
            end
        end
    catch ex
        if isa(ex, TypeError) && (ex.func == :store_cell)
            rethrow(ex)
        else
            error("at row $(nrows+1), column $col : $ex)")
        end
    end
    !isempty(error_str) && error(error_str)

    return (nrows, ncols)
end

# todo: keyword argument for # of digits to print
writedlm_cell(io::IO, elt::AbstractFloat, dlm, quotes) = print(io, elt)
function writedlm_cell(io::IO, elt::AbstractString, dlm::T, quotes::Bool) where T
    if quotes && !isempty(elt) && (('"' in elt) || ('\n' in elt) || ((T <: AbstractChar) ? (dlm in elt) : occursin(dlm, elt)))
        print(io, '"', replace(elt, r"\"" => "\"\""), '"')
    else
        print(io, elt)
    end
end
writedlm_cell(io::IO, elt, dlm, quotes) = print(io, elt)
function writedlm(io::IO, a::AbstractMatrix, dlm; opts...)
    optsd = val_opts(opts)
    quotes = get(optsd, :quotes, true)
    pb = PipeBuffer()
    lastc = last(axes(a, 2))
    for i = axes(a, 1)
        for j = axes(a, 2)
            writedlm_cell(pb, a[i, j], dlm, quotes)
            j == lastc ? print(pb,'\n') : print(pb,dlm)
        end
        (bytesavailable(pb) > (16*1024)) && write(io, take!(pb))
    end
    write(io, take!(pb))
    nothing
end

writedlm(io::IO, a::AbstractArray{<:Any,0}, dlm; opts...) = writedlm(io, reshape(a,1), dlm; opts...)

# write an iterable row as dlm-separated items
function writedlm_row(io::IO, row, dlm, quotes)
    state = start(row)
    while !done(row, state)
        (x, state) = next(row, state)
        writedlm_cell(io, x, dlm, quotes)
        done(row, state) ? print(io,'\n') : print(io,dlm)
    end
end

# If the row is a single string, write it as a string rather than
# iterating over characters. Also, include the common case of
# a Number (handled correctly by the generic writedlm_row above)
# purely as an optimization.
function writedlm_row(io::IO, row::Union{Number,AbstractString}, dlm, quotes)
    writedlm_cell(io, row, dlm, quotes)
    print(io, '\n')
end

# write an iterable collection of iterable rows
function writedlm(io::IO, itr, dlm; opts...)
    optsd = val_opts(opts)
    quotes = get(optsd, :quotes, true)
    pb = PipeBuffer()
    for row in itr
        writedlm_row(pb, row, dlm, quotes)
        (bytesavailable(pb) > (16*1024)) && write(io, take!(pb))
    end
    write(io, take!(pb))
    nothing
end

function writedlm(fname::AbstractString, a, dlm; opts...)
    open(fname, "w") do io
        writedlm(io, a, dlm; opts...)
    end
end

"""
    writedlm(f, A, delim='\\t'; opts)

Write `A` (a vector, matrix, or an iterable collection of iterable rows) as text to `f`
(either a filename string or an `IO` stream) using the given delimiter
`delim` (which defaults to tab, but can be any printable Julia object, typically a `Char` or
`AbstractString`).

For example, two vectors `x` and `y` of the same length can be written as two columns of
tab-delimited text to `f` by either `writedlm(f, [x y])` or by `writedlm(f, zip(x, y))`.

# Examples
```jldoctest
julia> using DelimitedFiles

julia> x = [1; 2; 3; 4];

julia> y = [5; 6; 7; 8];

julia> open("delim_file.txt", "w") do io
           writedlm(io, [x y])
       end

julia> readdlm("delim_file.txt", '\\t', Int, '\\n')
4×2 Array{Int64,2}:
 1  5
 2  6
 3  7
 4  8

julia> rm("delim_file.txt")
```
"""
writedlm(io, a; opts...) = writedlm(io, a, '\t'; opts...)

show(io::IO, ::MIME"text/csv", a) = writedlm(io, a, ',')
show(io::IO, ::MIME"text/tab-separated-values", a) = writedlm(io, a, '\t')

end # module DelimitedFiles
