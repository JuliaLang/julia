module TOML
    using Dates

    include("parser.jl")
    include("print.jl")

    "Convert `TOML.Table` to `Dict{String,Any}`"
    function table2dict(tbl::Union{Nothing,Table})
        tbl == nothing && return Dict{String,Any}()
        return table2dict(get(tbl))
    end

    function table2dict(tbl::Table)
        ret = Dict{String,Any}()
        for (k,v) in tbl.values
            if isa(v, Table)
                ret[k] = table2dict(v)
            elseif isa(v, Array) && length(v)>0 && isa(v[1], Table)
                ret[k] = [table2dict(e) for e in v]
            else
                ret[k] = v
            end
        end
        return ret
    end

    "Parse IO input and return result as dictionary."
    function parse(io::IO)
        parser = Parser(io)
        res = parse(parser)
        length(parser.errors)>0 && throw(CompositeException(parser.errors))
        return table2dict(res)
    end

    "Parse string"
    function parse(str::AbstractString)
        io = IOBuffer(str)
        res = parse(io)
        close(io)
        return res
    end

    "Parse file"
    parsefile(filename::AbstractString) = open(parse, filename, "r")

end
