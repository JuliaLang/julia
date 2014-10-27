macro get_oid_fieldnames(arr, obj, fieldname)
    expr = Expr(:block)
    for i in 1:api.OID_RAWSZ
        push!(expr.args, :($arr[$i] = $obj.$(symbol(string(fieldname, i)))))
    end
    return expr 
end

macro get_header_fieldnames(arr, obj)
    args = {}
    for i in 1:128
        push!(args, :($arr[$i] = $obj.$(symbol(string("header", i)))))
    end
    return Expr(:block, args...)
end
