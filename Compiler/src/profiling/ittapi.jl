# Stubs
module ITTAPI

import ..String, ..Expr, ..LineNumberNode, ..nothing

function ittapi_zone_create(name::String, ex::Expr, linfo::LineNumberNode)
    return nothing
end

function ittapi_zone_begin(loc, active)
   return nothing
end

function ittapi_zone_end(ctx)
    return nothing
end

end
