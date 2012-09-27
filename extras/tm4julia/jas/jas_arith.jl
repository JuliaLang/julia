# source :  JAS_arith.jl
# purpose:  use private arithmetic source code
#
# author :  Jeffrey A. Sarnoff
# date   :  2012-Sep-27
#
#

module jas_arith

export jas_quorem

function jas_quorem(n::Int32, d::Int32)
    q = div(n,d) - ((n < 0) ? 1 : 0)
    r = n - (d*q)
    (q,r)
end

function jas_quorem(n::Int64, d::Int64)
    q = div(n,d) - ((n < 0) ? 1 : 0)
    r = n - (d*q)
    (q,r)
end

function jas_quorem(n::Signed, d::Signed)
    q = div(n,d) - ((n < 0) ? 1 : 0)
    r = n - (d*q)
    (q,r)
end


end # module
