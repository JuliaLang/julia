# Two leap-second arrays
# SETLEAPS is used when calculating the # of leap seconds from inputs
# GETLEAPS represents the leap second instant in a timeline that includes leap seconds
const SETLEAPS = [62214480000000,62230377600000,62261913600000,62293449600000,62324985600000,
62356608000000,62388144000000,62419680000000,62451216000000,62498476800000,
62530012800000,62561548800000,62624707200000,62703676800000,62766835200000,
62798371200000,62845632000000,62877168000000,62908704000000,62956137600000,
63003398400000,63050832000000,63271756800000,63366451200000,63476784000000]
const GETLEAPS = [62214480000000,62230377601000,62261913602000,62293449603000,62324985604000,
62356608005000,62388144006000,62419680007000,62451216008000,62498476809000,
62530012810000,62561548811000,62624707212000,62703676813000,62766835214000,
62798371215000,62845632016000,62877168017000,62908704018000,62956137619000,
63003398420000,63050832021000,63271756822000,63366451223000,63476784024000]

macro leapsunroll(a,r,var)
    A = eval(a)
    R = eval(r)
    ret = Expr(:block)
    push!(ret.args,:($var < $(A[1]) && return 0))
    push!(ret.args,:($var >= $(A[end]) && return $(endof(A)*1000)))
    push!(ret.args,searchsortedfirsttree(A[2:(endof(A)-1)],R,var))
    return ret
end
# Recursively build binary search tree w/ known lookup values
# A is sorted array of lookup values
# R is return values for each index of A
# i.e. R[1] is returned for values < A[1], R[2] for < A[2], etc.
function searchsortedfirsttree(A,R,var)
    l = length(A)
    mid = iseven(l) ? l>>>1 : (l>>>1)+1
    if mid == 1
        if l == 1
            return :($var < $(A[1]) ? $(R[1]) : $(R[2]))
        else # l == 2
            return :($var < $(A[1]) ? $(R[1]) : 
                     $var < $(A[2]) ? $(R[2]) : $(R[3]))
        end
    end
    iftree = Expr(:if)
    iftree.args = Array(Any,3)
    iftree.args[1] = :($var < $(A[mid])) # condition
    iftree.args[2] = searchsortedfirsttree(A[1:mid-1],R[1:mid],var)
    iftree.args[3] = searchsortedfirsttree(A[mid+1:end],R[mid+1:end],var)
    return iftree
end
function setleaps(ms)
    @leapsunroll(SETLEAPS,[1000:1000:((endof(SETLEAPS)-1)*1000)],ms)
end
function setleapsecond(ms)
    @leapsunroll([SETLEAPS[i]+1000 for i = 1:length(SETLEAPS)],
                 [1000:1000:((endof(SETLEAPS)-1)*1000)],ms)
end
function getleaps(ms)
    @leapsunroll(GETLEAPS,[1000:1000:((endof(SETLEAPS)-1)*1000)],ms)
end
function getleapsecond(ms)
    @leapsunroll([GETLEAPS[i]+1000 for i = 1:length(GETLEAPS)],
                 [1000:1000:((endof(SETLEAPS)-1)*1000)],ms)
end