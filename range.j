struct Range{T} <: Tensor{T,1}
    start::T
    step::T
    stop::T
end

struct Range1{T} <: Tensor{T,1}
    start::T
    stop::T
end

numel(r::Union(Range,Range1)) = length(r)
size(r::Union(Range,Range1)) = tuple(length(r))
length{T<:Int}(r::Range{T}) = max(0, div((r.stop-r.start+r.step), r.step))
length{T<:Int}(r::Range1{T}) = max(0, (r.stop-r.start + 1))
length(r::Range) = max(0, int32((r.stop-r.start) / r.step + 1))
length(r::Range1) = max(0, int32(r.stop-r.start + 1))

isempty(r::Range) = (r.step > 0 ? r.stop < r.start : r.stop > r.start)
isempty(r::Range1) = (r.stop < r.start)

start{T<:Int}(r::Range{T}) = r.start
done{T<:Int}(r::Range{T}, i) = (r.step < 0 ? (i < r.stop) : (i > r.stop))
next{T<:Int}(r::Range{T}, i) = (i, i+r.step)

start(r::Range1) = r.start
done(r::Range1, i) = (i > r.stop)
next(r::Range1, i) = (i, i+1)

# floating point ranges need to keep an integer counter
start(r::Range) = (1, r.start)
done{T}(r::Range{T}, st) =
    (r.step < 0 ? (st[2]::T < r.stop) : (st[2]::T > r.stop))
next{T}(r::Range{T}, st) =
    (st[2]::T, (st[1]::Int+1, r.start + st[1]::Int*r.step))

colon(start::Real, stop::Real, step::Real) = Range(promote(start, step, stop)...)
colon(start::Real, stop::Real) = Range1(promote(start, stop)...)

ref(r::Range, i::Index) =
    (x = r.start + (i-1)*r.step;
     (r.step<0 ? (x<r.stop) : (x>r.stop)) ? throw(BoundsError()) : x)
ref(r::Range1, i::Index) = (x = r.start + (i-1);
                            done(r,x) ? throw(BoundsError()) : x)

struct NDRange{N}
    ranges::NTuple{N,Any}
    empty::Bool
    NDRange(ranges) = new(ranges, any(map(isempty,ranges)))
    NDRange(r::())           =new(r,false)
    NDRange(r::(Any,))       =new(r,isempty(r[1]))
    NDRange(r::(Any,Any))    =new(r,isempty(r[1])||isempty(r[2]))
    NDRange(r::(Any,Any,Any))=new(r,isempty(r[1])||isempty(r[2])||isempty(r[3]))
end

start(r::NDRange{0}) = false
done(r::NDRange{0}, st) = st
next(r::NDRange{0}, st) = ((), true)

start(r::NDRange) = { start(r.ranges[i]) | i=1:length(r.ranges) }
done(r::NDRange, st) = r.empty || !bool(st)

function next{N}(r::NDRange{N}, st)
    nxt = ntuple(N, i->next(r.ranges[i], st[i]))
    vals = map(n->n[1], nxt)
    
    for itr=1:N
        ri = r.ranges[itr]
        ni = nxt[itr][2]
        if !done(ri, ni)
            st[itr] = ni
            return (vals, st)
        else
            st[itr] = start(ri)
        end
    end
    (vals, false)
end

function next(r::NDRange{2}, st)
    (r1, r2) = r.ranges
    (v1, n1) = next(r1, st[1])
    (v2, n2) = next(r2, st[2])
    vals = (v1, v2)
    
    if !done(r1, n1)
        st[1] = n1
        return (vals, st)
    else
        st[1] = start(r1)
    end
    if !done(r2, n2)
        st[2] = n2
        return (vals, st)
    end
    (vals, false)
end
