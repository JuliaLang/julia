## N-dimensional ranges ##

type NDRange{N}
    ranges::NTuple{N,Any}
    empty::Bool

    if eq_int(unbox(Size,N),unbox(Size,0))
        NDRange(r::())           = new(r, false)
    elseif eq_int(unbox(Size,N),unbox(Size,1))
        NDRange(r::(Any,))       = new(r, isempty(r[1]))
    elseif eq_int(unbox(Size,N),unbox(Size,2))
        NDRange(r::(Any,Any))    = new(r, isempty(r[1])||isempty(r[2]))
    elseif eq_int(unbox(Size,N),unbox(Size,3))
        NDRange(r::(Any,Any,Any))= new(r,
                                       isempty(r[1])||isempty(r[2])||isempty(r[3]))
    else
        NDRange(r::Tuple)        = new(r, anyp(isempty,r))
    end
end

NDRange{N}(r::NTuple{N,Any}) = NDRange{N}(r)
NDRange(rs...) = NDRange(rs)

start(r::NDRange{0}) = false
done(r::NDRange{0}, st) = st
next(r::NDRange{0}, st) = ((), true)

start(r::NDRange) = { start(r.ranges[i]) for i=1:length(r.ranges) }
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
