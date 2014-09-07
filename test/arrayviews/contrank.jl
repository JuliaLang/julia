# contrank arithmetics

using ArrayViews
using Base.Test
import ArrayViews: Subs, ContRank, contrank

const irealn = 3
const icolon = (:)
const irange = 1:5
const istepr = 1:2:5

const tinds = {irealn, icolon, irange, istepr}

crank{M}(::Type{ContRank{M}}) = M
crank(I::Subs...) = crank(contrank(I...))

function safe_crank(inds...)
    r = 0
    for i in inds
        if isa(i, Colon)
            r += 1
        elseif isa(i, UnitRange)
            r += 1
            return r
        else
            return r
        end
    end
    return r
end

function test_crank(inds...)
    c = crank(inds...)
    s = safe_crank(inds...)
    if c != s
        error("Incorrect contrank result, on $(inds) ==> crank = $(c), safe_crank = $(s)")
    end
end

# 0D
test_crank()

# 1D
for i1 in tinds
    test_crank(i1)
end

# 2D
for i1 in tinds, i2 in tinds
    test_crank(i1, i2)
end

# 3D
for i1 in tinds, i2 in tinds, i3 in tinds
    test_crank(i1, i2, i3)
end

# 4D
for i1 in tinds, i2 in tinds, i3 in tinds, i4 in tinds
    test_crank(i1, i2, i3, i4)
end

# 5D
for i1 in tinds, i2 in tinds, i3 in tinds, i4 in tinds, i5 in tinds
    test_crank(i1, i2, i3, i4, i5)
end


