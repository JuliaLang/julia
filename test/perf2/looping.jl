function testlooping1(p, sz)
    index = 1        
    for k = 1:sz[3]
        for j = 1:sz[2]
            jkprod = j*k
            for i = 1:sz[1]
                p[index] = i*jkprod
                index += 1
            end
        end
    end
end

function oneloop(p, index::Int, jkprod::Int, imax::Int)
    for i = 1:imax
        p[index] = i*jkprod
        index += 1
    end
    return index
end

function testlooping2(p, sz)
    index = 1        
    for k = 1:sz[3]
        for j = 1:sz[2]
            index = oneloop(p, index, j*k, sz[1])
        end
    end
end

sz = (1000,1000,10)
p = Array(Int, sz)
szsmall = (10,10,10)
psmall = Array(Int, szsmall)
testlooping1(psmall, szsmall)
@time testlooping1(p, sz)
p1 = copy(p)
testlooping2(psmall, szsmall)
@time testlooping2(p, sz)
@assert p == p1
