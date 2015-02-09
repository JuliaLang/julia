function add1!(x,y)
    x[y] .+= 1
end

function devec_add1!(x,y)
    for i=1:length(y)
        x[y[i]] += 1
    end
end

function devec_add1_logical!(x,y)
    for i=1:length(y)
        if y[i]
            x[i] += 1
        end
    end
end
