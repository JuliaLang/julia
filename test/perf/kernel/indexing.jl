function add_one!(x,y)
    x[y] += 1
end

function devec_add_one!(x,y)
	for i=1:length(y)
        x[y[i]] += 1
    end
end

function devec_add_one_logical!(x,y)
	for i=1:length(y)
        if y[i]
            x[i] += 1
        end
    end
end
