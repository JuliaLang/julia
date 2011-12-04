## file formats ##

function dlmread(fname::String, dlm::Char)
    nr = int(split(readall(`wc -l $fname`),' ')[1])
    f = open(fname)

    row = split(readline(f), dlm, true)
    nc = length(row)
    a = Array(Float64, nr, nc)

    for i=1:nr
        for j=1:nc
            el = row[j]
            a[i,j] = (length(el)==0 ? 0 : float64(el))
        end
        if i < nr
            row = split(readline(f), dlm, true)
        end
    end
    close(f)
    return a
end

dlmwrite(fname::String, a) = dlmwrite(fname, a, ',')

# todo: keyword argument for # of digits to print
function dlmwrite(fname::String, a, dlm::Char)
    f = open(fname, "w")
    nr, nc = size(a)
    for i=1:nr
        for j=1:nc
            write(f, string(a[i,j]))
            if j < nc
                write(f, dlm)
            end
        end
        write(f, '\n')
    end
    close(f)
    nothing
end

csvread(fname::String) = dlmread(fname, ',')
csvwrite(fname::String, a) = dlmwrite(fname, a, ',')
