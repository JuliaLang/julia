function print_joined(io, strings, delim, last)
    i = start(strings)
    if done(strings,i)
        return
    end
    str, i = next(strings,i)
    print(io, str)
    is_done = done(strings,i)
    while !is_done
        str, i = next(strings,i)
        is_done = done(strings,i)
        print(io, is_done ? last : delim)
        print(io, str)
    end
end

function print_joined(io, strings, delim)
    i = start(strings)
    is_done = done(strings,i)
    while !is_done
        str, i = next(strings,i)
        is_done = done(strings,i)
        print(io, str)
        if !is_done
            print(io, delim)
        end
    end
end
print_joined(io, strings) = print_joined(io, strings, "")

join(args...) = sprint(print_joined, args...)
