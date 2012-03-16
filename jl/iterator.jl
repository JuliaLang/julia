## various useful iterator constructs ##

type ShivaIterator
    create::Function
    destroy::Function
end

function start(ci::ShivaIterator)
    itr = ci.create()
    itr, start(itr)
end

function next(ci::ShivaIterator, state)
    v, s = next(state...)
    v, (state[1], s)
end

function done(ci::ShivaIterator, state)
    if done(state...)
        ci.destroy()
        return true
    end
    false
end

zip(xs::AbstractArray, ys::AbstractArray) = [(xs[i], ys[i]) | i in 1:min(length(xs), length(ys))]
zip(xss::AbstractArray...) = [map((v)->ref(v, i), xss) | i in 1:min(map(length, xss))]

enumerate(j::AbstractArray) = zip(1:length(j), j)
