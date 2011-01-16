## various useful iterator constructs ##

struct ShivaIterator
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
