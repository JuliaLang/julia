## various useful iterator constructs ##

# iterator which uses a thunk to create the iterator and another to destroy it

type ShivaIterator
    create::Function
    destroy::Function
end

function start(si::ShivaIterator)
    itr = si.create()
    itr, start(itr)
end

function next(si::ShivaIterator, state)
    v, s = next(state...)
    v, (state[1], s)
end

function done(si::ShivaIterator, state)
    if done(state...)
        si.destroy()
        return true
    end
    false
end

# like python's enumerate

type Enumerate
    itr
end

enumerate(itr) = Enumerate(itr)
start(en::Enumerate) = (start(en.itr), 1)
function next(en::Enumerate, state)
    v, s = next(en.itr, state[1])
    (v,state[2]), (s,state[2]+1)
end
done(en::Enumerate, state) = done(en.itr, state[1])
