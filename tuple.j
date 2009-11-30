ref(t::Tuple, i::Index) = tupleref(t, unbox(i))
length(t::Tuple) = box(Size, tuplelen(t))
