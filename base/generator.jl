"""
    Generator(f, iter)

Given a function `f` and an iterator `iter`, construct an iterator that yields
the values of `f` applied to the elements of `iter`.
The syntax `f(x) for x in iter` is syntax for constructing an instance of this
type.
"""
immutable Generator{I,F}
    f::F
    iter::I
end

Generator(f, c1, c...) = Generator(a->f(a...), zip(c1, c...))

start(g::Generator) = start(g.iter)
done(g::Generator, s) = done(g.iter, s)
function next(g::Generator, s)
    v, s2 = next(g.iter, s)
    g.f(v), s2
end

collect(g::Generator) = map(g.f, g.iter)
