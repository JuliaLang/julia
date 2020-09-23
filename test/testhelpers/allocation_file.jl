g(x) = x + 123456
function f(x)
    []
    Base.invokelatest(g, 0)
    Base.invokelatest(g, x)
    []
end
f(1.23)
