function runtests(name)
    println("     \033[1m*\033[0m \033[31m$(name)\033[0m")
    load("$name.jl")
end

function check_approx_eq(va, vb, astr, bstr)
    diff = abs(va - vb)
    sdiff = strcat("|", astr, " - ", bstr, "| < 1e-6")
    if diff < 1e-6
        nothing
    else
        error("assertion failed: ", sdiff, "\n  ", astr, " = ", va, "\n  ",
              bstr, " = ", vb)
    end
end

macro assert_approx_eq(a, b)
    quote
        check_approx_eq($a, $b, $string(a), $string(b))
    end
end

macro timeit(ex,name)
    quote
        t = Inf
        for i=1:5
            t = min(t, @elapsed $ex)
        end
        println(rpad(strcat($name,":"), 20), t)
    end
end

macro assert_fails(expr)
    ok = gensym()
    quote
        $ok = false
        try
            $expr
        catch
            $ok = true
        end
        if !($ok)
            error(strcat("assertion failed: expected ",$string(expr)," to fail"))
        end
    end
end

for t in ARGS
    runtests(t)
end
