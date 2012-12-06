require("test")
using Test

function runtests(name)
    println("     \033[1m*\033[0m \033[31m$(name)\033[0m")
    flush(OUTPUT_STREAM)
    load("$name")
end

function check_approx_eq(va, vb, Eps, astr, bstr)
    diff = max(abs(va - vb))
    sdiff = strcat("|", astr, " - ", bstr, "| < ", Eps)
    if diff < Eps
        nothing
    else
        error("assertion failed: ", sdiff, "\n  ", astr, " = ", va, "\n  ",
              bstr, " = ", vb)
    end
end

check_approx_eq(va, vb, astr, bstr) = check_approx_eq(va, vb, 10^4*length(va)*eps(max(max(abs(va)), max(abs(vb)))) * max(1, max(abs(va)), max(abs(vb))), astr, bstr)

macro assert_approx_eq_eps(a, b, c)
    quote
        check_approx_eq($(esc(a)), $(esc(b)), $(esc(c)), $(string(a)), $(string(b)))
    end
end

macro assert_approx_eq(a, b)
    quote
        check_approx_eq($(esc(a)), $(esc(b)), $(string(a)), $(string(b)))
    end
end

macro timeit(ex,name)
    quote
        t = Inf
        for i=1:5
            t = min(t, @elapsed $(esc(ex)))
        end
        println(rpad(strcat($name,":"), 20), t)
    end
end

for t in ARGS
    runtests(t)
    println("    \033[32;1mSUCCESS\033[0m")
end
