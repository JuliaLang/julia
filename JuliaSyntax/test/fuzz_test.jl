using JuliaSyntax

# Parser fuzz testing tools.

function parser_exception(str)
    try
        JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, str, ignore_errors=true)
        false
    catch
        true
    end
end

"""
Reduce test case via combination of bisection and random deletion.

This is suited to randomly generated strings. It might work with more code-like
strings too?
"""
function rand_reduce(str)
    while true
        if length(str) <= 1
            return str
        end
        m1 = thisind(str, length(str)÷2)
        m2 = nextind(str, m1)
        if parser_exception(str[1:m1])
            str = str[1:m1]
        elseif parser_exception(str[m2:end])
            str = str[m2:end]
        else
            chunklen = 10
            reduced = false
            if length(str) > chunklen
                for i = 1:100
                    m = thisind(str, rand(1:length(str)-chunklen))
                    s = str[1:m]*str[prevind(str, m+chunklen):end]
                    if parser_exception(s)
                        str = s
                        reduced = true
                        break
                    end
                end
            end
            if !reduced
                return str
            end
        end
    end
end

# The parser should never throw an exception. To test whether this is true,
# try passing randomly generated bad input data into it.
function fuzz_test(gen_bad_input, N)
    for i=1:N
        str = gen_bad_input()
        try
            JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, str, ignore_errors=true);
        catch
            @error "Parser threw exception" exception=current_exceptions()
            return str
        end
    end
    return nothing
end

function fuzz_binary(N)
    fuzz_test(N) do
        String(rand(UInt8, 1_000_000))
    end
end
