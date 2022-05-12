# This file is a part of Julia. License is MIT: https://julialang.org/license

function withlocales(f, newlocales)
    # save current locales
    locales = Dict{Int,String}()
    for cat in 0:9999
        cstr = ccall(:setlocale, Cstring, (Cint, Cstring), cat, C_NULL)
        if cstr != C_NULL
            locales[cat] = unsafe_string(cstr)
        end
    end
    timestrs = String[]
    try
        # change to each of given locales
        for lc in newlocales
            set = true
            for (cat, _) in locales
                set &= ccall(:setlocale, Cstring, (Cint, Cstring), cat, lc) != C_NULL
            end
            set && f()
        end
    finally
        # recover locales
        for (cat, lc) in locales
            cstr = ccall(:setlocale, Cstring, (Cint, Cstring), cat, lc)
        end
    end
end
