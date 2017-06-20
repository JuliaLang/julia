const LIB_MENUJL = string(splitdir(@__FILE__)[1], "/../lib/libmenujl.so")

enableRawMode() = ccall( (:enableRawMode, LIB_MENUJL), Void, ())
disableRawMode() = ccall( (:disableRawMode, LIB_MENUJL), Void, ())
readKey() = ccall( (:editorReadKey, LIB_MENUJL), Int, ())

@enum(Key,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN)


function clean(str::AbstractString)
    replace(str, "\n", "\\n")
end
