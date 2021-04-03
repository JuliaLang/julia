# This file is a part of Julia. License is MIT: https://julialang.org/license

## low-level pcre2 interface ##

module PCRE

import ..RefValue

# include($BUILDROOT/base/pcre_h.jl)
include(string(length(Core.ARGS) >= 2 ? Core.ARGS[2] : "", "pcre_h.jl"))

const PCRE_LIB = "libpcre2-8"

function create_match_context()
    JIT_STACK_START_SIZE = 32768
    JIT_STACK_MAX_SIZE = 1048576
    jit_stack = ccall((:pcre2_jit_stack_create_8, PCRE_LIB), Ptr{Cvoid},
                      (Csize_t, Csize_t, Ptr{Cvoid}),
                      JIT_STACK_START_SIZE, JIT_STACK_MAX_SIZE, C_NULL)
    ctx = ccall((:pcre2_match_context_create_8, PCRE_LIB),
                Ptr{Cvoid}, (Ptr{Cvoid},), C_NULL)
    ccall((:pcre2_jit_stack_assign_8, PCRE_LIB), Cvoid,
          (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}), ctx, C_NULL, jit_stack)
    return ctx
end

const THREAD_MATCH_CONTEXTS = Ptr{Cvoid}[C_NULL]

PCRE_COMPILE_LOCK = nothing

_tid() = Int(ccall(:jl_threadid, Int16, ())+1)
_nth() = Int(unsafe_load(cglobal(:jl_n_threads, Cint)))

function get_local_match_context()
    tid = _tid()
    ctx = @inbounds THREAD_MATCH_CONTEXTS[tid]
    if ctx == C_NULL
        @inbounds THREAD_MATCH_CONTEXTS[tid] = ctx = create_match_context()
    end
    return ctx
end

function __init__()
    resize!(THREAD_MATCH_CONTEXTS, _nth())
    fill!(THREAD_MATCH_CONTEXTS, C_NULL)
    global PCRE_COMPILE_LOCK = Threads.SpinLock()
end

# supported options for different use cases

# arguments to pcre2_compile
const COMPILE_MASK      =
      ALT_BSUX          |
      ALT_CIRCUMFLEX    |
      ALT_VERBNAMES     |
      ANCHORED          |
      # AUTO_CALLOUT    |
      CASELESS          |
      DOLLAR_ENDONLY    |
      DOTALL            |
      # DUPNAMES        |
      ENDANCHORED       |
      EXTENDED          |
      EXTENDED_MORE     |
      FIRSTLINE         |
      LITERAL           |
      MATCH_INVALID_UTF |
      MATCH_UNSET_BACKREF |
      MULTILINE         |
      NEVER_BACKSLASH_C |
      NEVER_UCP         |
      NEVER_UTF         |
      NO_AUTO_CAPTURE   |
      NO_AUTO_POSSESS   |
      NO_DOTSTAR_ANCHOR |
      NO_START_OPTIMIZE |
      NO_UTF_CHECK      |
      UCP               |
      UNGREEDY          |
      USE_OFFSET_LIMIT  |
      UTF

# arguments to pcre2_set_newline
const COMPILE_NEWLINE_MASK = (
      NEWLINE_CR,
      NEWLINE_LF,
      NEWLINE_CRLF,
      NEWLINE_ANY,
      NEWLINE_ANYCRLF,
      NEWLINE_NUL)

# arguments to pcre2_set_compile_extra_options
const COMPILE_EXTRA_MASK            =
      EXTRA_ALLOW_SURROGATE_ESCAPES |
      EXTRA_ALT_BSUX                |
      EXTRA_BAD_ESCAPE_IS_LITERAL   |
      EXTRA_ESCAPED_CR_IS_LF        |
      EXTRA_MATCH_LINE              |
      EXTRA_MATCH_WORD

# arguments to match
const EXECUTE_MASK      =
      # ANCHORED        |
      # COPY_MATCHED_SUBJECT |
      # ENDANCHORED     |
      NOTBOL            |
      NOTEMPTY          |
      NOTEMPTY_ATSTART  |
      NOTEOL            |
      # NO_JIT          |
      NO_START_OPTIMIZE |
      NO_UTF_CHECK      |
      PARTIAL_HARD      |
      PARTIAL_SOFT


const UNSET = ~Csize_t(0)  # Indicates that an output vector element is unset

function info(regex::Ptr{Cvoid}, what::Integer, ::Type{T}) where T
    buf = RefValue{T}()
    ret = ccall((:pcre2_pattern_info_8, PCRE_LIB), Cint,
                (Ptr{Cvoid}, UInt32, Ptr{Cvoid}),
                regex, what, buf)
    if ret != 0
        error(ret == ERROR_NULL      ? "PCRE error: NULL regex object" :
              ret == ERROR_BADMAGIC  ? "PCRE error: invalid regex object" :
              ret == ERROR_BADOPTION ? "PCRE error: invalid option flags" :
                                       "PCRE error: unknown error ($ret)")
    end
    return buf[]
end

function ovec_length(match_data)
    n = ccall((:pcre2_get_ovector_count_8, PCRE_LIB), UInt32,
              (Ptr{Cvoid},), match_data)
    return 2Int(n)
end

function ovec_ptr(match_data)
    ptr = ccall((:pcre2_get_ovector_pointer_8, PCRE_LIB), Ptr{Csize_t},
                (Ptr{Cvoid},), match_data)
    return ptr
end

function compile(pattern::AbstractString, options::Integer)
    if !(pattern isa Union{String,SubString{String}})
        pattern = String(pattern)
    end
    errno = RefValue{Cint}(0)
    erroff = RefValue{Csize_t}(0)
    re_ptr = ccall((:pcre2_compile_8, PCRE_LIB), Ptr{Cvoid},
                   (Ptr{UInt8}, Csize_t, UInt32, Ref{Cint}, Ref{Csize_t}, Ptr{Cvoid}),
                   pattern, ncodeunits(pattern), options, errno, erroff, C_NULL)
    if re_ptr == C_NULL
        error("PCRE compilation error: $(err_message(errno[])) at offset $(erroff[])")
    end
    return re_ptr
end

function jit_compile(regex::Ptr{Cvoid})
    errno = ccall((:pcre2_jit_compile_8, PCRE_LIB), Cint,
                  (Ptr{Cvoid}, UInt32), regex, JIT_COMPLETE)
    errno == 0 && return true
    errno == ERROR_JIT_BADOPTION && return false
    error("PCRE JIT error: $(err_message(errno))")
end

free_match_data(match_data) =
    ccall((:pcre2_match_data_free_8, PCRE_LIB), Cvoid, (Ptr{Cvoid},), match_data)

free_re(re) =
    ccall((:pcre2_code_free_8, PCRE_LIB), Cvoid, (Ptr{Cvoid},), re)

free_jit_stack(stack) =
    ccall((:pcre2_jit_stack_free_8, PCRE_LIB), Cvoid, (Ptr{Cvoid},), stack)

free_match_context(context) =
    ccall((:pcre2_match_context_free_8, PCRE_LIB), Cvoid, (Ptr{Cvoid},), context)

function err_message(errno::Integer)
    buffer = Vector{UInt8}(undef, 1024)
    ret = ccall((:pcre2_get_error_message_8, PCRE_LIB), Cint,
                (Cint, Ptr{UInt8}, Csize_t), errno, buffer, length(buffer))
    ret == ERROR_BADDATA && error("PCRE error: invalid errno ($errno)")
    # TODO: seems like there should be a better way to get this string
    return GC.@preserve buffer unsafe_string(pointer(buffer))
end

function exec(re, subject, offset, options, match_data)
    if !(subject isa Union{String,SubString{String}})
        subject = String(subject)
    end
    rc = ccall((:pcre2_match_8, PCRE_LIB), Cint,
               (Ptr{Cvoid}, Ptr{UInt8}, Csize_t, Csize_t, UInt32, Ptr{Cvoid}, Ptr{Cvoid}),
               re, subject, ncodeunits(subject), offset, options, match_data, get_local_match_context())
    # rc == -1 means no match, -2 means partial match.
    rc < -2 && error("PCRE.exec error: $(err_message(rc))")
    return rc >= 0
end

function exec_r(re, subject, offset, options)
    match_data = create_match_data(re)
    ans = exec(re, subject, offset, options, match_data)
    free_match_data(match_data)
    return ans
end

function exec_r_data(re, subject, offset, options)
    match_data = create_match_data(re)
    ans = exec(re, subject, offset, options, match_data)
    return ans, match_data
end

function create_match_data(re)
    p = ccall((:pcre2_match_data_create_from_pattern_8, PCRE_LIB),
              Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}), re, C_NULL)
    p == C_NULL && error("PCRE error: could not allocate memory")
    return p
end

function substring_number_from_name(re, name)
    n = ccall((:pcre2_substring_number_from_name_8, PCRE_LIB), Cint,
               (Ptr{Cvoid}, Cstring), re, name)
    return Int(n)
end

function substring_length_bynumber(match_data, number)
    s = RefValue{Csize_t}()
    rc = ccall((:pcre2_substring_length_bynumber_8, PCRE_LIB), Cint,
               (Ptr{Cvoid}, Cint, Ref{Csize_t}), match_data, number, s)
    rc < 0 && error("PCRE error: $(err_message(rc))")
    return Int(s[])
end

function substring_copy_bynumber(match_data, number, buf, buf_size)
    s = RefValue{Csize_t}(buf_size)
    rc = ccall((:pcre2_substring_copy_bynumber_8, PCRE_LIB), Cint,
               (Ptr{Cvoid}, UInt32, Ptr{UInt8}, Ref{Csize_t}),
               match_data, number, buf, s)
    rc < 0 && error("PCRE error: $(err_message(rc))")
    return Int(s[])
end

function capture_names(re)
    name_count = info(re, INFO_NAMECOUNT, UInt32)
    name_entry_size = info(re, INFO_NAMEENTRYSIZE, UInt32)
    nametable_ptr = info(re, INFO_NAMETABLE, Ptr{UInt8})
    names = Dict{Int,String}()
    for i = 1:name_count
        offset = (i-1)*name_entry_size + 1
        # The capture group index corresponding to name 'i' is stored as a
        # big-endian 16-bit value.
        high_byte = UInt16(unsafe_load(nametable_ptr, offset))
        low_byte = UInt16(unsafe_load(nametable_ptr, offset+1))
        idx = (high_byte << 8) | low_byte
        # The capture group name is a null-terminated string located directly
        # after the index.
        names[idx] = unsafe_string(nametable_ptr+offset+1)
    end
    return names
end

end # module
