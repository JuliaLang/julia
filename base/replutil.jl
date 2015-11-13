# This file is a part of Julia. License is MIT: http://julialang.org/license

# fallback text/plain representation of any type:
writemime(io::IO, ::MIME"text/plain", x) = showlimited(io, x)

function writemime(io::IO, ::MIME"text/plain", f::Function)
    if isgeneric(f)
        n = length(f.env)
        m = n==1 ? "method" : "methods"
        print(io, "$(f.env.name) (generic function with $n $m)")
    else
        show(io, f)
    end
end

# writemime for ranges, e.g.
#  3-element UnitRange{Int64} (1:3):
#   1,2,3
# or for more elements than fit on screen:
#   1.0,2.0,3.0,…,6.0,7.0,8.0
# or for zero elements:
#   0-element UnitRange{Int64} (3:1)
# with options to display pre-0.5 behavior, e.g. linspace(1.0, 3.0, 5)
# and to display type and fields, and/or list the contents like an array.
"""
    writemime(stream, mime, range)

prints a `Range` object with user-selectable options. By default it prints
the type, a shorthand definition, and a list of the contents. Additional
named keywords are as follows: `rangeverbose` [`true`] selects whether
info beyond the shorthand definition is shown. If verbose, then at minimum the
type info is shown (e.g., `UnitRange{Int64}`). `rangeinfo` = 0, [1], 2
selects whether the range definition should be given (0) not at all, (1) as
a shorthand (e.g., `1:3`), or (2) as full constructor (e.g., `LinSpace{Int64}(...)`).
`rangelist` [`true`] selects whether the contents should be shown.
If the range is empty, then at minimum the shorthand definition is shown.
"""
function writemime(io::IO, ::MIME"text/plain", r::Range;
    rangeverbose = true, rangeinfo = 1, rangelist = true)
    if rangeverbose                      # at minimum show type, e.g.
        print(io, summary(r))            #   5-element LinSpace{Float64}
        if rangeinfo == 1 ||             # default is to print shorthand, e.g.
          (rangeinfo == 0 && isempty(r))
            print(io, " ")               #   1:3
            if typeof(r) <: LinSpace
                show(io, r)
            else # FloatRange or OrdinalRange should be wrapped in parens
                print(io, "(")
                show(io,r)
                print(io, ")")
            end
        elseif rangeinfo == 2           # or print constructor info,
            print(io, "(")              #   (1,3)
            for i in 1:nfields(r)-1
                print(io, getfield(r, i), ",")
            end
            print(io, getfield(r, nfields(r)), ")")
        end
        if rangelist && !isempty(r)     # list contents if there are any
            println(io, ":")
            with_output_limit(()->print_range(io,r))
        end
    else                                # terse behavior (like v0.4 and earlier)
        show(io,r)                      # e.g., linspace(1.0, 3.0, 5)
    end
end
# Note that for above, one can select different options by overwriting, e.g.
#   Base.writemime(io::IO, mime:MIME"text/plain", r::Range) = Base.writemime(io,
#     mime, r, rangeverbose = false)

function writemime(io::IO, ::MIME"text/plain", v::AbstractVector)
    print(io, summary(v))
    if !isempty(v)
        println(io, ":")
        with_output_limit(()->print_matrix(io, v))
    end
end

writemime(io::IO, ::MIME"text/plain", v::AbstractArray) =
    with_output_limit(()->showarray(io, v, header=true, repr=false))

function writemime(io::IO, ::MIME"text/plain", v::DataType)
    show(io, v)
    # TODO: maybe show constructor info?
end

writemime(io::IO, ::MIME"text/plain", t::Associative) =
    showdict(io, t, limit=true)
writemime(io::IO, ::MIME"text/plain", t::Union{KeyIterator, ValueIterator}) =
    showkv(io, t, limit=true)

function writemime(io::IO, ::MIME"text/plain", t::Task)
    show(io, t)
    if t.state == :failed
        println(io)
        showerror(io, CapturedException(t.result, t.backtrace))
    end
end


# showing exception objects as descriptive error messages

showerror(io::IO, ex) = show(io, ex)

function showerror(io::IO, ex::BoundsError)
    print(io, "BoundsError")
    if isdefined(ex, :a)
        print(io, ": attempt to access ")
        writemime(io, MIME"text/plain"(), ex.a)
        if isdefined(ex, :i)
            print(io, "\n  at index [")
            if isa(ex.i, Range)
                print(io, ex.i)
            else
                print_joined(io, ex.i, ',')
            end
            print(io, ']')
        end
    end
end

function showerror(io::IO, ex::TypeError)
    print(io, "TypeError: ")
    ctx = isempty(ex.context) ? "" : "in $(ex.context), "
    if ex.expected === Bool
        print(io, "non-boolean ($(typeof(ex.got))) used in boolean context")
    else
        if isa(ex.got, Type)
            tstr = "Type{$(ex.got)}"
        else
            tstr = string(typeof(ex.got))
        end
        print(io, "$(ex.func): $(ctx)expected $(ex.expected), got $tstr")
        if ex.func === :apply && ex.expected <: Function && isa(ex.got, AbstractArray)
            print(io, "\nUse square brackets [] for indexing.")
        end
    end
end

function showerror(io::IO, ex, bt; backtrace=true)
    try
        showerror(io, ex)
    finally
        backtrace && show_backtrace(io, bt)
    end
end

function showerror(io::IO, ex::LoadError, bt; backtrace=true)
    print(io, "LoadError: ")
    showerror(io, ex.error, bt, backtrace=backtrace)
    print(io, "\nwhile loading $(ex.file), in expression starting on line $(ex.line)")
end
showerror(io::IO, ex::LoadError) = showerror(io, ex, [])

function showerror(io::IO, ex::InitError, bt; backtrace=true)
    print(io, "InitError: ")
    showerror(io, ex.error, bt, backtrace=backtrace)
    print(io, "\nduring initialization of module $(ex.mod)")
end
showerror(io::IO, ex::InitError) = showerror(io, ex, [])

function showerror(io::IO, ex::DomainError, bt; backtrace=true)
    print(io, "DomainError:")
    for b in bt
        code = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Cint), b-1, true)
        if length(code) == 7 && !code[6]  # code[6] == fromC
            if code[1] in (:log, :log2, :log10, :sqrt) # TODO add :besselj, :besseli, :bessely, :besselk
                print(io,"\n$(code[1]) will only return a complex result if called with a complex argument. Try $(code[1])(complex(x)).")
            elseif (code[1] == :^ && code[2] == symbol("intfuncs.jl")) || code[1] == :power_by_squaring #3024
                print(io, "\nCannot raise an integer x to a negative power -n. \nMake x a float by adding a zero decimal (e.g. 2.0^-n instead of 2^-n), or write 1/x^n, float(x)^-n, or (x//1)^-n.")
            elseif code[1] == :^ && (code[2] == symbol("promotion.jl") || code[2] == symbol("math.jl"))
                print(io, "\nExponentiation yielding a complex result requires a complex argument.\nReplace x^y with (x+0im)^y, Complex(x)^y, or similar.")
            end
            break
        end
    end
    backtrace && show_backtrace(io, bt)
    nothing
end

function showerror(io::IO, ex::SystemError)
    if ex.extrainfo == nothing
        print(io, "SystemError: $(ex.prefix): $(Libc.strerror(ex.errnum))")
    else
        print(io, "SystemError (with $(ex.extrainfo)): $(ex.prefix): $(Libc.strerror(ex.errnum))")
    end
end
showerror(io::IO, ::DivideError) = print(io, "DivideError: integer division error")
showerror(io::IO, ::StackOverflowError) = print(io, "StackOverflowError:")
showerror(io::IO, ::UndefRefError) = print(io, "UndefRefError: access to undefined reference")
showerror(io::IO, ex::UndefVarError) = print(io, "UndefVarError: $(ex.var) not defined")
showerror(io::IO, ::EOFError) = print(io, "EOFError: read end of file")
showerror(io::IO, ex::ErrorException) = print(io, ex.msg)
showerror(io::IO, ex::KeyError) = print(io, "KeyError: $(ex.key) not found")
showerror(io::IO, ex::InterruptException) = print(io, "InterruptException:")
showerror(io::IO, ex::ArgumentError) = print(io, "ArgumentError: $(ex.msg)")
showerror(io::IO, ex::AssertionError) = print(io, "AssertionError: $(ex.msg)")

function showerror(io::IO, ex::MethodError)
    # ex.args is a tuple type if it was thrown from `invoke` and is
    # a tuple of the arguments otherwise.
    is_arg_types = isa(ex.args, DataType)
    arg_types = is_arg_types ? ex.args : typesof(ex.args...)
    arg_types_param::SimpleVector = arg_types.parameters
    print(io, "MethodError: ")
    if isa(ex.f, Tuple)
        f = ex.f[1]
        print(io, "<inline> ")
    else
        f = ex.f
    end
    name = isgeneric(f) ? f.env.name : :anonymous
    if isa(f, DataType)
        print(io, "`$(f)` has no method matching $(f)(")
    else
        print(io, "`$(name)` has no method matching $(name)(")
    end
    for (i, typ) in enumerate(arg_types_param)
        print(io, "::$typ")
        i == length(arg_types_param) || print(io, ", ")
    end
    print(io, ")")
    # Check for local functions that shadow methods in Base
    if isdefined(Base, name)
        basef = eval(Base, name)
        if basef !== ex.f && isgeneric(basef) && method_exists(basef, arg_types)
            println(io)
            print(io, "you may have intended to import Base.$(name)")
        end
    end
    if !is_arg_types
        # Check for row vectors used where a column vector is intended.
        vec_args = []
        hasrows = false
        for arg in ex.args
            isrow = isa(arg,Array) && ndims(arg)==2 && size(arg,1)==1
            hasrows |= isrow
            push!(vec_args, isrow ? vec(arg) : arg)
        end
        if hasrows && applicable(f, vec_args...)
            print(io, "\n\nYou might have used a 2d row vector where a 1d column vector was required.",
                      "\nNote the difference between 1d column vector [1,2,3] and 2d row vector [1 2 3].",
                      "\nYou can convert to a column vector with the vec() function.")
        end
    end
    # Give a helpful error message if the user likely called a type constructor
    # and sees a no method error for convert
    if (f == Base.convert && !isempty(arg_types_param) && !is_arg_types &&
        isa(arg_types_param[1], DataType) &&
        arg_types_param[1].name === Type.name)
        construct_type = arg_types_param[1].parameters[1]
        println(io)
        print(io, "This may have arisen from a call to the constructor $construct_type(...),",
                  "\nsince type constructors fall back to convert methods.")
    end
    try
        show_method_candidates(io, ex)
    catch
        warn(io, "Error showing method candidates, aborted")
    end
end

#Show an error by directly calling jl_printf.
#Useful in Base submodule __init__ functions where STDERR isn't defined yet.
function showerror_nostdio(err, msg::AbstractString)
    stderr_stream = ccall(:jl_stderr_stream, Ptr{Void}, ())
    ccall(:jl_printf, UInt, (Ptr{Void},Cstring), stderr_stream, msg)
    ccall(:jl_printf, UInt, (Ptr{Void},Cstring), stderr_stream, ":\n")
    ccall(:jl_static_show, UInt, (Ptr{Void},Ptr{Void}), stderr_stream,
          pointer_from_objref(err))
    ccall(:jl_printf, UInt, (Ptr{Void},Cstring), stderr_stream, "\n")
end

const UNSHOWN_METHODS = ObjectIdDict(
    which(call, Tuple{Type, Vararg{Any}}) => true
)
function show_method_candidates(io::IO, ex::MethodError)
    is_arg_types = isa(ex.args, DataType)
    arg_types = is_arg_types ? ex.args : typesof(ex.args...)
    arg_types_param::SimpleVector = arg_types.parameters
    # Displays the closest candidates of the given function by looping over the
    # functions methods and counting the number of matching arguments.
    if isa(ex.f, Tuple)
        f = ex.f[1]
    else
        f = ex.f
    end

    lines = []
    # These functions are special cased to only show if first argument is matched.
    special = f in [convert, getindex, setindex!]
    funcs = [f]

    # An incorrect call method produces a MethodError for convert.
    # It also happens that users type convert when they mean call. So
    # pool MethodErrors for these two functions.
    f === convert && push!(funcs, call)

    for func in funcs
        name = isgeneric(func) ? func.env.name : :anonymous
        for method in methods(func)
            haskey(UNSHOWN_METHODS, method) && continue
            buf = IOBuffer()
            sig = method.sig.parameters
            use_constructor_syntax = func == call && !isempty(sig) && isa(sig[1], DataType) &&
                                     !isempty(sig[1].parameters) && isa(sig[1].parameters[1], DataType)
            print(buf, "  ", use_constructor_syntax ? sig[1].parameters[1].name : name)
            right_matches = 0
            tv = method.tvars
            if !isa(tv,SimpleVector)
                tv = Any[tv]
            end
            if !isempty(tv)
                show_delim_array(buf, tv, '{', ',', '}', false)
            end
            print(buf, "(")
            t_i = Any[arg_types_param...]
            right_matches = 0
            for i = 1 : min(length(t_i), length(sig))
                i > (use_constructor_syntax ? 2 : 1) && print(buf, ", ")
                # If isvarargtype then it checks wether the rest of the input arguements matches
                # the varargtype
                if Base.isvarargtype(sig[i])
                    sigstr = string(sig[i].parameters[1], "...")
                    j = length(t_i)
                else
                    sigstr = string(sig[i])
                    j = i
                end
                # Checks if the type of arg 1:i of the input intersects with the current method
                t_in = typeintersect(Tuple{sig[1:i]...}, Tuple{t_i[1:j]...})
                # If the function is one of the special cased then it should break the loop if
                # the type of the first argument is not matched.
                t_in === Union{} && special && i == 1 && break
                if use_constructor_syntax && i == 1
                    right_matches += i
                elseif t_in === Union{}
                    if Base.have_color
                        Base.with_output_color(:red, buf) do buf
                            print(buf, "::$sigstr")
                        end
                    else
                        print(buf, "!Matched::$sigstr")
                    end
                    # If there is no typeintersect then the type signature from the method is
                    # inserted in t_i this ensures if the type at the next i matches the type
                    # signature then there will be a type intersect
                    t_i[i] = sig[i]
                else
                    right_matches += j==i ? 1 : 0
                    print(buf, "::$sigstr")
                end
            end
            special && right_matches==0 && continue

            if length(t_i) > length(sig) && !isempty(sig) && Base.isvarargtype(sig[end])
                # It ensures that methods like f(a::AbstractString...) gets the correct
                # number of right_matches
                for t in arg_types_param[length(sig):end]
                    if t <: sig[end].parameters[1]
                        right_matches += 1
                    end
                end
            end

            if right_matches > 0
                if length(t_i) < length(sig)
                    # If the methods args is longer than input then the method
                    # arguments is printed as not a match
                    for sigtype in sig[length(t_i)+1:end]
                        if Base.isvarargtype(sigtype)
                            sigstr = string(sigtype.parameters[1], "...")
                        else
                            sigstr = string(sigtype)
                        end
                        print(buf, ", ")
                        if Base.have_color
                            Base.with_output_color(:red, buf) do buf
                                print(buf, "::$sigstr")
                            end
                        else
                            print(buf, "!Matched::$sigstr")
                        end
                    end
                end
                print(buf, ")")
                push!(lines, (buf, right_matches))
            end
        end
    end

    if !isempty(lines) # Display up to three closest candidates
        Base.with_output_color(:normal, io) do io
            println(io)
            print(io, "Closest candidates are:")
            sort!(lines, by = x -> -x[2])
            i = 0
            for line in lines
                println(io)
                if i >= 3
                    print(io, "  ...")
                    break
                end
                i += 1
                print(io, takebuf_string(line[1]))
            end
        end
    end
end

function show_trace_entry(io, fname, file, line, inlinedat_file, inlinedat_line, n)
    print(io, "\n")
    # if we have inlining information, we print the `file`:`line` first,
    # then show the inlining info, because the inlining location
    # corresponds to `fname`.
    if (inlinedat_file != symbol(""))
        # align the location text
        print(io, " [inlined code] from ")
    else
        print(io, " in ", fname, " at ")
    end

    print(io, file)

    if line >= 1
        try
            print(io, ":", line)
        catch
            print(io, '?') #for when dec is not yet defined
        end
    end

    if n > 1
        print(io, " (repeats ", n, " times)")
    end

    if (inlinedat_file != symbol(""))
        print(io, "\n in ", fname, " at ")
        print(io, inlinedat_file, ":", inlinedat_line)
    end
end

function show_backtrace(io::IO, t, set=1:typemax(Int))
    # we may not declare :eval_user_input
    # directly so that we get a compile error
    # in case its name changes in the future
    show_backtrace(io,
                    try
                        eval_user_input.env.name
                    catch
                        :(:) #for when client.jl is not yet defined
                    end, t, set)
end

function show_backtrace(io::IO, top_function::Symbol, t, set)
    process_entry(lastname, lastfile, lastline, last_inlinedat_file, last_inlinedat_line, n) =
        show_trace_entry(io, lastname, lastfile, lastline, last_inlinedat_file, last_inlinedat_line, n)
    process_backtrace(process_entry, top_function, t, set)
end

function show_backtrace(io::IO, top_function::Symbol, t::Vector{Any}, set)
    for entry in t
        show_trace_entry(io, entry...)
    end
end

# process the backtrace, up to (but not including) top_function
function process_backtrace(process_func::Function, top_function::Symbol, t, set; skipC = true)
    n = 1
    lastfile = ""; lastline = -11; lastname = symbol("#");
    last_inlinedat_file = ""; last_inlinedat_line = -1
    local fname, file, line
    count = 0
    for i = 1:length(t)
        lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Cint), t[i]-1, skipC)
        if lkup === nothing
            continue
        end
        fname, file, line, inlinedat_file, inlinedat_line, fromC = lkup

        if fromC && skipC; continue; end
        if i == 1 && fname == :error; continue; end
        if fname == top_function; break; end
        count += 1
        if !in(count, set); continue; end

        if file != lastfile || line != lastline || fname != lastname
            if lastline != -11
                process_func(lastname, lastfile, lastline, last_inlinedat_file, last_inlinedat_line, n)
            end
            n = 1
            lastfile = file; lastline = line; lastname = fname;
            last_inlinedat_file = inlinedat_file; last_inlinedat_line = inlinedat_line;
        else
            n += 1
        end
    end
    if n > 1 || lastline != -11
        process_func(lastname, lastfile, lastline, last_inlinedat_file, last_inlinedat_line, n)
    end
end
