# This file is a part of Julia. License is MIT: http://julialang.org/license

# fallback text/plain representation of any type:
writemime(io::IO, ::MIME"text/plain", x) = show(io, x)


# showing exception objects as descriptive error messages

showerror(io::IO, ex) = show(io, ex)

function showerror(io::IO, ex::BoundsError)
    print(io, "BoundsError")
    if isdefined(ex, :a)
        print(io, ": attempt to access ")
        if isa(ex.a, AbstractArray)
            print(io, summary(ex.a))
        else
            writemime(io, MIME"text/plain"(), ex.a)
        end
        if isdefined(ex, :i)
            !isa(ex.a, AbstractArray) && print(io, "\n ")
            print(io, " at index [")
            if isa(ex.i, Range)
                print(io, ex.i)
            else
                join(io, ex.i, ',')
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
        code = StackTraces.lookup(b)[1]
        if !code.from_c
            if code.func == :nan_dom_err
                continue
            elseif code.func in (:log, :log2, :log10, :sqrt) # TODO add :besselj, :besseli, :bessely, :besselk
                print(io,"\n$(code.func) will only return a complex result if called with a complex argument. Try $(string(code.func))(complex(x)).")
            elseif (code.func == :^ && code.file == Symbol("intfuncs.jl")) || code.func == :power_by_squaring #3024
                print(io, "\nCannot raise an integer x to a negative power -n. \nMake x a float by adding a zero decimal (e.g. 2.0^-n instead of 2^-n), or write 1/x^n, float(x)^-n, or (x//1)^-n.")
            elseif code.func == :^ &&
                    (code.file == Symbol("promotion.jl") || code.file == Symbol("math.jl") ||
                    code.file == Symbol(joinpath(".","promotion.jl")) || code.file == Symbol(joinpath(".","math.jl")))
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
showerror(io::IO, ex::KeyError) = print(io, "KeyError: key $(repr(ex.key)) not found")
showerror(io::IO, ex::InterruptException) = print(io, "InterruptException:")
showerror(io::IO, ex::ArgumentError) = print(io, "ArgumentError: $(ex.msg)")
showerror(io::IO, ex::AssertionError) = print(io, "AssertionError: $(ex.msg)")

function showerror(io::IO, ex::MethodError)
    # ex.args is a tuple type if it was thrown from `invoke` and is
    # a tuple of the arguments otherwise.
    is_arg_types = isa(ex.args, DataType)
    arg_types = is_arg_types ? ex.args : typesof(ex.args...)
    f = ex.f
    meth = methods_including_ambiguous(f, arg_types)
    if length(meth) > 1
        return showerror_ambiguous(io, meth, f, arg_types)
    end
    arg_types_param::SimpleVector = arg_types.parameters
    print(io, "MethodError: ")
    ft = typeof(f)
    name = ft.name.mt.name
    f_is_function = false
    if f == Base.convert && length(arg_types_param) == 2 && !is_arg_types
        f_is_function = true
        # See #13033
        T = striptype(ex.args[1])
        if T == nothing
            print(io, "First argument to `convert` must be a Type, got ", ex.args[1])
        else
            print(io, "Cannot `convert` an object of type ", arg_types_param[2], " to an object of type ", T)
        end
    elseif isempty(methods(f)) && !isa(f, Function)
        print(io, "objects of type $ft are not callable")
    else
        if ft <: Function && isempty(ft.parameters) &&
                isdefined(ft.name.module, name) &&
                ft == typeof(getfield(ft.name.module, name))
            f_is_function = true
            print(io, "no method matching ", name)
        elseif isa(f, Type)
            print(io, "no method matching ", f)
        else
            print(io, "no method matching (::", ft, ")")
        end
        print(io, "(")
        for (i, typ) in enumerate(arg_types_param)
            print(io, "::$typ")
            i == length(arg_types_param) || print(io, ", ")
        end
        print(io, ")")
    end
    if ft <: AbstractArray
        print(io, "\nUse square brackets [] for indexing an Array.")
    end
    # Check for local functions that shadow methods in Base
    if f_is_function && isdefined(Base, name)
        basef = getfield(Base, name)
        if basef !== ex.f && method_exists(basef, arg_types)
            println(io)
            print(io, "you may have intended to import Base.", name)
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
    if (f === Base.convert && !isempty(arg_types_param) && !is_arg_types &&
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

striptype{T}(::Type{T}) = T
striptype(::Any) = nothing

function showerror_ambiguous(io::IO, meth, f, args)
    print(io, "MethodError: ", f, "(")
    p = args.parameters
    for (i,a) in enumerate(p)
        print(io, "::", a)
        i == length(p) ? print(io, ")") : print(io, ", ")
    end
    print(io, " is ambiguous. Candidates:")
    for m in meth
        print(io, "\n  ", m)
    end
    nothing
end

#Show an error by directly calling jl_printf.
#Useful in Base submodule __init__ functions where STDERR isn't defined yet.
function showerror_nostdio(err, msg::AbstractString)
    stderr_stream = ccall(:jl_stderr_stream, Ptr{Void}, ())
    ccall(:jl_printf, Cint, (Ptr{Void},Cstring), stderr_stream, msg)
    ccall(:jl_printf, Cint, (Ptr{Void},Cstring), stderr_stream, ":\n")
    ccall(:jl_static_show, Csize_t, (Ptr{Void},Any), stderr_stream, err)
    ccall(:jl_printf, Cint, (Ptr{Void},Cstring), stderr_stream, "\n")
end

function show_method_candidates(io::IO, ex::MethodError)
    is_arg_types = isa(ex.args, DataType)
    arg_types = is_arg_types ? ex.args : typesof(ex.args...)
    arg_types_param = Any[arg_types.parameters...]
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
    funcs = Any[(f, arg_types_param)]

    # An incorrect call method produces a MethodError for convert.
    # It also happens that users type convert when they mean call. So
    # pool MethodErrors for these two functions.
    if f === convert && !isempty(arg_types_param)
        at1 = arg_types_param[1]
        if isa(at1,DataType) && (at1::DataType).name === Type.name && isleaftype(at1)
            push!(funcs, (at1.parameters[1], arg_types_param[2:end]))
        end
    end

    for (func,arg_types_param) in funcs
        for method in methods(func)
            buf = IOBuffer()
            s1 = method.sig.parameters[1]
            sig = method.sig.parameters[2:end]
            print(buf, "  ")
            if !isa(func, s1)
                # function itself doesn't match
                return
            else
                use_constructor_syntax = isa(func, Type)
                print(buf, use_constructor_syntax ? func : typeof(func).name.mt.name)
            end
            right_matches = 0
            tv = method.tvars
            if !isa(tv,SimpleVector)
                tv = Any[tv]
            end
            if !isempty(tv)
                show_delim_array(buf, tv, '{', ',', '}', false)
            end
            print(buf, "(")
            t_i = copy(arg_types_param)
            right_matches = 0
            for i = 1 : min(length(t_i), length(sig))
                i > 1 && print(buf, ", ")
                # If isvarargtype then it checks whether the rest of the input arguments matches
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
                if t_in === Union{}
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
            special && right_matches==0 && return # continue the do-block

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

function show_trace_entry(io, frame, n)
    print(io, "\n")
    show(io, frame, full_path=true)
    n > 1 && print(io, " (repeats ", n, " times)")
end

function show_backtrace(io::IO, t::Vector, set=1:typemax(Int))
    # we may not declare :eval_user_input
    # directly so that we get a compile error
    # in case its name changes in the future
    show_backtrace(io,
                    try
                        typeof(eval_user_input).name.mt.name
                    catch
                        :(:) #for when client.jl is not yet defined
                    end, t, set)
end

function show_backtrace(io::IO, top_function::Symbol, t::Vector, set)
    process_entry(last_frame, n) =
        show_trace_entry(io, last_frame, n)
    process_backtrace(process_entry, top_function, t, set)
end

function show_backtrace(io::IO, top_function::Symbol, t::Vector{Any}, set)
    for entry in t
        show_trace_entry(io, entry...)
    end
end

# process the backtrace, up to (but not including) top_function
function process_backtrace(process_func::Function, top_function::Symbol, t::Vector, set; skipC = true)
    n = 0
    last_frame = StackTraces.UNKNOWN
    count = 0
    for i = eachindex(t)
        lkups = StackTraces.lookup(t[i])
        for lkup in lkups
            if lkup === StackTraces.UNKNOWN
                continue
            end

            if lkup.from_c && skipC; continue; end
            if i == 1 && lkup.func == :error; continue; end
            if lkup.func == top_function; break; end
            count += 1
            if !in(count, set); continue; end

            if lkup.file != last_frame.file || lkup.line != last_frame.line || lkup.func != last_frame.func
                if n > 0
                    process_func(last_frame, n)
                end
                n = 1
                last_frame = lkup
            else
                n += 1
            end
        end
    end
    if n > 0
        process_func(last_frame, n)
    end
end
