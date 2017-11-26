# This file is a part of Julia. License is MIT: https://julialang.org/license

# fallback text/plain representation of any type:
show(io::IO, ::MIME"text/plain", x) = show(io, x)

# multiline show functions for types defined before multimedia.jl:
function show(io::IO, ::MIME"text/plain", iter::Union{KeyIterator,ValueIterator})
    print(io, summary(iter))
    isempty(iter) && return
    print(io, ". ", isa(iter,KeyIterator) ? "Keys" : "Values", ":")
    limit::Bool = get(io, :limit, false)
    if limit
        sz = displaysize(io)
        rows, cols = sz[1] - 3, sz[2]
        rows < 2 && (print(io, " …"); return)
        cols < 4 && (cols = 4)
        cols -= 2 # For prefix "  "
        rows -= 1 # For summary
    else
        rows = cols = typemax(Int)
    end

    for (i, v) in enumerate(iter)
        print(io, "\n  ")
        i == rows < length(iter) && (print(io, "⋮"); break)

        if limit
            str = sprint(0, show, v, env=io)
            str = _truncate_at_width_or_chars(str, cols, "\r\n")
            print(io, str)
        else
            show(io, v)
        end
    end
end

function show(io::IO, ::MIME"text/plain", t::Associative{K,V}) where {K,V}
    # show more descriptively, with one line per key/value pair
    recur_io = IOContext(io, :SHOWN_SET => t)
    limit::Bool = get(io, :limit, false)
    if !haskey(io, :compact)
        recur_io = IOContext(recur_io, :compact => true)
    end

    print(io, summary(t))
    isempty(t) && return
    print(io, ":")
    show_circular(io, t) && return
    if limit
        sz = displaysize(io)
        rows, cols = sz[1] - 3, sz[2]
        rows < 2   && (print(io, " …"); return)
        cols < 12  && (cols = 12) # Minimum widths of 2 for key, 4 for value
        cols -= 6 # Subtract the widths of prefix "  " separator " => "
        rows -= 1 # Subtract the summary

        # determine max key width to align the output, caching the strings
        ks = Vector{AbstractString}(uninitialized, min(rows, length(t)))
        vs = Vector{AbstractString}(uninitialized, min(rows, length(t)))
        keylen = 0
        vallen = 0
        for (i, (k, v)) in enumerate(t)
            i > rows && break
            ks[i] = sprint(0, show, k, env=recur_io)
            vs[i] = sprint(0, show, v, env=recur_io)
            keylen = clamp(length(ks[i]), keylen, cols)
            vallen = clamp(length(vs[i]), vallen, cols)
        end
        if keylen > max(div(cols, 2), cols - vallen)
            keylen = max(cld(cols, 3), cols - vallen)
        end
    else
        rows = cols = typemax(Int)
    end

    for (i, (k, v)) in enumerate(t)
        print(io, "\n  ")
        i == rows < length(t) && (print(io, rpad("⋮", keylen), " => ⋮"); break)

        if limit
            key = rpad(_truncate_at_width_or_chars(ks[i], keylen, "\r\n"), keylen)
        else
            key = sprint(0, show, k, env=recur_io)
        end
        print(recur_io, key)
        print(io, " => ")

        if limit
            val = _truncate_at_width_or_chars(vs[i], cols - keylen, "\r\n")
            print(io, val)
        else
            show(recur_io, v)
        end
    end
end

function show(io::IO, ::MIME"text/plain", f::Function)
    ft = typeof(f)
    mt = ft.name.mt
    if isa(f, Core.IntrinsicFunction)
        show(io, f)
        id = Core.Intrinsics.bitcast(Int32, f)
        print(io, " (intrinsic function #$id)")
    elseif isa(f, Core.Builtin)
        print(io, mt.name, " (built-in function)")
    else
        name = mt.name
        isself = isdefined(ft.name.module, name) &&
                 ft == typeof(getfield(ft.name.module, name))
        n = length(methods(f))
        m = n==1 ? "method" : "methods"
        sname = string(name)
        ns = (isself || '#' in sname) ? sname : string("(::", ft, ")")
        what = startswith(ns, '@') ? "macro" : "generic function"
        print(io, ns, " (", what, " with $n $m)")
    end
end

function show(io::IO, ::MIME"text/plain", r::LinSpace)
    # show for linspace, e.g.
    # linspace(1,3,7)
    # 7-element LinSpace{Float64}:
    #   1.0,1.33333,1.66667,2.0,2.33333,2.66667,3.0
    print(io, summary(r))
    if !isempty(r)
        println(io, ":")
        print_range(io, r)
    end
end

function show(io::IO, ::MIME"text/plain", t::Task)
    show(io, t)
    if t.state == :failed
        println(io)
        showerror(io, CapturedException(t.result, t.backtrace))
    end
end

show(io::IO, ::MIME"text/plain", X::AbstractArray) = showarray(io, X, false)
show(io::IO, ::MIME"text/plain", r::AbstractRange) = show(io, r) # always use the compact form for printing ranges

# display something useful even for strings containing arbitrary
# (non-UTF8) binary data:
function show(io::IO, ::MIME"text/plain", s::String)
    if isvalid(s)
        show(io, s)
    else
        println(io, sizeof(s), "-byte String of invalid UTF-8 data:")
        showarray(io, Vector{UInt8}(s), false; header=false)
    end
end

function show(io::IO, ::MIME"text/plain", opt::JLOptions)
    println(io, "JLOptions(")
    fields = fieldnames(JLOptions)
    nfields = length(fields)
    for (i, f) in enumerate(fields)
        v = getfield(opt, i)
        if isa(v, Ptr{UInt8})
            v = (v != C_NULL) ? unsafe_string(v) : ""
        elseif isa(v, Ptr{Ptr{UInt8}})
            v = unsafe_load_commands(v)
        end
        println(io, "  ", f, " = ", repr(v), i < nfields ? "," : "")
    end
    print(io, ")")
end


# showing exception objects as descriptive error messages

"""
    showerror(io, e)

Show a descriptive representation of an exception object.
"""
showerror(io::IO, ex) = show(io, ex)

function showerror(io::IO, ex::BoundsError)
    print(io, "BoundsError")
    if isdefined(ex, :a)
        print(io, ": attempt to access ")
        if isa(ex.a, AbstractArray)
            print(io, summary(ex.a))
        else
            show(io, MIME"text/plain"(), ex.a)
        end
        if isdefined(ex, :i)
            !isa(ex.a, AbstractArray) && print(io, "\n ")
            print(io, " at index [")
            if isa(ex.i, AbstractRange)
                print(io, ex.i)
            else
                join(io, ex.i, ", ")
            end
            print(io, ']')
        end
    end
end

function showerror(io::IO, ex::TypeError)
    print(io, "TypeError: ")
    if ex.expected === Bool
        print(io, "non-boolean ($(typeof(ex.got))) used in boolean context")
    else
        if isa(ex.got, Type)
            tstr = "Type{$(ex.got)}"
        else
            tstr = string(typeof(ex.got))
        end
        if isempty(ex.context)
            ctx = "in $(ex.func)"
        else
            ctx = "in $(ex.func), in $(ex.context)"
        end
        print(io, ctx, ", expected $(ex.expected), got ", tstr)
    end
end

function showerror(io::IO, ex, bt; backtrace=true)
    try
        with_output_color(have_color ? error_color() : :nothing, io) do io
            showerror(io, ex)
        end
    finally
        backtrace && show_backtrace(io, bt)
    end
end

function showerror(io::IO, ex::LoadError, bt; backtrace=true)
    print(io, "LoadError: ")
    showerror(io, ex.error, bt, backtrace=backtrace)
    print(io, "\nin expression starting at $(ex.file):$(ex.line)")
end
showerror(io::IO, ex::LoadError) = showerror(io, ex, [])

function showerror(io::IO, ex::InitError, bt; backtrace=true)
    print(io, "InitError: ")
    showerror(io, ex.error, bt, backtrace=backtrace)
    print(io, "\nduring initialization of module $(ex.mod)")
end
showerror(io::IO, ex::InitError) = showerror(io, ex, [])

function showerror(io::IO, ex::DomainError, bt; backtrace=true)
    if isa(ex.val, AbstractArray)
        compact = get(io, :compact, true)
        limit = get(io, :limit, true)
        print(IOContext(io, :compact => compact, :limit => limit),
              "DomainError with ", ex.val)
    else
        print(io, "DomainError with ", ex.val)
    end
    if isdefined(ex, :msg)
        print(io, ":\n", ex.msg)
    end
    backtrace && show_backtrace(io, bt)
    nothing
end

function showerror(io::IO, ex::SystemError)
    if ex.extrainfo === nothing
        print(io, "SystemError: $(ex.prefix): $(Libc.strerror(ex.errnum))")
    else
        print(io, "SystemError (with $(ex.extrainfo)): $(ex.prefix): $(Libc.strerror(ex.errnum))")
    end
end
showerror(io::IO, ::DivideError) = print(io, "DivideError: integer division error")
showerror(io::IO, ::StackOverflowError) = print(io, "StackOverflowError:")
showerror(io::IO, ::UndefRefError) = print(io, "UndefRefError: access to undefined reference")
showerror(io::IO, ::EOFError) = print(io, "EOFError: read end of file")
function showerror(io::IO, ex::ErrorException)
    print(io, ex.msg)
    if ex.msg == "type String has no field data"
        println(io)
        print(io, "Use `Vector{UInt8}(str)` instead.")
    end
end
showerror(io::IO, ex::KeyError) = print(io, "KeyError: key $(repr(ex.key)) not found")
showerror(io::IO, ex::InterruptException) = print(io, "InterruptException:")
showerror(io::IO, ex::ArgumentError) = print(io, "ArgumentError: $(ex.msg)")
showerror(io::IO, ex::AssertionError) = print(io, "AssertionError: $(ex.msg)")
showerror(io::IO, ex::OverflowError) = print(io, "OverflowError: $(ex.msg)")

function showerror(io::IO, ex::UndefVarError)
    if ex.var in [:UTF16String, :UTF32String, :WString, :utf16, :utf32, :wstring, :RepString]
        return showerror(io, ErrorException("""
        `$(ex.var)` has been moved to the package LegacyStrings.jl:
        Run Pkg.add("LegacyStrings") to install LegacyStrings on Julia v0.5-;
        Then do `using LegacyStrings` to get `$(ex.var)`.
        """))
    end
    print(io, "UndefVarError: $(ex.var) not defined")
end

function showerror(io::IO, ex::InexactError)
    print(io, "InexactError: ", ex.func, '(', ex.T, ", ", ex.val, ')')
end

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
    kwargs = NamedTuple()
    if startswith(string(ft.name.name), "#kw#")
        f = ex.args[2]
        ft = typeof(f)
        name = ft.name.mt.name
        arg_types_param = arg_types_param[3:end]
        kwargs = ex.args[1]
        ex = MethodError(f, ex.args[3:end])
    end
    if f == Base.convert && length(arg_types_param) == 2 && !is_arg_types
        f_is_function = true
        # See #13033
        T = striptype(ex.args[1])
        if T === nothing
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
            if isa(f, DataType) && f.abstract
                # Print a more appropriate message if the only method
                # on the type is the default one from sysimg.jl.
                ms = methods(f)
                if length(ms) == 1
                    m = first(ms)
                    if Base.is_default_method(m)
                        print(io, "no constructors have been defined for $f")
                        return
                    end
                end
            end
            print(io, "no method matching ", f)
        else
            print(io, "no method matching (::", ft, ")")
        end
        print(io, "(")
        for (i, typ) in enumerate(arg_types_param)
            print(io, "::$typ")
            i == length(arg_types_param) || print(io, ", ")
        end
        if !isempty(kwargs)
            print(io, "; ")
            for (i, (k, v)) in enumerate(pairs(kwargs))
                print(io, k, "=")
                show(IOContext(io, :limit => true), v)
                i == length(kwargs) || print(io, ", ")
            end
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
            print(io, "You may have intended to import Base.", name)
        end
    end
    if (ex.world != typemax(UInt) && method_exists(ex.f, arg_types) &&
        !method_exists(ex.f, arg_types, ex.world))
        curworld = ccall(:jl_get_world_counter, UInt, ())
        println(io)
        print(io, "The applicable method may be too new: running in world age $(ex.world), while current world is $(curworld).")
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
        arg_types_param[1].name === Type.body.name)
        construct_type = arg_types_param[1].parameters[1]
        println(io)
        print(io, "This may have arisen from a call to the constructor $construct_type(...),",
                  "\nsince type constructors fall back to convert methods.")
    end
    try
        show_method_candidates(io, ex, kwargs)
    catch
        warn(io, "Error showing method candidates, aborted")
    end
end

striptype(::Type{T}) where {T} = T
striptype(::Any) = nothing

function showerror_ambiguous(io::IO, meth, f, args)
    print(io, "MethodError: ", f, "(")
    p = args.parameters
    for (i,a) in enumerate(p)
        print(io, "::", a)
        i < length(p) && print(io, ", ")
    end
    print(io, ") is ambiguous. Candidates:")
    sigfix = Any
    for m in meth
        print(io, "\n  ", m)
        sigfix = typeintersect(m.sig, sigfix)
    end
    if isa(unwrap_unionall(sigfix), DataType) && sigfix <: Tuple
        print(io, "\nPossible fix, define\n  ")
        Base.show_tuple_as_call(io, :function,  sigfix)
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

function show_method_candidates(io::IO, ex::MethodError, kwargs::NamedTuple = NamedTuple())
    is_arg_types = isa(ex.args, DataType)
    arg_types = is_arg_types ? ex.args : typesof(ex.args...)
    arg_types_param = Any[arg_types.parameters...]
    # Displays the closest candidates of the given function by looping over the
    # functions methods and counting the number of matching arguments.
    f = ex.f
    ft = typeof(f)
    lines = []
    # These functions are special cased to only show if first argument is matched.
    special = f in [convert, getindex, setindex!]
    funcs = Any[(f, arg_types_param)]

    # An incorrect call method produces a MethodError for convert.
    # It also happens that users type convert when they mean call. So
    # pool MethodErrors for these two functions.
    if f === convert && !isempty(arg_types_param)
        at1 = arg_types_param[1]
        if isa(at1,DataType) && (at1::DataType).name === Type.body.name && !Core.Inference.has_free_typevars(at1)
            push!(funcs, (at1.parameters[1], arg_types_param[2:end]))
        end
    end

    for (func,arg_types_param) in funcs
        for method in methods(func)
            buf = IOBuffer()
            tv = Any[]
            sig0 = method.sig
            if Base.is_default_method(method)
                continue
            end
            while isa(sig0, UnionAll)
                push!(tv, sig0.var)
                sig0 = sig0.body
            end
            s1 = sig0.parameters[1]
            sig = sig0.parameters[2:end]
            print(buf, "  ")
            if !isa(func, s1)
                # function itself doesn't match
                return
            else
                # TODO: use the methodshow logic here
                use_constructor_syntax = isa(func, Type)
                print(buf, use_constructor_syntax ? func : typeof(func).name.mt.name)
            end
            print(buf, "(")
            t_i = copy(arg_types_param)
            right_matches = 0
            for i = 1 : min(length(t_i), length(sig))
                i > 1 && print(buf, ", ")
                # If isvarargtype then it checks whether the rest of the input arguments matches
                # the varargtype
                if Base.isvarargtype(sig[i])
                    sigstr = string(unwrap_unionall(sig[i]).parameters[1], "...")
                    j = length(t_i)
                else
                    sigstr = string(sig[i])
                    j = i
                end
                # Checks if the type of arg 1:i of the input intersects with the current method
                t_in = typeintersect(rewrap_unionall(Tuple{sig[1:i]...}, method.sig),
                                     rewrap_unionall(Tuple{t_i[1:j]...}, method.sig))
                # If the function is one of the special cased then it should break the loop if
                # the type of the first argument is not matched.
                t_in === Union{} && special && i == 1 && break
                if t_in === Union{}
                    if Base.have_color
                        Base.with_output_color(Base.error_color(), buf) do buf
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
                    if t <: rewrap_unionall(unwrap_unionall(sig[end]).parameters[1], method.sig)
                        right_matches += 1
                    end
                end
            end

            if right_matches > 0 || length(ex.args) < 2
                if length(t_i) < length(sig)
                    # If the methods args is longer than input then the method
                    # arguments is printed as not a match
                    for (k, sigtype) in enumerate(sig[length(t_i)+1:end])
                        sigtype = isvarargtype(sigtype) ? unwrap_unionall(sigtype) : sigtype
                        if Base.isvarargtype(sigtype)
                            sigstr = string(sigtype.parameters[1], "...")
                        else
                            sigstr = string(sigtype)
                        end
                        if !((min(length(t_i), length(sig)) == 0) && k==1)
                            print(buf, ", ")
                        end
                        if Base.have_color
                            Base.with_output_color(Base.error_color(), buf) do buf
                                print(buf, "::$sigstr")
                            end
                        else
                            print(buf, "!Matched::$sigstr")
                        end
                    end
                end
                kwords = Symbol[]
                if isdefined(ft.name.mt, :kwsorter)
                    kwsorter_t = typeof(ft.name.mt.kwsorter)
                    kwords = kwarg_decl(method, kwsorter_t)
                    length(kwords) > 0 && print(buf, "; ", join(kwords, ", "))
                end
                print(buf, ")")
                show_method_params(buf, tv)
                print(buf, " at ", method.file, ":", method.line)
                if !isempty(kwargs)
                    unexpected = Symbol[]
                    if isempty(kwords) || !(any(endswith(string(kword), "...") for kword in kwords))
                        for (k, v) in pairs(kwargs)
                            if !(k in kwords)
                                push!(unexpected, k)
                            end
                        end
                    end
                    if !isempty(unexpected)
                        Base.with_output_color(Base.error_color(), buf) do buf
                            plur = length(unexpected) > 1 ? "s" : ""
                            print(buf, " got unsupported keyword argument$plur \"", join(unexpected, "\", \""), "\"")
                        end
                    end
                end
                if ex.world < min_world(method)
                    print(buf, " (method too new to be called from this world context.)")
                end
                # TODO: indicate if it's in the wrong world
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
                print(io, String(take!(line[1])))
            end
        end
    end
end

function show_trace_entry(io, frame, n; prefix = "")
    print(io, "\n", prefix)
    show(io, frame, full_path=true)
    n > 1 && print(io, " (repeats ", n, " times)")
end

# Contains file name and file number. Gets set when a backtrace
# or methodlist is shown. Used by the REPL to make it possible to open
# the location of a stackframe/method in the editor.
global LAST_SHOWN_LINE_INFOS = Tuple{String, Int}[]

function show_backtrace(io::IO, t::Vector)
    resize!(LAST_SHOWN_LINE_INFOS, 0)
    filtered = Any[]
    process_backtrace((fr, count) -> push!(filtered, (fr, count)), t)
    isempty(filtered) && return

    if length(filtered) == 1 && StackTraces.is_top_level_frame(filtered[1][1])
        f = filtered[1][1]
        if f.line == 0 && f.file == Symbol("")
            # don't show a single top-level frame with no location info
            return
        end
    end

    print(io, "\nStacktrace:")
    frame_counter = 0
    for (last_frame, n) in filtered
        frame_counter += 1
        show_trace_entry(IOContext(io, :backtrace => true), last_frame, n, prefix = string(" [", frame_counter, "] "))
        push!(LAST_SHOWN_LINE_INFOS, (string(last_frame.file), last_frame.line))
    end
end

function show_backtrace(io::IO, t::Vector{Any})
    for entry in t
        show_trace_entry(io, entry...)
    end
end

# call process_func on each frame in a backtrace
function process_backtrace(process_func::Function, t::Vector, limit::Int=typemax(Int); skipC = true)
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
            count += 1
            if count > limit; break; end

            if lkup.file != last_frame.file || lkup.line != last_frame.line || lkup.func != last_frame.func || lkup.linfo !== lkup.linfo
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

"""
Determines whether a method is the default method which is provided to all types from sysimg.jl.
Such a method is usually undesirable to be displayed to the user in the REPL.
"""
function is_default_method(m::Method)
    return m.module == Base && m.file == Symbol("sysimg.jl") && m.sig == Tuple{Type{T},Any} where T
end
