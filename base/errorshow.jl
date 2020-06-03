# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    showerror(io, e)

Show a descriptive representation of an exception object `e`.
This method is used to display the exception after a call to [`throw`](@ref).

# Examples
```jldoctest
julia> struct MyException <: Exception
           msg::AbstractString
       end

julia> function Base.showerror(io::IO, err::MyException)
           print(io, "MyException: ")
           print(io, err.msg)
       end

julia> err = MyException("test exception")
MyException("test exception")

julia> sprint(showerror, err)
"MyException: test exception"

julia> throw(MyException("test exception"))
ERROR: MyException: test exception
```
"""
showerror(io::IO, ex) = show(io, ex)

show_index(io::IO, x::Any) = show(io, x)
show_index(io::IO, x::Slice) = show_index(io, x.indices)
show_index(io::IO, x::LogicalIndex) = show_index(io, x.mask)
show_index(io::IO, x::OneTo) = print(io, "1:", x.stop)
show_index(io::IO, x::Colon) = print(io, ':')


function showerror(io::IO, ex::BoundsError)
    print(io, "BoundsError")
    if isdefined(ex, :a)
        print(io, ": attempt to access ")
        summary(io, ex.a)
        if isdefined(ex, :i)
            !isa(ex.a, AbstractArray) && print(io, "\n ")
            print(io, " at index [")
            if ex.i isa AbstractRange
                print(io, ex.i)
            elseif ex.i isa AbstractString
                show(io, ex.i)
            else
                for (i, x) in enumerate(ex.i)
                    i > 1 && print(io, ", ")
                    show_index(io, x)
                end
            end
            print(io, ']')
        end
    end
    Experimental.show_error_hints(io, ex)
end

function showerror(io::IO, ex::TypeError)
    print(io, "TypeError: ")
    if ex.expected === Bool
        print(io, "non-boolean (", typeof(ex.got), ") used in boolean context")
    else
        if isvarargtype(ex.got)
            targs = (ex.got,)
        elseif isa(ex.got, Type)
            targs = ("Type{", ex.got, "}")
        else
            targs = ("a value of type $(typeof(ex.got))",)
        end
        if ex.context == ""
            ctx = "in $(ex.func)"
        elseif ex.func === Symbol("keyword argument")
            ctx = "in keyword argument $(ex.context)"
        else
            ctx = "in $(ex.func), in $(ex.context)"
        end
        print(io, ctx, ", expected ", ex.expected, ", got ", targs...)
    end
    Experimental.show_error_hints(io, ex)
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
    print(io, "\nin expression starting at $(ex.file):$(ex.line)")
end
showerror(io::IO, ex::LoadError) = showerror(io, ex, [])

function showerror(io::IO, ex::InitError, bt; backtrace=true)
    print(io, "InitError: ")
    showerror(io, ex.error, bt, backtrace=backtrace)
    print(io, "\nduring initialization of module ", ex.mod)
end
showerror(io::IO, ex::InitError) = showerror(io, ex, [])

function showerror(io::IO, ex::DomainError)
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
    Experimental.show_error_hints(io, ex)
    nothing
end

function showerror(io::IO, ex::SystemError)
    if @static(Sys.iswindows() ? ex.extrainfo isa WindowsErrorInfo : false)
        errstring = Libc.FormatMessage(ex.extrainfo.errnum)
        extrainfo = ex.extrainfo.extrainfo
    else
        errstring = Libc.strerror(ex.errnum)
        extrainfo = ex.extrainfo
    end
    if extrainfo === nothing
        print(io, "SystemError: $(ex.prefix): ", errstring)
    else
        print(io, "SystemError (with $extrainfo): $(ex.prefix): ", errstring)
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
        print(io, "Use `codeunits(str)` instead.")
    end
end
showerror(io::IO, ex::KeyError) = (print(io, "KeyError: key ");
                                   show(io, ex.key);
                                   print(io, " not found"))
showerror(io::IO, ex::InterruptException) = print(io, "InterruptException:")
showerror(io::IO, ex::ArgumentError) = print(io, "ArgumentError: ", ex.msg)
showerror(io::IO, ex::AssertionError) = print(io, "AssertionError: ", ex.msg)
showerror(io::IO, ex::OverflowError) = print(io, "OverflowError: ", ex.msg)

showerror(io::IO, ex::UndefKeywordError) =
    print(io, "UndefKeywordError: keyword argument $(ex.var) not assigned")

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
    print(io, "InexactError: ", ex.func, '(')
    nameof(ex.T) === ex.func || print(io, ex.T, ", ")
    print(io, ex.val, ')')
    Experimental.show_error_hints(io, ex)
end

typesof(args...) = Tuple{Any[ Core.Typeof(a) for a in args ]...}

function print_with_compare(io::IO, @nospecialize(a::DataType), @nospecialize(b::DataType), color::Symbol)
    if a.name === b.name
        Base.show_type_name(io, a.name)
        n = length(a.parameters)
        print(io, '{')
        for i = 1:n
            if i > length(b.parameters)
                printstyled(io, a.parameters[i], color=color)
            else
                print_with_compare(io::IO, a.parameters[i], b.parameters[i], color)
            end
            i < n && print(io, ',')
        end
        print(io, '}')
    else
        printstyled(io, a; color=color)
    end
end

function print_with_compare(io::IO, @nospecialize(a), @nospecialize(b), color::Symbol)
    if a === b
        print(io, a)
    else
        printstyled(io, a; color=color)
    end
end

function show_convert_error(io::IO, ex::MethodError, @nospecialize(arg_types_param))
    # See #13033
    T = striptype(ex.args[1])
    if T === nothing
        print(io, "First argument to `convert` must be a Type, got ", ex.args[1])
    else
        print_one_line = isa(T, DataType) && isa(arg_types_param[2], DataType) && T.name != arg_types_param[2].name
        printstyled(io, "Cannot `convert` an object of type ")
        print_one_line || printstyled(io, "\n  ")
        print_with_compare(io, arg_types_param[2], T, :light_green)
        printstyled(io, " to an object of type ")
        print_one_line || printstyled(io, "\n  ")
        print_with_compare(io, T, arg_types_param[2], :light_red)
    end
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
    kwargs = ()
    if endswith(string(ft.name.name), "##kw")
        f = ex.args[2]
        ft = typeof(f)
        name = ft.name.mt.name
        arg_types_param = arg_types_param[3:end]
        kwargs = pairs(ex.args[1])
        ex = MethodError(f, ex.args[3:end])
    end
    if f === Base.convert && length(arg_types_param) == 2 && !is_arg_types
        f_is_function = true
        show_convert_error(io, ex, arg_types_param)
    elseif isempty(methods(f)) && isa(f, DataType) && f.abstract
        print(io, "no constructors have been defined for ", f)
    elseif isempty(methods(f)) && !isa(f, Function) && !isa(f, Type)
        print(io, "objects of type ", ft, " are not callable")
    else
        if ft <: Function && isempty(ft.parameters) &&
                isdefined(ft.name.module, name) &&
                ft == typeof(getfield(ft.name.module, name))
            f_is_function = true
        end
        print(io, "no method matching ")
        show_signature_function(io, isa(f, Type) ? Type{f} : typeof(f))
        print(io, "(")
        for (i, typ) in enumerate(arg_types_param)
            print(io, "::", typ)
            i == length(arg_types_param) || print(io, ", ")
        end
        if !isempty(kwargs)
            print(io, "; ")
            for (i, (k, v)) in enumerate(kwargs)
                print(io, k, "=")
                show(IOContext(io, :limit => true), v)
                i == length(kwargs) || print(io, ", ")
            end
        end
        print(io, ")")
    end
    # catch the two common cases of element-wise addition and subtraction
    if (f === Base.:+ || f === Base.:-) && length(arg_types_param) == 2
        # we need one array of numbers and one number, in any order
        if any(x -> x <: AbstractArray{<:Number}, arg_types_param) &&
            any(x -> x <: Number, arg_types_param)

            nouns = Dict(
                Base.:+ => "addition",
                Base.:- => "subtraction",
            )
            varnames = ("scalar", "array")
            first, second = arg_types_param[1] <: Number ? varnames : reverse(varnames)
            print(io, "\nFor element-wise $(nouns[f]), use broadcasting with dot syntax: $first .$f $second")
        end
    end
    if ft <: AbstractArray
        print(io, "\nUse square brackets [] for indexing an Array.")
    end
    # Check for local functions that shadow methods in Base
    if f_is_function && isdefined(Base, name)
        basef = getfield(Base, name)
        if basef !== ex.f && hasmethod(basef, arg_types)
            print(io, "\nYou may have intended to import ")
            show_unquoted(io, Expr(:., :Base, QuoteNode(name)))
        end
    end
    if (ex.world != typemax(UInt) && hasmethod(ex.f, arg_types) &&
        !hasmethod(ex.f, arg_types, world = ex.world))
        curworld = get_world_counter()
        print(io, "\nThe applicable method may be too new: running in world age $(ex.world), while current world is $(curworld).")
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
    Experimental.show_error_hints(io, ex, arg_types_param, kwargs)
    try
        show_method_candidates(io, ex, kwargs)
    catch ex
        @error "Error showing method candidates, aborted" exception=ex,catch_backtrace()
    end
end

striptype(::Type{T}) where {T} = T
striptype(::Any) = nothing

function showerror_ambiguous(io::IO, meth, f, args)
    print(io, "MethodError: ")
    show_signature_function(io, isa(f, Type) ? Type{f} : typeof(f))
    print(io, "(")
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
        if all(m->morespecific(sigfix, m.sig), meth)
            print(io, "\nPossible fix, define\n  ")
            Base.show_tuple_as_call(io, :function,  sigfix)
        else
            println(io)
            print(io, "To resolve the ambiguity, try making one of the methods more specific, or ")
            print(io, "adding a new method more specific than any of the existing applicable methods.")
        end
    end
    nothing
end

#Show an error by directly calling jl_printf.
#Useful in Base submodule __init__ functions where stderr isn't defined yet.
function showerror_nostdio(err, msg::AbstractString)
    stderr_stream = ccall(:jl_stderr_stream, Ptr{Cvoid}, ())
    ccall(:jl_printf, Cint, (Ptr{Cvoid},Cstring), stderr_stream, msg)
    ccall(:jl_printf, Cint, (Ptr{Cvoid},Cstring), stderr_stream, ":\n")
    ccall(:jl_static_show, Csize_t, (Ptr{Cvoid},Any), stderr_stream, err)
    ccall(:jl_printf, Cint, (Ptr{Cvoid},Cstring), stderr_stream, "\n")
end

function show_method_candidates(io::IO, ex::MethodError, @nospecialize kwargs=())
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
        if isa(at1,DataType) && (at1::DataType).name === Type.body.name && !Core.Compiler.has_free_typevars(at1)
            push!(funcs, (at1.parameters[1], arg_types_param[2:end]))
        end
    end

    for (func, arg_types_param) in funcs
        for method in methods(func)
            buf = IOBuffer()
            iob0 = iob = IOContext(buf, io)
            tv = Any[]
            sig0 = method.sig
            while isa(sig0, UnionAll)
                push!(tv, sig0.var)
                iob = IOContext(iob, :unionall_env => sig0.var)
                sig0 = sig0.body
            end
            s1 = sig0.parameters[1]
            sig = sig0.parameters[2:end]
            print(iob, "  ")
            if !isa(func, rewrap_unionall(s1, method.sig))
                # function itself doesn't match
                continue
            else
                show_signature_function(iob, s1)
            end
            print(iob, "(")
            t_i = copy(arg_types_param)
            right_matches = 0
            for i = 1 : min(length(t_i), length(sig))
                i > 1 && print(iob, ", ")
                # If isvarargtype then it checks whether the rest of the input arguments matches
                # the varargtype
                if Base.isvarargtype(sig[i])
                    sigstr = (unwrap_unionall(sig[i]).parameters[1], "...")
                    j = length(t_i)
                else
                    sigstr = (sig[i],)
                    j = i
                end
                # Checks if the type of arg 1:i of the input intersects with the current method
                t_in = typeintersect(rewrap_unionall(Tuple{sig[1:i]...}, method.sig),
                                     rewrap_unionall(Tuple{t_i[1:j]...}, method.sig))
                # If the function is one of the special cased then it should break the loop if
                # the type of the first argument is not matched.
                t_in === Union{} && special && i == 1 && break
                if t_in === Union{}
                    if get(io, :color, false)
                        Base.with_output_color(Base.error_color(), iob) do iob
                            print(iob, "::", sigstr...)
                        end
                    else
                        print(iob, "!Matched::", sigstr...)
                    end
                    # If there is no typeintersect then the type signature from the method is
                    # inserted in t_i this ensures if the type at the next i matches the type
                    # signature then there will be a type intersect
                    t_i[i] = sig[i]
                else
                    right_matches += j==i ? 1 : 0
                    print(iob, "::", sigstr...)
                end
            end
            special && right_matches == 0 && continue

            if length(t_i) > length(sig) && !isempty(sig) && Base.isvarargtype(sig[end])
                # It ensures that methods like f(a::AbstractString...) gets the correct
                # number of right_matches
                for t in arg_types_param[length(sig):end]
                    if t <: rewrap_unionall(unwrap_unionall(sig[end]).parameters[1], method.sig)
                        right_matches += 1
                    end
                end
            end

            if right_matches > 0 || length(arg_types_param) < 2
                if length(t_i) < length(sig)
                    # If the methods args is longer than input then the method
                    # arguments is printed as not a match
                    for (k, sigtype) in enumerate(sig[length(t_i)+1:end])
                        sigtype = isvarargtype(sigtype) ? unwrap_unionall(sigtype) : sigtype
                        if Base.isvarargtype(sigtype)
                            sigstr = (sigtype.parameters[1], "...")
                        else
                            sigstr = (sigtype,)
                        end
                        if !((min(length(t_i), length(sig)) == 0) && k==1)
                            print(iob, ", ")
                        end
                        if get(io, :color, false)
                            Base.with_output_color(Base.error_color(), iob) do iob
                                print(iob, "::", sigstr...)
                            end
                        else
                            print(iob, "!Matched::", sigstr...)
                        end
                    end
                end
                kwords = kwarg_decl(method)
                if !isempty(kwords)
                    print(iob, "; ")
                    join(iob, kwords, ", ")
                end
                print(iob, ")")
                show_method_params(iob0, tv)
                print(iob, " at ", method.file, ":", method.line)
                if !isempty(kwargs)
                    unexpected = Symbol[]
                    if isempty(kwords) || !(any(endswith(string(kword), "...") for kword in kwords))
                        for (k, v) in kwargs
                            if !(k in kwords)
                                push!(unexpected, k)
                            end
                        end
                    end
                    if !isempty(unexpected)
                        Base.with_output_color(Base.error_color(), iob) do iob
                            plur = length(unexpected) > 1 ? "s" : ""
                            print(iob, " got unsupported keyword argument$plur \"", join(unexpected, "\", \""), "\"")
                        end
                    end
                end
                if ex.world < reinterpret(UInt, method.primary_world)
                    print(iob, " (method too new to be called from this world context.)")
                elseif ex.world > reinterpret(UInt, method.deleted_world)
                    print(iob, " (method deleted before this world age.)")
                end
                # TODO: indicate if it's in the wrong world
                push!(lines, (buf, right_matches))
            end
        end
    end

    if !isempty(lines) # Display up to three closest candidates
        Base.with_output_color(:normal, io) do io
            print(io, "\nClosest candidates are:")
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

# Contains file name and file number. Gets set when a backtrace
# or methodlist is shown. Used by the REPL to make it possible to open
# the location of a stackframe/method in the editor.
global LAST_SHOWN_LINE_INFOS = Tuple{String, Int}[]

function show_trace_entry(io, frame, n; prefix = "")
    push!(LAST_SHOWN_LINE_INFOS, (string(frame.file), frame.line))
    print(io, "\n", prefix)
    show(io, frame, full_path=true)
    n > 1 && print(io, " (repeats ", n, " times)")
end

# In case the line numbers in the source code have changed since the code was compiled,
# allow packages to set a callback function that corrects them.
# (Used by Revise and perhaps other packages.)
#
# Set this with
#     Base.update_stackframes_callback[] = my_updater!
# where my_updater! takes a single argument and works in-place. The argument will be a
# Vector{Any} storing tuples (sf::StackFrame, nrepetitions::Int), and the updater should
# replace `sf` as needed.
const update_stackframes_callback = Ref{Function}(identity)

const BIG_STACKTRACE_SIZE = 50 # Arbitrary constant chosen here

function show_reduced_backtrace(io::IO, t::Vector)
    recorded_positions = IdDict{UInt, Vector{Int}}()
    #= For each frame of hash h, recorded_positions[h] is the list of indices i
    such that hash(t[i-1]) == h, ie the list of positions in which the
    frame appears just before. =#

    modulecolordict = Dict("" => :default)

    displayed_stackframes = []
    repeated_cycle = Tuple{Int,Int,Int}[]
    # First:  line to introuce the "cycle repetition" message
    # Second: length of the cycle
    # Third:  number of repetitions
    frame_counter = 1
    while frame_counter < length(t)
        (last_frame, n) = t[frame_counter]
        frame_counter += 1 # Indicating the next frame

        current_hash = hash(last_frame)
        positions = get(recorded_positions, current_hash, Int[])
        recorded_positions[current_hash] = push!(positions, frame_counter)

        repetitions = 0
        for index_p in length(positions)-1:-1:1 # More recent is more likely
            p = positions[index_p]
            cycle_length = frame_counter - p
            i = frame_counter
            j = p
            while i < length(t) && t[i] == t[j]
                i += 1
                j += 1
            end
            if j >= frame_counter-1
                #= At least one cycle repeated =#
                repetitions = div(i - frame_counter + 1, cycle_length)
                push!(repeated_cycle, (length(displayed_stackframes), cycle_length, repetitions))
                frame_counter += cycle_length * repetitions - 1
                break
            end
        end

        if repetitions==0
            push!(displayed_stackframes, (last_frame, n))
        end
    end

    try invokelatest(update_stackframes_callback[], displayed_stackframes) catch end

    println(io, "\nStacktrace:")
    ndigits = length(digits(length(t)))

    modulecolorcycler = Iterators.cycle(STACKTRACE_MODULECOLORS)

    push!(repeated_cycle, (0,0,0)) # repeated_cycle is never empty
    frame_counter = 1
    for i in 1:length(displayed_stackframes)
        (frame, n) = displayed_stackframes[i]

        modul = getmodule(frame)
        if !haskey(modulecolordict, modul)
            modulecolordict[modul] = iterate(modulecolorcycler)[1]
        end
        modulecolor = modulecolordict[modul]
        
        print_frame(io, frame_counter, getfunc(frame), getfield(frame, :inlined), getmodule(frame),
            getfile(frame, stacktrace_expand_basepaths(), stacktrace_contract_userdir()),
            getline(frame), getsigtypes(frame), getvarnames(frame),
            ndigits, modulecolor)
        
        if i < length(displayed_stackframes)
            println(io)
            stacktrace_linebreaks() && println(io)
        end

        while repeated_cycle[1][1] == i # never empty because of the initial (0,0,0)
            cycle_length = repeated_cycle[1][2]
            repetitions = repeated_cycle[1][3]
            popfirst!(repeated_cycle)
            printstyled(io,
                "--- the last ", cycle_length, " lines are repeated ",
                  repetitions, " more time", repetitions>1 ? "s" : "", " ---\n", color = :light_black)
            stacktrace_linebreaks() && println(io)
            frame_counter += cycle_length * repetitions
        end
        frame_counter += 1
    end
end

function expandbasepath(str)

    basefileregex = if Sys.iswindows()
        r"^\.\\\w+\.jl$"
    else
        r"^\./\w+\.jl$"
    end

    if !isnothing(match(basefileregex, str))
        sourcestring = find_source_file(str[3:end]) # cut off ./
    else
        str
    end
end

function replaceuserpath(str)
    str1 = replace(str, homedir() => "~")
    # seems to be necessary for some paths with small letter drive c:// etc
    replace(str1, lowercasefirst(homedir()) => "~")
end

getline(frame) = frame.line
function getfile(frame, expandbase::Bool, contractuser::Bool)
    file = string(frame.file)
    if expandbase
        file = expandbasepath(file)
    end
    if contractuser
        file = replaceuserpath(file)
    end

    file
end
getfunc(frame) = string(frame.func)
getmodule(frame) = try; string(frame.linfo.def.module) catch; "" end
getsigtypes(frame) = try;  frame.linfo.specTypes.parameters[2:end] catch; "" end
getmethod(frame) = try;  frame.linfo.def catch; nothing end
function getvarnames(frame)
    m = getmethod(frame)
    if isnothing(m)
        []
    else
        tv, decls, file, line = arg_decl_parts(m)
        # this is a list of tuples (variable name, variable type)
        # we take only the variable names
        first.(decls[2:end])
    end
end

const STACKTRACE_MODULECOLORS = [:light_blue, :light_yellow, :light_red,
        :light_green,:light_magenta, :light_cyan,
        :blue, :yellow, :red, :green, :magenta, :cyan]
stacktrace_expand_basepaths()::Bool = parse(Bool,
    get(ENV, "JULIA_STACKTRACE_EXPAND_BASEPATHS", "true"))
stacktrace_contract_userdir()::Bool = parse(Bool,
    get(ENV, "JULIA_STACKTRACE_CONTRACT_USERDIR", "true"))
stacktrace_linebreaks()::Bool = parse(Bool, get(ENV, "JULIA_STACKTRACE_LINEBREAKS", "true"))

function print_trace(io::IO, trace; print_linebreaks::Bool)

    n = length(trace)
    ndigits = length(digits(n))

    modulecolordict = Dict("" => :default)
    modulecolorcycler = Iterators.cycle(STACKTRACE_MODULECOLORS)

    for (i, frame) in enumerate(trace)

        modul = getmodule(frame)
        if !haskey(modulecolordict, modul)
            modulecolordict[modul] = iterate(modulecolorcycler)[1]
        end
        modulecolor = modulecolordict[modul]

        file = getfile(frame, stacktrace_expand_basepaths(), stacktrace_contract_userdir())
        line = getline(frame)
        varnames = getvarnames(frame)
        func = getfunc(frame)
        sigtypes = getsigtypes(frame)
        inlined = getfield(frame, :inlined)

        push!(LAST_SHOWN_LINE_INFOS, (file, line))
        print_frame(io, i, func, inlined, modul, file, line, sigtypes, varnames, ndigits, modulecolor)
        if i < n
            println(io)
            print_linebreaks && println(io)
        end
    end
end

function print_frame(io, i, func, inlined, modul, file, line, specialization_types,
    variable_names, width_digits, modulecolor)

    # frame number
    print(io, lpad("[" * string(i) * "]", width_digits + 2))
    print(io, " ")
    
    # function name
    printstyled(io, func, bold = true)
   
    if !isempty(variable_names)
        # type signature
        printstyled(io, "(", color = :light_black)

        for (i, (stype, varname)) in enumerate(zip(specialization_types, variable_names))
            if i > 1
                printstyled(io, ", ", color = :light_black)
            end
            printstyled(io, string(varname), color = :light_black, bold = true)
            printstyled(io, "::")
            printstyled(io, string(stype), color = :light_black)
        end

        printstyled(io, ")", color = :light_black)
    end

    println(io)
    
    # @
    printstyled(io, " " ^ (width_digits + 1) * "@ ", color = :light_black)

    # module
    if !isempty(modul)
        printstyled(io, modul, color = modulecolor)
        print(io, " ")
    end

    # filepath
    pathparts = splitpath(file)
    folderparts = pathparts[1:end-1]
    if !isempty(folderparts)
        printstyled(io, joinpath(folderparts...) * (Sys.iswindows() ? "\\" : "/"), color = :light_black)
    end

    # filename, separator, line
    # use escape codes for formatting, printstyled can't do underlined and color
    # codes are bright black (90) and underlined (4)
    print(io, "\033[90;4m$(pathparts[end] * ":" * string(line))\033[0m")

    # inlined
    printstyled(io, inlined ? " [inlined]" : "", color = :light_black)
end


function show_backtrace(io::IO, t::Vector)
    resize!(LAST_SHOWN_LINE_INFOS, 0)
    filtered = process_backtrace(t)
    isempty(filtered) && return

    if length(filtered) == 1 && StackTraces.is_top_level_frame(filtered[1][1])
        f = filtered[1][1]
        if f.line == 0 && f.file == Symbol("")
            # don't show a single top-level frame with no location info
            return
        end
    end

    if length(filtered) > BIG_STACKTRACE_SIZE
        show_reduced_backtrace(IOContext(io, :backtrace => true), filtered)
        return
    end

    println(io, "\nStacktrace:")

    # process_backtrace returns a Tuple{Frame, Int}
    frames = first.(filtered)

    print_trace(io, t; print_linebreaks = stacktrace_linebreaks())
end

# I think this is not needed anymore?

# function show_backtrace(io::IO, t::Vector{Any})
#     # t is a pre-processed backtrace (ref #12856)
#     if length(t) < BIG_STACKTRACE_SIZE
#         try invokelatest(update_stackframes_callback[], t) catch end
#         for entry in t
#             show_trace_entry(io, entry...)
#         end
#     else
#         show_reduced_backtrace(io, t, false)
#     end
# end

function is_kw_sorter_name(name::Symbol)
    sn = string(name)
    return !startswith(sn, '#') && endswith(sn, "##kw")
end

# For improved user experience, filter out frames for include() implementation
# - see #33065. See also #35371 for extended discussion of internal frames.
function _simplify_include_frames(trace)
    i = length(trace)
    kept_frames = trues(i)
    first_ignored = nothing
    while i >= 1
        frame, _ = trace[i]
        mod = parentmodule(frame)
        if isnothing(first_ignored)
            if mod === Base && frame.func === :_include
                # Hide include() machinery by default
                first_ignored = i
            end
        else
            # Hack: allow `mod==nothing` as a workaround for inlined functions.
            # TODO: Fix this by improving debug info.
            if mod in (Base,Core,nothing) && 1+first_ignored-i <= 5
                if frame.func == :eval
                    kept_frames[i:first_ignored] .= false
                    first_ignored = nothing
                end
            else
                # Bail out to avoid hiding frames in unexpected circumstances
                first_ignored = nothing
            end
        end
        i -= 1
    end
    if !isnothing(first_ignored)
        kept_frames[i:first_ignored] .= false
    end
    return trace[kept_frames]
end

function process_backtrace(t::Vector, limit::Int=typemax(Int); skipC = true)
    n = 0
    last_frame = StackTraces.UNKNOWN
    count = 0
    ret = Any[]
    for i in eachindex(t)
        lkups = t[i]
        if lkups isa StackFrame
            lkups = [lkups]
        else
            lkups = StackTraces.lookup(lkups)
        end
        for lkup in lkups
            if lkup === StackTraces.UNKNOWN
                continue
            end

            if (lkup.from_c && skipC) || is_kw_sorter_name(lkup.func)
                continue
            end
            count += 1
            if count > limit
                break
            end

            if lkup.file != last_frame.file || lkup.line != last_frame.line || lkup.func != last_frame.func || lkup.linfo !== lkup.linfo
                if n > 0
                    push!(ret, (last_frame, n))
                end
                n = 1
                last_frame = lkup
            else
                n += 1
            end
        end
        count > limit && break
    end
    if n > 0
        push!(ret, (last_frame, n))
    end
    return _simplify_include_frames(ret)
end

function show_exception_stack(io::IO, stack::Vector)
    # Display exception stack with the top of the stack first.  This ordering
    # means that the user doesn't have to scroll up in the REPL to discover the
    # root cause.
    nexc = length(stack)
    for i = nexc:-1:1
        if nexc != i
            printstyled(io, "caused by [exception ", i, "]\n", color=:light_black)
        end
        exc, bt = stack[i]
        showerror(io, exc, bt, backtrace = bt!==nothing)
        println(io)
    end
end

# Defined here rather than error.jl for bootstrap ordering
function show(io::IO, ip::InterpreterIP)
    print(io, typeof(ip))
    if ip.code isa Core.CodeInfo
        print(io, " in top-level CodeInfo for $(ip.mod) at statement $(Int(ip.stmt))")
    else
        print(io, " in $(ip.code) at statement $(Int(ip.stmt))")
    end
end
