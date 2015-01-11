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

function writemime(io::IO, ::MIME"text/plain", v::AbstractVector)
    if isa(v, Range)
        show(io, v)
    else
        print(io, summary(v))
        if !isempty(v)
            println(io, ":")
            with_output_limit(()->print_matrix(io, v))
        end
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
writemime(io::IO, ::MIME"text/plain", t::Union(KeyIterator, ValueIterator)) =
    showkv(io, t, limit=true)


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

function showerror(io::IO, ex, bt)
    try
        showerror(io, ex)
    finally
        show_backtrace(io, bt)
    end
end

function showerror(io::IO, ex::LoadError, bt)
    print(io, "LoadError: ")
    showerror(io, ex.error, bt)
    print(io, "\nwhile loading $(ex.file), in expression starting on line $(ex.line)")
end
showerror(io::IO, ex::LoadError) = showerror(io, ex, [])

function showerror(io::IO, ex::DomainError, bt)
    println(io, "DomainError:")
    for b in bt
        code = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Cint), b, true)
        if length(code) == 5 && !code[4]  # code[4] == fromC
            if code[1] in (:log, :log2, :log10, :sqrt) # TODO add :besselj, :besseli, :bessely, :besselk
                println(io,"$(code[1]) will only return a complex result if called with a complex argument.")
                print(io, "try $(code[1]) (complex(x))")
            elseif (code[1] == :^ && code[2] == symbol("intfuncs.jl")) || code[1] == :power_by_squaring
                println(io, "Cannot raise an integer x to a negative power -n. Make x a float by adding")
                print(io, "a zero decimal (e.g. 2.0^-n instead of 2^-n), or write 1/x^n, float(x)^-n, or (x//1)^-n")
            end
            break
        end
    end
    show_backtrace(io, bt)
end

showerror(io::IO, ex::SystemError) = print(io, "SystemError: $(ex.prefix): $(strerror(ex.errnum))")
showerror(io::IO, ::DivideError) = print(io, "DivideError: integer division error")
showerror(io::IO, ::StackOverflowError) = print(io, "StackOverflowError:")
showerror(io::IO, ::UndefRefError) = print(io, "UndefRefError: access to undefined reference")
showerror(io::IO, ex::UndefVarError) = print(io, "UndefVarError: $(ex.var) not defined")
showerror(io::IO, ::EOFError) = print(io, "EOFError: read end of file")
showerror(io::IO, ex::ErrorException) = print(io, ex.msg)
showerror(io::IO, ex::KeyError) = print(io, "KeyError: $(ex.key) not found")
showerror(io::IO, ex::InterruptException) = print(io, "InterruptException:")

function showerror(io::IO, ex::MethodError)
    print(io, "MethodError: ")
    name = isgeneric(ex.f) ? ex.f.env.name : :anonymous
    if isa(ex.f, DataType)
        print(io, "`$(ex.f)` has no method matching $(ex.f)(")
    else
        print(io, "`$(name)` has no method matching $(name)(")
    end
    for (i, arg) in enumerate(ex.args)
        if isa(arg, Type) && arg != typeof(arg)
            print(io, "::Type{$(arg)}")
        else
            print(io, "::$(typeof(arg))")
        end
        i == length(ex.args) || print(io, ", ")
    end
    print(io, ")")
    # Check for local functions that shaddow methods in Base
    if isdefined(Base, name)
        f = eval(Base, name)
        if f !== ex.f && isgeneric(f) && applicable(f, ex.args...)
            println(io)
            print(io, "you may have intended to import Base.$(name)")
        end
    end
    # Check for row vectors used where a column vector is intended.
    vec_args = []
    hasrows = false
    for arg in ex.args
        isrow = isa(arg,Array) && ndims(arg)==2 && size(arg,1)==1
        hasrows |= isrow
        push!(vec_args, isrow ? vec(arg) : arg)
    end
    if hasrows && applicable(ex.f, vec_args...)
        print(io, "\n\nYou might have used a 2d row vector where a 1d column vector was required.")
        print(io, "\nNote the difference between 1d column vector [1,2,3] and 2d row vector [1 2 3].")
        print(io, "\nYou can convert to a column vector with the vec() function.")
    end
    # Give a helpful error message if the user likely called a type constructor
    # and sees a no method error for convert
    if ex.f == Base.convert && !isempty(ex.args) && isa(ex.args[1], Type)
        println(io)
        print(io, "This may have arisen from a call to the constructor $(ex.args[1])(...), ")
        print(io, "since type constructors fall back to convert methods in julia v0.4.")
    end

    # Display up to three closest candidates
    lines = Array((IOBuffer, Int), 0)
    for method in methods(ex.f)
        n = length(ex.args)
        if n != length(method.sig)
            continue
        end
        buf = IOBuffer()
        print(buf, "  $(ex.f.env.name)(")
        first = true
        right_matches = 0
        for (arg, sigtype) in Zip2{Any,Any}(ex.args, method.sig)
            if first
                first = false
            else
                print(buf, ", ")
            end
            if typeof(arg) <: sigtype
                right_matches += 1
                print(buf, "::$(sigtype)")
            else
                Base.with_output_color(:red, buf) do buf
                    print(buf, "::$(sigtype)")
                end
            end
        end
        if right_matches > 0
            print(buf, ")")
            push!(lines, (buf, right_matches))
        end
    end
    if length(lines) != 0
        Base.with_output_color(:normal, io) do io
            println(io, "\nClosest candidates are:")
            sort!(lines, by = x -> -x[2])
            i = 0
            for line in lines
                if i >= 3
                    println(io, "  ...")
                    break
                end
                i += 1
                println(io, takebuf_string(line[1]))
            end
        end
    end
end

function show_trace_entry(io, fname, file, line, n)
    print(io, "\n")
    print(io, " in ", fname, " at ", file)
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
end

function show_backtrace(io::IO, t, set=1:typemax(Int))
    # we may not declare :eval_user_input
    # directly so that we get a compile error
    # in case its name changes in the future
    show_backtrace(io, try
                         symbol(string(eval_user_input))
                       catch
                         :(:) #for when client.jl is not yet defined
                       end, t, set)
end

# show the backtrace, up to (but not including) top_function
function show_backtrace(io::IO, top_function::Symbol, t, set)
    n = 1
    lastfile = ""; lastline = -11; lastname = symbol("#")
    local fname, file, line
    count = 0
    for i = 1:length(t)
        lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Cint), t[i], true)
        if lkup === ()
            continue
        end
        fname, file, line, fromC = lkup
        if fromC; continue; end
        if i == 1 && fname == :error; continue; end
        if fname == top_function; break; end
        count += 1
        if !in(count, set); continue; end
        if file != lastfile || line != lastline || fname != lastname
            if lastline != -11
                show_trace_entry(io, lastname, lastfile, lastline, n)
            end
            n = 1
            lastfile = file; lastline = line; lastname = fname
        else
            n += 1
        end
    end
    if n > 1 || lastline != -11
        show_trace_entry(io, lastname, lastfile, lastline, n)
    end
end
