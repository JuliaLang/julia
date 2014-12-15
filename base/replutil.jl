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

showerror(io::IO, e) = show(io, e)

function showerror(io::IO, e::TypeError)
    ctx = isempty(e.context) ? "" : "in $(e.context), "
    if e.expected === Bool
        print(io, "type: non-boolean ($(typeof(e.got))) ",
                  "used in boolean context")
    else
        if isa(e.got, Type)
            tstr = "Type{$(e.got)}"
        else
            tstr = string(typeof(e.got))
        end
        print(io, "type: $(e.func): ",
                  "$(ctx)expected $(e.expected), ",
                  "got $tstr")
        if e.func === :apply && e.expected <: Function && isa(e.got, AbstractArray)
            println(io)
            print(io, "Use square brackets [] for indexing.")
        end
    end
end

function showerror(io::IO, e, bt)
    try
        showerror(io, e)
    finally
        show_backtrace(io, bt)
    end
end

showerror(io::IO, e::LoadError) = showerror(io, e, [])
function showerror(io::IO, e::LoadError, bt)
    showerror(io, e.error, bt)
    print(io, "\nwhile loading $(e.file), in expression starting on line $(e.line)")
end

function showerror(io::IO, e::DomainError, bt)
    print(io, "DomainError")
    for b in bt
        code = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Cint), b, true)
        if length(code) == 5 && !code[4]  # code[4] == fromC
            if code[1] in (:log, :log2, :log10, :sqrt) # TODO add :besselj, :besseli, :bessely, :besselk
                print(io, "\n", code[1],
                      " will only return a complex result if called with a complex argument.",
                      "\ntry ", code[1], "(complex(x))")
            elseif (code[1] == :^ && code[2] == symbol("intfuncs.jl")) ||
                   code[1] == :power_by_squaring
                print(io, "\nCannot raise an integer x to a negative power -n. Make x a float by adding")
                print(io, "\na zero decimal (e.g. 2.0^-n instead of 2^-n), or write 1/x^n, float(x)^-n, or (x//1)^-n")
            end
            break
        end
    end
    show_backtrace(io, bt)
end

showerror(io::IO, e::SystemError) = print(io, "$(e.prefix): $(strerror(e.errnum))")
showerror(io::IO, ::DivideError) = print(io, "integer division error")
showerror(io::IO, ::StackOverflowError) = print(io, "stack overflow")
showerror(io::IO, ::UndefRefError) = print(io, "access to undefined reference")
showerror(io::IO, e::UndefVarError) = print(io, e.var, " not defined")
showerror(io::IO, ::EOFError) = print(io, "read: end of file")
showerror(io::IO, e::ErrorException) = print(io, e.msg)
showerror(io::IO, e::KeyError) = (print(io, "key not found: "); show(io, e.key))
showerror(io::IO, e::InterruptException) = print(io, "interrupt")

function showerror(io::IO, e::MethodError)
    name = isgeneric(e.f) ? e.f.env.name : :anonymous
    if isa(e.f, DataType)
        print(io, "`$(e.f)` has no method matching $(e.f)(")
    else
        print(io, "`$(name)` has no method matching $(name)(")
    end
    for (i, arg) in enumerate(e.args)
        if isa(arg, Type) && arg != typeof(arg)
            print(io, "::Type{$(arg)}")
        else
            print(io, "::$(typeof(arg))")
        end
        i == length(e.args) || print(io, ", ")
    end
    print(io, ")")
    # Check for local functions that shaddow methods in Base
    if isdefined(Base, name)
        f = eval(Base, name)
        if f !== e.f && isgeneric(f) && applicable(f, e.args...)
            println(io)
            print(io, "you may have intended to import Base.$(name)")
        end
    end
    # Check for row vectors used where a column vector is intended.
    vec_args = []
    hasrows = false
    for arg in e.args
        isrow = isa(arg,Array) && ndims(arg)==2 && size(arg,1)==1
        hasrows |= isrow
        push!(vec_args, isrow ? vec(arg) : arg)
    end
    if hasrows && applicable(e.f, vec_args...)
        print(io, "\n\nYou might have used a 2d row vector where a 1d column vector was required.")
        print(io, "\nNote the difference between 1d column vector [1,2,3] and 2d row vector [1 2 3].")
        print(io, "\nYou can convert to a column vector with the vec() function.")
    end

    # Display up to three closest candidates
    lines = Array((IOBuffer, Int), 0)
    for method in methods(e.f)
        n = length(e.args)
        if n != length(method.sig)
            continue
        end
        buf = IOBuffer()
        print(buf, "  $(e.f.env.name)(")
        first = true
        right_matches = 0
        for (arg, sigtype) in Zip2{Any,Any}(e.args, method.sig)
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
