# fallback text/plain representation of any type:
writemime(io, ::MIME"text/plain", x) = show(io, x)

function writemime(io, ::MIME"text/plain", v::Function)
    if isgeneric(v)
        show_method_table(io, methods(v), 5)
    else
        show(io, v)
    end
end

function writemime(io, ::MIME"text/plain", v::AbstractVector)
    if isa(v, Ranges)
        show(io, v)
    else
        print(io, summary(v))
        if !isempty(v)
            println(io, ":")
            print_matrix(io, v)
        end
    end
end

function writemime(io, ::MIME"text/plain", v::DataType)
    show(io, v)
    methods(v)  # force constructor creation
    if isgeneric(v)
        if v === v.name.primary
            name = string(v.name.name)
        else
            name = repr(v)
        end
        print(io, "  (use methods($name) to see constructors)")
    end
end

# showing exception objects as descriptive error messages

show_error(io::IO, e) = show(io, e)

function show_error(io::IO, e::TypeError)
    ctx = isempty(e.context) ? "" : "in $(e.context), "
    if e.expected === Bool
        print(io, "type: non-boolean ($(typeof(e.got))) ",
                  "used in boolean context")
    else
        if isa(e.got,Type)
            tstr = "Type{$(e.got)}"
        else
            tstr = string(typeof(e.got))
        end
        print(io, "type: $(e.func): ",
                  "$(ctx)expected $(e.expected), ",
                  "got $tstr")
    end
end

function show_error(io::IO, e, bt)
    try
        show_error(io, e)
    finally 
        show_backtrace(io, bt)
    end
end

show_error(io::IO, e::LoadError) = show_error(io, e, {})
function show_error(io::IO, e::LoadError, bt)
    show_error(io, e.error, bt)
    print(io, "\nat $(e.file):$(e.line)")
end

show_error(io::IO, e::SystemError) = print(io, "$(e.prefix): $(strerror(e.errnum))")
show_error(io::IO, ::DivideError) = print(io, "integer division error")
show_error(io::IO, ::StackOverflowError) = print(io, "stack overflow")
show_error(io::IO, ::UndefRefError) = print(io, "access to undefined reference")
show_error(io::IO, ::EOFError) = print(io, "read: end of file")
show_error(io::IO, e::ErrorException) = print(io, e.msg)
show_error(io::IO, e::KeyError) = print(io, "key not found: $(e.key)")
show_error(io::IO, e::InterruptException) = print(io, "interrupt")

function show_error(io::IO, e::MethodError)
    name = e.f.env.name
    if is(e.f,convert) && length(e.args)==2
        print(io, "no method $(name)(Type{$(e.args[1])},$(typeof(e.args[2])))")
    else
        print(io, "no method $(name)$(typeof(e.args))")
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
        lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Int32), t[i], 0)
        if lkup === ()
            continue
        end
        fname, file, line = lkup
        if i == 1 && fname == :error; continue; end
        if fname == top_function; break; end
        count += 1
        if !contains(set, count); continue; end
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
    @windows_only if WORD_SIZE == 64 warn_once("\nbacktraces on your platform are often misleading or partially incorrect") end
end
