# shell

ls() = system("ls")

catfile(file::String) = system(strcat("cat ", file))

# timing

clock() = ccall(:clock_now, Float64, ())

function tic()
    t0 = clock()
    tls(:TIMERS, (t0, get(tls(), :TIMERS, ())))
    return t0
end

function toc(noisy::Bool)
    t1 = clock()
    timers = get(tls(), :TIMERS, ())
    if is(timers,())
        error("toc() without tic()")
    end
    t0 = timers[1]
    tls(:TIMERS, timers[2])
    t = t1-t0
    if noisy
        println("elapsed time: ", t, " seconds")
    end
    t
end

toc() = toc(true)
toq() = toc(false)

# prints elapsed time
# returns expression value
macro time(ex)
  t0, val, t1 = gensym(3)
  quote
    local $t0 = clock()
    local $val = $ex
    local $t1 = clock()
    println("elapsed time: ", $t1-$t0, " seconds")
    $val
  end
end

# prints nothing
# returns elapsed time
macro elapsed(ex)
  t0, val, t1 = gensym(3)
  quote
    local $t0 = clock()
    local $val = $ex
    local $t1 = clock()
    $t1-$t0
  end
end

function peakflops()
    a = rand(2000,2000)
    t = @elapsed a*a
    t = @elapsed a*a
    floprate = (2*2000.0^3/t)
    println("The peak flop rate is ", floprate*1e-9, " gigaflops")
    floprate
end

macro benchmark(n,ex,T)
    s = gensym()
    quote
        local $s
        @time for i=1:long($n)
            x = convert($T,i)
            $s = $ex
        end
        $s
    end
end

# source files, editing

edit(fl::String) = edit(fl, 1)
function edit(fl::String, line::Int)
    issrc = fl[end-1:end] == ".j"
    if issrc
        jmode = "$JULIA_HOME/contrib/julia-mode.el"
        run(`emacs $fl --eval "(progn
                                 (require 'julia-mode \"$jmode\")
                                 (julia-mode)
                                 (goto-line $line))"`)
        load(fl)
    else
        run(`emacs $fl --eval "(goto-line $line)"`)
    end
end

function_loc(f::Function) = function_loc(f, (Any...))

function function_loc(f::Function, types)
    m = getmethods(f, types)
    while !is(m,())
        if isa(m[3],LambdaStaticData)
            ast = m[3].ast
            ln = ast.args[3].args[1]
            if isa(ln,Expr) && is(ln.head,:line)
                return (string(ln.args[2]), ln.args[1])
            end
        end
        m = m[5]
    end
    error("could not find function definition")
end

edit(f::Function) = edit(function_loc(f)...)
edit(f::Function, t) = edit(function_loc(f,t)...)
