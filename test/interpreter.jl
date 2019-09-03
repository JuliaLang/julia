# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# interpreted but inferred/optimized top-level expressions with vars
let code = """
           while true
               try
                   this_is_undefined_29213
                   ed = 0
                   break
               finally
                   break
               end
           end
           print(42)
           """
    @test read(`$(Base.julia_cmd()) --startup-file=no --compile=min -e $code`, String) == "42"
end

let code = "Threads.atomic_add!(Threads.Atomic{Int}(40), 2)"
    @test read(`$(Base.julia_cmd()) --startup-file=no --compile=min -E $code`, String) == "40\n"
end

let p = Pipe(),
    c = pipeline(`$(Base.julia_cmd()) --startup-file=no --compile=min -E 'error()'`, stderr=p)
    proc = run(c, wait=false)
    readline(p)
    @test readline(p) == "Stacktrace:"
    wait(proc)
    close(p)
end
