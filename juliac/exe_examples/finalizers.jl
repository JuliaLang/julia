#!/usr/bin/env -S julia --project=@scriptdir

module Main2

mutable struct Foo end

mutable struct Bar end

@noinline function cleanup!(x::Foo)
    println(Core.stdout, "Ran finalizer (2) on: $(objectid(x))")
    return nothing
end

@noinline function cleanup!(x::Bar)
    println(Core.stdout, "Ran finalizer (3) on: $(objectid(x))")
    return nothing
end

@noinline function do_work()
    # Basic finalizers with fully concrete signatures work:
    x = Foo()
    finalizer(x) do o
        println(Core.stdout, "Ran finalizer (1) on: $(objectid(o))")
    end

    # Out-of-line is fine too
    y = Foo()
    finalizer(cleanup!, y)

    # This one can't be supported (yet) since the dispatch it will
    # eventually trigger is unknown at compile-time:
    z = rand(Bool) ? Foo() : Bar() 
    # finalizer(cleanup!, z) # broken


    # TODO: support `atexit()` + add tests
end

Base.@ccallable function main() :: Cint
    do_work()
    GC.gc()
    return 0
end

end
