#!/usr/bin/env -S julia --project=@scriptdir

module Main2

Base.@ccallable function main() :: Cint

    # LazyString is currently uninferable, due to abstractly-typed fields.

    static_lazystr = lazy"Testing!"
    # println(Core.stdout, static_lazystr) # broken

    dynamic_lazystr = lazy"Testing! $(rand(UInt))"
    # println(Core.stdout, dynamic_lazystr) # broken

    return 0
end

end
