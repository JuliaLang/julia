#!/usr/bin/env -S julia --project=@scriptdir

module Main2

Base.@ccallable function main() :: Cint
    println(Core.stdout, "Hello, world!")
    return 0
end

end
