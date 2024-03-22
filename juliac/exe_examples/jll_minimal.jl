#!/usr/bin/env -S julia --project=@scriptdir

module Main2

using Libdl
using OpenSpecFun_jll
Base.@ccallable function main()::Cint
    println(Core.stdout,"Hello, world!")
    a = rand(10)
    openspecfun
    println(Core.stdout, dlsym(libopenspecfun, :zairy_))
    return 0
end

end
