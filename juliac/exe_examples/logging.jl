#!/usr/bin/env -S julia --project=@scriptdir

module Main2

Base.@ccallable function main() :: Cint
    @info "test message with $("interpolation")" hello="world"
    return 0
end

end
