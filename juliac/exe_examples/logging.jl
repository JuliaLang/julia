#!/usr/bin/env -S julia --project=@scriptdir

module Main2

# TODO: This ends up loading before Base, breaking loading
# using StyledStrings # Load full `print()` functionality for StyledStrings

Base.@ccallable function main() :: Cint
    @error "test message with $("interpolation")" hello="world"
    # @info  styled"and even {cyan:styled} strings!"
    return 0
end

end
