module Submodule1
using HasSubmodules

function __init__()
    HasSubmodules.sub1_loaded = true
end

f(x) = x + 1

export f

end
