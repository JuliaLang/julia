module Subpackage1
using HasSubpackages

function __init__()
    HasSubpackages.sub1_loaded = true
end

f(x) = x + 1

export f

end
