module Subpackage2

using ADep
using HasSubpackages

bar(x) = foo(x) + 1

export bar

function __init__()
    HasSubpackages.sub2_loaded = true
end

end
