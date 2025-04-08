module HasDepWithSubmodules

using HasSubmodules
@submodule_using HasSubmodules.Submodule1

g(x) = f(x) - 1

export g

end
