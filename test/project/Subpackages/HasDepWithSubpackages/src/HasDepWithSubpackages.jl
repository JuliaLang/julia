module HasDepWithSubpackages

using HasSubpackages
@subpackage_using HasSubpackages.Subpackage1

g(x) = f(x) - 1

export g

end
