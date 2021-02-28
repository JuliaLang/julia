# retain this module for backwards compatibility with packages that use
# it for tests.
module Furlongs
export Furlong
import Test
Base.@deprecate_binding Furlong Test.GenericDimensionful
end
