# retain this module for backwards compatibility with packages that use
# it for tests.
module Furlongs
import Test
export Furlong
const Furlong = Test.GenericDimensionful
# change to deprecation once Statistics stdlib is updated:
# Base.@deprecate_binding Furlong Test.GenericDimensionful
end
