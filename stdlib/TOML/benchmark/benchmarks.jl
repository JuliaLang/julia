using BenchmarkTools
import TOML
import Random
import StableRNGs

randstring(args...) = Random.randstring(StableRNGs.StableRNG(0), args...)
rand(args...) = Random.rand(StableRNGs.StableRNG(0), args...)

const SUITE = BenchmarkGroup()


##########
# Arrays #
##########

SUITE["arrays"] = BenchmarkGroup()

const homogeneous_array = "foo = [" * join(ones(Int, 10^5), ',') * "]"
SUITE["arrays"]["homogeneous"] = @benchmarkable TOML.parse(homogeneous_array)

const heterogeneous_array = "foo = [1.0," * join(ones(Int, 10^5), ',') * "]"
SUITE["arrays"]["heterogeneous"] = @benchmarkable TOML.parse(heterogeneous_array)


###########
# Strings #
###########

SUITE["strings"] = BenchmarkGroup()

const long_string = "foo = " * repr(randstring(10^6))
SUITE["strings"]["long"] = @benchmarkable TOML.parse(long_string)

const short_strings = "foo = [" * join([repr(randstring(3)) for i in 1:10^4], ',') * "]"
SUITE["strings"]["short"] = @benchmarkable TOML.parse(short_strings)


###########
# Numbers #
###########

SUITE["numbers"] = BenchmarkGroup()

const integer_array = "foo = [" * join(rand(Int, 10^5), ',') * "]"
SUITE["numbers"]["integers"] = @benchmarkable TOML.parse(integer_array)

const float_array = "foo = [" * join(rand(10^5), ',') * "]"
SUITE["numbers"]["floats"] = @benchmarkable TOML.parse(float_array)


###################
# Array of Tables #
###################

SUITE["array of tables"] = BenchmarkGroup()

const many_empty_array_of_tables =
    repeat("[[foo]] \n", 10^4)
SUITE["array of tables"]["empty"] = @benchmarkable TOML.parse(many_empty_array_of_tables)


#################
# Registry file #
#################

SUITE["registry"] = BenchmarkGroup()

const registry_toml = read(joinpath(@__DIR__, "files", "Registry.toml"), String)
SUITE["registry"]["Registry.toml"] = @benchmarkable TOML.parse(registry_toml)

const compat_toml = read(joinpath(@__DIR__, "files", "Compat.toml"), String)
SUITE["registry"]["Compat.toml"] = @benchmarkable TOML.parse(compat_toml)


############
# Overhead #
############

SUITE["parse empty"] = @benchmarkable TOML.parse("")
