# The messages used by the framework for internal
# communication. Plugins writers are encouraged to add new types to
# this hierarchy, in their plugins. For example, see plugins/fails.jl
abstract TestMessage    <: Exception

abstract FailedSomehow  <: TestMessage
type ThrewException     <: FailedSomehow
    it::Exception
end

# TODO: Give it a payload for better reporting
type FailedAssertion    <: FailedSomehow end

abstract PassedSomehow  <: TestMessage
type Passed             <: PassedSomehow end

abstract FrameworkError <: TestMessage

type NotImplemented     <: FrameworkError
    it
end

# TODO: give it a payload, for better reporting
type FailedCheckPoint   <: TestMessage end

macro check (expr)
    quote
        if !$expr
            throw(FailedCheckPoint())
        end
    end
end

# It might be sensible to compare the contents of the internal
# exception too, but I don't see an general way of doing this.
     ==(l::ThrewException, r::ThrewException) = typeof(l) == typeof(r)
isequal(l::ThrewException, r::ThrewException) =        l  ==        r

include("scopes.jl")
include("parse.jl")

# This file defines hard-wired core of the framework. Anything that
# that can be (and is) implemented as a plugin, appears elsewhere.


# This macro is used to define tests and all their metadata
macro test(expressions...)
    block = canonicalize_input(expressions)
    existing_scope  = nothing
    parsed_tests    = {}
    parse(block, existing_scope, parsed_tests)
    # What the framework does with the test specification is not
    # hard-wired. Clients are free to define custom ways of dealing
    # with the tests, and installing them by binding them to
    # `test_rurrer`. (Think about improving this name. Do we need
    # separate hooks for the three possibly separate phases of
    # collecting, running and reporting.)
    test_runner(parsed_tests)
    # For now, don't generate any code.
    nothing
end

function construct_test(spec)
    core = quote
        try
            if $(spec[:code]) == true
                Passed()
            else
                FailedAssertion()
            end
        catch exception
            isa(exception, FailedCheckPoint) ? FailedAssertion() : ThrewException(exception)
        end
    end
    spec[:combined_code] = core
    for entry in _construct_test_plugins
        transform = entry.plugin
        transform(spec)
    end
end

# Will this have to be made pluggable, or is a hard-wired one good
# enough?
function run_test(spec)
    construct_test(spec)
    spec[:result] = eval(spec[:combined_code])
end

available_test_runners = Dict{Symbol, Function}()
load("runners/simple.jl")

function run_tests(filename, runner::Symbol)
    # The 'runner' is, in principle, a triumverae of collector,
    # runner, and reporter. But we'll ignore that for the time being
    # and just get something simple to work
    if has(available_test_runners, runner)
        global test_runner = available_test_runners[runner]
        load(filename)
    else
        error("YouTest: Test runner '$(runner)' is not registered.")
    end
end

################## Plugin support ###################

# Support for transforming <symbol> to <symbol> := <value>
default = Dict{Symbol, Any}()

# Plugging in to the construction of the expression that will be
# evaluated when the test is run. (Will probably need to include
# priorities or levels)
_construct_test_plugins = {}

type PluginListEntry
    name
    level  :: Real
    plugin :: Function
end

# ToDo
## macro construct_test_plugin(level, name, body)
##     :(add_construct_test_plugin($name, $level, function(spec) $body end))
## end

function add_construct_test_plugin(name, level::Real, plugin::Function)
    pos = findfirst((entry->entry.level > level), _construct_test_plugins)
    if pos == 0   pos=length(_construct_test_plugins) + 1  end
    insert(_construct_test_plugins, pos, PluginListEntry(name, level, plugin))
end

function show_construct_test_plugin_levels()
    for p in _construct_test_plugins
        # How the hell do you align floats on the point?
        println("$(lpad(p.level,3,' ')) $(p.name)")
    end
end

# What to do if the same metadatum name is set more than once in the
# same scope, at the same level. The possibilities are:
#
# Combine them: specify the varargs callable which 
# Error              :error
# The last one wins: :overwrite



# ToDo: Multiple bindings at same level are combined rather than
# overriding
accumulates = Set{Symbol}()

############## The standard plugins ####################

include("plugins/fails.jl")
include("plugins/skip.jl")
include("plugins/throws.jl")
include("plugins/fixtures.jl")
include("plugins/parametrize.jl")
include("plugins/quickcheck.jl")
