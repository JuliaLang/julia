# These tests exist to assist with the original development of the
# framework. In its very early stages the framework is not uesable to
# test itself.

# documenting_tests.jl contains prettier and more informative tests
# which aim to document the features available in the framework.

# The core of the framework
include("test.jl")

# The framework caters for the possibility of collecting, running and
# reporting the tests in different ways, by plugging in different
# (potentially client-defined) functions for performing these tasks.

# To test the framework itself, we use a collector which immediately
# stores the parsed test in the global variable 'latest_tests', and
# immediately runs those tests. This allows us to use arbitrary code
# to check the behaviour of the parser, collector and runner and
# plugins.

include("runners/development_testing.jl")

# Make the framework use our test collector
test_runner = development_testing_runner

# A single test, consisting of a single-line expression
@test true
# Only one test was specified. It passes.
check_test_results(Passed)

# A number of tests, each consisting of a single-line expression
@test true false 1==1 1==2
# Four tests: two pass, two fail
check_test_results(Passed, FailedAssertion, Passed, FailedAssertion)

# A single single-line test, written in a block
@test begin
    1==1
end
# Only one test was specified. It passes.
check_test_results(Passed)

# TODO: make linemode the default: find a good keyword for the opposite
# Four single-line tests, wirtten in a block
@test begin
    true
    false
    1 == 1
    1 == 2
end
# Four tests: two pass, two fail
check_test_results(Passed, FailedAssertion, Passed, FailedAssertion)

# A SINGLE multiline test
@test begin
    single
    true
    false
    1 == 1
    1 == 2 # This is the assertion taken into account
end
# Only one test was specified. It fails.
check_test_results(FailedAssertion)


# Test failure because of exeption being thrown
@test {1=>2}[2] ""+3
check_test_results(FailedSomehow,  FailedSomehow)
check_test_results(ThrewException, ThrewException)
@assert     latest_tests[1][:result] == ThrewException(KeyError(2))
@assert isa(latest_tests[2][:result].it, MethodError)

################################################################################
################################################################################
#     The 'fails' directive states that all tests in this group should fail
################################################################################
################################################################################

# Fails applied in outer line mode
@test fails true false 1==1 1==2
# Four tests: two pass, two fail; with reversed expectations
check_test_results(FailedSomehow,      PassedSomehow,    FailedSomehow,      PassedSomehow)
check_test_results(UnexpectedlyPassed, ExpectedlyFailed, UnexpectedlyPassed, ExpectedlyFailed)


# Fails applied to linemode in a block
@test begin
    fails
    true
    false
    1==1
    1==2
end
# Four tests: two pass, two fail; with reversed expectations
check_test_results(FailedSomehow,      PassedSomehow,    FailedSomehow,      PassedSomehow)
check_test_results(UnexpectedlyPassed, ExpectedlyFailed, UnexpectedlyPassed, ExpectedlyFailed)

# Fails applied to a block
@test begin
    single
    fails
    true
    false
    1==1
    1==2
end
# Block combines all lines into a single test. Fail as expected
check_test_results(ExpectedlyFailed)

# Conditionally fails
new_frobnicator() = my_operating_system == :Windows ? false : true
my_operating_system = :Windows
@test new_frobnicator()   fails := my_operating_system == :Windows
check_test_results(ExpectedlyFailed)
my_operating_system = :Linux
@test new_frobnicator()   fails := my_operating_system == :Windows
check_test_results(Passed)


################################################################################
################################################################################
#     The 'skip' directive states that this group should not be run
################################################################################
################################################################################

# Skip applied in outer line mode
@test skip true false 1==1 1==2
# Four tests: All pass because skipped
check_test_results(PassedSomehow, PassedSomehow, PassedSomehow, PassedSomehow)
check_test_results(Skipped,       Skipped,       Skipped,       Skipped)

# Skip applied to linemode in a block
@test begin
    skip
    true
    false
    1==1
    1==2
end
# Four tests: all skipped
check_test_results(Skipped, Skipped, Skipped, Skipped)

# Skip applied to a block
@test begin
    skip
    single
    true
    false
    1==1
    1==2
end
# Just one test, skipped
check_test_results(Skipped)


# Conditionally fails
new_frobnicator() = my_operating_system == :Windows ? false : true
my_operating_system = :Windows
@test new_frobnicator()   skip := my_operating_system == :Windows
check_test_results(Skipped)
my_operating_system = :Linux
@test new_frobnicator()   skip := my_operating_system == :Windows
check_test_results(Passed)


# Skip applied to nested blocks
@test begin
    skip
    begin
        single
        a = 1
        a == a
    end
    begin
        single
        b = 1
        b == b
    end
end
# Four tests: two pass, two fail; with reversed expectations
check_test_results(Skipped, Skipped)


################################################################################
################################################################################
#     The 'throws' directive marks the test as failing unless an
#     exception of the given type is thrown
################################################################################
################################################################################

# An exception of any type must be thrown
@test throws {1=>2}[2]
check_test_results(Passed)

# Not throwing anything is a failure
@test throws {1=>2}[1]
check_test_results(FailedSomehow)
check_test_results(CorrectExceptionNotThrown)

# An exception of type KeyError must be thrown
@test throws := KeyError {1=>2}[2]
check_test_results(Passed)

@test throws := MethodError {1=>2}[2]
check_test_results(FailedSomehow)
check_test_results(CorrectExceptionNotThrown)

# Invented by fails
@test fails throws := MethodError {1=>2}[2]
check_test_results(PassedSomehow)
check_test_results(ExpectedlyFailed)

# Overriden by skip
@test skip throws := MethodError {1=>2}[1]
check_test_results(Skipped)

@test begin
    throws := KeyError; {1=>2}[2]
    div(1,0); throws := DivideByZeroError
    throws := ErrorException;  a_name_that_will_not_exist_929165245
    throws; error("hmm")
end
check_test_results(Passed, Passed, Passed, Passed)

@test begin
    {2=>1}[2] == 1
    {1=>2}[:a] = 2; throws := MethodError
    {1=>2}[2];      throws := KeyError
    {2=>2}[2] == 2
end
check_test_results(Passed, Passed, Passed, Passed)

@test begin
    throws := KeyError
    {1=>2}[2]
    {2=>2}[2]; fails
    {3=>2}[2]
end
check_test_results(Passed, ExpectedlyFailed, Passed)

################################################################################
################################################################################
#     `setup` and `teardown`
################################################################################
################################################################################

# Setup applies to all tests in the block
@test begin
    setup := begin
        a = 2
        b = 3
    end
    a+b == b+a
    a-b == b-a
    a*b == b*a
    a/b == b/a
end
check_test_results(Passed, FailedAssertion, Passed, FailedAssertion)


# One line fixtures, setup and teardown
resource = {1=>{}}
acquire_resource(key) = push(resource[key], :a)
release_resource(key) = push(resource[key], :r)
@test begin
    setup    := acquire_resource(1)
    teardown := release_resource(1)
    a+b == b+a
    a-b == b-a
    a*b == b*a
    a/b == b/a
end
check_test_results(Passed, FailedAssertion, Passed, FailedAssertion)
@assert isequal(resource[1], {:a, :r, :a, :r, :a, :r, :a, :r})


# Multiline fixtures, combinations of setup and teardown
resource = {1=>{}, 2=>{}, 3=>{}}
acquire_resource(key) = push(resource[key], :a)
release_resource(key) = push(resource[key], :r)
@test begin
    setup := begin
        a,b = 2,3
        acquire_resource(1)
        acquire_resource(3)
    end
    teardown := begin
        release_resource(2)
        release_resource(3)
    end
    a+b == b+a
    a-b == b-a
    a*b == b*a
    a/b == b/a
end
check_test_results(Passed, FailedAssertion, Passed, FailedAssertion)
@assert isequal(resource[1], {:a,     :a,     :a,     :a,   })
@assert isequal(resource[2], {    :r,     :r,     :r,     :r})
@assert isequal(resource[3], {:a, :r, :a, :r, :a, :r, :a, :r})

# Nesting fixtures combines the setup code.
@test begin
    setup := a = 1
    begin
        setup := b = 2
        a+b == 3
        b-a == 1
        b-a == 666
    end
    begin
        setup := b = 3
        a+b == 4
        b-a == 2
        b-a == 666
    end
end
check_test_results(Passed, Passed, FailedAssertion, Passed, Passed, FailedAssertion)

# Multiple fixture directives at same level are combined into a single
# block
@test begin
    setup := a = 2
    setup := b = 3
    a+b == b+a
    a-b == b-a
    a*b == b*a
    a/b == b/a
end
check_test_results(Passed, FailedAssertion, Passed, FailedAssertion)

# Making sure that name lookup failures are reported properly
@test begin
    setup := a = 2
    begin
        some_name_that_should_not_be_visible_anywhere_92401965203820
    end
end
check_test_results(FailedSomehow)
check_test_results(ThrewException)

################################################################################
################################################################################
#     `parametrize` reruns the same test with different sets of parameters
################################################################################
################################################################################

@test begin
    parametrize := ((a,b),
                    (2,3),
                    (3,3),
                    (4,3))
    a+b == b+a
    a-b == b-a
end
check_test_results(Passed, ParametrizedFailed)
@assert latest_tests[2][:result].which == {(2,3), (4,3)}

################################################################################
################################################################################
#     Test names
################################################################################
################################################################################

function check_names(kind::Symbol, expected...)
    expected = {name for name in expected}
    actual = map(t->t[:scope][:name, kind], latest_tests)
    if !isequal(expected, actual)
        error("Wrong test names.\nExpected: ",expected,"\nActual: ", actual)
    end
end

# Names can be set with the `name` directive
@test name := "foo" 1==1
check_names(:here, "foo")

# Literal strings are a shorthand
@test "foo" 1==1
check_names(:here, "foo")

@test begin
    "foo"
    single
    a = 1
    b = 2
    a+b == b+a
end
check_names(:here, "foo")

@test begin
    "one";   1==1
    "two";   2==2
    "three"; 3==3
    "four";  4==4
    begin "five"
        5==5
    end
end
check_names(:here, "one", "two", "three", "four", "five")

# Implicit line mode with nesting and composition
@test begin
    single
    "outer"
            1==1 # Joined with 3==3, because we're not in linemode
    "two";  2==2
            3==3 # Joined with 1==1
    "four"; 4==4
    begin "five"
        5==5
    end
end
check_names(:here,     "outer",            "two",            "four",            "five")
check_names(:combine, {"outer"}, {"outer", "two"}, {"outer", "four"}, {"outer", "five"})

# Same again, in line mode, 2 and 4 will be separate tests.
@test begin
    "outer"
            1==1
    "two";  2==2
            3==3
    "four"; 4==4
    begin "five"
        5==5
    end
end
check_names(:here,     "outer",            "two",   "outer",            "four",            "five")
check_names(:combine, {"outer"}, {"outer", "two"}, {"outer"}, {"outer", "four"}, {"outer", "five"})


################################################################################
################################################################################
#     Intra-test checkpoints
################################################################################
################################################################################

@test begin
    single
    a = 1
    @check a == 1
    a += 2
    @check a == 3
    a += 3
    @check a == 6
    a += 4
    a == 10
end
check_test_results(Passed)

@test begin
    single
    a = 1
    @check a == 1
    a += 2
    @check a == 666
    a += 3
    @check a == 6
    a += 10
    a == 10
end
check_test_results(FailedAssertion)





@test begin
    "a=1"
    setup := a=1
    begin "b=2"
        setup := b=2
        begin "c=3"
            setup := c=3
            "+"; a+b+c == 6
            "*"; a+b+c == 6
        end
        begin "c=4"
            setup := c=4
            "+"; a+b+c == 7
            "*"; a*b*c == 8
        end
    end
    begin "b=3"
        setup := b=3
        begin "c=3"
            setup := c=3
            "+"; a+b+c == 7
            "*"; a*b*c == 9
        end
        begin "c=4"
            setup := c=4
            "+"; a+b+c == 8
            "*"; a*b*c == 12
        end
    end
end
check_test_results(Passed, Passed, Passed, Passed, Passed, Passed, Passed, Passed)
check_names(:here, "+", "*", "+", "*", "+", "*", "+", "*")
check_names(:combine,
            {"a=1", "b=2", "c=3", "+"},
            {"a=1", "b=2", "c=3", "*"},
            {"a=1", "b=2", "c=4", "+"},
            {"a=1", "b=2", "c=4", "*"},
            {"a=1", "b=3", "c=3", "+"},
            {"a=1", "b=3", "c=3", "*"},
            {"a=1", "b=3", "c=4", "+"},
            {"a=1", "b=3", "c=4", "*"})

            
println("Development tests completed.")

                
