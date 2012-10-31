# These tests serve the double purpose of putting the test suite
# through its paces, and of acting as a guide to its usage.

# TODO: provide a sensible interface for running tests externally
include("test.jl")
test_runner = simple_test_runner
verbose = false

@test begin
    "Group 1" # Topmost component of hierarchical name
    begin
        "dict" # Next level of hierarchical name
        setup := d = {1=>2} # Fixture
        "lookup success";    d[1] == 1;
        "lookup failure";    d[2];      throws := KeyError
        "overwrite"; single; d[1] = 3; d[1] == 3
        "has success";   has(d,1)
        "has failure";   has(d,2) == false
        # A mult-expression test
        begin single
            "length"
            @check length(d) == 1
            d[2] = 6
            @check length(d) == 2
            d[9] = 0
            @check length(d) == 3
            del(d,2)
            @check length(d) == 2
            del(d,3)
            @check length(d) == 1
            del(d,1)
                   length(d) == 0
        end
    end
    # Fail and Skip
    fails; 1>2
    fails := version() < (3,2); new_frob()
    skip  := version() < (3,2); new_frob()
    # Repeat tests with different inputs
    begin "commute"
        parametrize := ((a,b),
                        (2,3),
                        (3,3),
                        (4,3))
        a+b == b+a; "+"
        a*b == b*a; "*"
    end
    # ################# Not Implemented Yet #####################
    # Quickcheck
    begin "commute"
        generate := (a::Int, b::Int)
        "+"; a+b == b+a
        "*"; a*b == b*a
    end
    # Stream capture
    begin "printing"
        begin
            setup := input = 12
            check_stream := stdout = "12";   print(input)
            check_stream := stdout = "12\n"; println(input)
            begin
                capture_stream := stdout = it
                single; println(input); length(it) == 3
                begin single
                    print(input)
                    length(it) == 2
                end
            end
        end
    end
    # Stream injection (needs more thought)
    begin
        :send_to_stream := stdin_stream = "1\n2\n1\n"
        @check get_choice(stdin_stream) == 1
        @check get_choice(stdin_stream) == 2
               get_choice(stdin_stream) == 1
    end
    # Time limits
    begin
        do_this(); time_limit := 2h
        do_that(); time_limit := 5s; kill_after := 10s
    end        
end

#####################################
########## Getting started ##########
#####################################

# TODO: This is in serious need of cleaning up




# The simplest single test.
@test 1==1

# Three tests. Each is run as a separate test by the framework.
@test 1==1  1+2==2+1  2>1

# Alternative way of writing the same three tests.
@test begin
    1==1
    1+2==2+1
    2>1
end

# Multi-line tests. This is a single test.
@test begin
    single
    a = 1
    b = 2
    a+b == b+a
end

# A group of two multi-line tests
@test begin
    begin
        single
        a = 1
        b = 2
        a+b == b+a
    end
    begin
        single
        a = 1
        b = 2
        a*b == b*a
    end
end

# Fixtures: A more convenient way of writing the last pair of tests.
@test begin
    setup := begin
        a = 1
        b = 2
    end
    a+b == b+a
    a*b == b*a
end

# ... or even more concisely
@test setup := (a = 1; b = 2)  a+b == b+a   a*b == b*a

# teardown is also available.

# A test with multiple (3) checkpoints
@test begin
    single
    a = 1
    @check a == 1
    a += 5
    @check a == 6
    a -= 4
    a == 2
end

# Identifying tests. Test can be named with the `name` directive.
@test name := "equality" 1==1

# <literal string>   is a shorthand for   name := <literal string>

# Three different ways of writing the same test and giving it the name
# "equality".
@test "equality" 1==1

@test 1==1  "equality"

@test begin
    single
    "equality"
    a = 1
    b = 1
    a == b
end

# Testing for exceptions
@test throws := KeyError {1=>2}[2]

@test {1=>2}[2] throws := KeyError

@test begin
    throws := KeyError
    {1=>2}[2]
    {1=>2}[3]
    {1=>2}[4]
end

# Parametrization
@test begin
    parametrize := ((a,b),
                    (2,3),
                    (3,3),
                    (4,3))
    a+b == b+a
    a*b == b*a
end

# Quickcheck
@test begin
    generate := (a::Int, b::Int)
    a+b == b+a
    a*b == b*a
end


# Failing
# Skipping


###############################################
################ single mode ##################
###############################################

# By default, any non-metadata expression is taken to be a separate
# test. Any metadata expression within a block applies to all the
# tests within that block (and, depending on its kind, perhaps to
# inner tests in inner blocks too). To create a test which consists of
# a sequence of expressions, wrap the expressions in a block
# containing the `single` keyword.

# Three tests. Each of them is supposed to fail. Each of them has the
# name "foo".
@test fails "foo" 1>2 1>=2 1==2

# A single test, called "foo"
@test begin
    single
    "foo"
    a = 1
    b = 2
    a + b == 3
end

##############################
####### Test metadata ########
##############################

# The tests may be annotated with the
#
#   <keyword> := <expression>
#
# syntax. The test parser always installs such information into the
# test scope. It is possible to write plugins which recognize the data
# and affect the test in arbitrary ways.

# The `fails` keyword states that the tests to which it applies are
# supposed to fail.

# One test which should fail
@test fails:=true 1>1
# The same single test which should fail
@test begin
    fails := true
    1>1
end

# `fails` on its own is a shorthand for `fails := true`, so the last
# two examples can be rewritten more concisely as
@test fails 1>1
@test begin
    fails
    1>1
end

# `fails` can be set to an arbitrary condition.
new_frobnicator() = my_operating_system == :Windows ? false : true
my_operating_system = :Windows
@test fails:= my_operating_system == :Windows  new_frobnicator()

# Uncaught exceptions are a form of failure
@test fails throw(KeyError(2))

# `throws` specifies a required exception type
@test throws := KeyError {1=>2}[2]
@test begin
    throws := KeyError
    {1=>2}[2]
end

# Tests can be given names or labels
@test name:= "Commutativity1" 1+2==2+1
@test begin
    name:= "Commutativity2"
    1+2 == 2+1
end

# Any literal string is a shorthand for use of the `name` directive,
# so the last two tests could equivalently have been written
@test "Commutativity3" 1+2==2+1
@test begin
    "Commutativity4"
    1+2 == 2+1
end


###########################################
#### Hierarchies, grouping and nesting ####
###########################################

# Tests can be grouped together and organised in an arbitrary tree
# structure.

# How metadata affects deeper branches depends on the metadata's
# type. Names are accumulated. The following set of tests contains
# eight different tests with the name components:

# 1) "Outer"
# 2) "Outer" "A"
# 3) "Outer" "A" "1"
# 4) "Outer" "A" "2"
# 5) "Outer" "A" "2" "i"
# 6) "Outer" "A" "2" "ii"
# 7) "Outer" "A"
# 8) "Outer" "B"

# Test runners and reporters can choose to do arbitrary things with
# these components.

@test begin
    name := "Outer"
    1==1 # The name of this test has one component: "Outer"
    begin
        name := "A"
        2==2 # Name has 2 components:        "Outer", "A"
        begin
            name := "1"
            3==3 # Name has 3 components:    "Outer", "A", "1"
        end
        begin
            name := "2"
            4==4 # Name has 3 components:    "Outer", "A", "2"
            name:="i";  5==5 # 4 components: "Outer", "A", "2" "i"
            6==6; name:="ii" # 4 components: "Outer", "A", "2" "ii"
        end
    end
    begin
        name := "B"
        7==7 # Name has 2 components:        "Outer", "B"
    end
end

# Block mode vs. line mode

# Block 

# Fixtures: use the `setup` and `teardown` keywords.

# ToDo: an example with teardown

@test begin
    "Commutativity"
    setup := begin
        a = 2
        b = 3
    end
    begin
        "addition"
        a+b == b+a
    end
    begin
        "multiplication"
        a*b == b*a
    end
end

# Parametrization
@test begin
    name := "Commutativity"
    parametrize := ((a,b),
                    (1,1),
                    (2,3),
                    (3,3),
                    (4,3))
    "+"; a+b == b+a
    "-"; a-b == b-a; fails
    "*"; a*b == b*a
    "/"; a/b == b/a; fails
end


# In the outermost (top level) block, @test interprets each code line
# as a separate, one-line test. In inner blocks, code lines are
# combined to make a single, multi-line test. To make an inner block
# define separate one-line tests, use the `oneliners` directive.

@test begin
    name := "Commutativity"
    parametrize := ((a,b),
                    (1,1),
                    (2,3),
                    (3,3),
                    (4,3))
    a+b == b+a
    a*b == b*a
    begin
        a-b == b-a
        a/b == b/a
        a^b == b^a
    end
end

# Quickcheck: Rather than parametrizing a test by choosing the
# parameter values by hand, use `generate` to feed random data to the
# test. When the test fails, Quickcheck will attempt to simplify the
# input, looking for the simplest example which causes the test to
# fail.

@test begin
    name := "Commutativity"
    generate := (a::Int, b::Int)
    a+b == b+a
    a*b == b*a
    begin
        linemode
        fails := a==b
        a-b == b-a
        a/b == b/a
        a^b == b^a
    end
end

# ToDo: Quickcheck size, quantification, etc.

# Testing for exceptions among multiple checkpoints.

# Arbitrary nesting


