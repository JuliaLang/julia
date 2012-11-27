YouTest
=======

* Unit test
* Useful test
* Universal test
* Ubiquitous test
* You! Test!

UTest: a bit dull and arleady taken.

YewTest: could inspire a pretty logo, and would be most search-friendly.

YouTest: most directly meaningful. Going with this one for now.

Philosophy
----------

YouTest is an experiment whose aim is to explore test framework
interfaces compatible with the beliefs that

* Testing is important
* Testing should be fun
* Testing should be made as easy and convenient as possible
* Testing tools should be powerful
* Testing tools should be flexible
* Testing tools should be unrestricted
* Julia provides the means to build good DSLs

As a consequence, YouTest

* uses a simple but flexible hierarchical syntax: like s-expressions
  and XML, but not as spartan as the former, and not as ugly or
  verbose as the latter.

* provides a tiny core of hard-wired features, along with mechanisms
  for injecting user-defined features on top. Most of YouTest's
  features are implemented through the latter.

Enough talk, show me the interface!
-----------------------------------

Here is dense example which throws a whole bunch of YouTest features
at you all at once. If you find it a bit overwhelming, you may want to
look ahead to the later sections, first.

The section after this one explains the general structure of YouTest
test specifications. The section after that introduces YouTest's
features one by one.

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
        # ########## The following have not been implemented yet ##############
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
   

Structure
---------

The @test macro is used to write tests. Three different categories of
expression may appear in a test specification:

1. Metadata
2. Blocks
3. Simple expressions

Metadata take one of the following three forms

1. `<keyword> := <expression>`
2. `<keyword>`
3. `<literal string>`

The second form is (usually) a shorthand for

    <keyword> := <keyword's default>.

The third form is a shorthand for

     name := <literal string>

Metadata appearing within a block apply to all toplevel tests within
that block, and, usually, also to tests within inner blocks.

Single expressions appearing within a block are separate tests, unless
the block contains the keyword `single`, in which case all the
non-metadata in the block are joined sequentially to form a single
test.

If a test evaluates to `true` it passes, if it evaluates to `false` it
fails. Non-boolean results are errors (a form of failure).

Use `@check <expression>` to insert extra checkponts at arbitrary
locations in mult-line tests (those created with `single`).


Basic Usage
-----------

A simple single test:

    @test 1<2

Three simple tests:

    @test 1<2 2==2 3>2

Three simple tests organized vertically:

    @test begin
        1< 2
        2==2
        3> 2
    end

The `single` keyword combines all non-metadata in the block into a
single test.

    @test begin
        single
        a = 1
        b = 2
        a+b == 3
    end

The last expression in a compound test is implicitly taken to be an
assertion, but extra assertions can be placed at arbitary points with
@check:

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

Tests can (and should!) be named:

    @test name := "add" 1+2 == 3

`<literal string>` is a shorthand for 'name := `<literal string>`:

    @test "add" 1+2 == 3

Names in nested blocks are combined to form compound names:

    @test begin
        "Commute"
        "+"; 1+2 == 2+1   # Full name: {"Commute", "+"}
        "*"; 1*2 == 2*1   # Full name: {"Commute", "-"}
    end

Tests can be marked as expecting to fail, either unconditionally

    @test fails 1>2

    @test begin
        single
        fails
        a = 1
        b = 2
        a-b == b-a
    end

or conditionally

    @test fails := today() == :Sunday  go_shopping()

    @test begin
        "Funky new feature in version 12.6"
        fails := version() < (12,6) # Applies to all 4 tests in this block

        begin "basic"
            funky(12) == 24
            funky(3)  ==  6
        end

        begin "advanced"
            funky_advanced(12) == 144
            funky_advanced(3)  ==   9
        end
     end

Fixtures:

    @test begin
        "Commutativity"
        setup := begin  # Applies to all 4 tests in this block
            a = 2
            b = 3
        end
        "+"; a+b == b+a
        "-"; a-b == b-a; fails 
        "*"; a*b == b*a
        "/"; a/b == b/a; fails
    end

`teardown` is also available.


Parametrization: evaluate the test repeatedly with different data:

    @test begin
        "Commutativity"
        parametrize := ((a,b),
                        (1,2),
                        (2,2),
                        (3,2))
        "+"; a+b == b+a
        "-"; a-b == b-a; fails 
        "*"; a*b == b*a
        "/"; a/b == b/a; fails
    end

Exceptions:

    @test begin
        {2=>1}[2] == 1
        {1=>2}[:a] = b; throws := MethodError
        {1=>2}[2];      throws := KeyError
        {2=>2}[2] == 2
    end

    @test begin
        throws := KeyError
        {1=>2}[2]
        {2=>2}[2]; fails
        {3=>2}[2]
    end

(Not available yet) Quickcheck:

    @test
        generate := (a::Int, b::Int)
        "+"; a+b == b+a
        "-"; a-b == b-a; fails 
        "*"; a*b == b*a
        "/"; a/b == b/a; fails
    end


Blocks can be nested to arbitrary depth.

    @test begin
        "a=1"
        setup := a=1
        begin "b=2"
            setup := b=2
            begin "c=3"
                setup := c=3
                "+"; a+b+c == 6
                "*"; a*b*c == 6
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

Plugin writers have full control over how nested metadata are
combined.

To be implemented:

Time limits

    @test
        sleep(2.5); time_limit := 3s
        sleep(59);  time_limit := 1m
        frob();     time_limit := 5s; kill_after := 20s
    end

Stream side effects

    @test
        print(12);   check_stream := stdout = "12"
        println(12); chekc_stream := stdout = "12\n"
        begin
            capture_stream := stdout = s
            print(12);
            length(s) == 2
        end
    end
