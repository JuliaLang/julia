# This runner is designed for use in the development tests. It
# collects the tests in any @test block and immediately runs them, and
# stores them along with their metadata in the global variable
# latest_tests. The results may then be inspected and checked with
# arbitrary code.

# The function check_test_results is useful for quickly checking the
# framework messages returned by the tests stored in latest_tests.
function check_test_results(expected_types...)
    wrong() = error("Wrong test results.\nExpected types: ",expected_types,"\nActual values: ",actual_values)
    actual_values = map(t->t[:result], latest_tests)
    if length(expected_types) != length(actual_values) wrong() end
    for (actual, expected) in zip(actual_values, expected_types)
        if !isa(actual, expected)
            wrong()
        end
    end
end



function development_testing_runner(t)
    global latest_tests = t
    for t in latest_tests run_test(t) end
end
