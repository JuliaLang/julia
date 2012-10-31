# A very simple test runner. After a block of tests is parsed, it is
# immediately executed and the results are reported.

function simple_test_runner(tests)
    for test in tests
        run_test(test)
        result = test[:result]
        full_name = join(test[:scope][:name, :combine], " / ")
        if length(full_name) == 0 full_name = "<unnamed>" end
        if isa(result, PassedSomehow)
            verbose ? print("\n$(full_name)") : print(".")
        else
            verbose ? println(" $(result)") : println("\n$(full_name) $(result)")
        end
    end
end


available_test_runners[:simple] = simple_test_runner
