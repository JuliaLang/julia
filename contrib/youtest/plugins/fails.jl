# The test failed, but the `fails` marker told us to expect this. Test
# runners can deal with this information as they please, but, by
# default, this expected result is a PASS.
type ExpectedlyFailed   <: PassedSomehow
    it # The origial failure message
end
# The reverse of ExpectedlyFailed: the test passes, contrary to
# expectation. By default, we consider this a FAIL.
type UnexpectedlyPassed <: FailedSomehow end


# Make the parser accept `fails` as a shorthand for `fails := true`
default[:fails] = true

# What the test constructor should do when it encounters a scope
# containing  fails := <condition>
function construct_test_plugin__fails(spec)
    if has(spec[:scope], :fails)
        #
        should_fail = spec[:scope][:fails]
        # Get the uninverted test expression
        original_code = spec[:combined_code]
        # Wrap the test in the logic inversion code
        wrapped_code = quote
            original_message = $original_code
            if $should_fail
                if isa(original_message, PassedSomehow)
                    UnexpectedlyPassed()
                else
                    ExpectedlyFailed(original_message)
                end
            else
                original_message
            end
        end
        # Replace the original with the wrapped version
        spec[:combined_code] = wrapped_code
    end
end

# Each plugin has to be installed at a certain level, which determines
# when, relative to any other plugins, it will get to process the
# test.
this_plugin_level = 90
# To see levels of installed plugins, call
# show_construct_test_plugin_levels. The latter needs a name by which
# to report the plugin's level.
this_plugin_name = "fails"
add_construct_test_plugin(this_plugin_name, this_plugin_level, construct_test_plugin__fails)

# TODO:
# @macro construct_test_plugin(level, name, body)
    
