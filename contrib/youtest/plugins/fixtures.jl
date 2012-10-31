# ToDo write a more robust version of this, and provide it as a
# framework tool.
function append_code(existing_expression, new_bit)
    quote
        $existing_expression
        $new_bit
    end
end

multiple_binding_strategy[:setup]    = append_code
multiple_binding_strategy[:teardown] = append_code

function construct_test_plugin__fixtures(spec)
    scope = spec[:scope]
    if has(scope, :setup) || has(scope, :teardown)
        # Use :combine scoping to make inner block inherit setup from outer ones.
        setup    = gather_code_from_visible_scopes(scope, :setup)
        teardown = gather_code_from_visible_scopes(scope, :teardown)
        inner_code = spec[:combined_code]
        wrapped_code = quote
            $setup
            result = $inner_code # Will need to think about hygiene
            $teardown # Should this be wrapped in error suppression?
            result
        end
        spec[:combined_code] = wrapped_code
    end
end

function gather_code_from_visible_scopes(scope, which)
    # TODO: There are lots of places in the code where I'm annoyed by
    # not having a splicing unquote. This makes lots of superfluous
    # blocks appear all over the generated code. This function is a
    # prime example. Look at the code generated by this function and
    # stamp out the redundant begin...ends.
    portions = has(scope, which) ? scope[which, :combine] : expr(:block)
    expr(:block, portions)
end

add_construct_test_plugin("fixtures", 10, construct_test_plugin__fixtures)
