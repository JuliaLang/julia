type Skipped <: PassedSomehow end

default[:skip] = true

function construct_test_plugin__skip(spec)
    scope = spec[:scope]
    # Hmm, should probably evaluate the condition at test run time
    # rather than test construction time.
    if has(scope, :skip) && eval(scope[:skip])
        spec[:combined_code] = :(Skipped())
    end
end

# This one overrides pretty much everything, give it a high level.
add_construct_test_plugin("skip", 100, construct_test_plugin__skip)
