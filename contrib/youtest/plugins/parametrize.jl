# TODO: hygiene

type ParametrizedFailed <: FailedSomehow
    which
end

function construct_test_plugin__parametrize(spec)
    scope = spec[:scope]
    if has(scope, :parametrize)
        parameter_spec   = scope[:parametrize].args
        parameter_names  = parameter_spec[1]
        parameter_values = expr(:tuple, parameter_spec[2:end])
        original_code = spec[:combined_code]
        failures = gensym("failures")
        wrapped_code = quote
            $failures = {}
            for $parameter_names = $parameter_values
                result = $original_code
                if !isa(result, PassedSomehow)
                    push($failures, $parameter_names)
                end
            end
            if length($failures) == 0
                Passed()
            else
                ParametrizedFailed($failures)
            end
        end
        global b = wrapped_code
        spec[:combined_code] = wrapped_code
    end
end

add_construct_test_plugin("parametrize", 10, construct_test_plugin__parametrize)

