type CorrectExceptionNotThrown{T<:Exception} <: FailedSomehow
    target :: Type{T}
    actual #:: Union{A, Nothing} - doesn't work because no way to deduce when nothing
end

default[:throws] = Exception

function construct_test_plugin__throws(spec)
    scope = spec[:scope]
    if has(scope, :throws)
        ExpectedExceptionType = scope[:throws]
        original_code = spec[:combined_code]
        wrapped_code = quote
            original_message = $original_code
            ActualException = nothing
            if isa(original_message, ThrewException)
                ActualException = original_message.it
            end
            if isa(ActualException, $ExpectedExceptionType)
                Passed()
            else
                CorrectExceptionNotThrown($ExpectedExceptionType, ActualException)
            end
        end
        spec[:combined_code] = wrapped_code
    end
end


add_construct_test_plugin("throws", 80, construct_test_plugin__throws)
