# The `test` dialect (§9): enough, with the core structural ops, to exercise
# every feature of the data structure without Julia semantics.

const TEST_KINDS = Dict{Symbol,Kind}()

function register_test_dialect!()
    d = register_dialect!(:test)
    TEST_KINDS[:iconst] = register_kind!(d, :iconst; result=1, effects=FLAG_PURE,
        schema=P[:value=>OC_VALUE])
    TEST_KINDS[:add] = register_kind!(d, :add; result=1, effects=FLAG_PURE,
        schema=P[:a=>OC_VALUE, :b=>OC_VALUE])
    TEST_KINDS[:mul] = register_kind!(d, :mul; result=1, effects=FLAG_PURE,
        schema=P[:a=>OC_VALUE, :b=>OC_VALUE])
    TEST_KINDS[:icmp] = register_kind!(d, :icmp; result=1, effects=FLAG_PURE,
        schema=P[:pred=>OC_CONST, :a=>OC_VALUE, :b=>OC_VALUE])
    TEST_KINDS[:print] = register_kind!(d, :print; result=0,
        schema=P[:value=>OC_VALUE])                       # effectful
    TEST_KINDS[:opaque] = register_kind!(d, :opaque; result=1, varargs=true, minops=0)
    TEST_KINDS[:delay] = register_kind!(d, :delay; result=1, is_delay=true,
        effects=FLAG_PURE, schema=P[:data=>OC_VALUE, :init=>OC_VALUE, :reset=>OC_VALUE],
        minops=2, varargs=false)
    return d
end
