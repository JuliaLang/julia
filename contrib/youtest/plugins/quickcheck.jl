function construct_test_plugin__quickcheck(spec)
    if has(spec[:scope], :generate)
        #throw(NotImplemented("Quickcheck plugin"))
        println("YouTest: Warning, Quickcheck plugin not implemented yet.")
    end
end

add_construct_test_plugin("quickcheck", 10, construct_test_plugin__quickcheck)
