
# Backtrace utility functions
function ip_has_file_and_func(ip, file, funcs)
    return any(fr -> (string(fr.file) == file && fr.func in funcs), StackTraces.lookup(ip))
end

function scrub_backtrace(bt)
    do_test_ind = findfirst(ip -> ip_has_file_and_func(ip, @__FILE__, (:do_test, :do_test_throws)), bt)
    if do_test_ind != 0 && length(bt) > do_test_ind
        bt = bt[do_test_ind + 1:end]
    end
    name_ind = findfirst(ip -> ip_has_file_and_func(ip, @__FILE__, (Symbol("macro expansion"),)), bt)
    if name_ind != 0 && length(bt) != 0
        bt = bt[1:name_ind]
    end
    return bt
end

