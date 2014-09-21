immutable RUsage
    ru_utime_sec::Clong         #  user CPU time used
    ru_utime_usec::Clong        #  user CPU time used
    ru_stime_sec::Clong         #  system CPU time used
    ru_stime_usec::Clong        #  system CPU time used
    ru_maxrss::Clong            #  maximum resident set size
    ru_ixrss::Clong             #  integral shared memory size
    ru_idrss::Clong             #  integral unshared data size
    ru_isrss::Clong             #  integral unshared stack size
    ru_minflt::Clong            #  page reclaims (soft page faults)
    ru_majflt::Clong            #  page faults (hard page faults)
    ru_nswap::Clong             #  swaps
    ru_inblock::Clong           #  block input operations
    ru_oublock::Clong           #  block output operations
    ru_msgsnd::Clong            #  IPC messages sent
    ru_msgrcv::Clong            #  IPC messages received
    ru_nsignals::Clong          #  signals received
    ru_nvcsw::Clong             #  voluntary context switches
    ru_nivcsw::Clong            #  involuntary context switches
end

function get_vmsize()
    ru = Array(RUsage, 1)
    ccall(:getrusage, Cint, (Cint, Ptr{Void}), 0, ru)
    return ru[1].ru_maxrss
end

function run_mtest(name, testf)
    print ("Testing $name...")
    for i in 1:2
        print("priming process...")
        testf()
    end
    vm1 = get_vmsize_rusage()
    println("monitored run...")
    testf()
    vm2 = get_vmsize_rusage()

    diff = vm2 - vm1
    WARN = (diff > 1000) ? "<===================================== WARNING" : ""
    println("Memory Test ($name) : VMSize difference : $diff KB $WARN")
end



function mtest_create_strings()
    for i in 1:10^8
        string("$i")
    end
    gc()
end

function mtest_remotecall_fetch()
    for i in 1:10^5
        remotecall_fetch(1, myid)
    end
    gc()
end

run_mtest("create_strings", () -> mtest_create_strings())
run_mtest("remotecall_fetch", () -> mtest_remotecall_fetch())


