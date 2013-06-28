
#     
# immutable RUsage
#     ru_utime_sec::Clong         #  user CPU time used 
#     ru_utime_usec::Clong        #  user CPU time used 
#     ru_stime_sec::Clong         #  system CPU time used 
#     ru_stime_usec::Clong        #  system CPU time used 
#     ru_maxrss::Clong            #  maximum resident set size 
#     ru_ixrss::Clong             #  integral shared memory size 
#     ru_idrss::Clong             #  integral unshared data size 
#     ru_isrss::Clong             #  integral unshared stack size 
#     ru_minflt::Clong            #  page reclaims (soft page faults) 
#     ru_majflt::Clong            #  page faults (hard page faults) 
#     ru_nswap::Clong             #  swaps 
#     ru_inblock::Clong           #  block input operations 
#     ru_oublock::Clong           #  block output operations 
#     ru_msgsnd::Clong            #  IPC messages sent 
#     ru_msgrcv::Clong            #  IPC messages received 
#     ru_nsignals::Clong          #  signals received 
#     ru_nvcsw::Clong             #  voluntary context switches 
#     ru_nivcsw::Clong            #  involuntary context switches 
# end
# 
# RUSAGE_SELF::Cint is defined a 0 both on Mac and Linux....
# 
# ru1 = Array(RUsage, 1)
# ru2 = Array(RUsage, 1)
# 
# diff = ru2[1].ru_maxrss - ru1[1].ru_maxrss
# WARN = (diff > 1000) ? "WARNING" : ""
# 
# ccall(:getrusage, Cint, (Cint, Ptr{Void}), 0, ru1)
# ccall(:getrusage, Cint, (Cint, Ptr{Void}), 0, ru2)

function get_vmsize()
    lines = split(open(readall, "/proc/self/status"), "\n")
    vm_line = split(filter(x->beginswith(x, "VmSize"), lines)[1])
    vmsize = int(vm_line[2])
    
    if uppercase(vm_line[3]) == "KB"
        vmunits = 1
    elseif uppercase(vm_line[3]) == "MB"
        vmunits = 1000
    else
        error("Unknown unit $(vm_line[3])")
    end
    
    vmsize * vmunits
end

function run_mtest(name, testf)
    print ("Testing $name...")
    for i in 1:2
        print ("priming process...")
        testf()             # priming
    end
    vm1 = get_vmsize()
    println ("monitored run...")
    testf()             # actual run        
    vm2 = get_vmsize()

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



run_mtest("create_strings", () -> mtest_create_strings())
    
    
function mtest_remotecall_fetch()
    for i in 1:10^5
        remotecall_fetch(1, myid)
    end
    gc()
end

run_mtest("remotecall_fetch", () -> mtest_remotecall_fetch())

    