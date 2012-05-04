## vcloud interface ##

VCPATH = "/home/vcloud/vcloudpy"

function vcloud_vappnames()
    txt = split(readall(`python $VCPATH/getvdc.py -o vApp`),Set(' ','\n'),false)
    names = {}
    i=0
    for i=1:length(txt)
        if txt[i]=="va_name"
            push(names, cstring(txt[i+1]))
        end
    end
    names
end

NETNAME = ""

function vcloud_networkname()
    global NETNAME
    if NETNAME!=""
        return NETNAME
    end
    txt = split(readall(`python $VCPATH/getvdc.py -o Networks`),Set(' ','\n'),false)
    for i=1:length(txt)
        if txt[i]=="nw_name"
            NETNAME = txt[i+1]
            return NETNAME
        end
    end
    error("could not determine network name")
end

function vcloud_nodeip(name)
    line = readall(`python $VCPATH/getip.py -n $name -w $(vcloud_networkname())`)
    cstring(line[1:end-1])
end

function vcloud_delnode(name)
    run(`python $VCPATH/delvapp.py -n $name`)
end

VCNodesInUse = Dict()

function vcloud_waitboot(name)
    print("powering on node $name"); flush(stdout_stream)
    while true
        state = ""
        success = false
        while !success
            try
                state = readall(`python $VCPATH/getvappstate.py -n $name`)
                success = true
            catch
            end
        end
        if state=="4\n"
            break
        end
        print("."); flush(stdout_stream)
        sleep(1)
    end
    println()
end

function wait_ping(host)
    print("checking connectivity to $host"); flush(stdout_stream)
    while true
        cmd = `ssh $host true`
        read_from(cmd); read_from(stderr(cmd))
        if run(cmd)
            break
        end
        print("."); flush(stdout_stream)
        sleep(0.5)
    end
    println()
end

function vcloud_newnodes(n)
    nodes = vcloud_vappnames()
    names1 = {}
    hn = gethostname()
    for nn = nodes
        if n > 0 && !has(VCNodesInUse,nn) && nn!="julia-template" && nn!=hn
            spawn(`python $VCPATH/powervapp.py -n $nn -s 0`)
            VCNodesInUse[nn] = true
            push(names1, nn)
            n -= 1
        end
    end
    sleep(1)
    for nn = names1
        vcloud_waitboot(nn)
    end
    i0 = length(nodes)+1
    names = { cstring("julia$(uint2str(i,10,2))") | i=i0:(i0+n-1) }
    for nn = names
        println("creating new node $nn")
        spawn(`python $VCPATH/clonevapp.py -s julia-template -n $nn`)
        VCNodesInUse[nn] = true
    end
    sleep(2)
    for nn = names
        vcloud_waitboot(nn)
    end
    ips = map(vcloud_nodeip, append(names1,names))
    foreach(wait_ping, ips)
    ips
end

function addprocs_vcloud(n)
    addrs = vcloud_newnodes(n)
    addprocs_ssh(addrs)
end
