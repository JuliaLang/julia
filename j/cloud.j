### Interface for Cloud infrastructure ###

abstract InfoCloud

abstract InstanceCloud

function show(x::InfoCloud)
    println(x.machines)
    println(x.headinfo)
    println(x.cloudservice)
    println(x.splitset)
end

function currentcloud()
    toreturn = ""
    try
        toreturn = info_cloud.cloudservice
    catch e
        toreturn =  "No defualt cloud set"
    end #try/catch
    return toreturn
end

function setdefualtcloud(cloudservice::String)

    cloud = quote

        ### Functions ###
        addprocs_cloud = $symbol(strcat("addprocs_",cloudservice))
        killprocs_cloud = $symbol(strcat("killprocs_",cloudservice))
        loadmachines_cloud = $symbol(strcat("loadmachines_",cloudservice))
        startnodes_cloud = $symbol(strcat("startnodes_",cloudservice))
        newmachine_cloud = $symbol(strcat("newmachine_",cloudservice))
        killmachine_cloud = $symbol(strcat("killmachine_",cloudservice))
        killnode_cloud = $symbol(strcat("killnode_",cloudservice))

        ### Variable(s) ###
        info_cloud = $symbol(strcat("info_",cloudservice))

    end #quote
    load(strcat(cloudservice,".j"))
    eval(cloud)
end #function

#setdefualtcloud("ec2")