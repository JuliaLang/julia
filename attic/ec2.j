### EC2 Infrastrucure ###


type InstanceEc2 <: InstanceCloud
    ip::String
    id::String

    InstanceEc2() = new (" "," ")
    InstanceEc2(ip::String,id::String) = new (ip,id)
end #type

function show(inst::InstanceEc2)
     
     if inst.ip != ""
        #print(`ec2-describe-instances $inst.id`)
        print("IP: ", inst.ip," ID: ", inst.id)
        ## should add a line that parses if the output if the id is empty
     else
        print("this instance is empty")
     end #if/else
end #func


### VERY IMPORTANT to note that i don't check that id's match here
### since this information can always be retrieved

function ==(inst1::InstanceEc2, inst2::InstanceEc2)
    return inst1.id == inst2.id
end #func

type InfoEc2 <: InfoCloud
    ### Info Cloud Standards ###
    machines::Array
    julianodes::Array
    headnode::InstanceEc2
    cloudservice::String
    splitset::Set

    ### ec2 specific ###
    ec2_cert_path::String
    ec2_cert_pk_path::String 

    #ssh key 
    ssh_key_path::String
    ssh_key_name::String

    #ip generation
    subnet_mask::String
    ip_ranges::Array

    #other options
    def_inst_size::String
    stopping_behavior::String
    ami_id::String
    group_id::String
    subnet_id::String
    vpc_id::String
   

    InfoEc2() = new ({}, {}, InstanceEc2(),"ec2", Set(' ','\n','\t') , "", "", "", "", "", {}, "", "", "", "", "", "")
end #type

function show(info::InfoEc2)
    println("                        EC2 Info \n")
    println("EC2 certificate path:             ", info.ec2_cert_path)
    println("EC2 Private Key Certificate Path: ", info.ec2_cert_pk_path)
    println("SSH Key Path:                     ", info.ssh_key_path)
    println("SSH Key Name:                     ", info.ssh_key_name)
    println("Instance Size:                    ", info.def_inst_size)
    println("Stopping Behavior:                ", info.stopping_behavior)
    println("AMI ID:                           ", info.ami_id)
    println("Group ID:                         ", info.group_id)
    println("Subnet ID:                        ", info.subnet_id)
    println("Subnet Mask:                      ", info.subnet_mask)
    println("IP Ranges:                        ", info.ip_ranges)
    println("Head Node info                    ", info.headnode)
    println("Split Set                         ", info.splitset)
    println("Machines                          ", info.machines)
    println("julianodes:    ", info.julianodes)

end #func

#create info variable
info_ec2 = InfoEc2()

let dummy = nothing

    ### Configurables ###

    #Certificates

    
    info_ec2.ec2_cert_path = "/home/ec2-user/cert-IGLXLRCG3BY5LB3R4FDMJO2MQDYEE35Q.pem"
    info_ec2.ec2_cert_pk_path = "/home/ec2-user/pk-IGLXLRCG3BY5LB3R4FDMJO2MQDYEE35Q.pem"

    #ssh key 
    info_ec2.ssh_key_path = "/home/ec2-user/julia64.pem"
    info_ec2.ssh_key_name = "julia64.pem"

    #other options
    info_ec2.def_inst_size = "m1.small"
    info_ec2.stopping_behavior = "terminate"
    info_ec2.ami_id = "ami-f572b19c"
    info_ec2.group_id = "sg-3b6c4052"



    ### Private Functions ###

    function remove(array::Array, element)
        for i = 1:numel(array)
            if (element == array[i])
                del(array, i)
                break
            end #if
        end #for
    end    

    function wait_ping(host::String)
        print("checking connectivity to $host"); flush(stdout_stream)
        keypath = info_ec2.ssh_key_path
        while true
            ### Jeff here is where you might have to change the ssh command ###
            cmd = `ssh -o StrictHostKeyChecking=no -i $keypath $host true`
            read_from(cmd)
            read_from(stderr(cmd))
            if run(cmd)
                break
            end
            print(".")
            flush(stdout_stream)
            sleep(2)
        end
        println()
    end
    

    ### Global Functions ###
    global runningnodes_ec2
    global addprocs_ec2
    global killprocs_ec2
    global loadmachines_ec2
    global startnodes_ec2
    global killnode_ec2
    global newmachine_ec2
    global killmachine_ec2
    global setheadnode_ec2

    ## note that it is kinda akward that start nodes takes and array, and killnode takes only a string   

    function killmachine_ec2(id::String,ec2_cert_pk_path::String, ec2_cert_path::String)
        try
           toreturn = split(readall(`ec2-terminate-instances -K $ec2_cert_pk_path -C $ec2_cert_path $id`),info_ec2.splitset,false)
           del(info_ec2.machines, i)
           ## incase it still exists
           remove(info_ec2.julianodes, InstanceEc2("",id))
       catch e
           toreturn = strcat("There was an error in trying to terminate a machine \n",show(e))
       end #try/catch
       return toreturn
    end #func

    function newmachine_ec2(ec2_cert_pk_path::String, ec2_cert_path::String, ssh_key_name::String, inst_size::String, ami_id::String, stopping_behavior::String, group_id::String)
        ##ec2-command
        try 
            output = readall(`ec2-run-instances -K $ec2_cert_pk_path -C $ec2_cert_path -k $ssh_key_name -t $inst_size $ami_id --instance-initiated-shutdown-behavior $stopping_behavior -g $group_id`)
            
            #find the instance name
            println("the EC2 output: ",output)
            output = split(output,info_ec2.splitset,false)
            for i = 1:numel(output)
                if(output[i] == "INSTANCE")
                    id = output[i+1]
                    push(info_ec2.machines, InstanceEc2(ip,id))
                    break
                end #if
            end #for
            findip(id)
            info_ec2.machines
        catch e
            println("There was an error, in creating a new virtual instance. Try running the loadmachines method")
            println(e)
        end #try/catch
    end #func

    function runningnodes_ec2()
        info_ec2.julianodes
    end #func

    function killnode_ec2(id::String)
        ## TODO kill julia proccess via ssh
        remove(info_ec2.julianodes, InstanceEc2("", id))
    end #func


    function killprocs_ec2(bob::Array, killmachine::Bool)
        if(typeof(bob[1]) == InstanceEc2)
            machine = true
        end #if
        for el = bob
            if(machine)
                el = el.id
            end #if
            #not yet implemeted
            #killnode_ec2(el)
            if (killmachine)
                killmachine_ec2(el)
                remove(info_ec2.machines, InstanceEc2("", el))
            end #if
        end #for
        info_ec2.julianodes
    end #func


    function addprocs_ec2(n::Int, useidle::Bool)
        println("Don't be suprised if it takes a while to check connectivity")
        #find idle machines, start those before creating new machines
        if(useidle)
            temp = info_ec2.machines[:]
            for i = info_ec2.julianodes
                remove(temp, i)
            end #for
            m = length(temp)
            println("Found ", length(temp) , " idle machines")
            if m >= n
                println("Using Machines: ", temp[1:1+n])
                startnodes_ec2(temp[1:1+n])
                n = 0
            else
                println("Using Machines: ", temp)
                startnodes_ec2(temp)
                n = n-m
            end
        end #if

        prevsize = length(info_ec2.machines)
        for i = 1:n
            newmachine_ec2()
        end
        println("previous size ",prevsize," n ",n, " julianodes size ",length(info_ec2.julianodes))
        startnodes_ec2(info_ec2.machines[prevsize+1:prevsize+n])
        info_ec2.julianodes
    end #func

    function findip(id::String)
        info = split(readall(`ec2-describe-instances -K $ec2pk -C $ec2cert`), info_ec2.splitset, false)

        for j = 1:numel(info)
            if(info[j] == "Instance" && inf0[j+1] == id)
                ip = info[j+3]
            end #if
        end #for
        
        remove(info.machines,InstanceEc2("",id))
        push(info.machines, InstanceEc2(ip,id))
        ip
    end #func

    function loadmachines_ec2() 
        ### add feature to figure out what the head node id is ###
        newarray = {}

        #info = split(readall(`ec2-describe-instances -K /home/ec2-user/pk-RGFHMGBBL4X6I6CBFNPYNKOPUXWP35CJ.pem -C /home/ec2-user/cert-RGFHMGBBL4X6I6CBFNPYNKOPUXWP35CJ.pem --filter "vpc-id=$vpc"`), info_ec2.splitset, false)
        ec2pk = info_ec2.ec2_cert_pk_path
        ec2cert = info_ec2.ec2_cert_path
        info = split(readall(`ec2-describe-instances -K $ec2pk -C $ec2cert`), info_ec2.splitset, false)
        for i = 1:numel(info)
            if(info[i] == "INSTANCE")
                id = info[i+1]
                ip = info[i+3]
                ## The machine your working on is considered the head node
                ## and is not in the machines array.
                if( id != info_ec2.headnode.id)
                    temp = InstanceEc2(ip,id)
                    println("Found instance: ",temp)
                    push(newarray, temp)
                else
                    head = InstanceEc2(ip,id)
                    println("Found Head Node: ", head)
                    info_ec2.headnode = head
                end #if/else
            end #if
        end #for
        info_ec2.machines = newarray
    end #func

    function startnodes_ec2(machines::Array)
        for machine = machines
            ip = machine.ip
            wait_ping(ip)
            addprocs_ssh(ip, info_ec2.ssh_key_path)
            push(info_ec2.julianodes, machine)
        end #for
    end #func


    function setheadnode_ec2(loc::Int) 
        info_ec2.headnode = info_ec2.machines[loc]
        del(info_ec2.machines, loc)
    end #func
     ## functions called with default args

    

    newmachine_ec2() = 
        newmachine_ec2(info_ec2.ec2_cert_pk_path, info_ec2.ec2_cert_path, info_ec2.ssh_key_name, info_ec2.def_inst_size, info_ec2.ami_id, info_ec2.stopping_behavior, info_ec2.group_id)

    killmachine_ec2(id::String) = killmachine_ec2(id,info_ec2.ec2_cert_pk_path, info_ec2.ec2_cert_path)

    killprocs_ec2(ids) = killprocs_ec2(ids, true)

    addprocs_ec2(n) = addprocs_ec2(n,true)
    


end #let

   

