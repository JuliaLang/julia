### EC2 Infrastrucure ###


type InstanceEc2 <: InstanceCloud
    ip::String
    id::String

    InstanceEc2() = new ("","")
end #type

function show(inst::InstanceEc2)
     #print("IP: ", inst.ip," ID: ", inst.id)
     if inst.id != ""
        print(`ec2-describe-instances $inst.id`)
        ## should add a line that parses if the output if the id is empty
     else
        print("this instance is empty")
     end #if/else
end #func


### VERY IMPORTANT to note that i don't check that id's match here
### since this information can always be retrieved

function ==(inst1::InstanceEc2, inst2::InstanceEc2)
    return inst1.ip == inst2.ip 
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
   

    InfoEc2() = new ({}, {}, InstanceEc2(),"ec2", Set(' ','\n','\t') , "", "", "", "", "", {}, "", "", "", "", "")
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

#instance ip addresses as strings
let dummy = nothing

    ### Configurables ###

    #Certificates
    info_ec2.ec2_cert_path = "/home/ec2-user/cert-RGFHMGBBL4X6I6CBFNPYNKOPUXWP35CJ.pem"
    info_ec2.ec2_cert_pk_path = "/home/ec2-user/pk-RGFHMGBBL4X6I6CBFNPYNKOPUXWP35CJ.pem"

    #ssh key 
    info_ec2.ssh_key_path = "/home/ec2-user/Trial.pem"
    info_ec2.ssh_key_name = "Trial"

    #ip generation
    info_ec2.subnet_mask = "10.0.0."
    info_ec2.ip_ranges = [17:50]
    headnode_lastnumber = "17"

    #other options
    info_ec2.def_inst_size = "m1.small"
    info_ec2.stopping_behavior = "terminate"
    info_ec2.ami_id = "ami-71ff3e18"
    info_ec2.group_id = "sg-3f889a53"
    info_ec2.subnet_id = "subnet-694a6a00"



    ### Private Functions ###

    function remove(array::Array, element)
        for i = 1:numel(array)
            if (element == array[i])
                del(array, i)
                break
            end #if
        end #for
    end    

    function generateip(subnet_mask::String,ip_ranges::String)
        toreturn = strcat(subnet_mask,ip_ranges[1])
        del(ip_ranges,1)
        return toreturn
    end #func

    function wait_ping(host::String)
        print("checking connectivity to $host"); flush(stdout_stream)
        key = info_ec2.ssh_key_path
        while true
            cmd = `ssh -i $key $host true`
            read_from(cmd); read_from(stderr(cmd))
            if run(cmd)
                break
            end
            print("."); flush(stdout_stream)
            sleep(0.5)
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

    ## note that it is kinda akward that start nodes takes and array, and killnode takes only a string   

    function killmachine_ec2(id::String,ec2_cert_pk_path::String, ec2_cert_path::String)
        toreturn = ""
        for i = 1:numel(info_ec2.machines)
           if(info_ec2.machines[i].ip == ip)
               id = info_ec2.machines[i].id
               try
                   toreturn = readall(`ec2-terminate-instances -K $ec2_cert_pk_path -C $ec2_cert_path $id`)
                   del(info_ec2.machines, i)
                   ## incase it still exists
                   remove(info_ec2.julianodes, InstanceEc2(ip,id))
               catch e
                    toreturn = strcat("There was an error in trying to terminate: ", show(info_ec2.julianodes[i]), "\n",show(e))
               end #try/catch
               break
           end #if
       end #for
       return toreturn
    end #func

    function newmachine_ec2(ec2_cert_pk_path::String, ec2_cert_path::String, ssh_key_name::String, inst_size::String, ami_id::String, ipaddress::String, stopping_behavior::String, subnet_id::String, group_id::String)
        ##ec2-command
        try 
            output = readall(`ec2-run-instances -K $ec2_cert_pk_path -C $ec2_cert_path -k $ssh_key_name -t $inst_size $ami_id --private-ip-address $ipaddress --instance-initiated-shutdown-behavior $stopping_behavior -s $subnet_id -g $group_id`)
            
            #find the instance name
            println("the EC2 output: ",output)
            output = split(output,info_ec2.splitset,false)
            for i = 1:numel(output)
                if(output[i] == "INSTANCE")
                    id = output[i+1]
                    push( info_ec2.machines, InstanceEc2(ipaddress,id))
                    break
                end #if
            end #for
        catch e
            println("There was an error, in creating a new virtual instance. Try running the loadmachines method")
            println(e)
        end #try/catch
    end #func

    function runningnodes_ec2()
        info_ec2.julianodes
    end #func

    function killnode_ec2(ip::String)
        println("must find kill julia procs via ssh")
        remove(info_ec2.julianodes, InstanceEc2(ip, ""))
    end #func

    killprocs_ec2(ips) = killprocs_ec2(ips, true)

    function killprocs_ec2(ips, killmachine::Bool)
        for ip = ips
            killnode_ec2(ip)
            if (killmachine)
                killmachine_ec2(ip)
                remove(info_ec2.machines, InstanceEc2(ip, ""))
            end #if
        end #for
        info_ec2.julianodes
    end #func

    addprocs_ec2(n) = addprocs_ec2(n,true)

    function addprocs_ec2(n::Int, useidle::Bool)
        #find idle machines, start those before creating new machines
        if(useidle)
            temp = info_ec2.machines[:]
            for i = info_ec2.machines
                remove(temp, i)
            end #for
            m = length(temp)
            println("Found ", length(temp) , " idle machines")
            if m >= n
                startnodes_ec2(temp[1:1+n])
                n = 0
            else
                startnodes_ec2(temp)
                n = n-m
            end
        end #if

        prevsize = length(info_ec2.julianodes)
        for i = 1:n
            ip = generateip(info_ec2.subnet_mask,info_ec2.ip_ranges)
            newmachine_ec2(ip)
        end
        startnodes_ec2(info.ec2.julianodes[prevsize+1:prevsize+n])
        info_ec2.julianodes
    end #func

    function loadmachines_ec2() 
        newarray = []
        info = split(readall(`ec2-describe-instances -K /home/ec2-user/pk-RGFHMGBBL4X6I6CBFNPYNKOPUXWP35CJ.pem -C /home/ec2-user/cert-RGFHMGBBL4X6I6CBFNPYNKOPUXWP35CJ.pem --filter "private-ip-address=*" --filter "instance-id=*" --filter "group-id=$info_ec2.group_id"`), info_ec2.splitset, false)
        for i = 1:numel(info)
                if(info[i] == "INSTANCE")
                    id = info[i+1]
                    ip = info[i+10]
                    ## The machine your working on is considered the head node
                    ## and is in the machines array.
                    if( ip != info_ec2.headnode.ip)
                        push(newarray, InstanceEc2(ip,id))
                    else
                        info_ec2.headinfo = InstanceEc2(ip,id)
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

     ## functions called with default args

    newmachine_ec2(ipaddress::String) = 
        newmachine_ec2(info_ec2.ec2_cert_pk_path, info_ec2.ec2_cert_path, info_ec2.ssh_key_name, info_ec2.def_inst_size, info_ec2.ami_id, ipaddress,info_ec2.stopping_behavior, info_ec2.subnet_id, info_ec2.group_id)

    killmachine_ec2(id::String) = killmachine_ec2(id,info_ec2.ec2_cert_pk_path, info_ec2.ec2_cert_path)


    ### Setup ###
    info_ec2.headnode = InstanceEc2(strcat(info_ec2.subnet_mask,headnode_lastnumber), "not set")
    remove(info_ec2.ip_ranges, headnode_lastnumber)


end #let

   

