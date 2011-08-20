### EC2 Infrastrucure ###


type Ec2Instance
    ip::String
    id::String
end #type

function show(inst::Ec2Instance)
     print("IP: ", inst.ip," ID: ", inst.id)
end #func

type Ec2Info
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

    Ec2Info() = new ("", "", "", "", "", {""}, "", "", "", "", "")
end #type

function show(info::Ec2Info)
    println("                EC2 Default Paramters are")
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
end #func

#create info variable
ec2_info = Ec2Info()

#instance ip addresses as strings
let instances = {}

### Configurables ###

#Certificates
ec2_info.ec2_cert_path = "/home/ec2-user/cert-RGFHMGBBL4X6I6CBFNPYNKOPUXWP35CJ.pem"
ec2_info.ec2_cert_pk_path = "/home/ec2-user/pk-RGFHMGBBL4X6I6CBFNPYNKOPUXWP35CJ.pem"

#ssh key 
ec2_info.ssh_key_path = "/home/ec2-user/Trial.pem"
ec2_info.ssh_key_name = "Trial"

#ip generation
ec2_info.subnet_mask = "10.0.0."
ec2_info.ip_ranges = [18:50]

#other options
ec2_info.def_inst_size = "m1.small"
ec2_info.stopping_behavior = "terminate"
ec2_info.ami_id = "ami-71ff3e18"
ec2_info.group_id = "sg-3f889a53"
ec2_info.subnet_id = "subnet-694a6a00"


### Private Functions ###

function generateip(subnet_mask,ip_ranges)
    toreturn = strcat(subnet_mask,ip_ranges[1])
    del(ip_ranges,1)
    return toreturn
end #func

function kill_ec2_instance(ip)
   #run ec2 command if succsessful remove ip from instances
   #ec2-terminate-instance
   for i = 1:numel(instances)
       if(instances[i].ip == ip)
           name = instances[i].id
           println(ec2_terminate_instance(name))
           del(instances, i)
           break
       end #if
   end #for
end #func

function wait_ping(host)
    print("checking connectivity to $host"); flush(stdout_stream)
    key = ec2_info.ssh_key_path
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


## functions called with default args

create_ec2_instance(ipaddress) = 
    create_ec2_instance(ec2_info.ec2_cert_pk_path, ec2_info.ec2_cert_path, ec2_info.ssh_key_name, ec2_info.def_inst_size, ec2_info.ami_id, ipaddress,ec2_info.stopping_behavior, ec2_info.subnet_id, ec2_info.group_id)

ec2_terminate_instance(id) = ec2_terminate_instance(id,ec2_info.ec2_cert_pk_path, ec2_info.ec2_cert_path)


## ec2 wrapper functions

function ec2_run_instance(ec2_cert_pk_path, ec2_cert_path, ssh_key_name, inst_size, ami_id, ipaddress,
     stopping_behavior, subnet_id, group_id)
    readall(`ec2-run-instances -K $ec2_cert_pk_path -C $ec2_cert_path -k $ssh_key_name -t $inst_size $ami_id --private-ip-address $ipaddress --instance-initiated-shutdown-behavior $stopping_behavior -s $subnet_id -g $group_id`)
end #func

function ec2_terminate_instance(id,ec2_cert_pk_path, ec2_cert_path)
     readall(`ec2-terminate-instances -K $ec2_cert_pk_path -C $ec2_cert_path $id`)
end #func

### Global Functions ###
global ec2_instances
global ec2_instances_add
global addprocs_ec2
global killprocs_ec2
global create_ec2_instance

function create_ec2_instance(ec2_cert_pk_path, ec2_cert_path, ssh_key_name, inst_size, ami_id, ipaddress,
     stopping_behavior, subnet_id, group_id)
    ##ec2-command
    output = ec2_run_instance(ec2_cert_pk_path, ec2_cert_path, ssh_key_name, inst_size, ami_id, ipaddress, stopping_behavior, subnet_id, group_id)
    
    #find the instance name
    println("the EC2 output: ",output)
    output = split(output,Set(' ','\n'),false)
    for i = 1:numel(output)
        if(output[i] == "INSTANCE")
            id = output[i+1]
            push( instances, Ec2Instance(ipaddress,id))
            break
        end #if
    end #for
end #func

function ec2_instances()
    instances
end #func

function ec2_instances_add(x::Ec2Instance)
    push(instances, x)
end #func


function killprocs_ec2(ips)
    for ip = ips
        kill_ec2_instance(ip)
    end #for
    instances
end #func


function addprocs_ec2(n)
    prevsize = length(instances)
    for i = 1:n
        ip = generateip(ec2_info.subnet_mask,ec2_info.ip_ranges)
        create_ec2_instance(ip)
    end
    for i = 1:n
        ip = instances[prevsize+i].ip
        wait_ping(ip)
        addprocs_ssh(ip, ec2_info.ssh_key_path)
    end #for
    #println("New Machine(s) Created: ", length(instaces)-prevsize)
    println("YAY")
    instances
end #func

end #let
