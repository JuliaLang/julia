for (sym, (stdin, stdout, stderr)) in  {(:spawn_opts_inherit, (STDIN,STDOUT,STDERR)),
                       (:spawn_opts_swallow, (null_handle,null_handle,null_handle))}
@eval begin
 ($sym)(stdios::StdIOSet,exitcb::Callback,closecb::Callback) = (stdios,exitcb,closecb)
 ($sym)(stdios::StdIOSet,exitcb::Callback) = (stdios,exitcb,false)
 ($sym)(stdios::StdIOSet) = (stdios,false,false)
 ($sym)() = (($stdin,$stdout,$stderr),false,false)
 ($sym)(in::StreamOrNot) = ((in,$stdout,$stderr),false,false)
 ($sym)(in::StreamOrNot,out::StreamOrNot) = ((in,out,$stderr),false,false)
end
end
