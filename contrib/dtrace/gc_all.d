BEGIN
{
    printf("Tracing Julia GC times... Hit Ctrl-C to end and print results.\n");
}

julia$target:::gc-begin
{
    time[pid] = timestamp;
    start[pid] = timestamp;
}

julia$target:::gc-stop_the_world
/start[pid]/
{
    @stop_the_world_usecs[pid] = quantize((timestamp - time[pid]) / 1000);
    time[pid] = timestamp;
}

julia$target:::gc-end
/start[pid]/
{
    @gc_total_usecs[pid] = quantize((timestamp - start[pid]) / 1000);
    @gc_phase_usecs[pid] = quantize((timestamp - time[pid]) / 1000);
    time[pid] = timestamp;
    start[pid] = 0;
}

julia$target:::gc-finalizer
/time[pid]/
{
    @finalizer[pid] = quantize((timestamp - time[pid]) / 1000);
    time[pid] = 0;
}
