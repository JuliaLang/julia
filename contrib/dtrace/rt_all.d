BEGIN
{
    printf("Tracing Julia Task events... Hit Ctrl-C to end.");
}

julia$target:::rt-run-task
{
    printf("Task running: %x", arg0);
}

julia$target:::rt-pause-task
{
    printf("Task pausing: %x", arg0);
}

julia$target:::rt-new-task
{
    printf("Task created: %x (Parent %x)", arg1, arg0);
}

julia$target:::rt-start-task
{
    printf("Task started: %x", arg0);
}

julia$target:::rt-finish-task
{
    printf("Task finished: %x", arg0);
}

julia$target:::rt-start-process-events
{
    printf("Task processing libuv events: %x", arg0);
}

julia$target:::rt-finish-process-events
{
    printf("Task processed libuv events: %x", arg0);
}

// unused in the runtime, so dtrace complains
// julia$target:::rt-taskq-insert
// {
//     printf("Thread %x inserting task to multiq: %x", arg0, arg1);
// }
//
// julia$target:::rt-taskq-get
// {
//     printf("Thread %x popped task from multiq: %x", arg0, arg1);
// }

julia$target:::rt-sleep-check-wake
{
    printf("Thread waking: %x (was sleeping?: %d)", arg0, arg1);
}

julia$target:::rt-sleep-check-wakeup
{
    printf("Thread wakeup: %x", arg0);
}

julia$target:::rt-sleep-check-sleep
{
    printf("Thread trying to sleep: %x", arg0);
}

julia$target:::rt-sleep-check-taskq-wake
{
    printf("Thread waking due to non-empty task queue: %x", arg0);
}

julia$target:::rt-sleep-check-task-wake
{
    printf("Thread waking due to popped task: %x", arg0);
}

julia$target:::rt-sleep-check-uv-wake
{
    printf("Thread waking due to libuv: %x", arg0);
}
