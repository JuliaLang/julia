/* Julia DTrace provider */

provider julia {
    probe gc__begin(int collection);
    probe gc__stop_the_world();
    probe gc__mark__begin();
    probe gc__mark__end(int64_t scanned_bytes, int64_t perm_scanned_bytes);
    probe gc__sweep__begin(int full);
    probe gc__sweep__end();
    probe gc__end();
    probe gc__finalizer();

    probe rt__run__task(jl_task_t *task);
    probe rt__pause__task(jl_task_t *task);
    probe rt__new__task(jl_task_t *parent, jl_task_t *child);
};

#pragma D attributes Evolving/Evolving/Common provider julia provider
#pragma D attributes Evolving/Evolving/Common provider julia module
#pragma D attributes Evolving/Evolving/Common provider julia function
#pragma D attributes Evolving/Evolving/Common provider julia name
#pragma D attributes Evolving/Evolving/Common provider julia args
