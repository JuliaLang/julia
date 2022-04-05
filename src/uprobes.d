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

    probe rt__run__task(uint task);
    probe rt__pause__task(uint task);
    probe rt__new__task(uint parent, uint child);
    probe rt__start__task(uint task);
    probe rt__finish__task(uint task);
    probe rt__start__process__events(uint task);
    probe rt__finish__process__events(uint task);
    probe rt__sleep__check__wake(jl_ptls_t other, int8_t old_state);
    probe rt__sleep__check__wakeup(jl_ptls_t ptls);
    probe rt__sleep__check__sleep(jl_ptls_t ptls);
    probe rt__sleep__check__taskq__wake(jl_ptls_t ptls);
    probe rt__sleep__check__task__wake(jl_ptls_t ptls);
    probe rt__sleep__check__uv__wake(jl_ptls_t ptls);

    probe rt__start__typeinf();
    probe rt__finish__typeinf();
    probe rt__start__codegen();
    probe rt__finish__codegen();
    probe rt__start__compile();
    probe rt__finish__compile();
    probe rt__start__llvmopt();
    probe rt__finish__llvmopt();
};

#pragma D attributes Evolving/Evolving/Common provider julia provider
#pragma D attributes Evolving/Evolving/Common provider julia module
#pragma D attributes Evolving/Evolving/Common provider julia function
#pragma D attributes Evolving/Evolving/Common provider julia name
#pragma D attributes Evolving/Evolving/Common provider julia args
