Module.preRun.push(function() {
    if (typeof Asyncify !== "undefined") {
        Asyncify.instrumentWasmExports = function (exports) { return exports; };
        Asyncify.handleSleep = function (startAsync) {
            if (ABORT) return;
            Module['noExitRuntime'] = true;
            if (Asyncify.state === Asyncify.State.Normal) {
                // Prepare to sleep. Call startAsync, and see what happens:
                // if the code decided to call our callback synchronously,
                // then no async operation was in fact begun, and we don't
                // need to do anything.
                var reachedCallback = false;
                var reachedAfterCallback = false;
                var task = get_current_task();
                startAsync(function(returnValue) {
                assert(!returnValue || typeof returnValue === 'number'); // old emterpretify API supported other stuff
                if (ABORT) return;
                Asyncify.returnValue = returnValue || 0;
                reachedCallback = true;
                if (!reachedAfterCallback) {
                    // We are happening synchronously, so no need for async.
                    return;
                }
                schedule_and_wait(task);
                });
                reachedAfterCallback = true;
                if (!reachedCallback) {
                    Module['_jl_task_wait']();
                }
            } else if (Asyncify.state === Asyncify.State.Rewinding) {
                // Stop a resume.
                finish_schedule_task();
            } else {
                abort('invalid state: ' + Asyncify.state);
            }
            return Asyncify.returnValue;
        };
    }
});

function get_current_task() {
    return Module['_jl_get_current_task']();
}

function get_root_task() {
    return Module['_jl_get_root_task']();
}

function task_ctx_ptr(task) {
    return Module["_task_ctx_ptr"](task);
}

function ctx_save(ctx) {
    var stackPtr = stackSave();

    // Save the bottom of the C stack in the task context. It simultaneously
    // serves as the top of the asyncify stack.
    HEAP32[ctx + 4 >> 2] = stackPtr;

    Asyncify.state = Asyncify.State.Unwinding;
    Module['_asyncify_start_unwind'](ctx);
    if (Browser.mainLoop.func) {
        Browser.mainLoop.pause();
    }
}

function do_start_task(old_stack)
{
    try {
        // start_task is always the entry point for any task
        Module['_start_task']();
    } catch(e) {
        stackRestore(old_stack)
        if (e !== e+0 && e !== 'killed') throw e;
        maybe_schedule_next();
        return;
    }
    // Either unwind or normal exit. In either case, we're back at the main task
    if (Asyncify.state === Asyncify.State.Unwinding) {
        // We just finished unwinding for a sleep.
        Asyncify.state = Asyncify.State.Normal;
        Module['_asyncify_stop_unwind']();
    }
    stackRestore(old_stack);
    maybe_schedule_next();
}

function schedule_and_wait(task) {
    Module['_jl_schedule_task'](task);
    Module['_jl_task_wait']();
}

function finish_schedule_task() {
    Asyncify.state = Asyncify.State.Normal;
    Module['_asyncify_stop_rewind']();
}

next_ctx = 0;
next_need_start = true;
function set_next_ctx(ctx, needs_start) {
    next_ctx = ctx;
    next_need_start = needs_start;
}

function root_ctx() {
    return task_ctx_ptr(get_root_task())
}

function ctx_switch(lastt_ctx) {
    if (lastt_ctx == root_ctx()) {
        // If we're in the root context, switch to
        // the new ctx now, else we'll get there after
        // unwinding.
        return schedule_next()
    } else if (lastt_ctx == 0) {
        throw 'killed';
    } else {
        return ctx_save(lastt_ctx);
    }
}

function schedule_next()
{
    old_stack = stackSave();
    var next_task_stack = HEAP32[next_ctx + 4 >> 2];
    if (!next_need_start) {
        Asyncify.state = Asyncify.State.Rewinding;
        Module['_asyncify_start_rewind'](next_ctx);
        if (Browser.mainLoop.func) {
            Browser.mainLoop.resume();
        }
    }
    next_ctx = -1;
    stackRestore(next_task_stack);
    do_start_task(old_stack)
}

function maybe_schedule_next() {
    assert(next_ctx != -1);
    if (next_ctx == root_ctx() || next_ctx == 0) {
        return;
    }
    schedule_next()
}
