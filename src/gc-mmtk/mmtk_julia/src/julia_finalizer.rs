use mmtk::memory_manager;
use mmtk::util::Address;
use mmtk::util::ObjectReference;
use mmtk::vm::ObjectTracer;
use mmtk::vm::VMBinding;
use mmtk::Mutator;

use crate::JuliaVM;

use crate::arraylist_grow;
use crate::jl_gc_get_have_pending_finalizers;
use crate::jl_gc_get_marked_finalizers_list;
use crate::jl_gc_get_thread_finalizer_list;
use crate::jl_gc_get_to_finalize_list;

// Entries in the thread-local and marked finalizer lists may have tagged object
// pointers. These must match the `GC_FIN_*` defines in gc-common.h.
/// The paired finalizer is an unboxed c function pointer.
pub const GC_FIN_CFUNC_TAG: usize = 0x1;
/// The object pointer is a c object pointer, not a `jl_value_t *`.  It must
/// have alignment >= 4 and will be finalized at the next quiescent period.
pub const GC_FIN_COBJ_TAG: usize = 0x2;
/// All bits used to tag finalizer list entries.
pub const GC_FIN_TAG_MASK: usize = GC_FIN_CFUNC_TAG | GC_FIN_COBJ_TAG;

/// This is a Rust implementation of finalizer scanning in _jl_gc_collect() in gc.c
pub fn scan_finalizers_in_rust<T: ObjectTracer>(tracer: &mut T) {
    use crate::mmtk::vm::ActivePlan;
    let to_finalize = ArrayListT::to_finalize_list();
    let marked_finalizers_list = ArrayListT::marked_finalizers_list();
    let jl_gc_have_pending_finalizers: *mut i32 = unsafe { jl_gc_get_have_pending_finalizers() };

    // Current length of marked list: we only need to trace objects after this length if this is a nursery GC.
    let mut orig_marked_len = marked_finalizers_list.len;

    // Sweep thread local list: if they are not alive, move to to_finalize.
    for mutator in <JuliaVM as VMBinding>::VMActivePlan::mutators() {
        let list = ArrayListT::thread_local_finalizer_list(mutator);
        sweep_finalizer_list(
            list,
            to_finalize,
            Some(marked_finalizers_list),
            jl_gc_have_pending_finalizers,
        );
    }

    // If this is a full heap GC, we also sweep marked list.
    if !crate::collection::is_current_gc_nursery() {
        sweep_finalizer_list(
            marked_finalizers_list,
            to_finalize,
            None,
            jl_gc_have_pending_finalizers,
        );
        orig_marked_len = 0;
    }

    // Go through thread local list again and trace objects
    for mutator in <JuliaVM as VMBinding>::VMActivePlan::mutators() {
        let list = ArrayListT::thread_local_finalizer_list(mutator);
        mark_finlist(list, 0, tracer);
    }
    // Trace new objects in marked list
    mark_finlist(marked_finalizers_list, orig_marked_len, tracer);
    // Trace objects in to_finalize (which are just pushed in sweeping thread local list)
    mark_finlist(to_finalize, 0, tracer);
}

/// This maps to arraylist_t in arraylist.h. Defining the type allows us to access the list in Rust.
/// typedef struct {
///     size_t len;
///     size_t max;
///     void **items;
///     void *_space[AL_N_INLINE];
/// } arraylist_t;
#[repr(C)]
struct ArrayListT {
    len: usize,
    max: usize,
    items: *mut Address,
    // There are one more field in the end but we dont use it. So omit it.
}

impl ArrayListT {
    // Some arraylist_t pointers used in finalizer implementation.

    /// ptls->finalizers: new finalizers are registered into this thread local list
    fn thread_local_finalizer_list(mutator: &mut Mutator<JuliaVM>) -> &mut ArrayListT {
        let list = unsafe { jl_gc_get_thread_finalizer_list(mutator.mutator_tls.0 .0) };
        unsafe { &mut *list.to_mut_ptr() }
    }
    /// to_finalize: objects that are dead are in this list waiting for finalization
    fn to_finalize_list<'a>() -> &'a mut ArrayListT {
        let list = unsafe { jl_gc_get_to_finalize_list() };
        unsafe { &mut *list.to_mut_ptr() }
    }
    /// finalizer_list_marked: objects that are alive and traced, thus we do not need to scan them again in future nursery GCs.
    fn marked_finalizers_list<'a>() -> &'a mut ArrayListT {
        let list = unsafe { jl_gc_get_marked_finalizers_list() };
        unsafe { &mut *list.to_mut_ptr() }
    }

    fn get(&self, i: usize) -> Address {
        debug_assert!(i < self.len);
        unsafe { *self.items.add(i) }
    }
    fn set(&mut self, i: usize, val: Address) {
        debug_assert!(i < self.len);
        unsafe { *self.items.add(i) = val }
    }
    fn push(&mut self, val: Address) {
        self.grow(1);
        self.set(self.len - 1, val);
    }
    fn grow(&mut self, n: usize) {
        let newlen = self.len + n;
        if newlen > self.max {
            // Call into C to grow the list.
            unsafe {
                arraylist_grow(Address::from_mut_ptr(self as _), n);
            }
        }
        self.len = newlen
    }
}

fn gc_ptr_clear_tag(addr: Address, tag: usize) -> Address {
    let addr = unsafe { Address::from_usize(addr & !tag) };
    debug_assert!(!addr.is_zero());
    addr
}

pub fn gc_ptr_tag(addr: Address, tag: usize) -> bool {
    addr & tag != 0
}

/// Drop every registered finalizer, so none is ever run.
///
/// LXR has no working finalizer path: it never calls `Scanning::process_weak_refs`, so lists
/// are never swept and, more importantly, `mark_finlist` never runs -- and that trace is what
/// keeps a registered object alive until its finalizer has been scheduled. Registered objects
/// are therefore reclaimed with live entries still naming them, and `jl_gc_run_all_finalizers`
/// at exit runs each one against recycled memory: an invalid `free` on the C path, a segfault
/// reading the argument's type on the Julia path.
///
/// Until that path is built, drop the entries instead. This changes no observable behaviour --
/// LXR already never ran a finalizer -- it only stops the crash at exit. Weak references are
/// separately already treated as strong, so nothing here needs to clear them.
///
/// Called at the *start* of `release`, on every pause, which is what makes it airtight:
/// reclamation happens later in the same pause (`STWRCDecsAndSweep`) or in a later one, so an
/// entry is always dropped before the object it names can be freed.
///
/// This leaks whatever the finalizers would have released -- GMP limbs, file descriptors,
/// malloc'd buffers. Acceptable for bring-up, not for a real GC.
pub fn drop_all_finalizers() {
    use crate::mmtk::vm::ActivePlan;
    use mmtk::vm::VMBinding;
    for mutator in <JuliaVM as VMBinding>::VMActivePlan::mutators() {
        ArrayListT::thread_local_finalizer_list(mutator).len = 0;
    }
    ArrayListT::marked_finalizers_list().len = 0;
    ArrayListT::to_finalize_list().len = 0;
}

/// Whether a finalizer list entry is still live, i.e. must stay on the list.
///
/// `memory_manager::is_live_object` dispatches to the owning policy, and `ImmixSpace::is_live`
/// has a reference-counting branch that no other plan takes. For an object that is *not*
/// marked -- which is every dead entry, and dead entries are exactly the ones this sweep is
/// looking for -- it falls through to `object_forwarding::is_forwarded`, a question about
/// evacuation state. LXR is built with `lxr_no_evac`, so nothing ever moves, that state is
/// never established, and reading it faults inside `side_metadata_access`. ConcurrentImmix
/// shares this whole file and never sees the problem because it never enters that branch.
///
/// So answer the question in LXR's own terms instead: objects the plan does not reference
/// count are never reclaimed and are always live; everything else is live if it is retained by
/// a count or was reached by the trace. No forwarding query, because nothing moves.
pub fn object_is_live(v: ObjectReference) -> bool {
    let plan = crate::SINGLETON.get_plan();
    if let Some(lxr) = plan.downcast_ref::<mmtk::plan::lxr::LXR<JuliaVM>>() {
        if !lxr.is_rc_object(v) {
            return true;
        }
        return lxr.rc.count(v) > 0 || lxr.is_marked(v);
    }
    memory_manager::is_live_object(v)
}

// sweep_finalizer_list in gc.c
fn sweep_finalizer_list(
    list: &mut ArrayListT,
    to_finalize: &mut ArrayListT,
    // finalizer_list_marked is None if list (1st parameter) is finalizer_list_marked.
    // Rust does not allow sending the same mutable reference as two different arguments (cannot borrow __ as mutable more than once at a time)
    mut finalizer_list_marked: Option<&mut ArrayListT>,
    jl_gc_have_pending_finalizers: *mut i32,
) {
    if list.len == 0 {
        return;
    }

    let mut i = 0;
    let mut j = 0;
    while i < list.len {
        let v0: Address = list.get(i);
        let v = unsafe {
            ObjectReference::from_raw_address_unchecked(gc_ptr_clear_tag(v0, GC_FIN_TAG_MASK))
        };
        if v0.is_zero() {
            i += 2;
            // remove from this list
            continue;
        }

        let fin = list.get(i + 1);
        let (isfreed, isold) = if gc_ptr_tag(v0, GC_FIN_COBJ_TAG) {
            (true, false)
        } else {
            let isfreed = !object_is_live(v);
            let isold = finalizer_list_marked.is_some() && !isfreed;
            (isfreed, isold)
        };
        if isfreed || isold {
            // remove from this list
        } else {
            if j < i {
                list.set(j, list.get(i));
                list.set(j + 1, list.get(i + 1));
            }
            j += 2;
        }
        if isfreed {
            to_finalize.push(v0);
            to_finalize.push(fin);
            unsafe {
                *jl_gc_have_pending_finalizers = 1;
            }
        }
        if isold {
            let finalizer_list_marked = finalizer_list_marked.as_mut().unwrap();
            finalizer_list_marked.push(v0);
            finalizer_list_marked.push(fin);
        }
        i += 2;
    }

    list.len = j;
}

// gc_mark_finlist in gc.c
fn mark_finlist<T: ObjectTracer>(list: &mut ArrayListT, start: usize, tracer: &mut T) {
    if list.len <= start {
        return;
    }

    let mut i = start;
    while i < list.len {
        let cur = list.get(i);
        let cur_i = i;
        let mut cur_tag: usize = 0;

        if cur.is_zero() {
            i += 1;
            continue;
        }

        let new_obj_addr = if gc_ptr_tag(cur, GC_FIN_CFUNC_TAG) {
            // Skip next
            i += 1;
            debug_assert!(i < list.len);
            cur_tag = GC_FIN_CFUNC_TAG;
            gc_ptr_clear_tag(cur, GC_FIN_CFUNC_TAG)
        } else {
            // unsafe: We checked `cur.is_zero()` before.
            cur
        };
        if gc_ptr_tag(cur, GC_FIN_COBJ_TAG) {
            i += 1;
            continue;
        }

        let new_obj = unsafe { ObjectReference::from_raw_address_unchecked(new_obj_addr) };

        let traced = tracer.trace_object(new_obj);
        // if object has moved, update the list applying the tag
        list.set(cur_i, unsafe {
            Address::from_usize(traced.to_raw_address() | cur_tag)
        });

        i += 1;
    }
}
