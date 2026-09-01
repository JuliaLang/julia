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

// Entries in the thread-local, marked and `to_finalize` finalizer lists may have tagged
// object pointers. These must match the `GC_FIN_*` defines in gc-common.h.
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
            let isfreed = !memory_manager::is_live_object(v);
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

/// Advance `i` to the next entry of `list` that is an object reference, returning
/// `(slot, object, tag)`.
fn next_finalizer_object(
    list: &ArrayListT,
    i: &mut usize,
) -> Option<(usize, ObjectReference, usize)> {
    while *i < list.len {
        let cur = list.get(*i);
        let slot = *i;
        if cur.is_zero() {
            *i += 1;
            continue;
        }
        let mut tag = 0;
        let obj = if gc_ptr_tag(cur, GC_FIN_CFUNC_TAG) {
            *i += 1;
            debug_assert!(*i < list.len);
            tag = GC_FIN_CFUNC_TAG;
            gc_ptr_clear_tag(cur, GC_FIN_CFUNC_TAG)
        } else {
            cur
        };
        if gc_ptr_tag(cur, GC_FIN_COBJ_TAG) {
            *i += 1;
            continue;
        }
        *i += 1;
        return Some((
            slot,
            unsafe { ObjectReference::from_raw_address_unchecked(obj) },
            tag,
        ));
    }
    None
}

/// The entries of `to_finalize`, as root nodes for the InitialMark snapshot.
pub fn to_finalize_root_nodes() -> Vec<ObjectReference> {
    let list = ArrayListT::to_finalize_list();
    let mut nodes = Vec::with_capacity(list.len / 2);
    let mut i = 0;
    while let Some((_, obj, _)) = next_finalizer_object(list, &mut i) {
        nodes.push(obj);
    }
    nodes
}

// gc_mark_finlist in gc.c
fn mark_finlist<T: ObjectTracer>(list: &mut ArrayListT, start: usize, tracer: &mut T) {
    if list.len <= start {
        return;
    }
    let mut i = start;
    while let Some((slot, obj, tag)) = next_finalizer_object(list, &mut i) {
        let traced = tracer.trace_object(obj);
        // if the object has moved, update the list, re-applying the tag
        list.set(slot, unsafe {
            Address::from_usize(traced.to_raw_address() | tag)
        });
    }
}
