use log::*;
use mmtk::plan::Plan;
use mmtk::util::constants::BYTES_IN_PAGE;
use mmtk::util::conversions;
use mmtk::util::heap::GCTriggerPolicy;
use mmtk::util::heap::SpaceStats;
use mmtk::MMTK;

use crate::JuliaVM;

use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use crate::jl_hrtime;

const DEFAULT_COLLECT_INTERVAL: usize = 5600 * 1024 * std::mem::size_of::<usize>();
const GC_ALWAYS_SWEEP_FULL: bool = false;

/// This tries to implement Julia-style GC triggering heuristics.
/// Note that for generational GC, Julia may trigger a full sweep collection for the following reasons:
/// 1. FULL_SWEEP_REASON_SWEEP_ALWAYS_FULL: always triggers a full sweep collection
/// 2. FULL_SWEEP_REASON_FORCED_FULL_SWEEP: if a full collection is forced by the user
/// 3. FULL_SWEEP_REASON_USER_MAX_EXCEEDED: i.e. heap_size > user_max
/// 4. FULL_SWEEP_REASON_LARGE_PROMOTION_RATE: using the size of the remembered set (promotion rate).
///
/// We support the reasons 1, 2, and 3. However, we do not support 4, since MMTk does not collect
/// promotion rate information about remembered sets.
/// For Immix, we obviously always do full heap collections.
pub struct JuliaGCTrigger {
    heap_target: AtomicUsize,
    max_total_memory: AtomicUsize,
    old_pause_time: AtomicUsize,
    old_mut_time: AtomicUsize,
    old_heap_size: AtomicUsize,
    old_alloc_diff: AtomicUsize,
    old_freed_diff: AtomicUsize,
    gc_start_time: AtomicUsize,
    gc_end_time: AtomicUsize,
    mutator_time: AtomicUsize,
    thrash_counter: AtomicUsize,
    thrashing: AtomicUsize,
    before_free_heap_size: AtomicUsize, // in bytes
    prev_sweep_full: AtomicBool,
    /// The number of pending allocation pages. The allocation requests for them have failed, and a GC is triggered.
    /// We will need to take them into consideration so that the new heap size can accomodate those allocations.
    pending_pages: AtomicUsize,
}

impl JuliaGCTrigger {
    pub fn new(max_total_mem: usize) -> Self {
        // values as defined in gc-stock.c
        Self {
            heap_target: AtomicUsize::new(DEFAULT_COLLECT_INTERVAL),
            max_total_memory: AtomicUsize::new(max_total_mem),
            old_pause_time: AtomicUsize::new(1e7 as usize),
            old_mut_time: AtomicUsize::new(1e9 as usize),
            old_heap_size: AtomicUsize::new(0),
            old_alloc_diff: AtomicUsize::new(DEFAULT_COLLECT_INTERVAL),
            old_freed_diff: AtomicUsize::new(DEFAULT_COLLECT_INTERVAL),
            gc_start_time: AtomicUsize::new(0),
            gc_end_time: AtomicUsize::new(0),
            mutator_time: AtomicUsize::new(0),
            thrash_counter: AtomicUsize::new(0),
            thrashing: AtomicUsize::new(0),
            before_free_heap_size: AtomicUsize::new(0),
            prev_sweep_full: AtomicBool::new(true),
            pending_pages: AtomicUsize::new(0),
        }
    }
}

impl GCTriggerPolicy<JuliaVM> for JuliaGCTrigger {
    fn on_gc_start(&self, mmtk: &'static MMTK<JuliaVM>) {
        // reserved pages now should include pending allocations
        let reserved_pages_now =
            mmtk.get_plan().get_reserved_pages() + self.pending_pages.load(Ordering::SeqCst);

        let now = unsafe { jl_hrtime() };
        self.gc_start_time.store(now as usize, Ordering::Relaxed);

        self.mutator_time.store(
            if self.gc_end_time.load(Ordering::Relaxed) == 0 {
                self.old_mut_time.load(Ordering::Relaxed)
            } else {
                self.gc_start_time.load(Ordering::Relaxed)
                    - self.gc_end_time.load(Ordering::Relaxed)
            },
            Ordering::Relaxed,
        );
        self.before_free_heap_size.store(
            conversions::pages_to_bytes(reserved_pages_now),
            Ordering::Relaxed,
        );

        self.prev_sweep_full.store(
            if let Some(gen) = mmtk.get_plan().generational() {
                gen.last_collection_full_heap()
            } else {
                false
            },
            Ordering::Relaxed,
        );
        info!(
            "On GC start: \
            reserved_pages_now = {}, \
            gc_start_time = {}, \
            mutator_time = {}, \
            before_free_heap_size = {}, \
            prev_sweep_full = {}",
            reserved_pages_now,
            self.gc_start_time.load(Ordering::Relaxed),
            self.mutator_time.load(Ordering::Relaxed),
            self.before_free_heap_size.load(Ordering::Relaxed),
            self.prev_sweep_full.load(Ordering::Relaxed),
        );
    }
    fn on_gc_end(&self, mmtk: &'static MMTK<JuliaVM>) {
        // note that we get the end time at this point but the actual end time
        // is recorded at collection::VMCollection::resume_mutators
        let gc_end_time = unsafe { jl_hrtime() };
        let pause = gc_end_time as usize - self.gc_start_time.load(Ordering::Relaxed);
        self.gc_end_time
            .store(gc_end_time as usize, Ordering::Relaxed);

        let reserved_pages_now =
            mmtk.get_plan().get_reserved_pages() + self.pending_pages.load(Ordering::SeqCst);
        let heap_size = conversions::pages_to_bytes(reserved_pages_now);

        let user_max = self.max_total_memory.load(Ordering::Relaxed) * 80 / 100;
        let alloc_diff = self.before_free_heap_size.load(Ordering::Relaxed)
            - self.old_heap_size.load(Ordering::Relaxed);
        let freed_diff = self.before_free_heap_size.load(Ordering::Relaxed) - heap_size;
        self.old_heap_size.store(heap_size, Ordering::Relaxed);

        // update the heap target only if the user did not force a GC
        let gc_auto = !mmtk.is_user_triggered_collection();
        if gc_auto {
            let mut target_allocs: f64 = 0.0;
            const ALLOC_SMOOTH_FACTOR: f64 = 0.95;
            const COLLECT_SMOOTH_FACTOR: f64 = 0.5;
            const TUNING_FACTOR: f64 = 2e4;
            let alloc_mem = mmtk_jl_gc_smooth(
                self.old_alloc_diff.load(Ordering::Relaxed),
                alloc_diff,
                ALLOC_SMOOTH_FACTOR,
            );
            let alloc_time = mmtk_jl_gc_smooth(
                self.old_mut_time.load(Ordering::Relaxed),
                self.mutator_time.load(Ordering::Relaxed),
                ALLOC_SMOOTH_FACTOR,
            );
            let gc_mem = mmtk_jl_gc_smooth(
                self.old_freed_diff.load(Ordering::Relaxed),
                freed_diff,
                COLLECT_SMOOTH_FACTOR,
            );
            let gc_time = mmtk_jl_gc_smooth(
                self.old_pause_time.load(Ordering::Relaxed),
                pause as usize, // note that we the stock GC discounts the sweep time
                COLLECT_SMOOTH_FACTOR,
            );

            self.old_alloc_diff.store(alloc_mem, Ordering::Relaxed);
            self.old_mut_time.store(alloc_time, Ordering::Relaxed);
            self.old_freed_diff.store(gc_mem, Ordering::Relaxed);
            self.old_pause_time.store(gc_time, Ordering::Relaxed);

            // thrashing estimator: if GC time more than 50% of the runtime
            if pause > self.mutator_time.load(Ordering::Relaxed)
                && self.thrash_counter.load(Ordering::Relaxed) >= 4
            {
                self.thrash_counter.fetch_add(1, Ordering::Relaxed);
            } else if self.thrash_counter.load(Ordering::Relaxed) > 0 {
                self.thrash_counter.fetch_sub(1, Ordering::Relaxed);
            }

            if alloc_mem != 0 && alloc_time != 0 && gc_mem != 0 && gc_time != 0 {
                let alloc_rate = alloc_mem as f64 / alloc_time as f64;
                let gc_rate = gc_mem as f64 / gc_time as f64;
                target_allocs = (heap_size as f64 * alloc_rate / gc_rate).sqrt() * TUNING_FACTOR;
            }

            if self.thrashing.load(Ordering::Relaxed) == 0
                && self.thrash_counter.load(Ordering::Relaxed) >= 3
            {
                // require 3 consecutive thrashing cycles to force the default allocator rate
                self.thrashing.store(1, Ordering::Relaxed);
                // and require 4 default allocations to clear
                self.thrash_counter.store(6, Ordering::Relaxed);
            } else if self.thrashing.load(Ordering::Relaxed) == 1
                && self.thrash_counter.load(Ordering::Relaxed) <= 2
            {
                self.thrashing.store(0, Ordering::Relaxed);
            }

            let mut target_heap = target_allocs + heap_size as f64;

            // compute some guardrails values
            let mut min_target_allocs = heap_size / 20; // 5% of heap size
            if min_target_allocs < DEFAULT_COLLECT_INTERVAL / 8 {
                // unless heap is small
                min_target_allocs = DEFAULT_COLLECT_INTERVAL / 8;
            }
            let mut max_target_allocs = mmtk_overallocation(
                self.before_free_heap_size.load(Ordering::Relaxed),
                heap_size,
                user_max,
            );
            if max_target_allocs < min_target_allocs {
                max_target_allocs = min_target_allocs;
            }

            if target_heap > user_max as f64 {
                target_allocs = if heap_size < user_max {
                    user_max as f64 - heap_size as f64
                } else {
                    1.0
                };
                info!(
                    "Heap size exceeds user limit, target_allocs = {}",
                    target_allocs
                );
            }

            if self.thrashing.load(Ordering::Relaxed) != 0 {
                let thrashing_allocs = (min_target_allocs as f64 * max_target_allocs as f64).sqrt();
                if target_allocs < thrashing_allocs {
                    target_allocs = thrashing_allocs;
                }
                info!("Thrashing detected, target_allocs = {}", target_allocs);
            }

            // then add the guardrails for transient issues
            if target_allocs > max_target_allocs as f64 {
                target_allocs = max_target_allocs as f64;
                info!(
                    "Target allocs exceeds rate limit max, target_allocs = {}",
                    target_allocs
                );
            }

            if target_allocs < min_target_allocs as f64 {
                target_allocs = min_target_allocs as f64;
                info!(
                    "Target allocs below rate limit min, target_allocs = {}",
                    target_allocs
                );
            }

            target_heap = target_allocs + heap_size as f64;
            if target_heap < DEFAULT_COLLECT_INTERVAL as f64 {
                target_heap = DEFAULT_COLLECT_INTERVAL as f64;
                info!(
                    "Target heap set to default interval, target_heap = {}",
                    target_heap
                );
            }
            self.heap_target
                .store(target_heap as usize, Ordering::Relaxed);
        }

        let mut sweep_full = false;

        // FULL_SWEEP_REASON_SWEEP_ALWAYS_FULL
        if GC_ALWAYS_SWEEP_FULL {
            trace!("Always do a full heap collection.");
            sweep_full = true;
        }

        // FULL_SWEEP_REASON_USER_MAX_EXCEEDED
        if heap_size > user_max {
            trace!("Heap size exceeds user limit");
            sweep_full = true;
        }

        // FULL_SWEEP_REASON_FORCED_FULL_SWEEP
        if !gc_auto && !self.prev_sweep_full.load(Ordering::Relaxed) {
            trace!("Forced full heap collection.");
            sweep_full = true;
        }

        if sweep_full {
            if let Some(gen) = mmtk.get_plan().generational() {
                // Force full heap in the next GC
                gen.force_full_heap_collection();
            }
        }
    }

    fn on_pending_allocation(&self, pages: usize) {
        self.pending_pages.fetch_add(pages, Ordering::SeqCst);
    }

    /// Is a GC required now?
    fn is_gc_required(
        &self,
        space_full: bool,
        space: Option<SpaceStats<JuliaVM>>,
        plan: &dyn Plan<VM = JuliaVM>,
    ) -> bool {
        if self.is_heap_full(plan) {
            return true;
        }

        plan.collection_required(space_full, space)
    }

    /// Is current heap full?
    fn is_heap_full(&self, plan: &dyn Plan<VM = JuliaVM>) -> bool {
        // reserved pages now should include pending allocations
        let reserved_pages_now =
            plan.get_reserved_pages() + self.pending_pages.load(Ordering::SeqCst);

        let heap_size = conversions::pages_to_bytes(reserved_pages_now);

        trace!(
            "Heap size = {}, heap target = {}",
            heap_size,
            self.heap_target.load(Ordering::Relaxed)
        );

        heap_size >= self.heap_target.load(Ordering::Relaxed)
    }

    /// Return the current heap size (in pages)
    fn get_current_heap_size_in_pages(&self) -> usize {
        self.heap_target.load(Ordering::Relaxed) / BYTES_IN_PAGE
    }

    /// Return the upper bound of heap size
    fn get_max_heap_size_in_pages(&self) -> usize {
        self.max_total_memory.load(Ordering::Relaxed) / BYTES_IN_PAGE
    }

    /// Can the heap size grow?
    fn can_heap_size_grow(&self) -> bool {
        true
    }
}

// copy of jl_gc_smooth from gc-stock.c
fn mmtk_jl_gc_smooth(old_val: usize, new_val: usize, factor: f64) -> usize {
    let est = factor * old_val as f64 + (1.0 - factor) * new_val as f64;
    if est <= 1.0 {
        1_usize // avoid issues with <= 0
    } else if est > (2usize << 36) as f64 {
        2usize << 36 // avoid overflow
    } else {
        est as usize
    }
}

// copy of overallocation from gc-stock.c
// an overallocation curve inspired by array allocations
// grows very fast initially, then much slower at large heaps
fn mmtk_overallocation(old_val: usize, val: usize, max_val: usize) -> usize {
    // compute maxsize = maxsize + 4*maxsize^(7/8) + maxsize/8
    // for small n, we grow much faster than O(n)
    // for large n, we grow at O(n/8)
    // and as we reach O(memory) for memory>>1MB,
    // this means we end by adding about 10% of memory each time at most
    let exp2 = usize::BITS as usize - old_val.leading_zeros() as usize;
    let inc = (1usize << (exp2 * 7 / 8)) * 4 + old_val / 8;
    // once overallocation would exceed max_val, grow by no more than 5% of max_val
    if inc + val > max_val && inc > max_val / 20 {
        max_val / 20
    } else {
        inc
    }
}
