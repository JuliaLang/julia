use log::{info, trace};
use mmtk::plan::Plan;
use mmtk::util::constants::BYTES_IN_PAGE;
use mmtk::util::conversions;
use mmtk::util::heap::{GCTriggerPolicy, SpaceStats};
use mmtk::util::os::{OSMemory, OS};
use mmtk::MMTK;

use crate::{jl_gc_get_hard_heap_limit, jl_gc_get_max_memory, jl_hrtime, JuliaVM};

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

const DEFAULT_COLLECT_INTERVAL: usize = 5600 * 1024 * std::mem::size_of::<usize>();
const GC_ALWAYS_SWEEP_FULL: bool = false;
const ALLOC_SMOOTH_FACTOR: f64 = 0.95;
const COLLECT_SMOOTH_FACTOR: f64 = 0.5;
const TUNING_FACTOR: f64 = 2e4;

/// Julia-style heap sizing heuristics for MMTk.
///
/// `max_total_memory` is a soft limit derived from Julia's heap size hint logic.
/// `hard_heap_limit` is a hard post-GC limit that overrides the dynamic target.
pub struct JuliaGCTrigger {
    heap_target: AtomicUsize,
    max_heap_size: AtomicUsize,
    max_total_memory: AtomicUsize,
    hard_heap_limit: AtomicUsize,
    old_pause_time: AtomicUsize,
    old_mut_time: AtomicUsize,
    old_heap_size: AtomicUsize,
    old_alloc_diff: AtomicUsize,
    old_freed_diff: AtomicUsize,
    gc_start_time: AtomicUsize,
    gc_end_time: AtomicUsize,
    mutator_time: AtomicUsize,
    thrash_counter: AtomicUsize,
    thrashing: AtomicBool,
    before_free_heap_size: AtomicUsize,
    next_sweep_full: AtomicBool,
    pending_pages: AtomicUsize,
    heap_size_after_last_full_gc: AtomicUsize,
}

impl JuliaGCTrigger {
    pub fn new() -> Self {
        let max_memory = unsafe { jl_gc_get_max_memory() };
        let total_memory = OS::get_system_total_memory().unwrap() as usize;
        let max_total_mem = std::cmp::min(max_memory, total_memory);
        let hard_heap_limit = unsafe { jl_gc_get_hard_heap_limit() };
        let initial_target = if hard_heap_limit != 0 {
            hard_heap_limit
        } else {
            DEFAULT_COLLECT_INTERVAL
        };

        Self {
            heap_target: AtomicUsize::new(initial_target),
            max_heap_size: AtomicUsize::new(total_memory),
            max_total_memory: AtomicUsize::new(max_total_mem),
            hard_heap_limit: AtomicUsize::new(hard_heap_limit),
            old_pause_time: AtomicUsize::new(1e7 as usize),
            old_mut_time: AtomicUsize::new(1e9 as usize),
            old_heap_size: AtomicUsize::new(0),
            old_alloc_diff: AtomicUsize::new(DEFAULT_COLLECT_INTERVAL),
            old_freed_diff: AtomicUsize::new(DEFAULT_COLLECT_INTERVAL),
            gc_start_time: AtomicUsize::new(0),
            gc_end_time: AtomicUsize::new(0),
            mutator_time: AtomicUsize::new(0),
            thrash_counter: AtomicUsize::new(0),
            thrashing: AtomicBool::new(false),
            before_free_heap_size: AtomicUsize::new(0),
            next_sweep_full: AtomicBool::new(false),
            pending_pages: AtomicUsize::new(0),
            heap_size_after_last_full_gc: AtomicUsize::new(0),
        }
    }

    fn user_max(&self) -> usize {
        self.max_total_memory.load(Ordering::Relaxed) * 80 / 100
    }

    fn maybe_force_full_heap(&self, mmtk: &'static MMTK<JuliaVM>) {
        if let Some(gen) = mmtk.get_plan().generational() {
            if self.next_sweep_full.load(Ordering::Relaxed) || GC_ALWAYS_SWEEP_FULL {
                gen.force_full_heap_collection();
            }
        }
    }
}

impl GCTriggerPolicy<JuliaVM> for JuliaGCTrigger {
    fn on_gc_start(&self, mmtk: &'static MMTK<JuliaVM>) {
        self.maybe_force_full_heap(mmtk);

        let reserved_pages_now =
            mmtk.get_plan().get_reserved_pages() + self.pending_pages.load(Ordering::SeqCst);
        let now = unsafe { jl_hrtime() } as usize;

        self.gc_start_time.store(now, Ordering::Relaxed);
        self.mutator_time.store(
            if self.gc_end_time.load(Ordering::Relaxed) == 0 {
                self.old_mut_time.load(Ordering::Relaxed)
            } else {
                now - self.gc_end_time.load(Ordering::Relaxed)
            },
            Ordering::Relaxed,
        );
        self.before_free_heap_size.store(
            conversions::pages_to_bytes(reserved_pages_now),
            Ordering::Relaxed,
        );

        trace!(
            "GC start: reserved_pages_now={}, mutator_time={}, before_free_heap_size={}, next_sweep_full={}",
            reserved_pages_now,
            self.mutator_time.load(Ordering::Relaxed),
            self.before_free_heap_size.load(Ordering::Relaxed),
            self.next_sweep_full.load(Ordering::Relaxed),
        );
    }

    fn on_gc_end(&self, mmtk: &'static MMTK<JuliaVM>) {
        let gc_end_time = unsafe { jl_hrtime() } as usize;
        let pause = gc_end_time - self.gc_start_time.load(Ordering::Relaxed);
        self.gc_end_time.store(gc_end_time, Ordering::Relaxed);

        let pending_pages = self.pending_pages.swap(0, Ordering::SeqCst);
        let reserved_pages_now = mmtk.get_plan().get_reserved_pages() + pending_pages;
        let heap_size = conversions::pages_to_bytes(reserved_pages_now);
        let user_max = self.user_max();
        let hard_heap_limit = self.hard_heap_limit.load(Ordering::Relaxed);

        let alloc_diff = self.before_free_heap_size.load(Ordering::Relaxed)
            - self.old_heap_size.load(Ordering::Relaxed);
        let freed_diff = self.before_free_heap_size.load(Ordering::Relaxed) - heap_size;
        self.old_heap_size.store(heap_size, Ordering::Relaxed);

        let gc_auto = !mmtk.is_user_triggered_collection();
        if gc_auto && hard_heap_limit == 0 {
            let mut target_allocs = 0.0;
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
                pause,
                COLLECT_SMOOTH_FACTOR,
            );

            self.old_alloc_diff.store(alloc_mem, Ordering::Relaxed);
            self.old_mut_time.store(alloc_time, Ordering::Relaxed);
            self.old_freed_diff.store(gc_mem, Ordering::Relaxed);
            self.old_pause_time.store(gc_time, Ordering::Relaxed);

            let thrash_counter = self.thrash_counter.load(Ordering::Relaxed);
            if pause > self.mutator_time.load(Ordering::Relaxed) && thrash_counter <= 4 {
                self.thrash_counter
                    .store(thrash_counter + 1, Ordering::Relaxed);
            } else if thrash_counter > 0 {
                self.thrash_counter
                    .store(thrash_counter - 1, Ordering::Relaxed);
            }

            if alloc_mem != 0 && alloc_time != 0 && gc_mem != 0 && gc_time != 0 {
                let alloc_rate = alloc_mem as f64 / alloc_time as f64;
                let gc_rate = gc_mem as f64 / gc_time as f64;
                target_allocs = (heap_size as f64 * alloc_rate / gc_rate).sqrt() * TUNING_FACTOR;
            }

            if !self.thrashing.load(Ordering::Relaxed)
                && self.thrash_counter.load(Ordering::Relaxed) >= 3
            {
                self.thrashing.store(true, Ordering::Relaxed);
                self.thrash_counter.store(6, Ordering::Relaxed);
            } else if self.thrashing.load(Ordering::Relaxed)
                && self.thrash_counter.load(Ordering::Relaxed) <= 2
            {
                self.thrashing.store(false, Ordering::Relaxed);
            }

            let mut target_heap = target_allocs + heap_size as f64;
            let mut min_target_allocs = heap_size / 20;
            if min_target_allocs < DEFAULT_COLLECT_INTERVAL / 8 {
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
                    (user_max - heap_size) as f64
                } else {
                    1.0
                };
            }

            if self.thrashing.load(Ordering::Relaxed) {
                let thrashing_allocs =
                    ((min_target_allocs as f64) * (max_target_allocs as f64)).sqrt();
                if target_allocs < thrashing_allocs {
                    target_allocs = thrashing_allocs;
                }
            }

            if target_allocs > max_target_allocs as f64 {
                target_allocs = max_target_allocs as f64;
            } else if target_allocs < min_target_allocs as f64 {
                target_allocs = min_target_allocs as f64;
            }

            target_heap = target_allocs + heap_size as f64;
            if target_heap < DEFAULT_COLLECT_INTERVAL as f64 {
                target_heap = DEFAULT_COLLECT_INTERVAL as f64;
            }
            self.heap_target
                .store(target_heap as usize, Ordering::Relaxed);
        } else if hard_heap_limit != 0 {
            self.heap_target.store(hard_heap_limit, Ordering::Relaxed);
        }

        if hard_heap_limit != 0 && heap_size > hard_heap_limit {
            eprintln!("Heap size exceeded hard limit of {hard_heap_limit} bytes.");
            std::process::abort();
        }

        let last_collection_full_heap = mmtk
            .get_plan()
            .generational()
            .is_some_and(|gen| gen.last_collection_full_heap());
        if !mmtk.get_plan().generational().is_some() || last_collection_full_heap {
            self.heap_size_after_last_full_gc
                .store(heap_size, Ordering::Relaxed);
        }

        let heap_size_after_last_full_gc =
            self.heap_size_after_last_full_gc.load(Ordering::Relaxed);
        let large_heap_growth = if heap_size_after_last_full_gc == 0 {
            false
        } else {
            let expected_heap_size = heap_size_after_last_full_gc
                + mmtk_overallocation(heap_size_after_last_full_gc, 0, usize::MAX);
            heap_size > expected_heap_size
        };
        let next_sweep_full = GC_ALWAYS_SWEEP_FULL || heap_size > user_max || large_heap_growth;
        self.next_sweep_full
            .store(next_sweep_full, Ordering::Relaxed);

        trace!(
            "GC end: heap_size={}, heap_target={}, user_max={}, hard_heap_limit={}, next_sweep_full={}",
            heap_size,
            self.heap_target.load(Ordering::Relaxed),
            user_max,
            hard_heap_limit,
            next_sweep_full,
        );

        if self.thrashing.load(Ordering::Relaxed) {
            info!(
                "GC thrashing detected: heap_size={}, heap_target={}",
                heap_size,
                self.heap_target.load(Ordering::Relaxed)
            );
        }
    }

    fn on_pending_allocation(&self, pages: usize) {
        self.pending_pages.fetch_add(pages, Ordering::SeqCst);
    }

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

    fn is_heap_full(&self, plan: &dyn Plan<VM = JuliaVM>) -> bool {
        let reserved_pages_now =
            plan.get_reserved_pages() + self.pending_pages.load(Ordering::SeqCst);
        let heap_size = conversions::pages_to_bytes(reserved_pages_now);
        let heap_target = self.heap_target.load(Ordering::Relaxed);

        trace!("Heap size = {}, heap target = {}", heap_size, heap_target);
        heap_size >= heap_target
    }

    fn get_current_heap_size_in_pages(&self) -> usize {
        self.heap_target.load(Ordering::Relaxed) / BYTES_IN_PAGE
    }

    fn get_max_heap_size_in_pages(&self) -> usize {
        let hard_heap_limit = self.hard_heap_limit.load(Ordering::Relaxed);
        let heap_limit = if hard_heap_limit != 0 {
            hard_heap_limit
        } else {
            self.max_heap_size.load(Ordering::Relaxed)
        };
        heap_limit / BYTES_IN_PAGE
    }

    fn can_heap_size_grow(&self) -> bool {
        true
    }
}

fn mmtk_jl_gc_smooth(old_val: usize, new_val: usize, factor: f64) -> usize {
    let est = factor * old_val as f64 + (1.0 - factor) * new_val as f64;
    if est <= 1.0 {
        1
    } else if est > (2usize << 36) as f64 {
        2usize << 36
    } else {
        est as usize
    }
}

fn mmtk_overallocation(old_val: usize, val: usize, max_val: usize) -> usize {
    let exp2 = usize::BITS as usize - old_val.leading_zeros() as usize;
    let inc = (1usize << (exp2 * 7 / 8)) * 4 + old_val / 8;
    if inc + val > max_val && inc > max_val / 20 {
        max_val / 20
    } else {
        inc
    }
}
