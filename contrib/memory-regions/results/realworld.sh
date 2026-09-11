#!/bin/bash
# The real-world matrix: the stock collector (its own heuristics, and the
# program's schedule - GC.gc(false) at the census cadence, GC.gc(true) at
# the end) against the regions with and without the census, at recording-class (W=200 words) and light (W=3)
# garbage per event. What it measures is Julia and its collector under the
# best case a hard real-time loop can arrange on an unprivileged Linux
# machine, not the OS:
#   - pinned to one core (CORE), so the run never migrates;
#   - the heap claimed and prefaulted before the loop (RESERVE MB, every
#     column alike), so no event takes a page fault - the log's "page
#     faults during the run" line must read 0;
#   - the real-time class (SCHED_FIFO, RTPRIO) when the machine grants one,
#     and the memory locked when ulimit -l allows; the log says which;
#   - OS preemption attributed per 10 000-event block from the thread's
#     involuntary-switch count, and reported beside the raw maximum as
#     "max, no preemption"; each configuration runs up to TRIES times and
#     the run with the fewest involuntary switches is the one kept.
# Interrupt time on the core can not be excluded without root; an isolated
# core (isolcpus/nohz_full, or a cpuset) and a real-time class (rtprio)
# are the administrator's part.
#   ./realworld.sh            writes log/realworld_*.log and prints a summary
#   CORE=3 RESERVE=1024 ./realworld.sh
set -uo pipefail
cd "$(dirname "$0")"
JULIA="${JULIA:-$(cd ../../.. && pwd)/usr/bin/julia}"; EV="${EV:-5000000}"; K=10000
CORE="${CORE:-29}"; RESERVE="${RESERVE:-512}"; TRIES="${TRIES:-5}"; RTPRIO="${RTPRIO:-50}"
# The real-time class, when the machine grants one (ulimit -r > 0): under
# SCHED_FIFO no time-shared task preempts the loop, and the log's scheduler
# line proves which class the run got.
if chrt -f "$RTPRIO" true 2>/dev/null; then RT="chrt -f $RTPRIO"; else
  RT=""; echo "no real-time class (ulimit -r = $(ulimit -r)): runs are SCHED_OTHER, preemption attributed per block"; fi
mkdir -p log data
# run_kept <log> <args...>: runs census.jl up to TRIES times, keeps the try
# with the fewest involuntary context switches, and appends that try's TSV
# row (its header only the first time) to data/realworld.tsv.
run_kept() {
  local log=$1; shift; local best=999999; local kept_tsv=""
  for try in $(seq 1 "$TRIES"); do
    REGIONS_TSV="$log.try$try.tsv" timeout 300 $RT taskset -c "$CORE" "$JULIA" --startup-file=no ../bench/census.jl "$@" > "$log.try$try" 2>&1
    sw=$(grep -oE "involuntary context switches during the run: [0-9]+" "$log.try$try" | grep -oE "[0-9]+$")
    if [ "${sw:-999999}" -lt "$best" ]; then best=$sw; cp "$log.try$try" "$log"; kept_tsv="$log.try$try.tsv"; fi
    [ "$sw" = "0" ] && break
  done
  if [ -n "$kept_tsv" ] && [ -f "$kept_tsv" ]; then
    [ -f data/realworld.tsv ] || head -n1 "$kept_tsv" > data/realworld.tsv
    grep -v '^#' "$kept_tsv" >> data/realworld.tsv
  fi
  rm -f "$log".try*; echo "kept (switches=$best): $log"
}
for cfg in "200 100" "3 1000"; do
  set -- $cfg; W=$1; B=$2
  run_kept log/realworld_auto_W${W}.log                auto $EV 100000 $K $W $B $RESERVE data/ccdf_auto_W${W}.tsv
  run_kept log/realworld_sched_W${W}_B${B}.log         sched $EV 100000 $K $W $B $RESERVE data/ccdf_sched_W${W}.tsv
  run_kept log/realworld_real_census_W${W}_B${B}.log   real $EV 100000 $K $W $B $RESERVE data/ccdf_census_W${W}.tsv
  run_kept log/realworld_real_nocensus_W${W}_B${B}.log real $EV 0      $K $W $B $RESERVE data/ccdf_nocensus_W${W}.tsv
done
echo "=== summary"
for f in log/realworld_*.log; do
  echo "-- $f"
  grep -E "heap reserve|scheduler|memory locked|final full|wall time|involuntary|page faults|stock collections|collections ===|pause p50|pause max|^p50|^p99 |^p99.9 |^p99.99|^max|over 100us|peak RSS" "$f" | cut -c1-110
done
