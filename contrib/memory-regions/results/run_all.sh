#!/bin/bash
# Every measurement of MEASUREMENTS.md, in order, each with its cores, its
# memory cap, its timeout, and its data file under data/. The logs go to
# log/ (not committed). plot.py then draws every plot from data/.
#
#   ./run_all.sh                   every row, M1 to M14
#   ONLY="M2 M5" ./run_all.sh      the rows named
#   ROUNDS=20 ./run_all.sh         more rounds of the paired cost rows
#
# Environment:
#   JULIA        the julia of this checkout (default ../../../usr/bin/julia)
#   VANILLA      a vanilla julia built at the base commit; M1 and M2 need it
#   GCBENCHMARKS a checkout of https://github.com/JuliaCI/GCBenchmarks; M1 needs it
#   CORE         the isolated core for the one-thread rows (default 29)
#   MTCORES      the cores of the multi-thread rows (default 24-31)
#   RTPRIO       the SCHED_FIFO priority for the latency rows when the machine
#                grants one (default 50); 0 turns it off
#   ROUNDS       the rounds of the paired cost rows M1, M2, M13 and M14
#                (default 10). A round runs both binaries, in an order that
#                alternates, so a drift of the machine moves both sides of a
#                round together; the interval of a row comes from the rounds.
#   GCTHREADS    the thread counts of M13 and M14 (default "1 4 8 16 32");
#                a count above the cores of the machine is skipped
#
# A row runs under `systemd-run --user --scope -p MemoryMax=…` when systemd
# is present and `USE_SCOPE` is not 0, under `timeout` always, and pinned
# with `taskset`. Set `USE_SCOPE=0` when the driver runs inside an isolated
# cpuset partition: a scope would move the row out of it. The
# latency rows (M3, M4, M6) take the real-time class when `chrt` grants it;
# the others never do: a FIFO thread that spins on one core starves the
# child process or the other threads of the same run.
set -uo pipefail
cd "$(dirname "$0")"
ROOT=$(cd ../../.. && pwd)
JULIA=${JULIA:-$ROOT/usr/bin/julia}
VANILLA=${VANILLA:-}
GCBENCHMARKS=${GCBENCHMARKS:-}
CORE=${CORE:-29}; MTCORES=${MTCORES:-24-31}; RTPRIO=${RTPRIO:-50}
ROUNDS=${ROUNDS:-10}
GCTHREADS=${GCTHREADS:-1 4 8 16 32}
ALLCORES=${ALLCORES:-0-31}
ONLY=${ONLY:-M1 M2 M3 M4 M5 M6 M7 M8 M9 M10 M11 M12 M13 M14}
DATA=data; LOG=log
mkdir -p "$DATA" "$LOG"
SHA=$(git -C "$ROOT" rev-parse --short=10 HEAD)

# --- the context every plot cites -------------------------------------------
{
    printf '# key\tvalue\n'
    printf 'date\t%s\n' "$(date -u +%Y-%m-%d)"
    printf 'sha\t%s\n' "$SHA"
    printf 'julia\t%s\n' "$("$JULIA" --startup-file=no -e 'print(VERSION)')"
    [ -n "$VANILLA" ] && printf 'vanilla\t%s\n' "$("$VANILLA" --startup-file=no -e 'print(Base.GIT_VERSION_INFO.commit[1:10])')"
    printf 'host\t%s\n' "$(hostname)"
    printf 'cpu\t%s\n' "$(grep -m1 'model name' /proc/cpuinfo | cut -d: -f2 | sed 's/^ *//')"
    printf 'kernel\t%s\n' "$(uname -r)"
    printf 'core\t%s\n' "$CORE"
    printf 'mtcores\t%s\n' "$MTCORES"
    printf 'rounds\t%s\n' "$ROUNDS"
    printf 'cores\t%s\n' "$(nproc)"
    printf 'isolated\t%s\n' "$(cat /sys/devices/system/cpu/isolated 2>/dev/null | tr -d '\n')"
    printf 'nohz_full\t%s\n' "$(cat /sys/devices/system/cpu/nohz_full 2>/dev/null | tr -d '\n')"
    printf 'governor\t%s\n' "$(cat /sys/devices/system/cpu/cpu"$CORE"/cpufreq/scaling_governor 2>/dev/null | tr -d '\n')"
    printf 'boost\t%s\n' "$(cat /sys/devices/system/cpu/cpufreq/boost 2>/dev/null | tr -d '\n')"
    if [ "$RTPRIO" -gt 0 ] && chrt -f "$RTPRIO" true 2>/dev/null; then printf 'realtime\tSCHED_FIFO %s\n' "$RTPRIO"
    else printf 'realtime\tnone\n'; fi
} > "$DATA/context.tsv"

# --- the runner ---------------------------------------------------------------
# A rerun of some rows (ONLY=...) appends to status.tsv: the last row of a
# name is the one that stands.
STATUS=$LOG/status.tsv
[ -s "$STATUS" ] || printf '# row\tname\texit\tseconds\n' > "$STATUS"
want() { case " $ONLY " in *" $1 "*) return 0;; *) return 1;; esac; }
RT=""; if [ "$RTPRIO" -gt 0 ] && chrt -f "$RTPRIO" true 2>/dev/null; then RT="chrt -f $RTPRIO"; fi
# A systemd scope puts the row in a cgroup of the user's slice, which is
# outside an isolated cpuset partition: inside the partition the scope takes
# the CPUs away again and `taskset` then refuses. USE_SCOPE=0 turns it off,
# and the row keeps its timeout and its pin.
SCOPE=""
[ "${USE_SCOPE:-1}" = 1 ] && command -v systemd-run >/dev/null && SCOPE="systemd-run --user --scope --quiet"

# run <row> <name> <MemoryMax> <timeout seconds> <cores> <rt: 0|1> <command...>
# stdout and stderr go to log/<name>.log; the exit code and the seconds to
# log/status.tsv. The command's own REGIONS_TSV is set by the caller.
run() {
    local row=$1 name=$2 mem=$3 tmo=$4 cores=$5 rt=$6; shift 6
    local pre="" t0 t1 rc
    [ "$rt" = 1 ] && pre="$RT"
    echo "[$row] $name: $*"
    t0=$(date +%s)
    if [ -n "$SCOPE" ]; then
        $SCOPE -p MemoryMax="$mem" timeout "$tmo" $pre taskset -c "$cores" "$@" > "$LOG/$name.log" 2>&1
    else
        timeout "$tmo" $pre taskset -c "$cores" "$@" > "$LOG/$name.log" 2>&1
    fi
    rc=$?; t1=$(date +%s)
    printf '%s\t%s\t%s\t%s\n' "$row" "$name" "$rc" "$((t1 - t0))" >> "$STATUS"
    [ $rc -ne 0 ] && echo "[$row] $name: exit $rc (see $LOG/$name.log)"
    return $rc
}
skip() { echo "[$1] skipped: $2"; printf '%s\tskipped\t-\t%s\n' "$1" "$2" >> "$STATUS"; }
fresh() { rm -f "$DATA/$1"; }
J="$JULIA --startup-file=no"

# --- M1: zero cost when unused ----------------------------------------------
if want M1; then
    if [ -z "$VANILLA" ] || [ -z "$GCBENCHMARKS" ]; then skip M1 "VANILLA and GCBENCHMARKS must be set"
    else
        fresh gcbench.tsv
        REGIONS_TSV=$DATA/gcbench.tsv CORE=$CORE MTCORES=$MTCORES \
            run M1 gcbench 16G 10800 0-31 0 bash ../bench/gcbench.sh "$VANILLA" "$JULIA" "$GCBENCHMARKS" "$ROUNDS"
    fi
fi

# --- M2: unit costs -----------------------------------------------------------
# unit_costs.jl prints TSV rows itself; the runs are joined into one file
# with a first column that names the run. Three runs: `vanilla` is the stock
# rows on the vanilla binary; `regions_stock` is the same stock rows on the
# regions binary in a process that never opens a window (the zero-cost
# question at the unit level); `regions` is every row on the regions binary,
# where the rows after the first window run with the barrier armed. No
# real-time class: the script waits for a child process.
if want M2; then
    fresh unit_costs.tsv
    printf '# binary\tround\tcost\tvalue\tunit\tsamples\n' > "$DATA/unit_costs.tsv"
    # One round is one process per binary. The order of the binaries turns
    # over between the rounds, so a drift of the machine does not sit in the
    # difference; the interval of a row comes from the rounds.
    unit_round() {   # unit_round <round> <name> <binary> <extra args...>
        local r=$1 name=$2 bin=$3; shift 3
        run M2 "unit_costs_${name}_r$r" 8G 900 "$CORE" 0 "$bin" --startup-file=no ../bench/unit_costs.jl "$@" \
            && grep -P '^\w+\t' "$LOG/unit_costs_${name}_r$r.log" | sed "s/^/$name\t$r\t/" >> "$DATA/unit_costs.tsv"
    }
    for r in $(seq 1 "$ROUNDS"); do
        if [ $((r % 2)) -eq 1 ]; then order="vanilla regions_stock regions"; else order="regions regions_stock vanilla"; fi
        for name in $order; do
            case $name in
                vanilla)       [ -n "$VANILLA" ] && unit_round "$r" vanilla "$VANILLA" stock 5 ;;
                regions_stock) unit_round "$r" regions_stock "$JULIA" stock 5 ;;
                regions)       unit_round "$r" regions "$JULIA" 5 ;;
            esac
        done
    done
    [ -n "$VANILLA" ] || echo "[M2] no VANILLA: the vanilla rows are missing"
fi

# --- M3: the tail, one Bool apart -------------------------------------------
if want M3; then
    fresh tail.tsv
    for v in alloc pooled; do
        REGIONS_TSV=$DATA/tail.tsv run M3 yardstick_$v 8G 900 "$CORE" 1 $J ../bench/yardstick.jl $v 20000000
    done
    for v in baseline regions; do
        REGIONS_TSV=$DATA/tail.tsv run M3 tail_$v 8G 900 "$CORE" 1 $J ../bench/tail.jl $v 20000000
    done
fi

# --- M4: the real-world loop -----------------------------------------------
# realworld.sh pins, takes the real-time class, keeps the run with the fewest
# involuntary switches, and writes data/realworld.tsv and data/ccdf_*.tsv.
if want M4; then
    fresh realworld.tsv
    JULIA=$JULIA CORE=$CORE RTPRIO=$RTPRIO run M4 realworld 8G 5400 0-31 0 bash ./realworld.sh
fi

# --- M5: the census -----------------------------------------------------------
if want M5; then
    fresh census_pause.tsv; fresh census_throughput.tsv
    for K in 300 1000 3000 10000 30000 100000; do
        for v in scoped coop full; do
            REGIONS_TSV=$DATA/census_pause.tsv run M5 census_${v}_K$K 8G 900 "$CORE" 0 $J ../bench/census.jl $v 2000000 100000 $K
        done
    done
    for W in 3 200; do
        REGIONS_TSV=$DATA/census_throughput.tsv run M5 census_autopool_W$W 8G 900 "$CORE" 0 $J ../bench/census.jl autopool 5000000 100000 10000 $W
        for B in 1 100 1000; do
            REGIONS_TSV=$DATA/census_throughput.tsv run M5 census_batch_W${W}_B$B 8G 900 "$CORE" 0 $J ../bench/census.jl batch 5000000 100000 10000 $W $B
        done
        REGIONS_TSV=$DATA/census_throughput.tsv run M5 census_pooled_W$W 8G 900 "$CORE" 0 $J ../bench/census.jl pooled 5000000 100000 10000 $W
    done
fi

# --- M6: paced, and the endurance run ----------------------------------------
if want M6; then
    fresh paced.tsv; fresh endurance.tsv
    REGIONS_TSV=$DATA/paced.tsv run M6 paced_baseline 8G 900 "$CORE" 1 $J --heap-size-hint=128M ../bench/paced.jl baseline 1000000
    REGIONS_TSV=$DATA/paced.tsv run M6 paced_regions 8G 900 "$CORE" 1 $J ../bench/paced.jl regions 1000000
    REGIONS_TSV=$DATA/endurance.tsv run M6 endurance 8G 2700 "$CORE" 1 $J ../bench/endurance.jl 18000000
fi

# --- M7: region-native against C++ -------------------------------------------
if want M7; then
    fresh native.tsv
    g++ -O2 -std=c++17 -o "$LOG/native" ../bench/native.cpp || echo "[M7] g++ failed"
    for W in 3 200; do
        REGIONS_TSV=$DATA/native.tsv run M7 native_region_W$W 8G 900 "$CORE" 0 $J ../bench/native.jl region 5000000 100000 $W
        REGIONS_TSV=$DATA/native.tsv run M7 native_stock_W$W 8G 900 "$CORE" 0 $J ../bench/native.jl stock 5000000 100000 $W
        [ -x "$LOG/native" ] && REGIONS_TSV=$DATA/native.tsv run M7 native_cpp_W$W 8G 900 "$CORE" 0 "$LOG/native" 5000000 $W
    done
fi

# --- M8: wholesale death, the showcases ---------------------------------------
# Three rounds, the binaries' modes alternating; plot.py keeps the minimum.
if want M8; then
    fresh showcase.tsv
    for r in 1 2 3; do
        for m in stock region; do
            REGIONS_TSV=$DATA/showcase.tsv run M8 showcase_binarytree_${m}_$r 8G 900 "$CORE" 0 $J ../demo/showcase_binarytree.jl $m 18
            REGIONS_TSV=$DATA/showcase.tsv run M8 showcase_linkedlist_${m}_$r 16G 900 "$CORE" 0 $J ../demo/showcase_linkedlist.jl $m 64
        done
        REGIONS_TSV=$DATA/showcase.tsv run M8 showcase_tree_$r 8G 900 "$MTCORES" 0 $J -t 4 ../demo/showcase_tree.jl
    done
fi

# --- M9: the growth bound -------------------------------------------------------
if want M9; then
    fresh census_bound.tsv
    REGIONS_TSV=$DATA/census_bound.tsv run M9 census_bound 8G 600 "$CORE" 0 $J ../bench/census_bound.jl
fi

# --- M10: the demonstrators ---------------------------------------------------
if want M10; then
    fresh demo_a.tsv; fresh demo_b.tsv; fresh demo_c.tsv; fresh demo_d.tsv
    REGIONS_TSV=$DATA/demo_a.tsv run M10 demo_a 8G 1800 "$CORE" 0 $J ../demo/bt_solver.jl
    REGIONS_TSV=$DATA/demo_b.tsv run M10 demo_b 8G 1800 "$MTCORES" 0 $J -t 4 ../demo/pathtrace.jl
    REGIONS_TSV=$DATA/demo_c.tsv run M10 demo_c 8G 1800 "$MTCORES" 0 $J -t 4 ../demo/optimistic_bst.jl
    REGIONS_TSV=$DATA/demo_d.tsv run M10 demo_d 8G 1800 "$MTCORES" 0 $J -t 4 ../demo/dmr.jl
fi

# --- M11: the discipline checker --------------------------------------------
# The hooked compiler is built once into log/regionck; a julia whose
# Compiler the patch does not fit skips the row and says so.
if want M11; then
    fresh checker.tsv
    if python3 ../tools/hook_patch.py "$LOG/regionck" "$JULIA" > "$LOG/hook_patch.log" 2>&1; then
        printf '# model\tevents\tviolations\tsites\n' > "$DATA/checker.tsv"
        for m in alloc clean; do
            JULIA_LOAD_PATH="$LOG/regionck/env:@stdlib" run M11 checker_$m 8G 900 "$CORE" 0 $J ../tools/checker_run.jl $m 100000 \
                && awk -v m=$m '/^violations: /{printf "%s\t100000\t%s\t%s\n", m, $2, $5}' "$LOG/checker_$m.log" >> "$DATA/checker.tsv"
        done
    else skip M11 "hook_patch.py does not apply to this julia (log/hook_patch.log)"; fi
fi

# --- M12: thread scaling of the sibling leaves ------------------------------
# The only row that leaves the isolated core: it waits for a quiet machine
# (load average below 4) for up to 30 minutes.
if want M12; then
    fresh scaling.tsv
    waited=0
    while [ "$(awk '{print int($1)}' /proc/loadavg)" -ge 4 ] && [ $waited -lt 1800 ]; do sleep 60; waited=$((waited + 60)); done
    if [ "$(awk '{print int($1)}' /proc/loadavg)" -ge 4 ]; then skip M12 "load average stayed above 4"
    else
        for t in 1 2 4 8; do
            REGIONS_TSV=$DATA/scaling.tsv run M12 scaling_pathtrace_t$t 8G 1800 "$MTCORES" 0 $J -t $t ../demo/pathtrace.jl
            REGIONS_TSV=$DATA/scaling.tsv run M12 scaling_dmr_t$t 8G 1800 "$MTCORES" 0 $J -t $t ../demo/dmr.jl
        done
    fi
fi

# --- M13: the collector on the whole machine ---------------------------------
# The cost of the unused region runtime on a parallel collection, against the
# thread count. Both binaries run the same script, in an order that turns
# over between the rounds. A row takes its own thread count and the matching
# GC thread count, which is half the threads, the default of julia. A count
# above the cores of the machine is skipped. Every run writes to a scratch
# file, which the driver appends with the binary and the round.
if want M13; then
    fresh parallel_gc.tsv
    printf '# binary\tround\tthreads\tgcthreads\tcollection\twall_ms\tmark_ms\tsweep_ms\tsafepoint_us\tlive_mb\n' > "$DATA/parallel_gc.tsv"
    cores=$(nproc)
    for r in $(seq 1 "$ROUNDS"); do
        for t in $GCTHREADS; do
            [ "$t" -le "$cores" ] || continue
            g=$(( t > 1 ? t / 2 : 1 ))
            if [ $((r % 2)) -eq 1 ]; then order="vanilla regions"; else order="regions vanilla"; fi
            for name in $order; do
                if [ "$name" = vanilla ]; then bin=$VANILLA; else bin=$JULIA; fi
                [ -n "$bin" ] || continue
                rm -f "$LOG/pgc.tsv"
                REGIONS_TSV=$LOG/pgc.tsv \
                    run M13 "parallel_gc_${name}_t${t}_r$r" 24G 1800 "$ALLCORES" 0 \
                    "$bin" --startup-file=no -t "$t" --gcthreads="$g" ../bench/parallel_gc.jl 12 \
                    && grep -v '^#' "$LOG/pgc.tsv" | sed "s/^/$name\t$r\t/" >> "$DATA/parallel_gc.tsv"
            done
        done
    done
fi

# --- M14: what a reset costs the other threads --------------------------------
# The checked reset stops the world, so its cost grows with the threads that
# run Julia code; the unchecked entry is the control. The region binary alone
# can run this row.
if want M14; then
    fresh reset_pause.tsv
    printf '# round\tmode\tthreads\tworkers\tkind\tindex\tvalue_us\tcollections\n' > "$DATA/reset_pause.tsv"
    cores=$(nproc)
    for r in $(seq 1 "$ROUNDS"); do
        for t in $GCTHREADS; do
            [ "$t" -le "$cores" ] || continue
            for mode in checked unsafe; do
                rm -f "$LOG/rp.tsv"
                REGIONS_TSV=$LOG/rp.tsv \
                    run M14 "reset_pause_${mode}_t${t}_r$r" 16G 900 "$ALLCORES" 0 \
                    $J -t "$t" ../bench/reset_pause.jl "$mode" 200 \
                    && grep -v '^#' "$LOG/rp.tsv" | sed "s/^/$r\t/" >> "$DATA/reset_pause.tsv"
            done
        done
    done
fi

echo "run_all: done; status in $STATUS"
awk -F'\t' 'NR > 1 && $3 != "0" && $3 != "-"' "$STATUS" | sed 's/^/failed: /'
