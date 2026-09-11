#!/bin/bash
# Switch the isolated partition for the HIL core on and off at runtime
# (cgroup v2), with no reboot. This replaces the boot parameter
# `isolcpus=domain,managed_irq,13,29`; keep `nohz_full=13,29
# rcu_nocbs=13,29 irqaffinity=...` on the kernel command line - those are
# boot-only, cost nothing while the CPUs do normal work, and complete the
# isolation while the partition is on.
#
#   sudo ./hil_isolation.sh on        take CPUS out of the scheduler
#   sudo ./hil_isolation.sh off       give them back, at once
#        ./hil_isolation.sh status    what holds right now
#        ./hil_isolation.sh run CMD   run CMD inside the partition, pinned
#                                     to CORE, SCHED_FIFO when granted
#
# `on` also hands the two cgroup.procs files to the invoking user, so that
# `run` needs no root afterward (moving a process needs write access on
# the destination's cgroup.procs and on the common ancestor's - the root's).
# CPUS (default 13,29 - the two threads of core 13), CORE (default 29) and
# RTPRIO (default 50) override the defaults.
#   ./hil_isolation.sh run ../results/realworld.sh   runs the matrix isolated.
set -euo pipefail
CPUS="${CPUS:-13,29}"; CORE="${CORE:-29}"; RTPRIO="${RTPRIO:-50}"
CG=/sys/fs/cgroup; HIL=$CG/hil
need_root() { [ "$(id -u)" = 0 ] || { echo "run this with sudo"; exit 1; }; }
case "${1:-status}" in
on) need_root
    if grep -qo "isolcpus=[^ ]*" /proc/cmdline; then
        echo "note: isolcpus is still on the command line; the boot isolation"
        echo "      already holds and the partition adds nothing to it"
    fi
    grep -qw cpuset "$CG/cgroup.subtree_control" || echo +cpuset > "$CG/cgroup.subtree_control"
    mkdir -p "$HIL"
    echo "$CPUS" > "$HIL/cpuset.cpus"
    echo "$CPUS" > "$HIL/cpuset.cpus.exclusive"
    echo isolated > "$HIL/cpuset.cpus.partition"
    state=$(cat "$HIL/cpuset.cpus.partition")
    [ "$state" = isolated ] || { echo "the partition was refused: $state"; exit 1; }
    if [ -n "${SUDO_USER:-}" ]; then
        chown "$SUDO_USER" "$HIL/cgroup.procs" "$CG/cgroup.procs"
    fi
    echo "on: CPUs $CPUS are out of the scheduler ($HIL)"
    ;;
off) need_root
    [ -d "$HIL" ] || { echo "already off"; exit 0; }
    echo member > "$HIL/cpuset.cpus.partition"
    if [ -s "$HIL/cgroup.procs" ]; then
        echo "off: CPUs $CPUS take load again; $HIL still holds processes and stays"
    else
        rmdir "$HIL"
        echo "off: CPUs $CPUS take load again"
    fi
    ;;
status)
    echo "cmdline:       $(grep -o 'isolcpus=[^ ]*' /proc/cmdline || echo 'no isolcpus') / $(grep -o 'nohz_full=[^ ]*' /proc/cmdline || echo 'no nohz_full')"
    echo "boot-isolated: '$(cat /sys/devices/system/cpu/isolated)'"
    if [ -d "$HIL" ]; then
        echo "partition:     $(cat "$HIL/cpuset.cpus.partition"), cpus $(cat "$HIL/cpuset.cpus.effective"), $(wc -l < "$HIL/cgroup.procs") processes"
    else
        echo "partition:     off"
    fi
    ;;
run) shift; [ $# -ge 1 ] || { echo "usage: $0 run <command...>"; exit 1; }
    [ -d "$HIL" ] || { echo "the partition is off; sudo $0 on first"; exit 1; }
    echo $$ > "$HIL/cgroup.procs" 2>/dev/null || echo $$ | sudo tee "$HIL/cgroup.procs" > /dev/null
    if chrt -f "$RTPRIO" true 2>/dev/null; then
        exec chrt -f "$RTPRIO" taskset -c "$CORE" "$@"
    else
        exec taskset -c "$CORE" "$@"
    fi
    ;;
*) echo "usage: sudo $0 on|off ; $0 status ; $0 run <command...>"; exit 1 ;;
esac
