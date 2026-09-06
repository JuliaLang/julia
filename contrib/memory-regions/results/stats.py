#!/usr/bin/env python3
"""The statistics of the measurements: a robust centre and an interval.

Every cost row of MEASUREMENTS.md is a paired measurement. One round runs
the two binaries one after the other, in an order that alternates, so that a
slow drift of the machine - the clock, the temperature, the page cache -
moves both sides of a round together. The statistic of a row is then the
per-round difference or the per-round ratio, and its interval comes from the
rounds, not from the samples inside one round.

  centre(xs)                  the median
  spread(xs)                  the median absolute deviation
  ci(xs)                      a percentile bootstrap interval of the median
  paired_diff(a, b)           the per-round differences b - a
  paired_ratio(a, b)          the per-round ratios b / a
  sign_p(diffs)               the two-sided sign test of "the centre is 0"
  fmt / fmt_ci                the cells of a table

The bootstrap is seeded, so a table is the same on every run of the script.
Ten rounds give a crude interval; the interval of a row with fewer than
`MIN_ROUNDS` rounds is reported, and the row says how many rounds it has.
The module needs nothing outside the standard library.
"""
import math
import random

BOOTSTRAP = 10_000
SEED = 20260906
MIN_ROUNDS = 5


def _clean(xs):
    return sorted(float(x) for x in xs if x is not None and math.isfinite(float(x)))


def quantile(xs, q):
    """The linear-interpolation quantile of a sample; None for an empty one."""
    s = _clean(xs)
    if not s:
        return None
    if len(s) == 1:
        return s[0]
    pos = q * (len(s) - 1)
    lo = int(math.floor(pos))
    hi = min(lo + 1, len(s) - 1)
    return s[lo] + (s[hi] - s[lo]) * (pos - lo)


def centre(xs):
    """The median: the robust centre of a small sample of round values."""
    return quantile(xs, 0.5)


def spread(xs):
    """The median absolute deviation, in the unit of the sample."""
    s = _clean(xs)
    if len(s) < 2:
        return None
    m = centre(s)
    return centre([abs(x - m) for x in s])


def ci(xs, alpha=0.05, stat=centre, n=BOOTSTRAP, seed=SEED):
    """A percentile bootstrap interval of `stat` over the sample.

    The sample is the per-round values of one row. The interval is honest
    about the rounds it has: with one round there is no interval.
    """
    s = _clean(xs)
    if len(s) < 2:
        return (None, None)
    rng = random.Random(seed)
    k = len(s)
    boots = []
    for _ in range(n):
        boots.append(stat([s[rng.randrange(k)] for _ in range(k)]))
    return (quantile(boots, alpha / 2), quantile(boots, 1 - alpha / 2))


def paired(a, b):
    """The rounds both sides have, in round order: [(a_r, b_r), ...].

    `a` and `b` are dictionaries round -> value.
    """
    return [(a[r], b[r]) for r in sorted(set(a) & set(b))]


def paired_diff(a, b):
    """The per-round differences b - a."""
    return [y - x for x, y in paired(a, b)]


def paired_ratio(a, b):
    """The per-round ratios b / a, for the rounds where a is not 0."""
    return [y / x for x, y in paired(a, b) if x]


def sign_p(diffs, null=0.0):
    """The two-sided sign test of "the centre of `diffs` is `null`".

    It answers the only question a handful of rounds can answer: does the
    difference keep its direction? Ties count for neither side.
    """
    s = [d - null for d in diffs if d is not None and d != null]
    n = len(s)
    if n == 0:
        return None
    k = sum(1 for d in s if d > 0)
    k = min(k, n - k)
    tail = sum(math.comb(n, i) for i in range(0, k + 1)) / (2.0 ** n)
    return min(1.0, 2.0 * tail)


def sig(v, digits=3):
    """A number with `digits` significant digits, as a table cell."""
    if v is None:
        return "—"
    if v == 0:
        return "0"
    return f"{v:.{digits}g}"


def fmt_ci(value, lo, hi, digits=3):
    """`value [lo, hi]`, or the value alone when there is no interval."""
    if value is None:
        return "—"
    if lo is None or hi is None:
        return sig(value, digits)
    return f"{sig(value, digits)} [{sig(lo, digits)}, {sig(hi, digits)}]"


def summary(values, digits=3, alpha=0.05):
    """The cell of a sample of round values: median with its interval."""
    lo, hi = ci(values, alpha)
    return fmt_ci(centre(values), lo, hi, digits)


def delta_summary(a, b, digits=3, alpha=0.05):
    """The cell of a paired difference b - a: the median with its interval
    and the sign test, from the rounds the two sides share."""
    d = paired_diff(a, b)
    if not d:
        return "—"
    lo, hi = ci(d, alpha)
    p = sign_p(d)
    cell = fmt_ci(centre(d), lo, hi, digits)
    return cell if p is None else f"{cell}, sign {sig(p, 2)}"


def ratio_summary(a, b, digits=3, alpha=0.05):
    """The cell of a paired ratio b / a."""
    r = paired_ratio(a, b)
    if not r:
        return "—"
    lo, hi = ci(r, alpha)
    return fmt_ci(centre(r), lo, hi, digits)


def rounds_of(values):
    """How many rounds a sample holds, for the `rounds` column of a table."""
    return len(_clean(values))
