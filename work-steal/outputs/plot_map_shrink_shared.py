


"""
plot_benchmark.py  —  generalized scheduler benchmark plotter
Usage:
    python plot_benchmark.py results.txt
    python plot_benchmark.py results.txt --out my_plot.png --title "Mergesort n=1M"
    python plot_benchmark.py results.txt --starting-size 0        # filter to one size
    python plot_benchmark.py results.txt --debug                  # show parse errors

Dimensions handled automatically:
    shrink_policy  ->  line colour
    pool_mode      ->  line style / marker  (gc = solid/circle, pool = dashed/square)
    starting_size  ->  line alpha + dash density
                       (rank 0 = opaque, rank 1 = medium dash, rank 2 = light dotted)

All dimensions are auto-discovered from the file.
"""

import re
import sys
import argparse
from itertools import product as iproduct

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

# ── colour / style tables ─────────────────────────────────────────────────────

SHRINK_COLORS = {
    "no_shrink": "#185FA5",
    "simple":    "#D85A30",
    "multi":     "#1D9E75",
    "no_copy":   "#7c3aed",
}
_EXTRA_COLORS = ["#b45309", "#0891b2", "#dc2626", "#64748b", "#be185d"]

POOL_MODE_STYLE = {
    "gc":   ("-",  "o"),
    "pool": ("--", "s"),
}
_EXTRA_POOL_STYLE = [(":", "^"), ("-.", "D")]

# starting_size rank -> (alpha, dashes)   dashes=None => use pool linestyle
_SIZE_ALPHA  = [1.0,  0.60, 0.35]
_SIZE_DASHES = [None, (5, 2), (1, 3)]


def _shrink_color(policy, cache):
    if policy in SHRINK_COLORS:
        return SHRINK_COLORS[policy]
    if policy not in cache:
        cache[policy] = _EXTRA_COLORS[len(cache) % len(_EXTRA_COLORS)]
    return cache[policy]


def _pool_style(pool, pool_list):
    if pool in POOL_MODE_STYLE:
        return POOL_MODE_STYLE[pool]
    idx = pool_list.index(pool)
    return _EXTRA_POOL_STYLE[idx % len(_EXTRA_POOL_STYLE)]


def _size_visuals(rank):
    alpha  = _SIZE_ALPHA[rank]  if rank < len(_SIZE_ALPHA)  else max(0.2, 1.0 - rank * 0.2)
    dashes = _SIZE_DASHES[rank] if rank < len(_SIZE_DASHES) else (1, 4)
    return alpha, dashes


# ── parser ────────────────────────────────────────────────────────────────────
#
# 18 whitespace-separated tokens per data row:
#  0 scheduler  1 steal_policy  2 shrink_policy  3 pool_mode  4 starting_size
#  5 workers    6 threshold     7 steal_ratio     8 avg        9 sd
# 10 speedup   11 tasks_s      12 grows          13 shrinks   14 minor_col
# 15 major_col 16 minor_words  17 major_words

def _is_data_line(line):
    return line.startswith("ws") or line.startswith("naive")


def _parse_steal_pct(token):
    m = re.search(r"\((\d+(?:\.\d+)?)%\)", token)
    return float(m.group(1)) if m else 0.0


def parse_file(path, debug=False):
    rows, skipped = [], []
    with open(path) as fh:
        for lineno, raw in enumerate(fh, 1):
            line = raw.strip()
            if not _is_data_line(line):
                continue
            tokens = line.split()
            if len(tokens) != 18:
                skipped.append((lineno,
                    f"expected 18 tokens, got {len(tokens)}: {line[:120]}"))
                continue
            try:
                rows.append(dict(
                    scheduler    = tokens[0],
                    steal_policy = tokens[1],
                    shrink_policy= tokens[2],
                    pool_mode    = tokens[3],
                    starting_size= int(tokens[4]),
                    workers      = int(tokens[5]),
                    threshold    = int(tokens[6]),
                    steal_pct    = _parse_steal_pct(tokens[7]),
                    avg          = float(tokens[8]),
                    sd           = float(tokens[9]),
                    speedup      = float(tokens[10]),
                    tasks_s      = int(tokens[11]),
                    grows        = int(tokens[12]),
                    shrinks      = int(tokens[13]),
                    minor_col    = int(tokens[14]),
                    major_col    = int(tokens[15]),
                    minor_words  = float(tokens[16]),
                    major_words  = float(tokens[17]),
                ))
            except (ValueError, IndexError) as exc:
                skipped.append((lineno, f"parse error ({exc}): {line[:120]}"))

    if debug:
        if skipped:
            print(f"\n[debug] {len(skipped)} lines skipped:")
            for ln, msg in skipped[:20]:
                print(f"  line {ln:4d}: {msg}")
            if len(skipped) > 20:
                print(f"  ... and {len(skipped)-20} more")
        else:
            print(f"[debug] all data lines parsed cleanly ({len(rows)} rows)")
    return rows


# ── index & lookup ────────────────────────────────────────────────────────────

def build_index(rows):
    """Key: (shrink_policy, pool_mode, starting_size, workers) -> row."""
    idx = {}
    for r in rows:
        key = (r["shrink_policy"], r["pool_mode"], r["starting_size"], r["workers"])
        idx[key] = r
    return idx


def get(idx, shrink, pool, size, workers, field):
    r = idx.get((shrink, pool, size, workers))
    return r[field] if r else None


# ── drawing helpers ───────────────────────────────────────────────────────────

def _draw_lines(ax, idx, shrink_policies, pool_modes, sizes, size_ranks,
                workers_list, field, extra_colors):
    for shrink, pool, size in iproduct(shrink_policies, pool_modes, sizes):
        ys = [get(idx, shrink, pool, size, w, field) for w in workers_list]
        if all(v is None for v in ys):
            continue
        ys = [v if v is not None else np.nan for v in ys]
        color      = _shrink_color(shrink, extra_colors)
        ls, marker = _pool_style(pool, pool_modes)
        rank       = size_ranks[size]
        alpha, dashes = _size_visuals(rank)
        kw = dict(color=color, linewidth=1.8, alpha=alpha,
                  marker=marker, markersize=4,
                  label=f"{shrink}/{pool}/sz={size}")
        if dashes is not None:
            kw["dashes"] = dashes
        else:
            kw["linestyle"] = ls
        ax.plot(workers_list, ys, **kw)


def _style_ax(ax, ylabel, title, workers_list):
    ax.set_title(title, fontsize=9)
    ax.set_xlabel("workers", fontsize=8)
    ax.set_ylabel(ylabel, fontsize=8)
    ax.set_xticks(workers_list)
    ax.tick_params(labelsize=8)
    ax.grid(True, alpha=0.25)


def _build_legend(shrink_policies, pool_modes, sizes, size_ranks, extra_colors):
    h = []
    for shrink in shrink_policies:
        h.append(Line2D([0],[0], color=_shrink_color(shrink, extra_colors),
                        linewidth=2.5, label=f"shrink={shrink}"))
    for pool in pool_modes:
        ls, mk = _pool_style(pool, pool_modes)
        h.append(Line2D([0],[0], color="gray", linestyle=ls, marker=mk,
                        linewidth=1.5, markersize=5, label=f"pool={pool}"))
    for size in sizes:
        rank = size_ranks[size]
        alpha, dashes = _size_visuals(rank)
        kw = dict(color="gray", linewidth=2.5, alpha=alpha, label=f"start_sz={size}")
        if dashes:
            kw["dashes"] = dashes
        h.append(Line2D([0],[0], **kw))
    return h


# ── Figure 1: line charts — varying workers ───────────────────────────────────

def fig_varying_workers(idx, shrink_policies, pool_modes, sizes, size_ranks,
                        workers_list, extra_colors, out_path, title_prefix):
    metrics = [
        ("speedup",     "speedup (x)",       "speedup vs workers"),
        ("steal_pct",   "steal ratio (%)",   "steal ratio vs workers"),
        ("minor_col",   "minor collections", "minor GC collections vs workers"),
        ("minor_words", "minor words (M)",   "minor words allocated vs workers"),
        ("major_words", "major words (M)",   "major words allocated vs workers"),
        ("grows",       "grows",             "grows vs workers"),
        ("shrinks",     "shrinks",           "shrinks vs workers"),
        ("avg",         "avg time (s)",      "avg time vs workers"),
        ("tasks_s",     "tasks / second",    "throughput vs workers"),
    ]
    ncols, nrows = 3, int(np.ceil(len(metrics) / 3))
    fig, axes = plt.subplots(nrows, ncols, figsize=(ncols*5, nrows*3.8))
    fig.suptitle(f"{title_prefix}  -  varying workers",
                 fontsize=12, fontweight="bold", y=1.02)

    for i, (field, ylabel, title) in enumerate(metrics):
        ax = axes.flatten()[i]
        _style_ax(ax, ylabel, title, workers_list)
        _draw_lines(ax, idx, shrink_policies, pool_modes, sizes,
                    size_ranks, workers_list, field, extra_colors)

    for j in range(len(metrics), axes.size):
        axes.flatten()[j].set_visible(False)

    handles = _build_legend(shrink_policies, pool_modes, sizes, size_ranks, extra_colors)
    fig.legend(handles=handles, loc="upper center", bbox_to_anchor=(0.5, 1.01),
               ncol=min(len(handles), 7), fontsize=7.5, frameon=False)
    fig.tight_layout()
    path = out_path.replace(".png", "_varying_workers.png")
    fig.savefig(path, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"saved -> {path}")



# ── Figure 2: bar charts — fixed workers = max ───────────────────────────────

def fig_fixed_workers(idx, shrink_policies, pool_modes, sizes, size_ranks,
                      workers_max, extra_colors, out_path, title_prefix):
    metrics = [
        ("minor_words", "minor words (M)",   f"minor words  (workers={workers_max})"),
        ("major_words", "major words (M)",   f"major words  (workers={workers_max})"),
        ("grows",       "grows",             f"grows  (workers={workers_max})"),
        ("shrinks",     "shrinks",           f"shrinks  (workers={workers_max})"),
    ]
    combos = [(s, p, sz)
              for s, p, sz in iproduct(shrink_policies, pool_modes, sizes)
              if get(idx, s, p, sz, workers_max, "speedup") is not None]
    if not combos:
        print("warning: no data at workers_max — skipping fixed_workers figure")
        return

    ncols2, nrows2 = 4, int(np.ceil(len(metrics) / 4))
    fig, axes = plt.subplots(nrows2, ncols2, figsize=(ncols2*4.5, nrows2*3.8))
    fig.suptitle(f"{title_prefix}  -  workers={workers_max} comparison",
                 fontsize=12, fontweight="bold", y=1.02)

    x       = np.arange(len(combos))
    width   = 0.65
    colors  = [_shrink_color(s, extra_colors) for s, _, _  in combos]
    hatches = ["//" if p == "pool" else ""    for _, p, _  in combos]
    alphas  = [_size_visuals(size_ranks[sz])[0] for _, _, sz in combos]
    xlabels = [f"{s}\n{p}\nsz={sz}"           for s, p, sz in combos]

    for i, (field, ylabel, title) in enumerate(metrics):
        ax = axes.flatten()[i]
        ax.set_title(title, fontsize=9)
        ax.set_ylabel(ylabel, fontsize=8)
        ax.tick_params(labelsize=7)
        ax.grid(True, alpha=0.25, axis="y")
        vals = [get(idx, s, p, sz, workers_max, field) or 0 for s, p, sz in combos]
        bars = ax.bar(x, vals, width=width, color=colors,
                      hatch=hatches, edgecolor="white")
        for bar, a in zip(bars, alphas):
            bar.set_alpha(a)
        ax.set_xticks(x)
        ax.set_xticklabels(xlabels, fontsize=6)

    for j in range(len(metrics), axes.size):
        axes.flatten()[j].set_visible(False)

    handles = _build_legend(shrink_policies, pool_modes, sizes, size_ranks, extra_colors)
    fig.legend(handles=handles, loc="upper center", bbox_to_anchor=(0.5, 1.01),
               ncol=min(len(handles), 7), fontsize=7.5, frameon=False)
    fig.tight_layout()
    path = out_path.replace(".png", "_fixed_workers.png")
    fig.savefig(path, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"saved -> {path}")



# ── main ──────────────────────────────────────────────────────────────────────

def make_plots(rows, out_path, title_prefix, filter_size=0):
    if filter_size is not None:
        rows = [r for r in rows if r["starting_size"] == filter_size]
        if not rows:
            sys.exit(f"No rows with starting_size={filter_size}.")
        print(f"filtered to starting_size={filter_size} -> {len(rows)} rows")

    idx             = build_index(rows)
    workers_list    = sorted(set(r["workers"]       for r in rows))
    shrink_policies = sorted(set(r["shrink_policy"] for r in rows))
    pool_modes      = sorted(set(r["pool_mode"]     for r in rows))
    sizes           = sorted(set(r["starting_size"] for r in rows))
    size_ranks      = {sz: i for i, sz in enumerate(sizes)}
    workers_max     = max(workers_list)
    extra_colors    = {}

    print(f"  shrink_policies = {shrink_policies}")
    print(f"  pool_modes      = {pool_modes}")
    print(f"  starting_sizes  = {sizes}")
    print(f"  workers         = {workers_list}")

    fig_varying_workers(idx, shrink_policies, pool_modes, sizes, size_ranks,
                        workers_list, extra_colors, out_path, title_prefix)
    fig_fixed_workers(idx, shrink_policies, pool_modes, sizes, size_ranks,
                      workers_max, extra_colors, out_path, title_prefix)



def main():
    p = argparse.ArgumentParser(description="Plot scheduler benchmark results.")
    p.add_argument("input",  help="path to the benchmark .txt file")
    p.add_argument("--out",   default="benchmark_plot.png",
                   help="output PNG base name (suffixes added automatically)")
    p.add_argument("--title", default="Scheduler benchmark",
                   help="title prefix for all figures")
    p.add_argument("--starting-size", type=int, default=None, dest="starting_size",
                   help="only plot rows with this starting_size value")
    p.add_argument("--debug", action="store_true",
                   help="print lines that failed to parse")
    args = p.parse_args()

    rows = parse_file(args.input, debug=args.debug)
    if not rows:
        sys.exit(f"No rows parsed from '{args.input}'. "
                 "Check that the file matches the expected column format.")
    print(f"parsed {len(rows)} rows")
    make_plots(rows, args.out, args.title, args.starting_size)


if __name__ == "__main__":
    main()