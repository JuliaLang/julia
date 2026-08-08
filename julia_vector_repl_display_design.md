# Displaying long 1-D arrays in a terminal: prior art, and the design it led to

**Research date:** 2026-08-07  
**Question:** How do other languages and tools display a long one-dimensional array/vector when it does not fit on screen, and what mechanisms are transferable to Julia?

**Status:** the survey below was gathered to settle a design disagreement on
[JuliaLang/julia#62592](https://github.com/JuliaLang/julia/pull/62592). It has since been acted
on: that PR's "whichever layout shows more entries" rule was dropped and replaced by a wrapped,
bracketed layout, gated behind `:compact => true` after review. [The design this led to](#the-design-this-led-to) records what was built and
which evidence drove each choice; the tool-by-tool sections are unchanged evidence.

## Executive summary

There is no single cross-language convention. The most relevant systems fall into four families:

1. **Wrapped vector syntax using width, with independent element elision.** NumPy is the clearest example, inherited almost verbatim by PyTorch and JAX. Values stay inside a one-dimensional delimiter structure and are greedily wrapped across lines. A separate threshold controls when the middle is replaced by `...`. Recent NumPy additionally prints `shape=(n,)` when elision makes shape impossible to infer. This is the closest mature precedent for a width-aware, wrapped 1-D Julia display.
2. **Vertical record/series displays with explicit metadata.** pandas Series and Polars Series spend one line per element, but compensate with useful index/shape/type metadata and aggressive row truncation. They are not simply “vertical arrays”: they are column-like labeled objects. Polars is particularly explicit (`shape: (n,)`, `Series: ...`), so rank ambiguity is impossible even though its values are vertical.
3. **Horizontal mathematical arrays whose orientation is semantic.** MATLAB/Octave and Maple are weak precedents for a true rank-1 array type because a “row vector” is itself a `1×N`/row-oriented mathematical object. Their horizontal display is not merely a pretty-printing choice. MATLAB and Octave use width to split a long row into labeled column ranges rather than changing it into a vertical list.
4. **General-purpose language reprs that deliberately avoid adaptive layout.** Python `list`, Haskell/GHCi, Rust `{:?}`, and `jq -c` emit a single logical line and leave wrapping to the terminal. Rust added an *opt-in* multiline `{:#?}` mode after explicitly deciding that making the verbose form the default would be undesirable. GHCi's own manual calls its `show`-based behavior “not ideal” for long output, but solved that with a custom-printer hook rather than a new default layout.

The most important evidence about **1-D versus 2-D ambiguity** is not a vote for vertical or horizontal layout. It is that mature systems that pack rank-1 values horizontally usually have a separate structural cue: nested brackets (NumPy/PyTorch/JAX/Wolfram), an explicit shape header (NumPy when summarized, xarray, Polars), or a rank-bearing reader syntax (Common Lisp). J is a cautionary counterexample: its own tutorial explicitly shows that a rank-1 five-item list and a rank-2 `1 5` array can display identically, and tells users to query `$` to distinguish them. That is documented ambiguity, not speculation.

A second strong pattern is **separating layout from output-volume policy**. Common Lisp standardized independent right-margin, element-count, and line-count controls. Wolfram has separate `PageWidth`, `TotalWidth`, `Short`, and notebook output-size controls. NumPy separates `linewidth` from `threshold`/`edgeitems`. pandas separates row limits from width. That separation is more common than making terminal shape alone decide how many values are emitted.

Finally, “uses terminal width” needs care. Several popular tools use a **stored print width**, not the current TTY width. In local pseudo-terminal tests, NumPy 2.3.5 remained at `linewidth=75`, PyTorch 2.10.0 at `linewidth=80`, xarray at `display_width=80`, pandas Series at its row/column settings, and IPython at `PlainTextFormatter.max_width=79` when the actual terminal was changed between 40 and 200 columns. R and Dyalog APL have explicit mechanisms that *can* track terminal resizing; MATLAB's Command Window layout does so through its frontend.

---

## Method and verification status

For runnable tools, I used an actual pseudo-terminal (`script` + `stty`) at all four combinations of **40 or 200 columns** and **6 or 40 rows**, and confirmed inside the process that `stdout` was a TTY and `os.get_terminal_size()` reported the requested dimensions. I tested short and long integer arrays and, where useful, wide strings. The local environment contained:

| Tool | Version actually run |
|---|---:|
| Python | 3.13.5 |
| IPython | 9.14.0 |
| NumPy | 2.3.5 |
| pandas | 2.2.3 |
| PyTorch | 2.10.0+cpu |
| JAX | 0.9.0.1 |
| xarray | 2026.4.0 |
| jq | 1.7 |

R, MATLAB, Octave, GHCi, Rust, Common Lisp, Dyalog APL, J, Wolfram/Mathematica, Maple, and Polars were not available locally. Their examples below are therefore explicitly marked **documentation-verified**, not locally executed. For MATLAB, R, Polars, Rust, Common Lisp, J, and Dyalog, the cited documentation contains textual output examples. For Wolfram and parts of Maple, the web documentation renders some outputs as notebook/typeset images, so I do **not** invent an exact plain-text transcript where the accessible documentation does not provide one.

A useful terminology distinction throughout the report:

- **live width / live height**: queried from or automatically coupled to the current terminal/window dimensions;
- **configured width / configured row count**: a stored option such as NumPy's `linewidth=75`, even if the user could set it to match a terminal;
- **terminal soft wrap**: the terminal visually wraps a single long output line, but the program itself inserted no newline. This is *not* counted as a width-aware layout algorithm.

---

## Comparison table

| Tool | Default / characteristic layout for 1-D | Uses width? | Uses height? | Elision | Index labels | 1-D vs 2-D disambiguation |
|---|---|---|---|---|---|---|
| Python `list` `repr` | Single logical line | No | No | None | No | Nested list delimiters: `[1,2]` vs `[[1,2]]`; lists themselves have no ndarray rank |
| IPython list display | Pretty multiline when long; commonly one item/line | **Configured** `max_width=79`, not live in my test | No | Prefix truncation after `max_seq_length=1000` | No | Same nesting rule as Python list |
| `pprint(..., compact=True)` | Greedy wrapped sequence syntax | **Configured** `width` (default 80) | No | None by `pprint` itself | No | Preserves nested delimiters |
| NumPy ndarray | Wrapped row-major `array([...])` | **Configured** `linewidth=75` by default; not live in my test | No | Center `...`; default threshold 1000, edgeitems 3 | No | Bracket nesting; since NumPy 2.2 summarized arrays add `shape=(n,)` when shape cannot be inferred |
| pandas Series | Vertical `index  value` pairs | Width matters mostly to value/column formatting; not a packing axis for Series | Default no; `display.max_rows=0` uses live terminal height | Center `..` between first/last rows | **Yes: actual Series index** | Series format + `dtype`/`Length`; DataFrame is a 2-D table with columns and row labels |
| Base R vector | Width-packed rows of values | **Yes** via `options(width=)`; some terminals can auto-update it | No direct height-based vector limit | `max.print`: stops after approximate max and reports omitted entries | **Yes:** `[k]` = index of first element on each physical line | Matrices have row/column labels (`[,1]`, `[1,]`) and 2-D grid; vector's `[k]` labels are line offsets, not rank syntax |
| R tibble / pillar | Vertical rows in a table; wide tables can tier/omit columns | **Yes** via R/pillar width | Fixed row budget, not live height by default | Omitted-row footer such as `# ℹ 96 more rows` | **Yes:** displayed row numbers | Explicit table header `# A tibble: r × c`; a one-column tibble remains visibly tabular |
| MATLAB | Horizontal row vector split into column blocks | **Yes**, Command Window width / optional 80-column matrix limit | Only via optional pager (`more on`), not element elision | No default element elision; emits all, possibly paged | Block labels `Columns i through j` | A row vector **is** a `1×N` 2-D array; horizontal orientation is semantic |
| GNU Octave | Horizontal rows split into width-sized column blocks | **Yes**, `split_long_rows`; terminal size available | Optional pager can use screenfuls | No ordinary element elision | Block labels `Columns i through j` | Same row/column-vector semantics as MATLAB |
| GHCi / Haskell list | Single line from `show` | No | No | None | No | List nesting only; no ndarray rank. GHCi supports custom interactive printer |
| Rust `{:?}` | Single line | No | No | None | No | Container/nesting syntax; no generic ndarray rank |
| Rust `{:#?}` | Vertical pretty form, one element per line | No width adaptation | No | None | No | Same delimiters; explicitly opt-in because verbose |
| Common Lisp | Pretty printer can fill/wrap within margin; vector reader syntax | **Yes**: `*print-right-margin*`; `nil` means infer stream line length if possible | **Yes if configured**: `*print-lines*` | `*print-length*` uses `...`; line-limit uses distinct `..` | No | Vector `#(...)`; rank-2 array `#2A((...)(...))` — rank is syntactic |
| Dyalog APL | Horizontal vector, folded continuation lines | **Yes** via `⎕PW`; optional `Auto_PW` follows session resize | No | No normal element elision | No | Classic display is spatial; optional array-notation/display tools can expose structure |
| J | Rank-1 on one line; higher ranks by lines/blank lines | **Configured** max line length (default 256), not shown as live terminal coupling | **Configured line-count** limit, not live height | Line truncation appends `...`; excess lines use a line `...` between retained regions | No | **Known ambiguity:** rank-1 length 5 and rank-2 shape `1 5` can print the same values on one line |
| Wolfram / Mathematica | List syntax, frontend/stream line breaking | **Yes** via `PageWidth`; notebook can use `WindowWidth` | `Short[...,n]` is line-budgeted; notebook output-size UI is separate | `Short`/`Skeleton[n]` explicitly marks *how many* elements were omitted | No | `{...}` vs `{{...}}`; structural nesting; rich notebook frontends add output controls |
| Maple | Row/column Vector types; large rtables become placeholder / viewer | Size threshold rather than simple terminal-width packing in cited docs | GUI viewer uses viewport/scrolling | Large vectors exceed `rtablesize` and use a placeholder; GUI can scroll | GUI rtable row/column headers available | `Vector[row]`/`Vector[column]` are explicit orientations/types; Matrix is distinct |
| Polars Series | Vertical, one value per row inside brackets | Table-width config exists but Series itself remains columnar | Row-count config (`tbl_rows`) rather than live height | Center `…` between head/tail | No positional index column | Explicit `shape: (n,)` and `Series: name [dtype]` header |
| PyTorch tensor | NumPy-like wrapped `tensor([...])` | **Configured** `linewidth=80`; not live in my test | No | Center `...`; threshold 1000, edgeitems 3 | No | Nested brackets; tensor wrapper; shape can be queried but not normally printed for summarized 1-D |
| JAX array | NumPy-like wrapped `Array([...], dtype=...)` | Uses NumPy print options; configured, not live in my test | No | Center `...` via NumPy options | No | Nested brackets; dtype wrapper |
| xarray DataArray | NumPy-like wrapped values plus dimension metadata | **Configured** `display_width=80` | `display_max_rows` exists for object sections; value array itself followed NumPy behavior in my test | NumPy-style center `...` for large value array | Usually coordinates are metadata, not a simple positional index column | Extremely explicit header: `(n: 1001)` vs `(row: 1, col: 12)` plus named dimensions |
| `jq` default | Pretty JSON: array elements vertically | No terminal-width adaptation | No | None | No | JSON nesting: `[...]` vs `[[...]]` |
| Excel / GUI dataframe viewers | Viewport/table, scroll both axes | **Yes, interactively** | **Yes, interactively** | Usually no semantic elision; viewport hides off-screen cells | **Yes:** row/column headers | Grid geometry and headers; not a textual REPL representation |

---

# Tool-by-tool evidence

## 1. Python built-in `list` representation

**Verified locally: Python 3.13.5.** Python's ordinary list representation does not adapt to terminal dimensions. It emits one logical line containing every element. At 40×6 and 200×40, the inserted characters were identical; only the terminal itself may soft-wrap the long line.

Short list:

```text
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
```

Longer list (still a single logical line):

```text
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99]
```

This representation has no truncation policy. Structural disambiguation comes from recursive syntax, e.g. `[1, 2]` versus `[[1, 2]]`; that is enough for lists, but Python lists do not themselves encode the ndarray rank distinction under discussion.

The nearby standard-library `reprlib` makes the opposite trade: it truncates a list by **item count**, not by terminal dimensions. Its default `maxlist` is 6, producing locally:

```text
[0, 1, 2, 3, 4, 5, ...]
```

This is prefix-only rather than head/tail summarization. See Python's [`reprlib`](https://docs.python.org/3/library/reprlib.html).

### `pprint`: a surprisingly close layout precedent

Python's `pprint` is not the default REPL list repr, but its `compact=True` mode is directly relevant because it performs greedy, width-aware wrapping **without changing list syntax**. The documented/default `width` is 80; `compact=True` “fits as many items as will fit within the `width` on each output line” ([Python `pprint` docs](https://docs.python.org/3/library/pprint.html)).

Verified locally at `width=40`:

```text
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
 22, 23, 24, 25, 26, 27, 28, 29]
```

At `width=80`:

```text
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
 22, 23, 24, 25, 26, 27, 28, 29]
```

With `compact=False`, the same 40-column case puts one scalar per line. At 200 columns, the 30-item list fits on one line in either mode. `pprint` therefore demonstrates that **packing policy can vary with width while preserving a stable syntactic identity**. It does not solve output volume: it has no normal “show head and tail” length elision for lists.

## 2. IPython

**Verified locally: IPython 9.14.0 in a pseudo-terminal.** IPython's plain-text formatter uses pretty-printing for containers rather than Python's raw `repr`. Current configuration documentation gives `PlainTextFormatter.max_width = 79` and `max_seq_length = 1000`; setting `max_seq_length=0` disables truncation ([IPython terminal config](https://ipython.readthedocs.io/en/stable/config/options/terminal.html)).

A 30-element list at both 40 and 200 real terminal columns was rendered as a multiline list, effectively one element per line:

```text
Out[4]:
[0,
 1,
 2,
 3,
 ...
 28,
 29]
```

(The `...` above abbreviates unchanged intermediate lines **in this report**, not IPython output; IPython printed all 30 values.)

For a list of length 1001, IPython printed values through 999 and then its own trailing `...` before `]`, reflecting `max_seq_length=1000`. Thus the two policies are again separate: a configured line width determines multiline layout, while a separate sequence count determines truncation.

A key empirical finding is that changing the actual pseudo-terminal from 40 to 200 columns did **not** change IPython's configured `max_width=79`; it is not a live terminal-width policy by default in this test.

## 3. NumPy

**Verified locally: NumPy 2.3.5.** This is the strongest direct precedent for a true rank-1 numeric array displayed in wrapped horizontal syntax.

The observed defaults were:

```text
edgeitems = 3
threshold = 1000
linewidth = 75
precision = 8
```

These match [`numpy.set_printoptions`](https://numpy.org/doc/stable/reference/generated/numpy.set_printoptions.html), whose documented defaults include threshold 1000, edgeitems 3, and linewidth 75.

A 40-element integer vector at every tested TTY size (40×6, 40×40, 200×6, 200×40) was:

```text
array([ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,
       17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
       34, 35, 36, 37, 38, 39])
```

The actual terminal width did not matter because NumPy used its stored `linewidth=75`. Explicitly changing the option *did* matter. At `linewidth=40`:

```text
array([ 0,  1,  2,  3,  4,  5,  6,  7,
        8,  9, 10, 11, 12, 13, 14, 15,
       16, 17, 18, 19, 20, 21, 22, 23,
       24, 25, 26, 27, 28, 29, 30, 31,
       32, 33, 34, 35, 36, 37, 38, 39])
```

At `linewidth=200`, the same vector was on one line.

For 1001 values, the independent threshold kicks in:

```text
array([   0,    1,    2, ...,  998,  999, 1000], shape=(1001,))
```

This is important in two ways. First, NumPy uses a **center ellipsis with symmetric edge samples** rather than a prefix-only cutoff. Second, since NumPy 2.2, a summarized repr includes the array's `shape` “when it cannot be inferred from the values” ([NumPy 2.2.0 release notes](https://numpy.org/doc/2.2/release/2.2.0-notes.html)). That change was explicitly made to repair structural information lost by elision; a `legacy=2.1` option exists for doctest/backward compatibility.

A `1×12` array instead prints nested brackets:

```text
array([[ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11]])
```

So ordinary short output encodes rank through bracket nesting, while long summarized output now adds a shape suffix too.

Wide elements simply reduce packing density. Twelve ~26-character strings at default settings became roughly one or two entries per physical line:

```text
array(['xxxxxxxxxxxxxxxxxxxxxxxx0', 'xxxxxxxxxxxxxxxxxxxxxxxx1',
       'xxxxxxxxxxxxxxxxxxxxxxxx2', 'xxxxxxxxxxxxxxxxxxxxxxxx3',
       'xxxxxxxxxxxxxxxxxxxxxxxx4', 'xxxxxxxxxxxxxxxxxxxxxxxx5',
       'xxxxxxxxxxxxxxxxxxxxxxxx6', 'xxxxxxxxxxxxxxxxxxxxxxxx7',
       'xxxxxxxxxxxxxxxxxxxxxxxx8', 'xxxxxxxxxxxxxxxxxxxxxxxx9',
       'xxxxxxxxxxxxxxxxxxxxxxxx10', 'xxxxxxxxxxxxxxxxxxxxxxxx11'],
      dtype='<U26')
```

### History

NumPy 1.14 deliberately made “stylistic changes in the way numpy arrays and scalars are printed,” warning that doctests would be affected; 1.14.1 then fixed several problems with the new array printing ([NumPy 1.14 release notes](https://numpy.org/doc/1.14/release.html)). NumPy 2.2's shape suffix is a second recorded representational change driven by ambiguity under summarization. These are useful evidence that display details become compatibility surfaces quickly.

## 4. pandas Series

**Verified locally: pandas 2.2.3.** A pandas Series is intentionally not represented as bare vector syntax. It is a labeled one-dimensional mapping-like column, so its vertical layout carries actual index information.

Observed defaults were `display.max_rows=60`, `display.min_rows=10`, `display.width=80`, and `display.max_colwidth=50`. A 100-element Series was identical at all four tested terminal shapes:

```text
0      0
1      1
2      2
3      3
4      4
      ..
95    95
96    96
97    97
98    98
99    99
Length: 100, dtype: int64
```

The index column is not vestigial: those values are the actual Series index and can be strings, dates, or arbitrary labels.

pandas has an unusually relevant **height-aware opt-in**. Current documentation says that in a terminal, with `large_repr='truncate'`, setting `display.max_rows=0` makes pandas auto-detect terminal height ([pandas options](https://pandas.pydata.org/docs/user_guide/options.html)). That behavior worked in the local test. At 40×6:

```text
0      0
1      1
2      2
      ..
97    97
98    98
99    99
Length: 100, dtype: int64
```

At 40×40 it expanded to the first 20 and last 20 rows around the omission marker. Thus pandas can genuinely use live terminal height, but does so only when the user selects `max_rows=0`; its normal row budget is fixed.

For wide strings, pandas did **not** consume a 200-column terminal to reveal more text under the tested defaults; `display.max_colwidth=50` inserted an ellipsis inside each value:

```text
0     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx...
1     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx...
...
11    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx...
dtype: object
```

A `1×12` DataFrame is visibly different from Series and can react horizontally to available/display width. At the 40-column pseudo-terminal it was:

```text
   0   1   2   3   ...  8   9   10  11
0   0   1   2   3  ...   8   9  10  11

[1 rows x 12 columns]
```

At 200 columns all 12 columns fit. For pandas, therefore, the vertical Series display is bound up with the semantic index and with a distinct DataFrame table grammar. It is not strong evidence that an unlabeled numeric vector should be one-value-per-line.

## 5. Base R

**Documentation-verified; not locally executed.** R is a very relevant wrapped-vector precedent because base R prints an atomic vector in **row-major chunks sized to a configured width**, while labeling each physical line with the one-based index of its first element.

The official `print.default` documentation says `width` “controls the maximum number of columns on a line used in printing vectors, matrices, etc.” and defaults to `getOption("width")`; `max` defaults to `getOption("max.print")` ([R `print.default`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/print.default.html)). The base `options()` documentation says `width` is normally 80 and `max.print` normally 99999. It also documents `setWidthOnResize`, which on suitable terminal/readline builds can update the width option when the terminal is resized ([R `options`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html)).

A public reproducible example at `options(width=20)` is:

```text
> 1:30
 [1]  1  2  3  4  5
 [6]  6  7  8  9 10
[11] 11 12 13 14 15
[16] 16 17 18 19 20
[21] 21 22 23 24 25
[26] 26 27 28 29 30
```

Source: [Stack Overflow example](https://stackoverflow.com/questions/72056213/how-to-set-the-width-of-an-error-message-in-r), consistent with the official width documentation.

The `[1]`, `[6]`, `[11]`, ... labels answer a subtle question: they are **not there primarily to say “this is 1-D.”** They are continuation coordinates. They let a reader locate elements after width-based wrapping. A matrix uses a different label grammar with column headers such as `[,1]` and row labels such as `[1,]`, so rank is also communicated by the surrounding matrix layout.

When `max.print` is exceeded, R stops and reports the number omitted, e.g. an R-help transcript contains:

```text
[ reached getOption("max.print") -- omitted 797124 entries ]
```

([R-help, 2009](https://stat.ethz.ch/pipermail/r-help/2009-July/396021.html)). This is **not** head/tail elision; it is an output cap. The same mailing-list thread is also evidence that users do change this knob, although there is no quantitative evidence here about how common that is.

A small historical footnote illustrates the cost of textual conventions: in 2021/2022 R-devel discussed a bug where the omitted-entry count could overflow and become negative for very long vectors because the message formatted a long count as `%d` ([R-devel thread](https://stat.ethz.ch/pipermail/r-devel/2022-January/081410.html)).

## 6. tibble / pillar

**Documentation-verified; not locally executed.** A one-column tibble looks superficially like a vertical vector, but it is deliberately a **table**. Current tibble documentation says it prints only as many rows and columns as fit on one screen, with a summary of the remainder, and exposes one-off `n` and `width` controls ([tibble formatting](https://tibble.tidyverse.org/reference/formatting.html)). Modern printing is delegated entirely to `pillar`.

A canonical example:

```text
# A tibble: 106 × 2
       x         y
   <int>     <dbl>
 1    -5     0.508
 2    -4     1.52
 3    -3     4.57
 4    -2    13.7
 5    -1    41.2
 6     0   123.
 7     1   370.
 8     2 1 111.
 9     3 3 333.
10     4 10 000.
# ℹ 96 more rows
```

([tibble vignette](https://tibble.tidyverse.org/articles/tibble.html)).

Pillar's current defaults are `pillar.print_max=20`, `pillar.print_min=10`, and `pillar.width=NULL`, meaning use R's `width` option. A width greater than the base R width can cause wide tables to be split into **multiple tiers** ([pillar options](https://pillar.r-lib.org/reference/pillar_options.html)). This is an interesting two-dimensional table strategy but does not directly answer how a bare vector should wrap.

History is explicit: tibble 1.1 “reworked output” to make the summary more concise and to show hidden rows/columns ([tibble changelog](https://tibble.tidyverse.org/news/index.html)); as of tibble 3.1.0 printing moved entirely to pillar. This ecosystem treats console representation as a designed UI with metadata and omission explanations, rather than attempting source-like vector syntax.

## 7. MATLAB

**Documentation-verified; not locally executed.** MATLAB is often cited as horizontal-vector precedent, but the semantic context matters: MATLAB's numeric “vector” is a row or column 2-D array. The colon operator creates a `1-by-n` row vector; there is no separate rank-1 ndarray whose display must be distinguished from a `1×n` matrix.

A current MathWorks Support example from R2025b shows a long row vector broken into labeled horizontal blocks:

```text
>> x = 1:1:15

x =

Columns 1 through 10

1 2 3 4 5 6 7 8 9 10

Columns 11 through 15

11 12 13 14 15
```

Source: [MathWorks Support, 2026](https://www.mathworks.com/matlabcentral/answers/2182689-how-to-display-row-vectors-without-extra-spaces-or-line-breaks-in-matlab-r2025b).

That same support answer says MATLAB does not expose configurable parameters to tune the default vector spacing/wrapping/column-labeling itself and suggests formatting through `num2str` for a custom one-line presentation.

The Command Window **does** expose frontend display controls. Current documentation says `Wrap lines` breaks input/output to the current Command Window width, and “Set matrix display width to eighty columns” limits matrix output; `more on` pages long output one screen at a time ([Command Window settings](https://www.mathworks.com/help/matlab/matlab_env/command-window-settings.html), [format output](https://www.mathworks.com/help/matlab/matlab_env/format-output.html)). An older MathWorks Support answer explicitly says the number of columns displayed per row is determined automatically from Command Window width ([MathWorks Answers, 2009](https://www.mathworks.com/matlabcentral/answers/98929-how-can-i-display-a-wide-matrix-in-the-matlab-command-window-without-the-rows-being-wrapped)).

`format compact` is frequently misunderstood in comparisons: it controls **blank line spacing**, not whether a vector is packed horizontally ([MathWorks format output](https://www.mathworks.com/help/matlab/matlab_env/format-output.html)).

The `Columns i through j` labels are therefore closer to R's `[k]` continuation coordinates than to a rank marker. They make a split horizontal row navigable. But because the row orientation itself is the value's mathematical shape, MATLAB does not face Julia's exact “horizontal 1-D could look like a 1×k matrix” problem.

## 8. GNU Octave

**Documentation-verified; not locally executed.** Octave closely follows MATLAB's matrix/vector display tradition. Its `split_long_rows` option controls whether rows wider than the terminal are split, and the documented example uses labeled column blocks:

```text
Columns 1 through 6:

  1  2  3  4  5  6
 11 12 13 14 15 16

Columns 7 through 10:

  7  8  9 10
 17 18 19 20
```

Source: [GNU Octave `split_long_rows`](https://octave.sourceforge.io/octave/function/split_long_rows.html).

Octave also has `terminal_size()` and paging controls for screenful-at-a-time output in the terminal ([GNU Octave terminal output manual](https://docs.octave.org/latest/Terminal-Output.html)). As in MATLAB, width changes how a matrix row is segmented; it does not normally elide elements. And as in MATLAB, a row vector has 2-D row/column semantics, so this is not a clean precedent for a language with a distinct rank-1 vector.

## 9. Haskell / GHCi

**Documentation-verified; not locally executed.** Ordinary GHCi uses `System.IO.print`, which converts values through `show`. A list therefore uses source-like one-line syntax and has no terminal-width or element-count truncation in the standard list `Show` instance.

Representative ordinary output:

```text
[1,2,3,4,5,6,7,8,9,10]
```

For a long list the logical representation simply keeps going on that line; terminal wrapping is external.

The most valuable evidence is the GHC manual's own assessment. Since GHC 7.6.1, the manual says this `show`-based printing “is not ideal in certain cases, like when the output is long,” and documents `-interactive-print` as a hook for a custom pretty printer ([GHCi manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html)). That is a documented dissatisfaction, but **not** a reversal to a width-sensitive default.

Third-party packages use the hook. For example, `pretty-simple` can be installed as GHCi's default interactive printer and formats nested structures more readably ([pretty-simple on Hackage](https://hackage.haskell.org/package/pretty-simple)). The ecosystem's answer is extensibility rather than a standardized long-list layout.

For Julia this matters mostly as evidence against treating “the default stayed one-line for decades” as proof that one-line output is considered good. GHC explicitly says otherwise.

## 10. Rust `{:?}` and `{:#?}`

**Documentation-verified; not locally executed.** Rust deliberately supports two Debug layouts.

Ordinary `{:?}` follows compact source-like syntax:

```text
["a", "b", "c"]
```

Pretty `{:#?}` prints compound values one element per line:

```text
[
    "a",
    "b",
    "c"
]
```

The design rationale is unusually explicit. RFC 640 says the single-line Debug convention can be hard to read for complex nested values, but “we wouldn’t want this ‘pretty printed’ version to be used by default, since it’s significantly more verbose” ([Rust RFC 640](https://rust-lang.github.io/rfcs/0640-debug-improvements.html)). The compromise was a syntax-level opt-in via `#`.

Neither mode adapts to terminal width or height, and neither elides by default. Rust therefore supplies evidence for a **stable user-selectable presentation mode**, not for an adaptive mode flip. Its bracket nesting distinguishes nested containers, but Rust's standard `Vec<T>` is not itself an N-dimensional array type, so it does not directly answer rank-1 versus `1×N` matrix ambiguity.

## 11. Common Lisp

**Standard/documentation-verified; not locally executed.** Common Lisp has the richest standardized separation of pretty-printing concerns in this survey.

Three controls are independent:

- `*print-right-margin*`: the right margin for pretty-printer layout. Its initial value is `nil`; when `nil`, the printer should use the maximum line length the output can display without wrap/truncation if this can be determined ([CLHS `*print-right-margin*`](https://www.lispworks.com/documentation/HyperSpec/Body/v_pr_rig.htm)).
- `*print-length*`: maximum elements at a given level. Initial value `nil` means unlimited; when exceeded, remaining elements are represented by `...` ([CLHS `*print-length*`](https://www.lispworks.com/documentation/HyperSpec/Body/v_pr_lev.htm)).
- `*print-lines*`: maximum lines of pretty output. Initial `nil` means unlimited; when exceeded, the final line gets `..` followed by pending closing delimiters ([CLHS `*print-lines*`](https://www.lispworks.com/documentation/HyperSpec/Body/v_pr_lin.htm)).

The `*print-length*` documentation gives exact output:

```text
0 -- (...)
1 -- (1 ...)
2 -- (1 2 ...)
3 -- (1 2 3 ...)
4 -- (1 2 3 4 ...)
5 -- (1 2 3 4 5 6)
```

The `*print-lines*` example, with a 25-column margin and 3-line limit, is:

```text
(PROGN (SETQ A 1
             B 2
             C 3 ..))
```

The standard intentionally makes `..` different from `...` so line-budget truncation can be visually distinguished from structural/length abbreviation, and so abbreviated output is less likely to be accidentally read back as valid input.

Common Lisp's array syntax also solves rank ambiguity syntactically. A vector prints with `#(...)`; a rank-2 array uses rank-bearing syntax such as `#2A((...)(...))` (see the [Common Lisp printer/array syntax reference](https://www.lispworks.com/documentation/HyperSpec/Body/22_aaa.htm) and array reader syntax in CLHS).

### History: unusually well documented

The 1989 X3J13 `PRETTY-PRINT-INTERFACE` issue records several proposal revisions, committee votes, and explicit rationale ([full issue write-up](https://www.lispworks.com/documentation/HyperSpec/Issues/iss270_w.htm)). It notes a lineage of pretty printers going back 13 years and extensive use in Symbolics, DEC Common Lisp, and CMU Common Lisp before standardization. It also specifically argues that a small `*print-lines*` value saves **computation time as well as output-medium space** because lines can be emitted incrementally.

This is strong prior art for treating width, element budget, and line budget as orthogonal constraints rather than deriving all three from a terminal's aspect ratio.

## 12. Dyalog APL

**Documentation-verified; not locally executed.** Classic Dyalog output treats a simple numeric vector as a horizontal sequence. `⎕PW` (“print width”) is the maximum number of output characters per line; if exceeded, display folds at or before that width and continuation portions are indented six spaces. Numeric arrays may fold earlier to avoid splitting an individual number ([Dyalog `⎕PW`](https://docs.dyalog.com/21.0/language-reference-guide/system-functions/pw/)).

The manual's exact example at `⎕PW←42` is:

```text
      ⎕PW←42
      ⎕←3⍴÷3
0.3333333333 0.3333333333 0.3333333333
      0.3333333333
```

There is no ordinary element elision here: width changes line folding, not the number of values retained.

`Auto_PW=1` couples `⎕PW` to the current Session window and updates it on resize; under Windows the documented default is `Auto_PW=0` ([Dyalog Auto_PW](https://docs.dyalog.com/21.0/windows-installation-and-configuration-guide/configuration-parameters/auto-pw/)). Thus Dyalog provides a clean distinction between a width *setting* and a frontend choice to keep that setting synchronized with live geometry.

APL's classic display relies heavily on spatial arrangement rather than Python-like nested delimiters. Modern Dyalog also has optional array-notation/display facilities, so the classic plain display should not be treated as the only available structural view. I did not locally verify a one-row matrix versus rank-1 ambiguity case and therefore do not claim one here.

## 13. J

**Documentation-verified; not locally executed.** J is the most useful cautionary example in the survey.

Its tutorial says “J displays a 1-cell on a single line” and gives:

```text
   5 $ 2
2 2 2 2 2
```

For a rank-2 array it normally uses one line per rank-1 cell:

```text
   2 5 $ 1 10
 1 10  1 10  1
10  1 10  1 10
```

But a one-row rank-2 array collapses to the same visual form as a five-item rank-1 list:

```text
   1 5 $ 1 10
1 10 1 10 1
```

The tutorial explicitly says: “This is not the same as a 5-item list, which has shape 5. Again, monad `$` shows the shape,” followed by:

```text
   $ 1 5 $ 1 10
1 5
```

Source: [J for C Programmers, Declarations](https://www.jsoftware.com/help/jforc/declarations.htm). Earlier in the same tutorial it is even more direct about scalar versus one-item-list ambiguity: “The displays of a scalar and a 1-item list are identical.”

J also has global **output control** rather than purely semantic array summarization. `9!:36` / `9!:37` control end-of-line convention, maximum line length, maximum lines before, and maximum lines after. The documented defaults are `0 256 0 222`. A line exceeding the maximum is truncated and `...` appended; if line count exceeds the before+after budget, a line containing `...` is inserted between retained regions ([J output control](https://www.jsoftware.com/help/dictionary/dx009.htm)).

This is a notably poor model for information preservation: line-width overflow can throw away the **right-hand tail** of a rank-1 line instead of reflowing it, and shape may be visually ambiguous. The output-control feature dates at least to J's 2000-era release history, but I found no design rationale explaining those particular defaults.

## 14. Wolfram Language / Mathematica

**Documentation-verified; not locally executed.** Wolfram separates line layout, deliberate shortening, and frontend output-size limits.

A one-dimensional list uses braces:

```wolfram
{1, 2, 3, 4, 5}
```

A matrix is nested structurally:

```wolfram
{{1, 2, 3}, {4, 5, 6}}
```

So horizontal wrapping does not create a rank ambiguity: nesting, not line orientation, carries the structure.

`PageWidth` controls how wide each line of text may be for streams, and notebook cells can use `WindowWidth`, `PaperWidth`, or an explicit value. `PageWidth -> WindowWidth` uses the full displayed window width ([Wolfram `PageWidth`](https://reference.wolfram.com/language/ref/PageWidth.html)).

Elision is separately expressible. `Short[expr]` prints approximately one line; `Short[expr,n]` prints about `n` lines. Omitted sequences are represented as `Skeleton[k]`, whose standard form explicitly records **k omitted elements** ([Wolfram `Short`](https://reference.wolfram.com/language/ref/Short.html), [`Skeleton`](https://reference.wolfram.com/language/ref/Skeleton.html)). Because the documentation's exact rendered skeleton glyph is a typeset special character and the online examples are image-based, I do not fabricate an ASCII transcript here.

For streams, `TotalWidth` can independently bound the total number of generated characters and request short forms if necessary ([Wolfram `TotalWidth`](https://reference.wolfram.com/language/ref/TotalWidth.html)). Notebook frontends add an `OutputSizeLimit` mechanism for very large rich outputs. This is another strong example of **width being a layout constraint, not the sole output-volume constraint**.

## 15. Maple

**Documentation-verified; not locally executed.** Maple is interesting because its modern GUI has moved beyond textual elision for large array-like objects.

Maple `Vector` is explicitly row- or column-oriented; if orientation is omitted, the default is a **column Vector**. The documentation's small examples are typeset vertically, e.g. conceptually:

```text
Vector(1..3, 5)
      [5]
      [5]
      [5]
```

and `Vector[row]([1,2,3])` is a row Vector. The exact web output is typeset rather than a plain TTY transcript, so this transcription is descriptive, not claimed verbatim. See [Maple Vector help](https://www.maplesoft.com/support/help/Maple/view.aspx?path=Vector).

For large vectors, current help says only “small” Vectors are displayed inline: size 1..25 in the TTY version and 1..10 in the GUI version. Larger Vectors are represented by a placeholder; `interface(rtablesize=value)` changes the threshold and `rtablesize=infinity` requests all inline ([Maple Vector help](https://www.maplesoft.com/support/help/Maple/view.aspx?path=Vector)). Matrices use analogous size thresholds ([Maple Matrix help](https://www.maplesoft.com/support/help/Maple/view.aspx?path=Matrix)).

The major historical change is GUI-side. Maple 2024 introduced **scrollable matrices/rtables**: only part of a large object is visible inline, and hovering reveals scroll bars so the user can browse entries directly. The release notes explicitly say many interface changes were initiated by customer requests; Maple 2025 extended this viewer with interactive row/column resizing ([Maple 2024 interface notes](https://www.maplesoft.com/support/help/maple/view.aspx?path=updates%2FMaple2024%2FInterface), [Maple 2025 interface notes](https://www.maplesoft.com/support/help/maple/view.aspx?path=updates%2FMaple2025%2FInterface)).

This is a genuine shift from “summarize/placeholder” toward “viewport over the full value,” but it depends on a rich document frontend and is not directly transferable to a terminal REPL.

## 16. Polars Series

**Documentation-verified; local installation was unavailable.** Polars is a useful modern example of choosing a vertical representation but making rank explicit in metadata.

Small Series:

```text
shape: (3,)
Series: '' [i64]
[
    1
    2
    3
]
```

Source: [Polars Series docs](https://docs.pola.rs/api/python/version/1/reference/series/index.html).

The same documentation contains a 25-element Series summarized as:

```text
shape: (25,)
Series: 'index' [i64]
[
    0
    1
    2
    3
    4
    …
    20
    21
    22
    23
    24
]
```

This is head/tail elision with a centered Unicode ellipsis. `Config.set_tbl_rows(n)` controls the maximum displayed rows for both DataFrames and Series; negative values display all ([Polars `set_tbl_rows`](https://docs.pola.rs/docs/python/dev/reference/api/polars.Config.set_tbl_rows.html)). Polars separately exposes table column/width controls for DataFrames.

Polars' vertical layout therefore comes with two cues Julia's current bare value column does not inherently share: an explicit `shape: (n,)` line and an explicit `Series: ... [dtype]` line. It does **not** print a positional index column.

## 17. PyTorch

**Verified locally: PyTorch 2.10.0+cpu.** PyTorch deliberately follows NumPy. Its own documentation says its print options are “shamelessly taken from NumPy” and documents defaults `precision=4`, `threshold=1000`, `edgeitems=3`, and `linewidth=80` ([`torch.set_printoptions`](https://docs.pytorch.org/docs/stable/generated/torch.set_printoptions.html)).

Forty integers:

```text
tensor([ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
        36, 37, 38, 39])
```

1001 integers:

```text
tensor([   0,    1,    2,  ...,  998,  999, 1000])
```

A `1×12` tensor uses nested brackets:

```text
tensor([[ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11]])
```

Like NumPy, the stored `linewidth`, not the tested live TTY width, controlled wrapping. PyTorch is therefore not independent evidence for this design so much as evidence that the NumPy convention transferred successfully into another major numeric ecosystem.

## 18. JAX

**Verified locally: JAX 0.9.0.1.** JAX explicitly documents `jax.numpy.set_printoptions` as an alias of NumPy's and says “JAX arrays are printed via NumPy” ([JAX documentation](https://docs.jax.dev/en/latest/_autosummary/jax.numpy.set_printoptions.html)).

Forty integers:

```text
Array([ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,
       17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
       34, 35, 36, 37, 38, 39], dtype=int32)
```

1001 integers:

```text
Array([   0,    1,    2, ...,  998,  999, 1000], dtype=int32)
```

A one-row 2-D array has nested brackets. JAX again reinforces NumPy's wrapped syntax + independent threshold pattern, but should not be counted as a fully independent design lineage.

## 19. xarray

**Verified locally: xarray 2026.4.0.** xarray is a particularly good demonstration of how **metadata can remove rank ambiguity completely while leaving value layout compact**.

Observed options were `display_width=80`, `display_max_rows=12`, and `display_values_threshold=200`; these defaults are documented by [`xarray.set_options`](https://docs.xarray.dev/en/stable/generated/xarray.set_options.html).

A 40-element 1-D DataArray:

```text
<xarray.DataArray (n: 40)> Size: 320B
array([ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,
       17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
       34, 35, 36, 37, 38, 39])
Dimensions without coordinates: n
```

A 1001-element 1-D DataArray:

```text
<xarray.DataArray (n: 1001)> Size: 8kB
array([   0,    1,    2, ...,  998,  999, 1000], shape=(1001,))
Dimensions without coordinates: n
```

A `1×12` 2-D DataArray:

```text
<xarray.DataArray (row: 1, col: 12)> Size: 96B
array([[ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11]])
Dimensions without coordinates: row, col
```

The header itself (`(n: 1001)` versus `(row: 1, col: 12)`) makes dimensionality unmistakable before the values are read.

One empirical discrepancy is worth recording. Although the documented `display_values_threshold=200` says it triggers summarization for NumPy-array data views, in xarray 2026.4.0 a 300-element integer `DataArray` printed all 300 values in my test; 1001 values summarized because the underlying NumPy threshold was crossed. I have not established whether this is a documentation mismatch, a changed implementation path, or context-specific behavior, so I do not rely on `display_values_threshold=200` as an observed 1-D cutoff.

## 20. `jq`

**Verified locally: jq 1.7.** jq gives a useful “format mode, not adaptive geometry” contrast.

Default pretty JSON array:

```text
[
  0,
  1,
  2,
  3,
  4,
  5
]
```

A 30-element array remained one value per line at both 40 and 200 terminal columns and both heights. There is no automatic elision.

With `-c`, the same value becomes one compact logical line:

```text
[0,1,2,3,4,5]
```

Again there is no truncation or width adaptation. The mode switch is explicit. See the [jq manual](https://jqlang.org/manual/) (`--compact-output` / `-c`). JSON's nested bracket syntax handles structural disambiguation.

## 21. Excel and rich dataframe viewers: a different category

A spreadsheet/viewer solves the problem by **not serializing the whole object into text at all**. Excel exposes persistent row and column headings and a scrollable two-dimensional viewport; row/column headings can also be included when printing ([Microsoft Excel row/column headings support](https://support.microsoft.com/en-us/office/print-row-and-column-headings-2f583ab0-a7a3-44a6-9a88-a5b17a7eeb41)). Notebook dataframe viewers and Maple 2024's rtable viewer follow the same broad interaction model.

This is worth noting because it uses both screen dimensions perfectly without eliding semantic data, but it is not a terminal representation and therefore transfers poorly to Julia's plain REPL. The transferable idea is narrower: **a viewport is better than clever serialization when the frontend can support one**. Julia notebook/IDE displays could reasonably diverge from terminal defaults just as Maple and Wolfram do.

---

# Findings across systems

## A. Width-aware wrapping is common, but live terminal-width adaptation is not

There is strong prior art for packing a 1-D sequence across the width:

- NumPy, PyTorch, and JAX use wrapped row-major syntax.
- Base R packs vector elements to a width and annotates continuation lines.
- Dyalog APL folds simple vectors at `⎕PW`.
- MATLAB/Octave split a row vector into width-sized column blocks.
- Python `pprint(compact=True)` greedily fills each configured-width line.
- Common Lisp's pretty printer can use a right margin inferred from the output stream.

But several of these use **configured widths** rather than asking the terminal on every display. The local tests make that distinction concrete: NumPy 75, PyTorch 80, xarray 80, and IPython 79 stayed fixed when the real pseudo-terminal changed from 40 to 200 columns. R has `setWidthOnResize`; Dyalog has `Auto_PW`; MATLAB's GUI knows its actual Command Window width. This suggests that “use width” and “track resize live” are separable design decisions.

## B. Height is much less commonly used for bare vectors than width

Live terminal height is rare in numeric/vector reprs. pandas is the clearest verified exception: `display.max_rows=0` causes terminal-height auto-detection. Common Lisp has a line budget (`*print-lines*`), but it is a configured printer control rather than automatically the current screen height. Octave/MATLAB can page output by screenfuls. Most other systems ignore height entirely and instead use a fixed element or row threshold.

That matters because an aspect-ratio-dependent layout switch is **not** the dominant precedent. More often, width controls *line breaking* while a separate count/line budget controls *how much* output exists.

## C. The dominant elision strategy for numeric arrays is head + tail, not prefix-only

NumPy/PyTorch/JAX use `...` between `edgeitems` from the beginning and end. pandas and Polars also retain head and tail around a central marker. This is valuable for ordered numeric data because the end often carries information (monotonic ranges, convergence, sentinel values, final states).

Prefix-only caps occur in general-purpose printers: R's `max.print`, Common Lisp `*print-length*`, `reprlib`, and IPython's sequence cap. Those mechanisms are primarily safeguards against unbounded output rather than curated numeric summaries.

J's line-length truncation is an especially weak behavior for vectors because it may discard the right side merely because a line is too long, conflating a **layout constraint** with a **data-selection constraint**.

## D. Systems that safely show a 1-D value horizontally usually carry rank somewhere else

This is the crux of the matrix-ambiguity question.

Strong disambiguators found in the survey:

- **Nested delimiters:** NumPy/PyTorch/JAX and Wolfram: `[1,2]` vs `[[1,2]]`, `{...}` vs `{{...}}`.
- **Explicit shape metadata:** NumPy 2.2+ when summarized (`shape=(1001,)`), xarray's `(n: 1001)`, Polars' `shape: (25,)`.
- **Rank-bearing syntax:** Common Lisp `#(...)` versus `#2A(...)`.
- **Different object grammar:** pandas Series versus DataFrame table.
- **Semantic orientation:** MATLAB/Octave/Maple row vectors are already row-oriented mathematical objects, so a horizontal representation does not introduce a false 1-D/2-D implication in those languages.

The cautionary case is J. J lacks a mandatory rank marker in its ordinary numeric display, and its own tutorial explicitly acknowledges identical displays for values of different shape. That is precisely the failure mode a language should avoid if users infer rank from orientation.

Therefore, the evidence does **not** support the proposition that horizontal rank-1 display is inherently ambiguous. It supports the narrower proposition that **horizontal display is safe when rank is independently encoded and risky when spatial orientation is itself a rank cue**.

## E. Index labels are useful when they solve navigation, not when they merely decorate output

R's `[1]`, `[6]`, ... labels have a clear job created by wrapping: they tell the user which vector offset begins a physical line. MATLAB/Octave's `Columns 1 through 10` blocks do the same at a coarser granularity. pandas' left column is even stronger: it is the actual semantic index.

Polars is an instructive counterexample: it displays a long Series vertically **without positional index labels**, because explicit shape/type metadata plus head/tail order is considered sufficient. NumPy likewise wraps without index labels.

So there is no broad precedent saying that a wrapped vector *must* acquire indices. Index labels are most justified if line breaks make positional lookup materially easier. They also consume horizontal space and can be confused with actual data in languages where indexing syntax is visually salient.

## F. Vertical one-element-per-line displays usually belong to richer “series/record” objects, or to explicit pretty modes

The strongest modern vertical examples are pandas Series and Polars Series. Both print metadata around the values; pandas prints an actual index, Polars prints shape/name/type. Rust's one-item-per-line form is explicitly requested with `{:#?}` rather than being the ordinary Debug form. jq's pretty JSON similarly has an explicit compact alternative.

This weakens a simple argument from popularity that “vertical is more readable, therefore vector REPLs usually use it.” Bare numeric array libraries (NumPy lineage, R, APL) more often use horizontal packing/wrapping; columnar data abstractions more often go vertical.

## G. Rich frontends increasingly solve large-output problems with interaction, not smarter ASCII

Wolfram notebooks use window-aware cells and output-size controls; Maple 2024 moved large rtables to an inline scrollable viewer and Maple 2025 enhanced it. Excel/dataframe viewers use scrollable grids. These are genuine design improvements, but they do not imply a terminal should emulate a GUI by printing more data. Instead they support having **frontend-specific display policies**.

## H. Representation changes have real compatibility cost

NumPy 1.14 warned that its printing overhaul would affect doctests, then issued printing fixes in 1.14.1. NumPy 2.2 again changed summarized reprs to add shape and offered `legacy=2.1`. Common Lisp's standardized pretty-print interface went through multiple committee revisions and votes. Rust put pretty Debug behind an explicit format flag rather than changing the default. These histories all indicate that representation is observable behavior users depend on, even when it is nominally “just pretty printing.”

---

# Documented regrets, reversals, and design debates

## GHCi: explicit admission that the default is poor for long output

The current GHC manual states that converting every result through `show` “is not ideal” when output is long. The chosen remedy was a custom `-interactive-print` hook, not a new adaptive built-in list layout. This is a rare official “the default has a known weakness” statement.  
Source: <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html>

## Rust: readable multiline output considered too verbose for the default

RFC 640's motivation says compact Debug can be difficult to read for complex values, then explicitly rejects making pretty output default because it is “significantly more verbose.” The final design makes pretty formatting opt-in with `{:#?}`.  
Source: <https://rust-lang.github.io/rfcs/0640-debug-improvements.html>

## NumPy 1.14: major style overhaul, followed by printing fixes

The 1.14 release called the new printing style a major user-visible change and warned about doctest impact; 1.14.1 fixed problems with the new array printing. This is evidence that even improvements with broad rationale can expose edge cases and compatibility assumptions.  
Source: <https://numpy.org/doc/1.14/release.html>

## NumPy 2.2: adding shape because truncation erased structure

NumPy added `shape=...` to summarized array reprs specifically so shape is always given when it cannot be inferred from displayed values. This is the clearest directly relevant historical change in the survey.  
Source: <https://numpy.org/doc/2.2/release/2.2.0-notes.html>

## Common Lisp: 1989 standardization after years of deployed pretty-printer experience

The X3J13 write-up records five proposal versions, committee votes, a 13-year lineage of predecessor printers, and extensive deployment. It standardized separate margin, line, length, nesting, and dispatch controls. This is unusually strong historical evidence for modular pretty-printing policy.  
Source: <https://www.lispworks.com/documentation/HyperSpec/Issues/iss270_w.htm>

## Maple: moving large arrays from placeholders toward inline scrolling

Maple's longstanding `rtablesize` behavior summarized oversized array-like objects with a placeholder. Maple 2024 added scrollable inline rtable output, explicitly among enhancements influenced by customer requests; 2025 added interactive row/column resizing. This is a frontend-driven reversal from static truncation toward a viewport.  
Sources: <https://www.maplesoft.com/support/help/maple/view.aspx?path=updates%2FMaple2024%2FInterface>, <https://www.maplesoft.com/support/help/maple/view.aspx?path=updates%2FMaple2025%2FInterface>

## MATLAB: long row-vector block labels are a known user friction point, but remain the default

A 2026 MathWorks Support question calls the `Columns 1 through 10` style “a little clunky”; the accepted support answer does not offer a switch for a different default vector layout and instead recommends `num2str` for custom presentation. This is **user-reported friction, not an official MathWorks admission of a design mistake**.  
Source: <https://www.mathworks.com/matlabcentral/answers/2182689-how-to-display-row-vectors-without-extra-spaces-or-line-breaks-in-matlab-r2025b>

## J: documented ambiguity, but no located design rationale or regret

J's own tutorial explicitly teaches that values of different shape can have identical default displays and tells the user to query shape. I found documentation of the behavior but not a historical issue or statement regretting it.  
Sources: <https://www.jsoftware.com/help/jforc/declarations.htm>, <https://www.jsoftware.com/help/dictionary/dx009.htm>

---

# Implications for Julia

These are implications of the external evidence, drawn before any design was settled on. What was
actually built from them is in [the following section](#the-design-this-led-to).

## 1. The strongest transferable mechanism is wrapped vector syntax with an independent truncation budget

NumPy is the cleanest precedent, with Python `pprint(compact=True)` independently demonstrating the underlying layout algorithm:

- retain a stable vector delimiter/syntax across widths;
- pack as many complete element renderings as fit on a line;
- continue on the next line with a consistent indent;
- make wide elements naturally reduce elements-per-line;
- decide head/tail elision through a separate element/line budget.

This uses width without requiring an orientation flip. It also behaves continuously as width changes: a 79-column and 80-column terminal differ by perhaps one element on some lines, not by changing the whole visual grammar from column to row.

## 2. Width and height should be treated as different constraints

The survey gives much stronger precedent for **width-aware line breaking** than for using terminal aspect ratio to select an entirely different representation. Height-aware behavior exists, especially pandas `max_rows=0`, but generally controls **how many rows are retained**, not whether a vector is semantically rendered as a row versus a column.

Common Lisp and Wolfram make this separation explicit in their APIs. For Julia, the most transferable idea is that terminal width can determine packing while terminal height (or an existing `:limit`/display-size budget) determines how many packed lines/elements survive.

## 3. The matrix-ambiguity concern is real only if rank is encoded by orientation

External evidence supports the reviewer's concern in one important way: J demonstrates that a 1-D array and a one-row 2-D array can become visually indistinguishable when the printer relies on spatial layout and provides no mandatory shape cue.

But the same evidence shows several successful ways around it:

- retain vector-specific delimiters around wrapped content;
- retain matrix-specific grammar for 2-D output;
- print explicit shape metadata when truncation destroys inferability;
- use a type/shape header that already identifies the object.

Julia already prints a type/size header (`100-element Vector{Int64}:`) before the values. That header is a stronger disambiguator than J's default display has. The external evidence therefore does **not** imply that “horizontal values necessarily mean matrix.” It implies that any width-using layout should preserve Julia's vector-specific grammar/header strongly enough that orientation is not the only cue.

## 4. A bracketed wrapped form has better cross-language support than a bare horizontal row

The closest precedents—NumPy, JAX, PyTorch, Python `pprint`, Wolfram—keep explicit delimiters while wrapping. The delimiters do two jobs:

1. they make continuation lines visibly part of one sequence rather than matrix rows;
2. they provide a stable visual identity independent of width.

That makes a bracketed wrapped form more transferable than MATLAB's bare row-vector form. MATLAB's syntax cannot be imported wholesale because its horizontal orientation *is* the object's 2-D shape.

## 5. Index labels are optional, not required by precedent

R demonstrates a good reason to add line-start indices: wrapped lines otherwise make it harder to know where position 101 is. MATLAB/Octave use block-range labels for the same reason. But NumPy, PyTorch, JAX, and Polars all summarize or wrap sequences without positional labels.

For Julia, index labels would be most defensible if the goal includes **random positional lookup in the printed output**. They are not needed merely to disambiguate vector from matrix, because type/shape syntax can do that more directly. They also cost scarce width.

A middle ground, if positional orientation proves useful, would be R-like labels only on continuation rows or around an elision boundary—but the survey provides no evidence that this is necessary for readability.

## 6. Center elision with head and tail is the strongest numeric-array convention

For numeric vectors specifically, the NumPy lineage, pandas, and Polars all preserve both ends. That is a stronger precedent than R/IPython's prefix-only safety cap. A Julia design that uses width more efficiently should probably still preserve the semantic distinction between:

- **layout wrapping**: no data omitted;
- **summary elision**: middle data deliberately omitted.

The omission marker should sit where data was removed, not merely at the physical right edge. J's line truncation is the clearest example of why conflating those concepts is undesirable.

## 7. An omission marker can carry more information than a bare ellipsis

Wolfram's `Skeleton[n]` explicitly tells the reader how many elements were omitted. R reports an omitted-entry count after hitting `max.print`; tibble reports “96 more rows”; NumPy 2.2 adds shape when omission makes structure ambiguous.

For Julia, a bare `⋮`/`…` is compact, but the prior art suggests two useful metadata possibilities when there is room:

- preserve the full length/shape in the header (Julia already does);
- optionally make the omission count discoverable or explicit in richer frontends.

Because Julia's existing header already says `100-element Vector{Int64}`, the marginal value of an inline omitted count is lower than in a headerless list.

## 8. “Use all the screen” is not itself a widespread design objective

Many systems intentionally cap output independently of terminal size. A 200-column terminal does not cause NumPy to increase its default threshold, PyTorch to emit more edge elements, or pandas to show more Series rows. A wide display typically affects **line utilization**, not **semantic sample size**.

This argues for distinguishing “wasted horizontal whitespace” from “we should reveal more values.” The first has strong prior art for fixing through wrapping. The second is a separate product decision with weaker precedent.

## 9. The evidence does not support a categorical conclusion that vertical is best

Vertical display wins in pandas and Polars, but those are Series/column abstractions with metadata, indexing, and table lineage. Rust uses vertical pretty output only when requested. jq's pretty mode is vertical but has no numeric-array summarization. Conversely, NumPy's family, R, APL, MATLAB/Octave, and Wolfram all make substantial use of horizontal space.

So the external evidence is mixed on pure readability. What is much less mixed is that **stable grammar + wrapping + independent elision** is a recurring successful combination.

## 10. Nor does the evidence prove Julia's current vertical form is wrong

Julia's current one-value-per-line form has genuine virtues that other systems sometimes choose deliberately: simple scanning, no mid-element wrapping decisions, stable output regardless of width, and no chance that a value row is mistaken for a matrix row. Rust's rationale explicitly acknowledges the readability of such multiline pretty output, and Polars demonstrates that a vertical Series can be a modern intentional design.

What the comparative evidence does show is narrower: the cost of the vertical form—unused horizontal capacity—is not an unavoidable consequence of preserving 1-D identity. NumPy-style wrapped syntax, R-style wrapping with continuation coordinates, and explicit shape/type metadata are all established alternatives.

---

# The design this led to

## What was built

A single wrapped layout replaces both the truncated single line and, on request, the starved
vertical one. Writing `screenheight` for the rows the vertical layout has for entries
(`displaysize(io)[1] - 4`):

| `screenheight` | Layout |
|---|---|
| `<= 1` | entries packed onto the summary line, unpadded, eliding from the middle |
| `2 … WRAPPED_MAX_ROWS` (3), the vector does not fit vertically, **and `:compact => true` is set** | entries packed across the width over exactly the rows the vertical layout would have used, right-aligned so columns line up |
| otherwise | vertical, unchanged |

The `:compact` gate is the substantive change from the first attempt at this. Making the packed
form the default at short display heights drew a strong objection from a reviewer, whose position
was that the vertical layout reads better and that Julia should not trade that away on the
terminal's behalf. Both halves of the design survive the gate: the centre elision applies
unconditionally, because it is a straight correction to discarding the tail; the packed multi-line
form applies only when the caller has already asked for a compact display, which is the case the
survey's opt-in precedents (implication in *Left on the table*, below) point at.

Entries are taken from both ends alternately until they stop fitting, with `…` marking the omitted
middle. Matrices and higher dimensions keep the previous height-only rule untouched. Two bail-outs
return to the vertical layout: entries whose `show` spans several lines (they cannot be measured),
and widths where fewer than two entries fit per line (packing would be the vertical layout with
brackets added).

```
                          # 6×80, :compact => true
100-element Vector{Int64}:
 [  1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
    …,  87,  88,  89,  90,  91,  92,  93,  94,  95,  96,  97,  98,  99, 100]

                          # 5×80 — the one-line case, both ends kept
100-element Vector{Int64}: [1, 2, 3, 4, 5, 6, 7, …, 94, 95, 96, 97, 98, 99, 100]
```

## Which evidence drove which choice

- **Wrapped, bracketed, greedily packed** rather than a bare horizontal row — finding A and
  implication 4. NumPy and `pprint(compact=True)` demonstrate the algorithm; the delimiters make
  continuation lines visibly one sequence and give a stable identity across widths. MATLAB's bare
  row form was rejected because its horizontal orientation *is* the object's 2-D shape.
- **Centre elision keeping both ends**, replacing truncation of the line at the width — finding C
  and implication 6. This is the strongest numeric-array convention in the survey (NumPy lineage,
  pandas, Polars), and J is the counterexample showing what conflating a layout constraint with a
  data-selection constraint costs.
- **Width packs, height budgets** — finding B and implication 2. Terminal height selects how many
  lines exist; width selects how many entries go on each. Neither decides *what the object looks
  like*. The rejected rule in #62592 derived the representation from the aspect ratio, which
  nothing in the survey does.
- **Right-aligned columns** — the vertical layout already aligns, R and NumPy align, and alignment
  is what makes the packed form scannable rather than a wall of commas.
- **No index labels** — implication 5 and finding E. NumPy, PyTorch, JAX and Polars all wrap or
  summarize without them; R's `[k]` solves positional lookup, not rank. Julia's `100-element
  Vector{Int64}:` header plus the brackets already carry rank, so labels would cost scarce width
  for navigation nobody asked for. They remain available later if lookup turns out to matter.
- **Rank ambiguity treated as answered, not avoided** — implication 3. J is ambiguous because
  orientation is its only rank cue; Julia's header is a stronger cue than NumPy's bracket nesting,
  and NumPy only needed `shape=(n,)` in 2.2 because it has no header at all.

## The console logger opts in

`ConsoleLogger` gives each logged value `displaysize[1] ÷ (nvalues + 1)` rows, so a message with a
few values leaves the array display room for barely an entry — the case this whole line of work
started from. It therefore sets `:compact` for array values, but only when that row budget is
short enough for the packed layout to apply:

```
┌ Info: processing                        # 24-row terminal, three values
│   data =
│    100-element Vector{Int64}:
│     [  1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,
│        …,  89,  90,  91,  92,  93,  94,  95,  96,  97,  98,  99, 100]
│   scale = 3.14159265358979
└   name = "run-7"
```

The narrowness is deliberate, and is the one place where reusing `:compact` rather than a dedicated
key has a real cost. `:compact` also reduces the precision numbers are printed with —
`3.14159265358979` renders as `3.14159` — so setting it across the logger's whole `IOContext` would
quietly drop digits from every logged scalar, which is the opposite of what denser log output is
for. Restricting it to arrays, and to the row budgets where it changes the layout, keeps that cost
off everything it would not buy anything for. A dedicated layout key would not need the
restriction.

## Where this departs from the prior art

- **Terminal height is used live.** Only pandas does this, and only when the user sets
  `max_rows=0` (finding B). The justification is local rather than borrowed: Julia's array display
  is already height-driven, so the packed form inherits the existing budget instead of introducing
  a new one. It changes how the rows are *used*, not how many there are. As in pandas, the
  behaviour it enables is opt-in — here through `:compact`.
- **An existing flag carries the opt-in, rather than a new one.** `:compact => true` already means
  "print this compactly", so extending it to select the layout as well as the entry rendering is a
  small stretch of an established key rather than a new control in the Common Lisp / NumPy /
  Wolfram mould. The alternative — a dedicated `:array_layout`-style selector — is cleaner by the
  prior art and remains available if `:compact` proves too blunt.
- **A threshold still switches layouts.** `WRAPPED_MAX_ROWS` is a cliff — under `:compact`, at 7
  terminal rows the packed form shows ~70 entries of `1:100`, at 8 rows the vertical form shows 3.
  The survey offers no precedent for choosing such a threshold, because most systems have no second
  layout to switch to. It is smaller and more predictable than the entry-count comparison it
  replaced, and it now only fires for callers that opted in, but it is the same kind of
  discontinuity.
- **Very narrow displays now show fewer entries than before.** Centre elision spends width on the
  tail, so at 4×40 the old truncating form showed `[1, 2, 3, 4…` and the new one shows
  `[1, …, 100]`. This is finding C applied deliberately: both ends beat more of one end.
- **No shape suffix, no omitted-element count.** NumPy 2.2's `shape=(n,)` and Wolfram's
  `Skeleton[n]` (implication 7) are redundant against a header that already states the length.

## Left on the table

- **Live width synchronization** (R `setWidthOnResize`, Dyalog `Auto_PW`) — Julia already queries
  `displaysize` per display, so this is had for free; noted only because most of the survey does
  not.
- **A first-class layout selector** — a stable, user-chosen mode (`IOContext(:array_layout => …)`
  or a REPL setting) rather than a reused flag, which is how Rust resolved the identical
  verbose-versus-default argument in RFC 640, and how NumPy, Common Lisp and Wolfram expose every
  control in this space. Gating on `:compact` is the cheap version of this. A dedicated selector is
  the natural home for anyone who wants the packed form at full terminal height, and the cleanest
  answer if the threshold above proves contentious.
- **Wider opt-in than the console logger.** `ConsoleLogger` opts in (see below); nothing else
  does. A REPL setting, or a display policy for non-interactive output, remains unexplored.
- **A scrollable viewport for rich frontends** (finding G, Maple 2024+) — out of scope for a
  terminal, but the precedent for IDE and notebook displays diverging from terminal defaults is
  strong.

---

# A compact set of mechanisms worth borrowing — and not borrowing

| Mechanism | Evidence | Transferability to Julia | Used? |
|---|---|---|---|
| Greedy width-aware wrapping inside stable vector delimiters | NumPy; Python `pprint(compact=True)` | **High** | Yes |
| Separate width from element/line budget | NumPy; Common Lisp; Wolfram; pandas | **High** | Yes |
| Head + tail around central omission | NumPy/PyTorch/JAX; pandas; Polars | **High** for numeric sequences | Yes |
| Explicit shape/rank metadata when elision obscures structure | NumPy 2.2; xarray; Polars | **High**, though Julia's existing header already supplies much of it | Already present |
| R-like line-start index labels | Base R | **Medium**; useful for navigation, costs width, not needed solely for rank | No |
| Live terminal-height fitting | pandas opt-in | **Medium**; useful as a line budget, weak precedent for changing orientation | Yes, as the line budget |
| Live width synchronization | R `setWidthOnResize`; Dyalog `Auto_PW`; MATLAB frontend | **Medium–high** if terminal integration is reliable | Already present |
| Split into “Columns i through j” blocks | MATLAB/Octave | **Low–medium**; tied to matrix/row-vector semantics and verbose headers | No |
| Bare horizontal values with no structural cue | J | **Low / avoid**; documented shape ambiguity | Avoided |
| Truncate a line at max width and append `...` | J | **Low / avoid**; layout constraint destroys tail data | Removed |
| One-element-per-line pretty mode | Rust `{:#?}`; Polars | **Legitimate alternative**, especially if stable/readable output is prioritized | Kept as the default; packing is the opt-in, as in RFC 640 |
| Rich scrollable viewport | Maple 2024+, spreadsheet viewers | **High for IDE/notebook frontend**, **low for plain terminal** | Not attempted |

---

# Bottom line

The best-supported general lesson is **not “make vectors horizontal”** and not “keep them vertical.” It is:

> **Keep dimensional identity stable in syntax/metadata, use width for reflow rather than for changing what the object appears to be, and control elision with a separate budget.**

NumPy is the strongest direct array precedent; base R is the strongest precedent for wrapped values plus positional continuation labels; Common Lisp is the strongest precedent for separating printer constraints; J is the strongest warning about shape ambiguity; Rust is the strongest documented argument for keeping a verbose vertical form opt-in rather than making it universal; and Maple/Wolfram show that richer frontends can solve the problem differently from terminals.

On the specific index-label question, prior art says labels are useful when they provide **navigation information** (R, pandas, MATLAB blocks), but they are not a prerequisite for a wrapped vector. On the matrix-ambiguity question, prior art says the danger is real when spatial orientation is the only rank signal (J), but readily avoided when vector syntax, nesting, type headers, or explicit shape metadata remain visible (NumPy, xarray, Polars, Common Lisp, Wolfram).

That leaves Julia's existing vertical form as a defensible design, not an obviously obsolete one. The evidence simply shows that a wrapped, vector-syntactic form can also be defensible without inheriting MATLAB's row-matrix semantics or J's ambiguity.

The design that came out of this keeps both, and lets the caller choose: the vertical form stays the default everywhere, and the wrapped form takes over only for callers that ask for a compact display and only where the vertical form has been starved of rows. The one thing the survey rules out unambiguously — truncating the line at the width and discarding the tail — is gone either way.

---

# Sources

Primary or official sources were preferred; community sources are included only where they provide a reproducible transcript or evidence of actual user friction.

- Python `pprint`: <https://docs.python.org/3/library/pprint.html>
- Python `reprlib`: <https://docs.python.org/3/library/reprlib.html>
- IPython terminal formatter options: <https://ipython.readthedocs.io/en/stable/config/options/terminal.html>
- NumPy print options: <https://numpy.org/doc/stable/reference/generated/numpy.set_printoptions.html>
- NumPy 1.14 release notes: <https://numpy.org/doc/1.14/release.html>
- NumPy 2.2 release notes: <https://numpy.org/doc/2.2/release/2.2.0-notes.html>
- pandas options: <https://pandas.pydata.org/docs/user_guide/options.html>
- R `print.default`: <https://stat.ethz.ch/R-manual/R-devel/library/base/html/print.default.html>
- R `options`: <https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html>
- R width transcript: <https://stackoverflow.com/questions/72056213/how-to-set-the-width-of-an-error-message-in-r>
- R `max.print` usage: <https://stat.ethz.ch/pipermail/r-help/2009-July/396021.html>
- R-devel omitted-count bug discussion: <https://stat.ethz.ch/pipermail/r-devel/2022-January/081410.html>
- tibble printing: <https://tibble.tidyverse.org/reference/formatting.html>
- tibble vignette: <https://tibble.tidyverse.org/articles/tibble.html>
- pillar options: <https://pillar.r-lib.org/reference/pillar_options.html>
- tibble changelog: <https://tibble.tidyverse.org/news/index.html>
- MATLAB Command Window settings: <https://www.mathworks.com/help/matlab/matlab_env/command-window-settings.html>
- MATLAB format output / paging: <https://www.mathworks.com/help/matlab/matlab_env/format-output.html>
- MATLAB long row-vector transcript (R2025b): <https://www.mathworks.com/matlabcentral/answers/2182689-how-to-display-row-vectors-without-extra-spaces-or-line-breaks-in-matlab-r2025b>
- MATLAB width behavior (MathWorks Support, 2009): <https://www.mathworks.com/matlabcentral/answers/98929-how-can-i-display-a-wide-matrix-in-the-matlab-command-window-without-the-rows-being-wrapped>
- GNU Octave `split_long_rows`: <https://octave.sourceforge.io/octave/function/split_long_rows.html>
- GNU Octave terminal output: <https://docs.octave.org/latest/Terminal-Output.html>
- GHCi custom interactive printing: <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html>
- `pretty-simple`: <https://hackage.haskell.org/package/pretty-simple>
- Rust RFC 640: <https://rust-lang.github.io/rfcs/0640-debug-improvements.html>
- Common Lisp printer controls overview: <https://www.lispworks.com/documentation/HyperSpec/Body/22_aaa.htm>
- Common Lisp `*print-length*`: <https://www.lispworks.com/documentation/HyperSpec/Body/v_pr_lev.htm>
- Common Lisp `*print-right-margin*`: <https://www.lispworks.com/documentation/HyperSpec/Body/v_pr_rig.htm>
- Common Lisp `*print-lines*`: <https://www.lispworks.com/documentation/HyperSpec/Body/v_pr_lin.htm>
- Common Lisp X3J13 pretty-printer issue: <https://www.lispworks.com/documentation/HyperSpec/Issues/iss270_w.htm>
- Dyalog APL `⎕PW`: <https://docs.dyalog.com/21.0/language-reference-guide/system-functions/pw/>
- Dyalog Auto_PW: <https://docs.dyalog.com/21.0/windows-installation-and-configuration-guide/configuration-parameters/auto-pw/>
- J shape/display tutorial: <https://www.jsoftware.com/help/jforc/declarations.htm>
- J output-control parameters: <https://www.jsoftware.com/help/dictionary/dx009.htm>
- Wolfram `PageWidth`: <https://reference.wolfram.com/language/ref/PageWidth.html>
- Wolfram `Short`: <https://reference.wolfram.com/language/ref/Short.html>
- Wolfram `Skeleton`: <https://reference.wolfram.com/language/ref/Skeleton.html>
- Wolfram `TotalWidth`: <https://reference.wolfram.com/language/ref/TotalWidth.html>
- Wolfram `OutputSizeLimit`: <https://reference.wolfram.com/language/ref/OutputSizeLimit.html>
- Maple Vector: <https://www.maplesoft.com/support/help/Maple/view.aspx?path=Vector>
- Maple Matrix: <https://www.maplesoft.com/support/help/Maple/view.aspx?path=Matrix>
- Maple 2024 interface / scrollable rtables: <https://www.maplesoft.com/support/help/maple/view.aspx?path=updates%2FMaple2024%2FInterface>
- Maple 2025 interface: <https://www.maplesoft.com/support/help/maple/view.aspx?path=updates%2FMaple2025%2FInterface>
- Polars Series: <https://docs.pola.rs/api/python/version/1/reference/series/index.html>
- Polars `set_tbl_rows`: <https://docs.pola.rs/docs/python/dev/reference/api/polars.Config.set_tbl_rows.html>
- PyTorch print options: <https://docs.pytorch.org/docs/stable/generated/torch.set_printoptions.html>
- JAX print options: <https://docs.jax.dev/en/latest/_autosummary/jax.numpy.set_printoptions.html>
- xarray options: <https://docs.xarray.dev/en/stable/generated/xarray.set_options.html>
- jq manual: <https://jqlang.org/manual/>
- Microsoft Excel row/column headings: <https://support.microsoft.com/en-us/office/print-row-and-column-headings-2f583ab0-a7a3-44a6-9a88-a5b17a7eeb41>
