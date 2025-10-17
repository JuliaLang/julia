# This file is a part of Julia. License is MIT: https://julialang.org/license

struct SelectorState
    area::@NamedTuple{height::Int, width::Int}
    query::String
    filter::FilterSpec
    candidates::Vector{HistEntry}
    scroll::Int
    selection::@NamedTuple{active::Vector{Int}, gathered::Vector{HistEntry}}
    hover::Int
end

SelectorState((height, width), query::String, filter::FilterSpec, candidates::Vector{HistEntry} = HistEntry[], gathered::Vector{HistEntry} = HistEntry[]) =
    SelectorState((height, width), query, filter, candidates, -length(gathered), (; active = Int[], gathered), 1)

const EMPTY_STATE = SelectorState((0, 0), "", FilterSpec(), [], 0, (active = Int[], gathered = HistEntry[]), 0)

STATES = Pair{SelectorState, SelectorState}[]

const LABELS = (
    gatherdivider = S"{italic:carried over}",
    preview_suggestion = S"Ctrl+S to save",
    help_prompt = S"{REPL_History_search_hint,shadow:try {REPL_History_search_hint,(slant=normal):?} for help} ",
)

const SYNC_UPDATE_BEGIN = "\eP=1s\e\\"
const SYNC_UPDATE_END = "\eP=2s\e\\"
const CLEAR_BELOW = "\e[1G\e[J"

"""
    redisplay_all(io::IO, oldstate::SelectorState, newstate::SelectorState, pstate::PromptState; buf)

Diff and redraw the entire UI (candidates, preview, prompt).

Uses ANSI sync sequences to update only changed regions between
`oldstate` and `newstate`, then reprints the prompt.
"""
function redisplay_all(io::IO, oldstate::SelectorState, newstate::SelectorState, pstate::REPL.LineEdit.PromptState;
                       buf::IOContext{IOBuffer} = IOContext(IOBuffer(), io))
    # Calculate dimensions
    oldrows = componentrows(oldstate)
    newrows = componentrows(newstate)
    # Redisplay components
    synccap = haskey(Base.current_terminfo(), :Sync)
    synccap && print(buf, SYNC_UPDATE_BEGIN)
    currentrow = 0
    if newstate.query == FILTER_SHORTHELP_QUERY
        print(buf, CLEAR_BELOW * '\n', FILTER_SHORTHELP)
        currentrow += 1 + count('\n', String(FILTER_SHORTHELP))
    elseif newstate.query == FILTER_LONGHELP_QUERY
        print(buf, CLEAR_BELOW * '\n', FILTER_LONGHELP)
        currentrow += 1 + count('\n', String(FILTER_LONGHELP))
    else
        println(buf) # Move to line under prompt
        currentrow += 1
        if oldstate.area.width > newstate.area.width || oldstate.query == FILTER_SHORTHELP_QUERY
            print(buf, CLEAR_BELOW)
            oldstate = EMPTY_STATE
        end
        refresh_cands = oldstate.query != newstate.query ||
            length(oldstate.candidates) != length(newstate.candidates) ||
            oldstate.area != newstate.area ||
            oldstate.scroll != newstate.scroll ||
            oldstate.selection.active != newstate.selection.active ||
            oldstate.hover != newstate.hover ||
            oldstate.filter != newstate.filter
        refresh_preview = refresh_cands ||
            oldstate.selection.gathered != newstate.selection.gathered ||
            gethover(oldstate) != gethover(newstate)
        if refresh_cands
            redisplay_candidates(buf, oldstate, oldrows.candidates, newstate, newrows.candidates)
            currentrow += newrows.candidates
        end
        if refresh_preview
            if !refresh_cands
                print(buf, '\n' ^ newrows.candidates)
                currentrow += newrows.candidates
            end
            redisplay_preview(buf, oldstate, oldrows.preview, newstate, newrows.preview)
            currentrow += max(0, newrows.preview - 1)
        end
    end
    # Restore row pos
    print(buf, "\e[", currentrow, "A\e[1G")
    redisplay_prompt(buf, oldstate, newstate, pstate)
    # Restore column pos
    print(buf, "\e[", textwidth(PROMPT_TEXT) + position(pstate.input_buffer) + 1, 'G')
    synccap && print(buf, SYNC_UPDATE_END)
    if Base.generating_output()
        # Write output in chunks seems to avoid a hang that happens here during precompilation
        # of the history on mac (io gets full without anything draining it?)
        seekstart(buf.io)
        data = read(buf.io)
        for chunk in Iterators.partition(data, 32)
            write(io, chunk)
            flush(io)
        end
    else
        write(io, seekstart(buf.io))
    end
    truncate(buf.io, 0)
    flush(io)
end

"""
    componentrows(state::SelectorState) -> (; candidates::Int, preview::Int)

Split available terminal rows into candidate list and preview panes.

Clamps preview height between one-third and two-thirds of the usable area.
"""
function componentrows(state::SelectorState)
    available_rows = 2 * (state.area.height - 1) ÷ 3 # REVIEW: maybe `min(height, ?)`
    preview_min, preview_max = available_rows ÷ 3, 2 * available_rows ÷ 3
    nlines_preview = countlines_selected(state)
    # To prevent jittering when key-repeat is happening with TAB at
    # the end of a list of multiple selected candidates, stop
    # the final candidate from affecting the size of the preview pane.
    if length(state.selection.active) > 2 &&
        last(state.selection.active) == lastindex(state.candidates)
        nlines_preview -= count('\n', state.candidates[end].content) + 1
    end
    preview_rows = clamp(nlines_preview, preview_min, preview_max)
    if preview_min <= 2
        preview_rows = 0 # Not worth just showing the frame
    end
    candidate_rows = available_rows - preview_rows
    (; candidates = candidate_rows, preview = preview_rows)
end

"""
    countlines_selected(state::SelectorState) -> Int

Count display lines needed for active and gathered entries.

Includes one line per entry plus extra lines for multi-line content
and a divider if any gathered entries exist.
"""
function countlines_selected((; candidates, selection)::SelectorState)
    (; active, gathered) = selection
    nlines = 0
    for idx in active
        entry = candidates[idx]
        nlines += 1 + count('\n', entry.content)
    end
    if !isempty(gathered)
        nlines += 1 # The divider line
        for entry in gathered
            nlines += 1 + count('\n', entry.content)
        end
    end
    nlines
end

const BASE_MODE = :julia

const MODE_FACES = Dict(
    :julia => :green,
    :shell => :red,
    :pkg => :blue,
    :help => :yellow,
)

"""
    redisplay_prompt(io::IO, oldstate::SelectorState, newstate::SelectorState, pstate::PromptState)

Redraw just the prompt line with updated query, separators, and hints.

Styles prefixes, match-type indicators, and result counts based on cursor position in `pstate`.
"""
function redisplay_prompt(io::IO, oldstate::SelectorState, newstate::SelectorState, pstate::REPL.LineEdit.PromptState)
    # oldstate.query == newstate.query && return
    hov = gethover(newstate)
    query = newstate.query
    styquery = S"$query"
    styconds = ConditionSet(styquery)
    qpos = position(pstate.input_buffer)
    kindname = ""
    patend = 0
    for (name, substrs) in (("words", styconds.words),
                            ("exact", styconds.exacts),
                            ("negated", styconds.negatives),
                            ("initialism", styconds.initialisms),
                            ("regexp", styconds.regexps),
                            ("fuzzy", styconds.fuzzy),
                            ("mode", styconds.modes))
        for substr in substrs
            start, len = substr.offset, substr.ncodeunits
            patend = max(patend, start + len)
            if start > 1
                if query[start] == FILTER_SEPARATOR
                    face!(styquery[start:start], :REPL_History_search_separator)
                else
                    face!(styquery[start:start], :REPL_History_search_prefix)
                    face!(styquery[start-1:start-1], :REPL_History_search_separator)
                end
            elseif start > 0
                face!(styquery[start:start],
                      if query[start] == FILTER_SEPARATOR
                          :REPL_History_search_separator
                      else
                          :REPL_History_search_prefix
                      end)
            end
            isempty(kindname) || continue
            if start <= qpos <= start + len
                kindname = name
                break
            end
        end
    end
    if patend < ncodeunits(query)
        if query[patend+1] == FILTER_SEPARATOR
            face!(styquery[patend+1:patend+1], :REPL_History_search_separator)
            if patend + 1 < ncodeunits(query) && query[patend+2] ∈ FILTER_PREFIXES
                face!(styquery[patend+2:patend+2], :REPL_History_search_prefix)
            elseif isempty(kindname)
                kindname = "separator"
            end
        elseif ncodeunits(query) == 1 && query[1] ∈ FILTER_PREFIXES
            face!(styquery[1:1], :REPL_History_search_prefix)
        end
    end
    prefix = S"{bold:▪:} "
    ncand = length(newstate.candidates)
    resultnum = S"{REPL_History_search_results:[$(ncand - newstate.hover + 1)/$ncand]}"
    padspaces = newstate.area.width - sum(textwidth, (prefix, styquery, resultnum))
    suffix = if isempty(styquery)
        LABELS.help_prompt
    elseif newstate.query ∈ (FILTER_SHORTHELP_QUERY, FILTER_LONGHELP_QUERY)
        S"{REPL_History_search_hint:help} "
    elseif kindname != ""
        S"{REPL_History_search_hint:$kindname} "
    else
        S""
    end
    if textwidth(suffix) < padspaces
        padspaces -= textwidth(suffix)
    else
        suffix = S""
    end
    # TODO: Replace with a face-based approach when possible
    print(io, pstate.p.prompt_prefix, prefix, "\e[0m",
          styquery, ' ' ^ max(0, padspaces), suffix, resultnum)
end

# Unicode circles:
# - large: ● ○
# - medium: ⏺🞉🞈🞇🞆🞅⚬🞊⦿⦾
# - small: •⋅∙∘◦
# - dots: 🞄⁃·

const LIST_MARKERS = if Sys.isapple()
    # '🞇' is not available by default, and '⬤' is oversized, so we must compromise.
    (selected = AnnotatedChar('⏺', [(:face, :REPL_History_search_selected)]),
     hover = AnnotatedChar('⦿', [(:face, :REPL_History_search_selected)]),
     unselected = AnnotatedChar('◦', [(:face, :REPL_History_search_unselected)]),
     pending = AnnotatedChar('·', [(:face, :shadow)]))
else
    # Linux tends to have pretty fantastic OOTB Unicode support, with fonts
    # like Symbola installed by default, so we can go for the best symbols.
    (selected = AnnotatedChar('⬤', [(:face, :REPL_History_search_selected)]),
     hover = AnnotatedChar('🞇', [(:face, :REPL_History_search_selected)]),
     unselected = AnnotatedChar('◦', [(:face, :REPL_History_search_unselected)]),
     pending = AnnotatedChar('🞄', [(:face, :shadow)]))
end

const NEWLINE_MARKER = S"{shadow:↩ }"
const LINE_ELLIPSIS = S"{shadow:…}"

"""
    hoveridx(state::SelectorState) -> Int

Compute the signed index into `candidates` or `gathered` for hover.

Positive values index `candidates`, negative values index `gathered`, zero is
invalid.
"""
function hoveridx(state::SelectorState)
    if state.hover > 0
        length(state.candidates) - state.hover + 1
    else
        state.hover
    end
end

"""
    ishover(state::SelectorState, idx::Int) -> Bool

Return true if `idx` matches the current hover position.

Used to highlight the hovered line in the UI.
"""
ishover(state::SelectorState, idx::Int) = idx == hoveridx(state)

"""
    gethover(state::SelectorState) -> Union{HistEntry, Nothing}

Return the `HistEntry` under the cursor (hover position), or `nothing`.

Handles positive hover for `candidates` and negative for `gathered`.
"""
function gethover(state::SelectorState)
    idx = hoveridx(state)
    if idx ∈ axes(state.candidates, 1)
        state.candidates[idx]
    elseif idx < 0 && -idx ∈ axes(state.selection.gathered, 1)
        state.selection.gathered[-idx]
    end
end

struct CandsState{V<:AbstractVector{HistEntry}}
    search::FilterSpec
    entries::V
    selected::Vector{Int}
    hover::Int
    rows::Int
    width::Int
end


"""
    candidates(state::SelectorState, rows::Int) -> (; active::CandsState, gathered::CandsState)

Compute visible slices of active and gathered entries for display.
"""
function candidates(state::SelectorState, rows::Int)
    gathshift = 0
    gathcount = clamp(-state.scroll, 0, length(state.selection.gathered))
    if gathcount >= rows
        gathshift = gathcount - rows + 1
        gathcount = rows - 1
    end
    actcount = rows - gathcount - sign(gathcount)
    offset = max(0, length(state.candidates) - actcount - max(0, state.scroll))
    candend = offset + actcount
    actcands = @view state.candidates[max(begin, begin+offset):min(end, candend)]
    actempty = actcount - length(actcands)
    actsel = Int[idx - offset for idx in state.selection.active]
    if !isempty(state.selection.gathered)
        append!(actsel, filter!(!isnothing, indexin(state.selection.gathered, actcands)))
    end
    active = CandsState(
        state.filter,
        actcands,
        actsel,
        rows + state.scroll - state.hover - actempty + gathshift + (state.scroll >= 0),
        actcount,
        state.area.width)
    gathcands = @view state.selection.gathered[begin+gathshift:min(end, gathshift+gathcount)]
    gathered = CandsState(
        state.filter,
        gathcands,
        collect(axes(gathcands, 1)),
        -state.hover - gathshift,
        gathcount,
        state.area.width)
    (; active, gathered)
end

"""
    redisplay_candidates(io::IO, oldstate::SelectorState, oldrows::Int, newstate::SelectorState, newrows::Int)

Diff and redraw the candidate list pane between two states.

Only lines that changed (entry text, selection, hover, width) are reprinted;
unchanged lines remain.
"""
function redisplay_candidates(io::IO, oldstate::SelectorState, oldrows::Int, newstate::SelectorState, newrows::Int)
    danglingdivider = false
    if oldstate.scroll < 0 && newstate.scroll == 0
        newrows -= 1
        danglingdivider = true
    end
    oldcands = candidates(oldstate, oldrows)
    newcands = candidates(newstate, newrows)
    samefilter = oldstate.filter == newstate.filter
    # Redisplay active candidates
    update_candidates(io, oldcands.active, newcands.active,
                      !samefilter || oldstate.scroll == 0 && !isempty(oldstate.selection.gathered))
    # Redisplay gathered candidates
    gathchange = oldrows != newrows || length(oldcands.gathered.entries) != length(newcands.gathered.entries)
    if isempty(newcands.gathered.entries) && !danglingdivider
    elseif gathchange || danglingdivider || oldstate.area != newstate.area
        netlines = newstate.area.width - textwidth(LABELS.gatherdivider) - 6
        leftlines = netlines ÷ 2
        rightlines = netlines - leftlines
        println(io, S" {shadow:╶$('─' ^ leftlines)╴$(LABELS.gatherdivider)╶$('─' ^ rightlines)╴} ")
    else
        println(io)
    end
    update_candidates(io, oldcands.gathered, newcands.gathered, gathchange != 0)
end

"""
    update_candidates(io::IO, oldcands::CandsState, newcands::CandsState, force::Bool = false)

Write an update to `io` that changes the display from `oldcands` to `newcands`.

Only changes are printed, and exactly `length(newcands.entries)` lines are printed.
"""
function update_candidates(io::IO, oldcands::CandsState, newcands::CandsState, force::Bool = false)
    thisline = 1
    for (i, (old, new)) in enumerate(zip(oldcands.entries, newcands.entries))
        oldsel, newsel = i ∈ oldcands.selected, i ∈ newcands.selected
        oldhov, newhov = i == oldcands.hover, i == newcands.hover
        if !force && old == new && oldsel == newsel && oldhov == newhov && oldcands.width == newcands.width
            println(io)
        else
            print_candidate(io, newcands.search, new, newcands.width;
                            selected = newsel, hover = newhov)
        end
        thisline = i + 1
    end
    for (i, new) in enumerate(newcands.entries)
        i <= length(oldcands.entries) && continue
        print_candidate(io, newcands.search, new, newcands.width;
                        selected = i ∈ newcands.selected,
                        hover = i == newcands.hover)
        thisline = i + 1
    end
    for _ in thisline:newcands.rows
        print(io, "\e[K ", LIST_MARKERS.pending, '\n')
    end
end

const DURATIONS = (
    m = 60,
    h = 60 * 60,
    d = 24 * 60 * 60,
    w = 7 * 24 * 60 * 60,
    y = 365 * 24 * 60 * 60,
)

"""
    humanage(seconds::Integer) -> String

Convert `seconds` into a compact age string with largest unit.

```julia-repl
julia> humanage(70)
"1m"

julia> humanage(4000)
"1h"
```
"""
function humanage(seconds::Integer)
    unit, count = :s, seconds
    for (dunit, dsecs) in pairs(DURATIONS)
        n = seconds ÷ dsecs
        n == 0 && break
        unit, count = dunit, n
    end
    "$count$unit"
end

"""
    print_candidate(io::IO, search::FilterSpec, cand::HistEntry, width::Int; selected::Bool, hover::Bool)

Render one history entry line with markers, mode hint, age, and highlighted content.

Truncates and focuses on matches to fit `width`.
"""
function print_candidate(io::IO, search::FilterSpec, cand::HistEntry, width::Int; selected::Bool, hover::Bool)
    print(io, ' ', if selected
              LIST_MARKERS.selected
          elseif hover
              LIST_MARKERS.hover
          else
              LIST_MARKERS.unselected
          end, ' ')
    age = humanage(floor(Int, ((now(UTC) - cand.date)::Millisecond).value ÷ 1000))
    agedec = S" {shadow,light,italic:$age}"
    modehint = if cand.mode == BASE_MODE
        S""
    else
        modeface = get(MODE_FACES, cand.mode, :grey)
        if hover
            S"{region: {bold,inverse,$modeface: $(cand.mode) }}"
        elseif ncodeunits(age) == 2
            S" {$modeface:◼}  "
        else
            S" {$modeface:◼} "
        end
    end
    decorationlen = 3 #= spc + marker + spc =# + textwidth(modehint) + textwidth(agedec) + 1 #= spc =#
    flatcand = replace(highlightcand(cand), r"\r?\n\s*" => NEWLINE_MARKER)
    candstr = focus_matches(search, flatcand, width - decorationlen)
    if hover
        face!(candstr, :region)
        face!(agedec, :region)
    end
    println(io, candstr, modehint, agedec, ' ')
end

"""
    highlightcand(cand::HistEntry) -> AnnotatedString

Syntax-highlight Julia content or return raw content otherwise.
"""
function highlightcand(cand::HistEntry)
    if cand.mode === :julia
        highlight(cand.content)
    else
        S"$(cand.content)"
    end
end

"""
    focus_matches(search::FilterSpec, content::AnnotatedString, targetwidth::Int) -> AnnotatedString

Center and trim `content` around matching regions, adding ellipses.

To best display matches, this function operates in multiple stages:
1. Find all matching character ranges in `content` via `matchregions(search, String(content))`.
2. Choose a primary match region that can be fully shown within `targetwidth`,
   preferring the first match.
3. Starting from the end of that region, expand a window leftwards up to
   `targetwidth`, accounting for character widths.
4. If the left bound exceeds the start of `content`, reserve space for a leading
   ellipsis (`LINE_ELLIPSIS`) and adjust the window.
5. Expand the window rightwards similarly, inserting a trailing ellipsis if
   there is remaining text.
6. Slice out the computed substring from `content`, preserving existing annotations.
7. Re-apply the match highlight face (`:REPL_History_search_match`) to any
   regions within the window.
8. Pad the result with spaces if its width is less than `targetwidth`.

The returned `AnnotatedString` is exactly `targetwidth` columns wide,
guaranteeing at least one full match is visible and highlighted.
"""
function focus_matches(search::FilterSpec, content::AnnotatedString{String}, targetwidth::Int)
    cstr = String(content) # zero-cost
    mregions = matchregions(search, cstr)
    isempty(mregions) && return rpad(rtruncate(content, targetwidth, LINE_ELLIPSIS), targetwidth)
    mstart = first(first(mregions))
    mlast = first(mregions)
    ellipwidth = textwidth(LINE_ELLIPSIS)
    # Assume approximately one cell per character, and refine later
    for (i, region) in Iterators.reverse(enumerate(mregions))
        if first(region) - mstart <= targetwidth - 2 * ellipwidth
            mlast = region
            break
        end
    end
    # Start at the end of the last region, and extend backwards `targetwidth` characters
    left, right = let pos = thisind(cstr, last(mlast)); (pos, pos) end
    width = textwidth(cstr[left])
    while left > firstindex(cstr)
        lnext = prevind(cstr, left)
        lwidth = textwidth(cstr[lnext])
        if width + lwidth > targetwidth - 2 * ellipwidth
            break
        end
        width += lwidth
        left = lnext
    end
    # Check to see if we have reached the beginning of the first match,
    # if we haven't we want to shrink the region to the left until the
    # beginning of the first match is reached.
    if left > first(mstart)
        while left > first(mstart)
            left = prevind(cstr, left)
            lwidth = textwidth(cstr[left])
            width += lwidth
            # We'll move according to the assumption that each character
            # is one cell wide, but account for the width correctly and
            # adjust for any underestimate later.
            for _ in 1:lwidth
                width -= textwidth(cstr[right])
                right = prevind(cstr, right)
                right == left && break
            end
        end
    end
    isltrunc, isrtrunc = left > firstindex(cstr), right < lastindex(cstr)
    # Use any available space to extend to the left.
    if width < targetwidth - (isltrunc + isrtrunc) * ellipwidth && left < firstindex(cstr)
        while left < firstindex(cstr)
            lnext = prevind(cstr, left)
            lwidth = textwidth(cstr[lnext])
            isnextltrunc = lnext > firstindex(cstr)
            nellipsis = isnextltrunc + isrtrunc
            if width + lwidth > targetwidth - nellipsis * ellipwidth
                break
            end
            width += lwidth
            left = lnext
        end
        isltrunc = left > firstindex(cstr)
    end
    # Use any available space to extend to the right.
    if width < targetwidth - (isltrunc + isrtrunc) * ellipwidth && right < lastindex(cstr)
        while right < lastindex(cstr)
            rnext = nextind(cstr, right)
            rwidth = textwidth(cstr[rnext])
            isnextrtrunc = rnext < lastindex(cstr)
            nellipsis = isltrunc + isnextrtrunc
            if width + rwidth > targetwidth - nellipsis * ellipwidth
                break
            end
            width += rwidth
            right = rnext
        end
    end
    # Construct the new region
    regstr = AnnotatedString(content[left:right])
    # Emphasise matches
    for region in mregions
        (last(region) < left || first(region) > right) && continue
        adjregion = (max(left, first(region)) - left + 1):(min(right, last(region)) - left + 1)
        face!(regstr, adjregion, :REPL_History_search_match)
    end
    # Add ellipses
    ellipstr = if left > firstindex(cstr) && right < lastindex(cstr)
        width += 2 * ellipwidth
        LINE_ELLIPSIS * regstr * LINE_ELLIPSIS
    elseif left > firstindex(cstr)
        width += ellipwidth
        LINE_ELLIPSIS * regstr
    elseif right < lastindex(cstr)
        width += ellipwidth
        regstr * LINE_ELLIPSIS
    else
        regstr
    end
    # Pad (if necessary)
    if width < targetwidth
        rpad(ellipstr, targetwidth)
    else
        ellipstr
    end
end

"""
    redisplay_preview(io::IO, oldstate::SelectorState, oldrows::Int, newstate::SelectorState, newrows::Int)

Diff and redraw the preview pane (right side) with boxed content.

Shows hover or gathered entries in a box.
"""
function redisplay_preview(io::IO, oldstate::SelectorState, oldrows::Int, newstate::SelectorState, newrows::Int)
    newrows == 0 && return
    function getcand(state::SelectorState, idx::Int)
        if idx ∈ axes(state.candidates, 1)
            state.candidates[idx]
        elseif -idx ∈ axes(state.selection.gathered, 1)
            state.selection.gathered[-idx]
        else
            throw(ArgumentError("Invalid candidate index: $idx")) # Should never happen
        end
    end
    function getselidxs(state::SelectorState)
        idxs = collect(-1:-1:-length(state.selection.gathered))
        append!(idxs, state.selection.active)
        sort!(idxs, by = i -> getcand(state, i).index)
    end
    rtruncpad(s::AbstractString, width::Int) =
        rpad(rtruncate(s, width, LINE_ELLIPSIS), width)
    bar = S"{shadow:│}"
    innerwidth = newstate.area.width - 2
    if oldstate.area != newstate.area || (oldstate.area.height - oldrows) != (newstate.area.height - newrows)
        println(io, S"{shadow:╭$('─' ^ innerwidth)╮}")
    else
        println(io)
    end
    if newrows - 2 < 1
        # Well, this is awkward.
    elseif isempty(newstate.selection.active) && isempty(newstate.selection.gathered)
        linesprinted = if (gethover(newstate) != gethover(oldstate) ||
            oldstate.area != newstate.area ||
            oldrows != newrows ||
            oldstate.filter != newstate.filter)
            hovcand = gethover(newstate)
            if !isnothing(hovcand)
                hovcontent = highlightcand(hovcand)
                for region in matchregions(newstate.filter, String(hovcontent))
                    face!(hovcontent[region], :REPL_History_search_match)
                end
                if hovcand.mode !== BASE_MODE
                    mcolor = get(MODE_FACES, hovcand.mode, :grey)
                    hovcontent = S"{bold,$mcolor:$(hovcand.mode)>} " * hovcontent
                end
                boxedcontent(io, hovcontent, newstate.area.width, newrows - 2)
            else
                0
            end
        else
            print(io, '\n' ^ (newrows - 2))
            newrows - 2
        end
        for _ in (linesprinted + 1):(newrows - 2)
            println(io, bar, ' '^innerwidth, bar)
        end
    else
        linesprinted = 0
        seltexts = AnnotatedString{String}[]
        for idx in getselidxs(newstate)
            entry = getcand(newstate, idx)
            content = highlightcand(entry)
            ishover(newstate, idx) && face!(content, :region)
            push!(seltexts, content)
        end
        linecount = sum(t -> 1 + count('\n', String(t)), seltexts, init=0)
        for (i, content) in enumerate(seltexts)
            clines = 1 + count('\n', String(content))
            if linesprinted + clines < newrows - 2 || (i == length(seltexts) && linesprinted + clines == newrows - 2)
                for line in eachsplit(content, '\n')
                    println(io, bar, ' ', rtruncpad(line, innerwidth - 2), ' ', bar)
                end
                linesprinted += clines
            else
                remaininglines = newrows - 2 - linesprinted
                for (i, line) in enumerate(eachsplit(content, '\n'))
                    i == remaininglines && break
                    println(io, bar, ' ', rtruncpad(line, innerwidth - 2), ' ', bar)
                end
                msg = S"{julia_comment:⋮ {italic:$(linecount - newrows + 3) lines hidden}}"
                println(io, bar, ' ', rtruncpad(msg, innerwidth - 2), ' ', bar)
                linesprinted += remaininglines
                break
            end
        end
        for _ in (linesprinted + 1):(newrows - 2)
            println(io, bar, ' ' ^ innerwidth, bar)
        end
    end
    if oldstate.area != newstate.area || length(oldstate.selection.active) != length(newstate.selection.active)
        if textwidth(LABELS.preview_suggestion) < innerwidth
            line = '─' ^ (innerwidth - textwidth(LABELS.preview_suggestion) - 2)
            print(io, S"{shadow:╰$(line)╴$(LABELS.preview_suggestion)╶╯}")
        else
            print(io, S"{shadow:╰$('─' ^ innerwidth)╯}")
        end
    end
end

"""
    boxedcontent(io::IO, content::AnnotatedString, width::Int, maxlines::Int) -> Int

Draw `content` inside a Unicode box, wrapping or truncating to `width` and `maxlines`.

Returns the number of printed lines.
"""
function boxedcontent(io::IO, content::AnnotatedString{String}, width::Int, maxlines::Int)
    function breaklines(content::AnnotatedString{String}, maxwidth::Int)
        textwidth(content) <= maxwidth && return [content]
        spans = AnnotatedString{String}[]
        basestr = String(content) # Because of expensive char iteration
        start, pos, linewidth = 1, 0, 0
        for char in basestr
            linewidth += textwidth(char)
            pos = nextind(basestr, pos)
            if linewidth > maxwidth
                spans = push!(spans, AnnotatedString(content[start:prevind(basestr, pos)]))
                start = pos
                linewidth = textwidth(char)
            end
        end
        if start <= length(basestr)
            spans = push!(spans, AnnotatedString(content[start:end]))
        end
        spans
    end
    left, right = S"{shadow:│} ", S" {shadow:│}"
    leftcont, rightcont = S"{shadow:┊▸}", S"{shadow:◂┊}"
    if maxlines == 1
        println(io, left,
                rpad(rtruncate(content, width - 4, LINE_ELLIPSIS), width - 4),
                right)
        return 1
    end
    printedlines = 0
    if ncodeunits(content) > (width * maxlines)
        content = AnnotatedString(rtruncate(content, width * maxlines, ' '))
    end
    lines = split(content, '\n')
    innerwidth = width - 4
    for (i, line) in enumerate(lines)
        printedlines >= maxlines && break
        if textwidth(line) <= innerwidth
            println(io, left, rpad(line, innerwidth), right)
            printedlines += 1
            continue
        end
        plainline = String(line)
        indent, ichars = 0, 1
        while isspace(plainline[ichars])
            indent += textwidth(plainline[ichars])
            ichars = nextind(plainline, ichars)
        end
        line = @view line[ichars:end]
        spans = breaklines(AnnotatedString(line), innerwidth - 2 - indent)
        for (i, span) in enumerate(spans)
            prefix, suffix = if i == 1
                S"", S"$LINE_ELLIPSIS "
            elseif i == length(spans)
                S"$LINE_ELLIPSIS", S" "
            else
                LINE_ELLIPSIS, LINE_ELLIPSIS
            end
            printedlines += 1
            println(io, ifelse(i == 1, left, leftcont), ' ' ^ indent,
                    prefix, rpad(span, innerwidth - 2 - indent), suffix,
                    ifelse(i == length(spans) || printedlines == maxlines,
                           right, rightcont))
            printedlines >= maxlines && break
        end
    end
    printedlines
end
