# Emoji Input

The following table lists the emoji that can be entered via tab completion of
their `\:name:` abbreviations in the Julia REPL (and in various other
editing environments). You can also get information on how to type an emoji by
entering it in the REPL help, i.e. by typing `?` and then entering the emoji in
the REPL (e.g., by copy-paste from somewhere you saw the emoji).

For the LaTeX-like abbreviations that complete to mathematical symbols and
other non-emoji characters, see [Unicode Input](@ref).

!!! warning

    This table may appear to contain missing characters in the second column, or even
    show characters that are inconsistent with the characters as they are rendered in
    the Julia REPL. In these cases, users are strongly advised to check their choice
    of fonts in their browser and REPL environment, as there are known issues with
    glyphs in many fonts.

```@eval
import REPL
Main.UnicodeTables.symbol_table(REPL.REPLCompletions.emoji_symbols)
```
