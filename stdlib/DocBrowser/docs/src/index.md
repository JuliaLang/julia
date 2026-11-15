```@meta
EditURL = "https://github.com/JuliaLang/julia/blob/master/stdlib/DocBrowser/docs/src/index.md"
```

# [DocBrowser](@id man-docbrowser)

`DocBrowser` is a full-screen browser of the docstrings of every loaded module. Open it by
pressing Enter at an empty `help?>` prompt, or by typing a name and pressing Alt-Enter to
open it at that name. The REPL loads the package the first time the browser is opened.

It shows docstrings, and a package's README when it has no module docstring, but not the
manuals, tutorials and guides that make up a package's full documentation. Each package's
page links to its full documentation where the browser can find it, from the docs badge in
its README.

The browser has three panes:

  * Packages: the loaded modules. The active project's direct dependencies, and modules
    loaded into `Main`, come first, followed by `Base`, `Core` and `Main`. Standard libraries
    and indirect dependencies that you did not ask for are folded away, but can be unfolded;
    each indirect dependency says which of your packages loaded it. Dependencies of the
    active project that are not loaded yet are listed too: select one and press Enter to
    load it with `import` and see its documentation.
  * Names: the names in the selected module, grouped into modules, types, functions, macros
    and constants. Each is marked `●` if exported, `○` if public, or `·` if internal.
    Internal and deprecated names are hidden until you press `i`.
  * Documentation: the docstrings of the selected name, or the module docstring (or README)
    of the selected module, with its version, licence, repository and full documentation. Functions list their
    methods, and types list their fields, subtypes and constructors.

Names mentioned in a docstring are links, and so are mentions of packages such as
`Example.jl`. Long pages start with links to their headings, extended help is folded away
until you press `x`, and each example has a link that pastes its code at the `julia>` prompt.

Use the arrow keys (or `h` `j` `k` `l`) to move, Enter to open, Tab to step through the links
of a page, and Backspace to go back. Press `/` to search the names of every loaded module,
and Ctrl-T to search the text of their docstrings instead. Press `y` to close the browser and
put the name being viewed at the `julia>` prompt, `e` to open the source of the selected
link or name in your editor (set by `JULIA_EDITOR`), `w`
to change the layout, and `?` to see every key. In terminals that support it, you can click
on anything, scroll with the mouse wheel, and use the mouse's back and forward buttons.
Right-click, Alt-click or Ctrl-click opens the source of a method, name, package or link.
Drag across the documentation to copy text, or double-click to copy a word and triple-click
to copy a line. To select text with your terminal instead, hold the key it uses for this
while you drag: Shift in most terminals, Option in iTerm2 and Fn in Terminal.app.
Terminals do not pass Cmd-click on to programs, so it cannot be used for this.

The browser opens again where it was last closed.

Packages whose repository the browser cannot find get a link that searches the web for
them. Set the `JULIA_DOCBROWSER_SEARCH_URL` environment variable to use another search
engine, with `%s` where the query goes, for example
`https://www.google.com/search?q=%s`. The default is DuckDuckGo.

```@docs
DocBrowser
DocBrowser.browse
```
