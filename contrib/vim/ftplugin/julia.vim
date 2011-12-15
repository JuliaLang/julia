" Vim filetype plugin file
" Language:	Julia
" Maintainer:	Carlo Baldassi <carlobaldassi@gmail.com>
" Last Change:	2011 dec 11

if exists("b:did_ftplugin")
	finish
endif

let b:did_ftplugin = 1

setlocal include="^\s*load\>"
setlocal suffixesadd=.j
setlocal comments=:#
setlocal commentstring=#%s
setlocal define="^\s*macro\>"

" Uncomment the following line if you want operators to be
" syntax-highlightened
let julia_highlight_operators=1

" Uncomment the following two lines to force julia source-code style
set shiftwidth=4
set expandtab

if has("gui_win32")
	let b:browsefilter = "Julia Source Files (*.j)\t*.j\n"
endif
