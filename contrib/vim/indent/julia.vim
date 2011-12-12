" Vim indent file
" Language:	Julia
" Maintainer:	Carlo Baldassi <carlobaldassi@gmail.com>
" Last Change:	2011 dec 11
" Notes:        based on Bram Moneaar's indent file for vim

"set nocindent
"set smartindent
set autoindent

setlocal indentexpr=GetJuliaIndent()
setlocal indentkeys+==end,=else,=catch

" Only define the function once.
if exists("*GetJuliaIndent")
  finish
endif

function GetJuliaIndent()
  " Find a non-blank line above the current line.
  let lnum = prevnonblank(v:lnum - 1)

  " At the start of the file use zero indent.
  if lnum == 0
    return 0
  endif

  " Add a 'shiftwidth' after if, else, elseif, for, while, try, catch, function, macro
  " begin, type, let and quote.
  let ind = indent(lnum)
  let line = getline(lnum)
  let i = match(line, '^\s*\(if\|while\|for\|try\|catch\|function\|else\%[if]\|macro\|begin\|type\|let\|quote\)\>')
  if i >= 0
    let ind += &sw
  endif

  " Subtract a 'shiftwidth' on a end, catch, else and elseif
  if getline(v:lnum) =~ '^\s*\(end\|catch\|else\%[if]\)\>'
    let ind = ind - &sw
  endif

  return ind
endfunction
