" Vim indent file
" Language:     Haskell
" Author:       motemen <motemen@gmail.com>
" Version:      0.1
" Last Change:  2007-07-25

if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetHaskellIndent()
setlocal indentkeys=!^F,o,O,0<Bar>,0=where,0=else,0=in

let b:undo_indent = 'setlocal 
      \ autoindent<
      \ expandtab<
      \ indentexpr<
      \ indentkeys<
      \ shiftwidth<
      \ tabstop<
      \'

function! GetHaskellIndent()
  let currline = getline(v:lnum)
  let currindt = indent(v:lnum)
  let prevline = getline(v:lnum - 1)
  let previndt = indent(v:lnum - 1)

  if prevline =~ '\%([!#$%&*+\./<=>?@\\^|~-]\|\<do\|{\)\s*\%(--.*\)\?$'
    let idx = match(prevline, '\<where\s\+\zs\S')
    return (idx > 0 ? idx : previndt) + &shiftwidth
  endif

  if prevline =~ '^\%(instance\|class\)\>.*\&.*\<where\s*\%(--.*\)\?$'
    return &shiftwidth
  endif

  if prevline =~ ')\s*\%(--.*\)\?$'
    let pos = getpos('.')
    normal k$
    let paren_end = getpos('.')
    normal %
    let paren_bgn = getpos('.')
    call cursor(pos)
    if paren_bgn[1] != paren_end[1]
      return paren_bgn[2] - 1
    endif
  endif

  let idx = match(prevline, '\<if\>')
  if idx > 0 && prevline !~ '\<then\>'
    return idx + &shiftwidth
  endif

  if currline =~ '^\s*else\>'
    let pos = searchpos('\<then\>', 'bnW')
    if pos[1] > 0
      return pos[1] - 1
    endif
  endif

  if prevline =~ '\<\%(case\|let\|where\)\s*\%(--.*\)\?$'
    return previndt + &shiftwidth
  endif

  let idx = match(prevline, '\<do\s\+\zs[^{]\|\<\%(case\>.*\&.*\<of\|where\)\s\+\zs\S')
  if idx > 0
    return idx
  endif

  if currline =~ '^\s*|'
    let idx = match(prevline, '\s\zs|\s')
    return idx > 0 ? idx : previndt + &shiftwidth
  endif

  if prevline =~ '\s|\s'
    let lnum = v:lnum - 1
    while getline(lnum - 1) =~ '\s|\s'
      let lnum -= 1 
    endwhile
    return indent(lnum - (getline(lnum) =~ '^\s*|')) + &shiftwidth
  endif

  if currline =~ '^\s*in\>'
    let pos = searchpos('\<let\>', 'bnW')
    if pos[1] > 0
      return pos[1] - 1
    endif
  endif

  let idx = match(prevline, '\<case\s\+\zs\S')
  if idx > 0
    return idx
  endif

  if currline =~ '^\s*where\>' && previndt == 0
    return &shiftwidth
  endif

  return currindt == 0 ? previndt : -1
endfunction
