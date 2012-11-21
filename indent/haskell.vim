" Vim indent file
" Language:     Haskell
" Author:       motemen <motemen@gmail.com>
" Version:      0.1
" Last Change:  2012-11-19

if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetHaskellIndent()
setlocal indentkeys=!^F,o,O,0<Bar>,0),0],0},0=where,0=else,0=in

let b:undo_indent = 'setlocal
      \ autoindent<
      \ expandtab<
      \ indentexpr<
      \ indentkeys<
      \ shiftwidth<
      \ tabstop<
      \'

let s:comment_patt = '\s*\%(--.*\)\?$'

function! GetHaskellIndent()
  let currline = getline(v:lnum)
  let currindt = indent(v:lnum)
  let prevline = getline(v:lnum - 1)
  let previndt = indent(v:lnum - 1)

  if currline =~ '^\s*[)\]}]'
    let pos = getpos('.')
    normal 0%
    if line('.') != v:lnum
      let idx = col('.') - 1
      let ss = getline('.')[idx :] 
      return ss =~ '^.'.s:comment_patt ? indent('.') : idx
    endif
    call cursor(pos)
  endif

  let idx = match(prevline, '[)\]}]'.s:comment_patt)
  if idx > 0
    let pos = getpos('.')
    call cursor(v:lnum - 1, idx + 1)
    normal %
    if line('.') != v:lnum - 1
      let idx = col('.') - 1
      return idx < previndt ? idx : -1
    endif
    call cursor(pos)
  endif

  let idx = match(prevline, '\<if\>')
  if idx > 0 && prevline !~ '\<then\>'
    return idx + &shiftwidth
  endif

  if currline =~ '^\s*else\>'
    let [lnum, idx] = searchpos('\<then\>', 'bnW')
    if idx > 0
      return idx - 1
    endif
  endif

  if prevline =~ '\<\%(case\>\&.*\<of\|do\|let\|where\)'.s:comment_patt
        \ || prevline =~ '[!#$%&(*+\./<=>?@\[\\^{|~-]'.s:comment_patt
    return previndt + &shiftwidth
  endif

  let idx = match(prevline, '[(\[{]')
  if idx > 0
    let pos = getpos('.')
    call cursor(v:lnum - 1, idx + 1)
    normal %
    if line('.') != v:lnum - 1 || col('.') == idx + 1
      return idx
    endif
    call cursor(pos)
  endif

  if currline =~ '^\s*|' && previndt <= currindt
    let idx = matchend(prevline, '\s\ze|\s')
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
    let [lnum, idx] = searchpos('\<let\>', 'bnW')
    if idx > 0
      let idx -= 1
      let ss = getline(lnum)[idx :]
      return ss =~ '^let'.s:comment_patt ? previndt : idx
    endif
  endif

  let idx = matchend(prevline,
        \ '\<\%(case\>\&.*\<of\|do\|let\|where\)\s\+\ze.')
  if idx > 0
    return (currline =~ '^\s*$' || currindt > idx) ? idx : -1
  endif

  if currline =~ '^\s*where\>' && previndt == 0
    return &shiftwidth
  endif

  return previndt > currindt ? -1 : previndt
endfunction
