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
  let currpos  = getpos('.')
  let currline = getline(v:lnum)
  let currindt = indent(v:lnum)
  let prevlnum = v:lnum - 1
  let prevline = getline(prevlnum)
  let previndt = indent(prevlnum)
  let blankln  = currline =~ '^\s*$'
  let quasidx  = 0

  if currline =~ '^\s*[)\]}]'
    normal 0%
    if line('.') != v:lnum
      let idx = col('.') - 1
      return match(getline('.'), '^.'.s:comment_patt, idx) >= 0
            \ ? indent('.') : idx
    endif
    call cursor(currpos)
  endif

  if prevline =~ '\<\%(case\>\&.*\<of\|do\|let\|where\)'.s:comment_patt
        \ || prevline =~ '[!#$%&(*+\./<=>?@\[\\^{|~-]'.s:comment_patt
    return previndt + &shiftwidth
  endif

  if prevline =~ '[(\[{]'
    normal 0
    while search('[()\[\]{}]', 'bW') == prevlnum
      let idx = col('.') - 1
      if idx == 0 | break | endif
      let c = prevline[idx]
      if c == '(' || c == '[' || c == '{'
        let quasidx = idx
        break
      endif
      normal %
      if line('.') != prevlnum | break | endif
    endwhile
    call cursor(currpos)
  endif

  if quasidx == 0
    let idx = match(prevline, '[)\]}]'.s:comment_patt)
    if idx > 0
      call cursor(prevlnum, idx + 1)
      normal %
      if line('.') != prevlnum
        let idx = col('.') - 1
        return idx < previndt ? idx : -1
      endif
      call cursor(currpos)
    endif
  endif

  let idx = match(prevline, '\<if\>', quasidx)
  if idx > 0 && prevline !~ '\<then\>'
    return idx + &shiftwidth
  endif

  if currline =~ '^\s*else\>'
    let [lnum, idx] = searchpos('\<then\>', 'bnW')
    if idx > 0 | return idx - 1 | endif
  endif

  if blankln
    let idx = match(prevline, '\s\zs|\s', quasidx)
    if idx > 0 | return idx | endif
  endif

  if currline =~ '^\s*|' && previndt <= currindt
    let idx = match(prevline, '\s\zs|\s', quasidx)
    return idx > 0 ? idx : previndt + &shiftwidth
  endif

  if currline =~ '^\s*in\>'
    let [lnum, idx] = searchpos('\<let\>', 'bnW')
    if idx > 0
      let idx -= 1
      return match(getline(lnum), '^let'.s:comment_patt, idx) >= 0
            \ ? previndt : idx
    endif
  endif

  let idx = matchend(prevline,
        \ '\<\%(case\>\&.*\<of\|do\|let\|where\)\s\+\ze.', quasidx)
  if idx > 0
    return (blankln || currindt > idx) ? idx : -1
  endif

  if currline =~ '^\s*where\>' && previndt == 0
    return &shiftwidth
  endif

  if quasidx > 0
    return quasidx
  else
    return previndt > currindt ? -1 : previndt
  endif

endfunction
