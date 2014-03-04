" Vim indent file
" Language:     Haskell
" Author:       motemen <motemen@gmail.com>
" Last Change:  2012-12-26

if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetHaskellIndent()
setlocal indentkeys=!^F,o,O,0<Bar>,0),0],0},0=where,0=else,0=in
if !exists('g:haskell_custom_tabs') || g:haskell_custom_tabs == 0
    setlocal expandtab tabstop=8
endif

let b:undo_indent = 'setlocal
      \ autoindent<
      \ expandtab<
      \ indentexpr<
      \ indentkeys<
      \ shiftwidth<
      \ tabstop<
      \'

let s:comment_expr = '\s*\%(--.*\)\?$'

function! GetHaskellIndent()
  if v:lnum <= 1 | return 0 | endif

  let currline = getline(v:lnum)
  if currline =~ '^\s*-\s'
    return searchpos('^\s*\zs{-', 'bnW')[1]
  endif

  let currpos  = getpos('.')
  if currline =~ '^\s*[)\]}]'
    normal 0%
    if line('.') != v:lnum
      let idx = col('.') - 1
      return match(getline('.'), '^.'.s:comment_expr, idx) >= 0
            \ ? indent('.') : idx
    endif
    call cursor(currpos)
  endif

  let blankln  = currline !~ '\S'
  let quasidx  = 0
  let prevlnum = v:lnum - 1
  let prevline = substitute(getline(prevlnum), s:comment_expr, '', '')
  let previndt = indent(prevlnum)
  let currline = substitute(currline, s:comment_expr, '', '')
  let currindt = indent(v:lnum)

  if prevline =~ '\<\%(case\>\&.*\<of\|do\|let\|where\)$'
        \ || prevline =~ '[!#$%&(*+\./<=>?@\[\\^{|~-]$'
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
    let idx = match(prevline, '[)\]}]$')
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
  if idx > 0 && synIDattr(synID(prevlnum, idx, 0), 'name') == 'hsString'
    let idx = 0
  endif
  if idx > 0 && prevline !~ '\<then\>'
    return idx + &shiftwidth
  endif

  if currline =~ '^\s*else\>'
    normal b
    let [lnum, idx] = searchpairpos('\<then\>', '', '\<else\>', 'bnW')
    if idx > 0 | return idx - 1 | endif
  endif

  if blankln
    let idx = match(prevline, '\s\zs|\s', quasidx)
    if idx > 0 | return idx | endif
  endif

  if currline =~ '^\s*|' && previndt <= currindt
    let idx = match(prevline, '\s\zs|\s', quasidx)
    if idx < 0
      let idx = match(prevline, '^\s*\%(let\|where\)\s\+\zs.\+', quasidx)
      let idx = idx < 0 ? idx : idx + &shiftwidth
    endif
    return idx > 0 ? idx : previndt + &shiftwidth
  endif

  if currline =~ '^\s*in\>'
    let [lnum, idx] = searchpos('\<let\>', 'bnW')
    if idx > 0
      let idx -= 1
      return match(getline(lnum), '^let'.s:comment_expr, idx) >= 0
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

  return quasidx > 0 ? quasidx
        \ : ((blankln || currindt > previndt) ? previndt : -1)

endfunction

