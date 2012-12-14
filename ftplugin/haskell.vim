" Vim filetype plugin file
" Language:         Haskell
" Maintainer:       Nikolai Weibull <now@bitwi.se>,
"                   Claus Reinke <claus.reinke@talk21.com>,
"                   etc.
" Latest Revision:  2012-12-14

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

let b:undo_ftplugin = 'setlocal
      \ comments<
      \ commentstring<
      \ formatoptions<
      \ include<
      \ includeexpr<
      \ suffixesadd<
      \'

" try gf on import line, or ctrl-x ctrl-i, or [I, [i, ..
setlocal include=^import\\s*\\(qualified\\)\\?\\s*
setlocal includeexpr=substitute(v:fname,'\\.','/','g').'.'
setlocal suffixesadd=hs,lhs,hsc

setlocal comments=s1fl:{-,mb:-,ex:-},:-- commentstring=--\ %s
setlocal formatoptions-=t formatoptions+=croql

let &cpo = s:cpo_save
unlet s:cpo_save
