" Vim plugin that generates new Haskell source file when you type
"    vim nonexistent.haskell.
"

if exists('g:loaded_vim_haskell')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

let s:vim_haskell_language_extensions = []

if exists('g:vim_haskell_language_extensions')
      \ && type(g:vim_haskell_language_extensions) == type([])
  let s:vim_haskell_language_extensions += g:vim_haskell_language_extensions
endif

function! MakeHaskellFile()
  if exists('b:applied_haskell_template')
    return
  endif

  let b:applied_haskell_template = 1

  if len(s:vim_haskell_language_extensions) == 0
    return
  endif

  call append(0, '{-# LANGUAGE ' . join(s:vim_haskell_language_extensions, ', ') . ' #-}')
endfunction

augroup plugin-vim-haskell
  au!
  au BufNewFile *.hs call MakeHaskellFile()
augroup END

let &cpo = s:save_cpo
unlet s:save_cpo

let g:loaded_vim_haskell = 1
