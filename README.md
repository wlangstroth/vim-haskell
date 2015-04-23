# vim-haskell

A vim configuration for use with one of the following plugins.

* [pathogen](https://github.com/tpope/vim-pathogen)
* [vundle](https://github.com/gmarik/vundle)
* [neobundle](https://github.com/Shougo/neobundle.vim)
* ...

## Authorship

The original authors listed in the scripts are Claus Reinke , motemen <motemen@gmail.com>, John Williams <jrw@pobox.com>

In addition, the syntax file has the following note:

    Thanks to Ryan Crumley for suggestions and John Meacham for
    pointing out bugs. Also thanks to Ian Lynagh and Donald Bruce Stewart
    for providing the inspiration for the inclusion of the handling
    of C preprocessor directives, and for pointing out a bug in the
    end-of-line comment handling.

## Configuration

If you want to specify the certain language-extensions whenever you create new .hs file,

```vim
let g:vim_haskell_language_extensions = [
	\ 'OverloadedStrings',
	\ 'TemplateHaskell'
	\ ]
```
