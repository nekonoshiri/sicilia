" Vim filetype plugin file
" Language: haskell

""""""""""""""""""""
"
" vim-haskell-sort-import
"
""""""""""""""""""""

" ファイル保存時に import をソート
autocmd BufWritePre <buffer> HaskellSortImport
