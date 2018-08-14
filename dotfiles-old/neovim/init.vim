""""""""""
" ftplugin
""""""""""

filetype plugin indent on " still needed
set runtimepath+=~/.dotfiles/neovim

""""""""""
" 表示関連
""""""""""

set number " 行番号
set list " 不可視文字
set listchars=tab:>\ ,trail:_
set cursorline " カーソル行
set colorcolumn=80 " 80 文字目
set showtabline=2 " 常にタブラインを表示

colorscheme koehler

""""""""""
" indent
""""""""""

set tabstop=2 " タブ表示幅
set expandtab " タブを空白に展開
set softtabstop=2 " タブ展開幅
set shiftwidth=2 " 自動インデント幅
set smartindent " 高度な自動インデント

""""""""""
" move
""""""""""

" 矢印キーでバッファ移動
nnoremap <Left>  :<C-u>bprev<CR>
nnoremap <Right> :<C-u>bnext<CR>

" 最上部までスクロールした後１行目に戻る
noremap <expr> <C-b> max([winheight(0) - 2, 1]) . "\<C-u>" .
  \ (line('.') < 1 + winheight(0) ? 'H' : 'L')
" 最下部までスクロール時最終行が見えたら止まる
noremap <expr> <C-f> max([winheight(0) - 2, 1]) . "\<C-d>" .
  \ (line('.') > line('$') - winheight(0) ? 'L' : 'H')
" 最後までスクロールした後はカーソル移動
noremap <expr> <C-y> (line('w0') <= 1 ? 'k' : "\<C-y>")
noremap <expr> <C-e> (line('w$') >= line('$') ? 'j' : "\<C-e>")

""""""""""
" backups
""""""""""

set backup " バックアップを残す
set backupdir-=. " カレントディレクトリにはバックアップを残さない

""""""""""
" terminal
""""""""""

" 遡行可能行数
set scrollback=10000

" ESC で command mode
tnoremap <silent> <ESC> <C-\><C-n>

" 新しいターミナル
command! Tnew :enew | :terminal

""""""""""
" その他
""""""""""

set mouse=a " 全モードでマウス使用
set hidden " 未保存でも他のファイルを開ける
set pumheight=10 " 補完メニュー高さ

 " Y で行末までヤンク
nnoremap Y y$

" W / Q
" command! -bar -nargs=* -complete=file -range=% -bang W
"   \ <line1>,<line2>write<bang> <args>
" command! -bar -nargs=* -complete=file -range=% -bang Write
"   \ <line1>,<line2>write<bang> <args>
" command! -bar -nargs=* -complete=file -range=% -bang Wq
"   \ <line1>,<line2>write<bang> <args> | :Sayonara
command! -bar -nargs=* -complete=file -range=% -bang WQ
  \ <line1>,<line2>write<bang> <args> | :Sayonara
"
" command! -bar -bang Wqall wqall<bang>
" command! -bar -bang WQall wqall<bang>
"
command! -bar Q :Sayonara
cabbrev q <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'Q' : 'q')<CR>
cabbrev wq <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'WQ' : 'wq')<CR>

" command! -bar Quit :Sayonara
" command! -bar -bang Qall qall<bang>


""""""""""
" dein
""""""""""

" dein の場所
let s:dein_dir = expand('~/.config/nvim/dein')
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

" プラグイン記述ファイルの場所
let s:toml_dir = expand('~/.dotfiles/neovim/dein_toml')
let s:toml = s:toml_dir . '/dein.toml'
let s:lazy_toml = s:toml_dir . '/dein_lazy.toml'

" dein が runtimepath になければ追加，更に dein 本体がなければ git clone
if &runtimepath !~# '/dein.vim'
  if !isdirectory(s:dein_repo_dir)
    execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
  endif
  execute "set runtimepath+=" . fnamemodify(s:dein_repo_dir, ':p')
endif

if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)
  call dein#load_toml(s:toml, {'lazy': 0})
  call dein#load_toml(s:lazy_toml, {'lazy': 1})
  call dein#end()
  call dein#save_state()
endif

" 未インストールプラグインのチェック
if dein#check_install()
  call dein#install()
endif

""""""""""
" vim-lightline
" lightline-buffer
""""""""""

" lightline-buffer の設定
let g:lightline = {
\   'tabline': {
\     'left': [['bufferinfo'], ['bufferbefore', 'buffercurrent', 'bufferafter']],
\     'right': [],
\   },
\   'component_expand': {
\     'buffercurrent': 'lightline#buffer#buffercurrent2',
\   },
\   'component_type': {
\     'buffercurrent': 'tabsel',
\   },
\   'component_function': {
\     'bufferbefore': 'lightline#buffer#bufferbefore',
\     'bufferafter': 'lightline#buffer#bufferafter',
\   'bufferinfo': 'lightline#buffer#bufferinfo',
\   },
\ }

""""""""""
" vim-better-whitespace
""""""""""

" ファイル保存時に末尾空白を自動削除
autocmd BufEnter * EnableStripWhitespaceOnSave
