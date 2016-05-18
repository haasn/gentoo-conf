" custom keybindings
noremap j gj
noremap k gk

let mapleader="\<SPACE>"

" easier buffer navigation
noremap <M-h> <C-w>h
noremap <M-j> <C-w>j
noremap <M-k> <C-w>k
noremap <M-l> <C-w>l
noremap <M-H> <C-w>H
noremap <M-J> <C-w>J
noremap <M-K> <C-w>K
noremap <M-L> <C-w>L

noremap <Leader>s <C-w>s
noremap <Leader>n :nohls<CR>:HierClear<CR>
noremap <Leader>b :make<CR>
noremap <Leader>r :checktime<CR>zuz
noremap <Leader>u :UndotreeToggle<CR>

noremap Cn :cn<CR>
noremap Cp :cp<CR>

autocmd VimResized * wincmd =

" fix bold colors
if !has('nvim')
    set term=rxvt-unicode
endif

set hlsearch
set is

syntax sync fromstart
syntax on

set background=dark
set showcmd
set ignorecase
set smartcase
set nojoinspaces
set scrolloff=3
set timeoutlen=500
set modeline
set autochdir
set autowrite
set clipboard=unnamed

set expandtab
set shiftwidth=4
set softtabstop=4

" code folding
set foldmethod=syntax
set foldnestmax=1
set foldlevel=0
set foldcolumn=1
set foldopen=block,hor,mark,percent,quickfix,search,tag,undo
let g:fastfold_fold_command_suffixes = ['x','X','a','A','o','O','c','C','r','R','m','M','i','n','N']
set sessionoptions-=folds

" au BufWritePost * normal zx
" au InsertEnter * let w:last_fdm=&foldmethod | setlocal foldmethod=manual
" au InsertLeave * let &l:foldmethod=w:last_fdm

" ctags
noremap <C-d> <C-]>
set tags=tags;
"let g:easytags_auto_highlight = 0
"let g:easytags_dynamic_files = 2
"let g:easytags_always_enabled = 1
"let g:easytags_async = 1

" undotree
set undofile
let g:undotree_WindowLayout=2
let g:undotree_SetFocusWhenToggle=1

" ctrlp
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

set cc=81
set number
set numberwidth=1
set mouse=

set completeopt=menu,menuone,longest,preview

" the default was just horrid
colorscheme peachpuff
highlight Identifier ctermfg=174
highlight Pmenu ctermbg=16 ctermfg=231
highlight PmenuSel ctermbg=162 ctermfg=231
highlight ErrorMsg ctermbg=88 ctermfg=231
highlight Search ctermbg=none ctermfg=48
highlight MatchParen ctermbg=none ctermfg=51
highlight ColorColumn ctermbg=236
highlight SpellBad ctermfg=1 ctermbg=none
highlight SpellCap ctermfg=none ctermbg=none
highlight SpellRare ctermfg=none ctermbg=none
highlight SpellLocal ctermfg=3 ctermbg=none
highlight mailURL ctermfg=2
highlight mailEmail ctermfg=5
highlight Folded ctermfg=7  ctermbg=236
highlight FoldColumn ctermfg=243 ctermbg=16
highlight LineNr ctermfg=240 ctermbg=16
highlight link CTagsConstant Identifier
highlight link Member Normal
highlight link EnumeratorName EnumerationName
highlight link EnumerationName Type
highlight EnumerationValue ctermfg=114
highlight DefinedName ctermfg=212
highlight StatusLineNC cterm=none ctermfg=240 ctermbg=16
highlight StatusLine cterm=bold ctermfg=247 ctermbg=16
highlight VertSplit cterm=none ctermbg=16 ctermfg=16

" error highlighting
highlight SyntaxError cterm=bold ctermfg=15 ctermbg=13
highlight SyntaxWarn cterm=bold ctermfg=11 ctermbg=8
highlight link SyntaxInfo Normal
let g:hier_highlight_group_qf = 'SyntaxError'
let g:hier_highlight_group_qfw = 'SyntaxWarn'
let g:hier_highlight_group_qfi = 'SyntaxInfo'

" diff highlighting
highlight DiffAdd ctermbg=22
highlight DiffDelete ctermbg=52 " foo
highlight DiffChange ctermbg=17

" misc stuff
let g:localvimrc_persistent = 1
let g:localvimrc_persistence_file = expand('$HOME') . '/.vim/lvimrc'

" match extra whitespace
highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+$\| \+\ze\t/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/

" custom aliases
command -bang -bar Quit q<bang>
command -bang -bar -nargs=? -complete=file -range=% Write <line1>,<line2>w<bang> <args>

set autoindent
set backspace=indent

execute pathogen#infect()
filetype plugin indent on

" extra filetypes
au BufNewFile,BufRead *.weechatlog setf weechatlog
au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl,*.hook setf glsl
au BufNewFile,BufRead *.vpy setf python
au BufNewFile,BufRead /tmp/zsh* setf zsh
