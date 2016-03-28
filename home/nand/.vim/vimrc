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

noremap Cn :cn<CR>
noremap Cp :cp<CR>

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
" set autochdir

set expandtab
set shiftwidth=4
set softtabstop=4

" code folding
set foldmethod=syntax
set foldnestmax=1
set foldlevel=0

" ctags
noremap <C-d> <C-]>
set tags=tags;
"let g:easytags_auto_highlight = 0
"let g:easytags_dynamic_files = 2
"let g:easytags_always_enabled = 1
"let g:easytags_async = 1

set cc=81
set foldcolumn=1
set foldopen=block,hor,mark,percent,quickfix,search,tag,undo
set number
set numberwidth=1
set mouse=

set completeopt=menu,menuone,longest,preview

" the default was just horrid
colorscheme peachpuff
highlight Identifier ctermfg=53
highlight Pmenu ctermbg=16 ctermfg=79
highlight PmenuSel ctermbg=49 ctermfg=79
highlight ErrorMsg ctermbg=32 ctermfg=79
highlight Search ctermbg=none ctermfg=29
highlight MatchParen ctermbg=none ctermfg=31
highlight ColorColumn ctermbg=80
highlight SpellBad ctermfg=1 ctermbg=none
highlight SpellCap ctermfg=none ctermbg=none
highlight SpellRare ctermfg=none ctermbg=none
highlight SpellLocal ctermfg=3 ctermbg=none
highlight mailURL ctermfg=2
highlight mailEmail ctermfg=5
highlight Folded ctermfg=7  ctermbg=80
highlight FoldColumn ctermfg=82 ctermbg=16
highlight LineNr ctermfg=81 ctermbg=16
highlight link CTagsConstant Identifier
highlight link Member Normal
highlight link EnumeratorName EnumerationName
highlight link EnumerationName Type
highlight EnumerationValue ctermfg=41
highlight DefinedName ctermfg=70
highlight StatusLineNC cterm=none ctermfg=81 ctermbg=16
highlight StatusLine cterm=bold ctermfg=84 ctermbg=16
highlight VertSplit cterm=none ctermbg=16 ctermfg=16

" error highlighting
highlight SyntaxError cterm=bold ctermfg=15 ctermbg=13
highlight SyntaxWarn cterm=bold ctermfg=11 ctermbg=8
highlight link SyntaxInfo Normal
let g:hier_highlight_group_qf = 'SyntaxError'
let g:hier_highlight_group_qfw = 'SyntaxWarn'
let g:hier_highlight_group_qfi = 'SyntaxInfo'

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
au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setf glsl
au BufNewFile,BufRead *.vpy setf python
au BufNewFile,BufRead /tmp/zsh* setf zsh
