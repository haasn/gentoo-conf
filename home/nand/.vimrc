" custom keybindings
noremap j gj
noremap k gk

" fix bold colors
set term=rxvt-unicode

set hlsearch
set is

syntax sync fromstart
syntax on

set background=dark
set showcmd
set showmatch
set ignorecase
set smartcase
set nojoinspaces
set scrolloff=3
set timeoutlen=0

set expandtab
set shiftwidth=4
set softtabstop=4

set cc=81

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

" match extra whitespace
highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+$\| \+\ze\t/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/

" custom aliases
command Reply :%s/^\t/> /g

command -bang -bar Quit q<bang>
command -bang -bar -nargs=? -complete=file -range=% Write <line1>,<line2>w<bang> <args>

set autoindent
set backspace=indent

filetype plugin indent on
