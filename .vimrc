set nocompatible

" Vim pathogen for easier plugin installation
execute pathogen#infect()

set grepprg=grep\ -nH\ $*

" Syntax highlighting
syntax enable
syntax on
filetype on
filetype plugin on

" Underlines current line
set cursorline

" Better case searching
set ignorecase
set smartcase

" Highlight searches
set hlsearch

set autoindent
filetype plugin indent on

" search will center on the line it's found in.
nnoremap N Nzz
nnoremap n nzz

" Swap ; and :  Convenient.
nnoremap ; :
nnoremap : ;

" In visual mode as well
vnoremap ; :
vnoremap : ;

" Move vertically by visual line
nnoremap j gj
nnoremap k gk

" Everyone one loves line numbers, right?
set nu

" Get out of insert mode with jk
inoremap jk <Esc>

" Create Blank Newlines and stay in Normal mode
nnoremap <silent> zj o<Esc>k
nnoremap <silent> zk O<Esc>j

" Y now yanks from current pos thru the end of the line
" Ignoring newline at the end
nnoremap <silent> Y y$

vnoremap // "1y/<C-R>1<CR>

inoremap ;w <Esc>:w

" Code folding
set foldenable
set foldlevelstart=10
set foldmethod=indent
nnoremap <space> za

set showcmd

" set cpoptions+=$

" Set colors--Makes vimdiff easier
colorscheme desert
highlight! link DiffText Todo

let g:closetag_html_style=1
source ~/.vim/bundle/closetag/closetag.vim

" Uses tabular plugin to align by = and :
nnoremap <Tab>   :Tab /=<CR>:Tab /:<CR>:Tab /,<CR>
vnoremap <Tab>   :Tab /=<CR>:Tab /:<CR>:Tab /,<CR>

" Enable mouse control
set mouse=a

" Swap ' and ` for marks
nnoremap ' `
nnoremap ` '

" Nicer scrolling
set so=10

" Backspace deletes backwards
nnoremap  X

" No more error sounds
set noerrorbells
let @p = ';%s/^\s*>>> //;%s/^\s*\.\.\. //gg=G'

" 
set clipboard+=unnamed
" set spell spelllang=en_us
