"""""""""""""""""""" General
"""""" Working Directory
cd /path/to/wd/
" set autochdir

"""""" Reading
set nocompatible
set backspace=2
"" auto reload vimrc
" autocmd! bufwritepost .vimrc source ~/.vimrc

""" jump to the last position when reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") |
              \ exe "normal! g'\"" | endif
endif

""" Resize
map - <C-W>-
map + <C-W>+
" map > <C-W>>
" map < <C-W><

""" status line
set laststatus=2
set statusline=\ %{HasPaste()}%<%(%f%)%m%r%h\ %w
set statusline+=\ \ %<%(%{CurDir()}%)
set statusline+=\ \ \ \ \ \ [%{&ff}/%Y] 
set statusline+=%=%-10.(%l,%c%V%)\ \ \ %p%%/%L
hi StatusLine ctermfg=gray ctermbg=black
hi StatusLineNC ctermfg=darkblue ctermbg=gray

"""""" Searching
set ignorecase
set smartcase
set incsearch
set hlsearch

"""""" Editing
" colorscheme torte
colorscheme solarized
let g:solarized_bold = 0
let g:solarized_italic = 0

""" FIXME: this won't work for me, use "ShowTrailingWhitespace" plugin instead
" autocmd ColorScheme * highlight TrailingWhitespace ctermbg=red guibg=red
" match TrailingWhitespace /\s\+$/

""" Normally, we do not keep backup files
set nobackup
" set backup
" set backupdir=~/vimfile/backup/

"" auto reload when file changed
" set ar


"""""""""""""""""""" Programming
"" filetype detection
filetype on
""" filetype-specific indenting
filetype indent on
filetype plugin on

""" syntax highlight
syntax on
set showmatch
set showmode

set autoindent
set smartindent

""" Tab
" set expandtab
" set tabstop=4
" set softtabstop=4
" set shiftwidth=4

""" Folding
" set foldenable
" set foldmethod=marker
" set foldlevel=0
" set foldcolumn=0

"""""" Python Programming under Windows
set guioptions-=m " menu bar
set guioptions-=T " tool bar
set guioptions-=r " right-hand scroll bar
set guioptions-=L " left-hand scroll bar

if has("gui_running")
    " GUI is running or is about to start
    " set lines=999 columns=999
    au GUIEnter * simalt ~x
else
    " This is console vim
    if exists("+lines")
        set lines=50
    endif
    if exists("+columns")
        set columns=100
    endif
endif

let mapleader=";"
let g:mapleader=";"

" nnoremap <leader>w :call TrimTrailingWhitespace()<CR>

"""""" Python
autocmd FileType python setlocal expandtab shiftwidth=4 tabstop=8 sts=4
"" `vim-indent-guides` does not work on vim 7.2, try `n<<` or `n>>`
autocmd FileType python setlocal foldmethod=indent foldlevel=99

"" execute python from within vim
"" NOTICE: :bd to kill the python process
map <leader>z :call SpawnPython()<CR>
"" `<M-x eshell> python` under Emacs
autocmd FileType python noremap <leader>q :!python %<CR>

"""""" HTML
autocmd FileType html setlocal expandtab shiftwidth=2 tabstop=8 sts=2
autocmd FileType html map <leader>q :!"/path/to/chrome" %:p<CR>

""""""""""""""""""" other plugins
"""""" Tagbar
nmap t :TagbarToggle<CR>
"" default to be right
let g:tagbar_left = 1
let g:tagbar_autofocus = 1
let g:tagbar_width = 30
"""""" NerdTree
nnoremap <leader>n :NERDTreeToggle<CR>
let g:NERDTreeWinPos = "right"  " default to be left
""" Close vim if the only window left open is the NERDTree
autocmd bufenter *
            \ if (winnr("$") == 1 && exists("b:NERDTreeType") &&
            \ b:NERDTreeType == "primary") | q | endif


""""""""""""""""""" Utility Functions
fun! HasPaste()
    if &paste
        return '[PASTE]'
    else
        return ''
    endif
endfun
fun! CurDir()
    let curdir = substitute(getcwd(), $HOME, "~", "")
    return curdir
endfun
fun! TrimTrailingWhitespace()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//g
    call cursor(l, c)
endfun
fun! SpawnPython()
    " vsp
    "" gt / gT for next / prev tab
    tabnew
    "" Finally, iPython under windows...
    "" Windows Binaries for Python Extension Packages:
    ""   http://www.lfd.uci.edu/~gohlke/pythonlibs/
    ""     setuptools, pyreadline, and ipython
    "" /path/to/python/Scripts/ipython.exe
    ConqueTerm ipython
endfun
