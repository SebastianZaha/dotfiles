set mouse=a
set nocompatible      " We're running Vim, not Vi!
set incsearch
set autoindent
set autoread          " Autoload file changes. You can undo by pressing u 
set backspace=indent,eol,start
set display+=lastline
set scrolloff=7 " cursor lines when at the top or bottom of the screen 
set sidescrolloff=5
set wildmenu
set ignorecase " Ignore case when searching
set smartcase " When searching try to be smart about cases
set incsearch " Makes search act like search in modern browsers
set noerrorbells
set novisualbell
set foldcolumn=1 " Add a bit extra margin to the left
set cursorline

set splitbelow
set splitright

set encoding=utf8

" Turn backup off
set nobackup
set nowb
set noswapfile

set timeout
set ttimeout
set timeoutlen=1000
set ttimeoutlen=1

set number relativenumber

set diffopt+=vertical,hiddenoff,algorithm:patience

set laststatus=2 " Always show

set clipboard+=unnamedplus

let mapleader = "\<Space>"


set noexpandtab " use tabs for indenting
set shiftwidth=4
set tabstop=4

set autoindent
set smartindent
set wrap
set linebreak
set showbreak=>>
set breakindent
set breakindentopt=sbr


syntax on             " Enable syntax highlighting

filetype on           " Enable filetype detection
filetype indent on    " Enable filetype-specific indenting
filetype plugin on    " Enable filetype-specific plugins

set omnifunc=syntaxcomplete#Complete
 
set history=10000

" Netrw
function! NetrwMapping()
  " up a dir
  nmap <buffer> h -^
  " into a dir / open file
  nmap <buffer> l <CR>
endfunction
augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END

" Terminal
nnoremap <silent> <Leader>` :let $VIM_DIR=expand('%:p:h')<CR>:terminal<CR>cd $VIM_DIR<CR>
if has('nvim')
	autocmd TermOpen * startinsert
	autocmd BufWinEnter,WinEnter term://* startinsert
	
	" autocmd TermClose * if !v:event.status && (match(expand('<amatch>'), '/bin/bash$') > -1) | exe(':Bclose') | endif
	" Auto close shell terminals (#15440)
	autocmd TermClose *
		\ if !v:event.status |
		\   let info = nvim_get_chan_info(&channel) |
		\   if get(info, 'argv', []) ==# [&shell] |
		\     exec ':Bclose' |
		\   endif |
		\ endif
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" Tags
nnoremap <C-]> g<C-]>
nnoremap <C-[> :pop<cr>

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" quickfix, location shortcuts
nnoremap ]q :cnext<cr>
nnoremap ]Q :clast<cr>
nnoremap [q :cprev<cr>
nnoremap [Q :cfirst<cr>
nnoremap ]l :lnext<cr>
nnoremap ]L :llast<cr>
nnoremap [l :lprev<cr>
nnoremap [L :lfirst<cr>

" Close the current buffer
noremap <C-w>b :Bclose<cr>

" Edit file in current directory 
nnoremap <leader>e :e %:p:h/<Tab>

" Switch CWD to the directory of the open buffer
noremap <leader>cd :cd %:p:h:gs/ /\\ /<cr>:pwd<cr>

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" 'thin' cursor in insert mode https://stackoverflow.com/a/42118416/1306453
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

""""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""
function! s:statusline_expr()
  let mod = "%{&modified ? '[+] ' : !&modifiable ? '[x] ' : ''}"
  let ro  = "%{&readonly ? '[RO] ' : ''}"
  let ft  = "%{len(&filetype) ? '['.&filetype.'] ' : ''}"
  let sep = ' %= '
  let pos = ' %-12(%l : %c%V%) '
  let pct = ' %P'

  return '  [%n] %F %<'.mod.ro.ft.sep.pos.'%*'.pct
endfunction
let &statusline = s:statusline_expr()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Don't close window, when deleting a buffer
command! Bclose call <SID>BufcloseCloseIt()
function! <SID>BufcloseCloseIt()
    let l:currentBufNum = bufnr("%")
    let l:alternateBufNum = bufnr("#")

    if buflisted(l:alternateBufNum)
        buffer #
    else
        bnext
    endif

    if bufnr("%") == l:currentBufNum
        new
    endif

    if buflisted(l:currentBufNum)
        execute("bdelete! ".l:currentBufNum)
    endif
endfunction

function! CmdLine(str)
    call feedkeys(":" . a:str)
endfunction

"""""""""""""""""""""""""""""

if has("unix") && filereadable("/proc/version")
  let lines = readfile("/proc/version")
  if lines[0] =~ "Microsoft"
    " wsl copy
    autocmd TextYankPost * call system('echo '.shellescape(join(v:event.regcontents, "\<CR>")).' |  clip.exe')
  endif
endif

" auto install vim-plug on first starting vim
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
if has('nvim')
	Plug 'neovim/nvim-lspconfig'
end 
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
  " ds, cs, yss, S in V mode
Plug 'fatih/vim-go'
" - binding to jump to netrw, netrw saner defaults
Plug 'tpope/vim-vinegar'
" clears search highlighting after you finish incremental search  
Plug 'romainl/vim-cool'
" Colors
Plug 'cocopon/iceberg.vim'
call plug#end()

colorscheme iceberg


" Auto close braces in insert mode
inoremap {<CR> {<CR>}<Esc>O

" Go
let g:go_template_autocreate = 0
autocmd FileType go nnoremap <buffer> <leader>t :GoTest ./...<CR>
" open wd (project root) and run tests from there instead of just current package
autocmd FileType go nnoremap <buffer> <leader>T :e .<CR>:GoTest ./...<CR>

" Save
inoremap <C-s> <C-O>:update<cr><esc>
nnoremap <C-s> :update<cr>

" Quit
nnoremap Q q
nnoremap q <esc>:q<cr>

" To use `ALT+{h,j,k,l}` to navigate windows from any mode
:tnoremap <A-h> <C-\><C-N><C-w>h
:tnoremap <A-j> <C-\><C-N><C-w>j
:tnoremap <A-k> <C-\><C-N><C-w>k
:tnoremap <A-l> <C-\><C-N><C-w>l
:inoremap <A-h> <C-\><C-N><C-w>h
:inoremap <A-j> <C-\><C-N><C-w>j
:inoremap <A-k> <C-\><C-N><C-w>k
:inoremap <A-l> <C-\><C-N><C-w>l
:nnoremap <A-h> <C-w>h
:nnoremap <A-j> <C-w>j
:nnoremap <A-k> <C-w>k
:nnoremap <A-l> <C-w>l
inoremap <C-6> <C-o><C-^>

" autocomplete
inoremap <C-/> <C-x><C-o>

" insert empty line above/below the cursor
nnoremap <silent><C-j> :set paste<CR>m`o<Esc>``:set nopaste<CR>
nnoremap <silent><C-k> :set paste<CR>m`O<Esc>``:set nopaste<CR>

" shortcuts for 3-way merge
map <Leader>1 :diffget LOCAL<CR>
map <Leader>2 :diffget BASE<CR>
map <Leader>3 :diffget REMOTE<CR>

noremap <Leader>d :Gvdiffsplit<CR>

" ============================================================================
" FZF {{{
" ============================================================================

let $FZF_DEFAULT_OPTS .= ' --inline-info'

let g:fzf_layout = { 'window': { 'width': 1, 'height': 1 } }
let g:fzf_preview_window = ['up:50%:wrap', 'ctrl-/']
let g:fzf_tags_command = 'ctags -R'

" Customize fzf colors to match your color scheme
" - fzf#wrap translates this to a set of `--color` options
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

nnoremap <silent> <leader><space> :AllFiles<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>w :Windows<CR>
nnoremap <silent> <leader>C :Colors<CR>
nnoremap <silent> <leader>: :BLines<CR>
nnoremap <silent> <leader>o :BTags<CR>
nnoremap <silent> <leader>O :Tags<CR>
nnoremap <silent> <leader>? :History<CR>
" command history
nnoremap <silent> <leader>; :History:<CR> 
nnoremap <silent> <leader>/ :execute 'RG! ' . input('Rg/')<CR>
nnoremap <silent> <Leader>rg :RG! <C-R><C-W><CR>
nnoremap <silent> <Leader>RG :RG! <C-R><C-A><CR>
xnoremap <silent> <Leader>rg y:RG! <C-R>"<CR>

" All files
command! -nargs=? -complete=dir AllFiles
  \ call fzf#run(fzf#wrap(fzf#vim#with_preview({
  \   'source': 'rg --files --no-ignore-parent --hidden -g "!.git" '.expand(<q-args>)
  \ })))

function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --no-ignore-parent --hidden --line-number --no-heading --color=always --smart-case %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let options = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  let options = fzf#vim#with_preview(options, 'up:50%:wrap', 'ctrl-/')
  call fzf#vim#grep(initial_command, 1, options, a:fullscreen)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

" }}}
" ============================================================================

if has('nvim')
    let g:terminal_color_0  = '#2e3436'
    let g:terminal_color_1  = '#cc0000'
    let g:terminal_color_2  = '#4e9a06'
    let g:terminal_color_3  = '#c4a000'
    let g:terminal_color_4  = '#3465a4'
    let g:terminal_color_5  = '#75507b'
    let g:terminal_color_6  = '#0b939b'
    let g:terminal_color_7  = '#d3d7cf'
    let g:terminal_color_8  = '#555753'
    let g:terminal_color_9  = '#ef2929'
    let g:terminal_color_10 = '#8ae234'
    let g:terminal_color_11 = '#fce94f'
    let g:terminal_color_12 = '#729fcf'
    let g:terminal_color_13 = '#ad7fa8'
    let g:terminal_color_14 = '#00f5e9'
    let g:terminal_color_15 = '#eeeeec'
endif

