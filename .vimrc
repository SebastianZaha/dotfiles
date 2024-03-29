" :help index will show you every default key-bindings.
" :map will show you every custom mapping.
" :map <leader> will show you every custom <leader> mapping.
" :verbose nmap will show you every custom normal mode mapping and where they are defined.

set mouse=a
set nocompatible      " We're running Vim, not Vi!
set incsearch
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

set diffopt+=vertical,hiddenoff,closeoff,algorithm:patience

set laststatus=2 " Always show

set clipboard+=unnamedplus

let mapleader = "\<Space>"

set expandtab " use spaces for indenting
set shiftwidth=4
set tabstop=4

set autoindent
set smartindent
set wrap
set linebreak
set showbreak=>>
set breakindent
set breakindentopt=sbr

" gvim
set guioptions-=m  "no menu
set guioptions-=T  "no toolbar
set guioptions-=R  "no right scrollbar
set guioptions-=L  "no left scrollbar

filetype indent plugin on

set omnifunc=syntaxcomplete#Complete
 
set history=10000

nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" Tags
nnoremap <C-]> g<C-]>
nnoremap <C-[> :pop<cr>

" quickfix
nnoremap ]q :cnext<cr>
nnoremap ]Q :clast<cr>
nnoremap [q :cprev<cr>
nnoremap [Q :cfirst<cr>
" location
nnoremap ]l :lnext<cr>
nnoremap ]L :llast<cr>
nnoremap [l :lprev<cr>
nnoremap [L :lfirst<cr>
" buffers
nnoremap ]b :bnext<cr> 
nnoremap [b :bprev<cr>
" tabs
nnoremap ]t :tabn<cr>
nnoremap [t :tabp<cr>

" Close the current buffer
noremap <C-w>b :Bclose<cr>

" Switch CWD to the directory of the open buffer
noremap <leader>cd :cd %:p:h:gs/ /\\ /<cr>:pwd<cr>

" Return to last edit position when opening files
" copied from :h last-position-jump
autocmd BufRead * autocmd FileType <buffer> ++once
    \ if &ft !~# 'commit\|rebase' && line("'\"") > 1 && line("'\"") <= line("$") | exe 'normal! g`"' | endif

" 'thin' cursor in insert mode https://stackoverflow.com/a/42118416/1306453
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

function! SetupSource()
    " Disable auto-wrap for text
    set formatoptions-=t

    " Automatically insert comment leaders:
    "   r - automatically insert comment leader when pressing <Enter>.
    "   o - automatically insert comment leader after 'o' or 'O'.
    setlocal formatoptions+=ro

    setlocal tw=80 ts=4 sts=4 sw=4 noet
endfunction
command! -bar SetupSource call SetupSource()

" Go
let g:go_template_autocreate = 0
augroup vim_go_plugin
autocmd!
autocmd FileType go nnoremap <buffer> <leader>t :GoTest ./...<CR>
" open wd (project root) and run tests from there instead of just current package
autocmd FileType go nnoremap <buffer> <leader>T :e .<CR>:GoTest ./...<CR>
autocmd FileType go nnoremap <buffer> <leader>r :GoRename<CR>
autocmd FileType go nnoremap <buffer> <leader>gr :GoReferrers<CR>
augroup END

" YAML indentation
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

function FormatBuffer()
  if &modified && !empty(findfile('.clang-format', expand('%:p:h') . ';'))
    let cursor_pos = getpos('.')
    :%!/opt/clang-format-static/clang-format-16
    call setpos('.', cursor_pos)
  endif
endfunction
autocmd BufWritePre *.h,*.hpp,*.c,*.cpp,*.vert,*.frag :call FormatBuffer()
" autocmd BufWritePre *.cpp,*.hpp,*.cc,*.hh,*.c,*.h lua vim.lsp.buf.format()

" just disable the annoying Ctrl-C bind in sql files.
" I'm using it to exit edit mode.
let g:ftplugin_sql_omni_key = '<C-ș>'

" Large screen layout: 3 vertical splits, last one with a netrw and a terminal
nnoremap <F2> :vs<CR>:vs<CR>:E<CR>:sp<CR>:term<CR>

noremap  <F5> :Make<CR>
tnoremap <F5> <C-\><C-N>:Make<CR>

" Save
inoremap <C-s> <C-O>:update<cr><esc>
nnoremap <C-s> :update<cr>

" Quit
nnoremap Q q
nnoremap q <esc>:q<cr>

inoremap <C-h> <C-O>h
inoremap <C-l> <C-O>a
inoremap <C-j> <C-O>j
inoremap <C-k> <C-O>k

" To use `ALT+{h,j,k,l}` to navigate windows from any mode
if has("unix") && system("uname") == "Darwin\n"
  :tnoremap ˙ <C-\><C-N><C-w>h
  :tnoremap ∆ <C-\><C-N><C-w>j
  :tnoremap ˚ <C-\><C-N><C-w>k
  :tnoremap ¬ <C-\><C-N><C-w>l
  :inoremap ˙ <C-\><C-N><C-w>h
  :inoremap ∆ <C-\><C-N><C-w>j
  :inoremap ˚ <C-\><C-N><C-w>k
  :inoremap ¬ <C-\><C-N><C-w>l
  :nnoremap ˙ <C-w>h
  :nnoremap ∆ <C-w>j
  :nnoremap ˚ <C-w>k
  :nnoremap ¬ <C-w>l
else
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
endif

" Enable switching back and forth to previous buffer with C-6 also in insert mode
inoremap <C-6> <C-O><C-^>

" autocomplete. the binding is actually C-/ but that's how vim wants it
set completeopt-=preview
inoremap <C-_> <C-x><C-o>

" insert empty line above/below the cursor
nnoremap <silent><C-j> :set paste<CR>m`o<Esc>``:set nopaste<CR>
nnoremap <silent><C-k> :set paste<CR>m`O<Esc>``:set nopaste<CR>

" shortcuts for 3-way merge
map <Leader>1 :diffget LOCAL<CR>
map <Leader>2 :diffget BASE<CR>
map <Leader>3 :diffget REMOTE<CR>

" git
noremap <Leader>gd :Gvdiffsplit<CR>
noremap <Leader>ga :Git blame<CR>
noremap <Leader>gl :Git l<CR><C-w>T
noremap <Leader>gp :Git push<CR>
noremap <leader>gs :G<CR><C-w>T
noremap <leader>gw :GBrowse<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins

" auto install vim-plug on first starting vim
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
if has('nvim')
  Plug 'neovim/nvim-lspconfig' " Collection of configurations for built-in LSP client
  Plug 'hrsh7th/nvim-cmp' " Autocompletion plugin
  Plug 'hrsh7th/cmp-nvim-lsp' " LSP source for nvim-cmp
  Plug 'saadparwaiz1/cmp_luasnip' " Snippets source for nvim-cmp
  Plug 'L3MON4D3/LuaSnip' " Snippets plugin

  Plug 'sakhnik/nvim-gdb'
  let g:nvimgdb_use_find_executables=0
  let g:nvimgdb_use_cmake_to_find_executables=0
end 
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb' " github integration

Plug 'tpope/vim-dispatch'
Plug 'fatih/vim-go'
" clears search highlighting after you finish incremental search  
Plug 'romainl/vim-cool'
" Colors
" ideas: https://github.com/mcchrish/vim-no-color-collections
" light
" Plug 'https://gitlab.com/yorickpeterse/vim-paper'
" Plug 'https://gitlab.com/yorickpeterse/nvim-grey.git'
" dark
 Plug 'https://gitlab.com/yorickpeterse/happy_hacking.vim.git'
 Plug 'cocopon/iceberg.vim'
 Plug 'ayu-theme/ayu-vim'
 Plug 'rakr/vim-one'
 Plug 'chiendo97/intellij.vim'
call plug#end()

syntax on " syntax highlighting
set termguicolors
set background=dark
let ayucolor="dark"
colorscheme ayu


set nocursorline


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Status line
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


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Don't close window when deleting a buffer
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" WSL fixes

if has("unix") && filereadable("/proc/version")
  let lines = readfile("/proc/version")
  if lines[0] =~ "Microsoft"
    " wsl copy
    autocmd TextYankPost * call system('echo '.shellescape(join(v:event.regcontents, "\<CR>")).' |  clip.exe')
  endif
endif


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Netrw
" open file in other window (previously focused) by default
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_list_hide= '^\.\/$,\.\.\/$'
let g:netrw_banner = 0

function! NetrwMapping()
  " up a dir
  nmap <buffer> h -^
  " into a dir / open file
  nmap <buffer> l <CR>
  " avoid the delay on quit
  nnoremap <buffer> qq :q<CR>
endfunction
augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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

" exit terminal insert mode
tnoremap <A-esc> <C-\><C-N>

" termdebug
nnoremap ,b :Break<CR>
nnoremap ,d :Clear<CR>

" e.g. for ladybird -> :AttachTo WebContent
command! -nargs=1 AttachTo call AttachTo(<q-args>)
function! AttachTo(process_name)
    packadd termdebug
    " system('sudo sysctl -w kernel.yama.ptrace_scope=0')
    let pid = system('pgrep -o '.a:process_name)
    execute(printf('Termdebug -p %d', pid))
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" FZF

nnoremap <silent> <leader><space> :AllFiles<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>? :History<CR>
" command history
nnoremap <silent> <leader>; :History:<CR> 
nnoremap <silent> <leader>/ :execute 'RG! ' . input('Rg/')<CR>
nnoremap <silent> <Leader>w/ :RG! <C-R><C-W><CR>

" All files:
" - include hidden
" - don’t respect ignore files (.gitignore, .ignore, etc.) in parent directories
command! -nargs=? -complete=dir AllFiles
  \ call fzf#run(fzf#wrap(fzf#vim#with_preview({
  \   'source': 'rg --files --no-ignore-parent --hidden -g "!.git" '.expand(<q-args>)
  \ })))

function! RipgrepFzf(query)
  let command_fmt = 'rg --column --no-ignore-parent --hidden --line-number --no-heading --color=always --smart-case %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command  = printf(command_fmt, '{q}')
  let options = {'options': [
	  		  \ '--reverse', '--disabled', '--query', a:query, '--prompt', '1. ripgrep> ',
			  \ '--bind', 'ctrl-q:select-all,ctrl-w:deselect-all',
			  \ '--bind', 'change:reload:sleep 0.1; '.reload_command,
			  \ '--bind', "alt-enter:unbind(change,alt-enter)+change-prompt(2. fzf> )+enable-search+clear-query"]}
  let options = fzf#vim#with_preview(options)
  call fzf#vim#grep(initial_command, 1, options)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>)


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" per project / project type configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if filereadable('bin/rails')
    exe 'compiler rake'
    set makeprg=bin/rails

    noremap <leader>tt :Make t<CR>
    noremap <leader>tf :Make t %<CR>
    noremap <leader>tl :execute 'Make t %:' . line('.')<CR>
endif

