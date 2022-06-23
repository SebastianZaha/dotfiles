set mouse=a
set nocompatible      " We're running Vim, not Vi!
set incsearch
set autoindent
set autoread          " Autoload file changes. You can undo by pressing u 
set backspace=indent,eol,start

syntax on             " Enable syntax highlighting

filetype on           " Enable filetype detection
filetype indent on    " Enable filetype-specific indenting
filetype plugin on    " Enable filetype-specific plugins
set omnifunc=syntaxcomplete#Complete
colorscheme desert

if &history < 1000
  set history=1000
endif

" always show at least one line above / below the cursor 
if !&scrolloff
  set scrolloff=1
endif
if !&sidescrolloff
  set sidescrolloff=5
endif

set display+=lastline


if has("unix")
  let lines = readfile("/proc/version")
  if lines[0] =~ "Microsoft"
    " wsl copy
    autocmd TextYankPost * call system('echo '.shellescape(join(v:event.regcontents, "\<CR>")).' |  clip.exe')
  endif
endif

" to install pugins:
" git clone <> ~/.vim/pack/plugins/start/<>

let mapleader = ","

nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

