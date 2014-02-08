set encoding=utf-8
source $VIMRUNTIME/vimrc_example.vim
source $VIMRUNTIME/mswin.vim
behave mswin

set nocompatible
filetype on
filetype plugin indent on
syntax on
syntax enable
set shortmess=at
set cursorline
set nrformats+=alpha
set modeline
set modelines=5
set number
set ruler
set background=dark
set hidden
set confirm
set noshowmode
set lazyredraw
set laststatus=2

if has ('gui_running')
        set guifont=monofur:h10
        set guioptions-=m
        set guioptions-=T
        set guioptions-=r
        set guioptions-=l
        set guioptions-=L
        set guioptions-=R
        colorscheme inkpot
        set lines=48
        set columns=85
endif

 " Generic Statusline {{{
  function! SetStatus()
    setl statusline+=
          \%1*\ %f
          \%H%M%R%W%7*\ \|
          \%2*\ %Y\ <<<\ %{&ff}%7*\ \|
  endfunction

  function! SetRightStatus()
    setl statusline+=
          \%5*\ %{StatusFileencoding()}%7*\ \|
          \%5*\ %{StatusBuffersize()}%7*\ \|
          \%=%<%7*\ \|
          \%5*\ %{StatusWrapON()}
          \%6*%{StatusWrapOFF()}\ %7*\|
          \%5*\ %{StatusInvisiblesON()}
          \%6*%{StatusInvisiblesOFF()}\ %7*\|
          \%5*\ %{StatusExpandtabON()}
          \%6*%{StatusExpandtabOFF()}\ %7*\|
          \%5*\ w%{StatusTabstop()}\ %7*\|
          \%3*\ %l,%c\ >>>\ %P
          \\ 
  endfunction " }}}

  " Update when leaving Buffer {{{
  function! SetStatusLeaveBuffer()
    setl statusline=""
    call SetStatus()
  endfunction
  au BufLeave * call SetStatusLeaveBuffer() " }}}

  " Update when switching mode {{{
  function! SetStatusInsertMode(mode)
    setl statusline=%4*
    if a:mode == 'i'
      setl statusline+=\ Insert\ \|
    elseif a:mode == 'r'
      setl statusline+=\ Replace\ \|
    elseif a:mode == 'normal'
      setl statusline+=\ \ \|
    endif
    call SetStatus()
    call SetRightStatus()
  endfunction

  au VimEnter     * call SetStatusInsertMode('normal')
  au InsertEnter  * call SetStatusInsertMode(v:insertmode)
  au InsertLeave  * call SetStatusInsertMode('normal')
  au BufEnter     * call SetStatusInsertMode('normal') " }}}

  " Some Functions shamelessly ripped and modified from Cream
  "fileencoding (three characters only) {{{
  function! StatusFileencoding()
    if &fileencoding == ""
      if &encoding != ""
        return &encoding
      else
        return " -- "
      endif
    else
      return &fileencoding
    endif
  endfunc " }}}
  " &expandtab {{{
  function! StatusExpandtabON()
    if &expandtab == 0
      return "tabs"
    else
      return ""
    endif
  endfunction "
  function! StatusExpandtabOFF()
    if &expandtab == 0
      return ""
    else
      return "tabs"
    endif
  endfunction " }}}
  " tabstop and softtabstop {{{
  function! StatusTabstop()

    " show by Vim option, not Cream global (modelines)
    let str = "" . &tabstop
    " show softtabstop or shiftwidth if not equal tabstop
    if   (&softtabstop && (&softtabstop != &tabstop))
    \ || (&shiftwidth  && (&shiftwidth  != &tabstop))
      if &softtabstop
        let str = str . ":sts" . &softtabstop
      endif
      if &shiftwidth != &tabstop
        let str = str . ":sw" . &shiftwidth
      endif
    endif
    return str

  endfunction " }}}
  " Buffer Size {{{
  function! StatusBuffersize()
    let bufsize = line2byte(line("$") + 1) - 1
    " prevent negative numbers (non-existant buffers)
    if bufsize < 0
      let bufsize = 0
    endif
    " add commas
    let remain = bufsize
    let bufsize = ""
    while strlen(remain) > 3
      let bufsize = "," . strpart(remain, strlen(remain) - 3) . bufsize
      let remain = strpart(remain, 0, strlen(remain) - 3)
    endwhile
    let bufsize = remain . bufsize
    " too bad we can't use "Â¿" (nr2char(1068)) :)
    let char = "b"
    return bufsize . char
  endfunction " }}}
  " Show Invisibles {{{
  function! StatusInvisiblesON()
    "if exists("g:LIST") && g:LIST == 1
    if &list
      if     &encoding == "latin1"
        return "Â¶"
      elseif &encoding == "utf-8"
        return "Â¶"
      else
        return "$"
      endif
    else
      return ""
    endif
  endfunction
  function! StatusInvisiblesOFF()
    "if exists("g:LIST") && g:LIST == 1
    if &list
      return ""
    else
      if     &encoding == "latin1"
        return "Â¶"
      elseif &encoding == "utf-8"
        return "Â¶"
      else
        return "$"
      endif
    endif
  endfunction " }}}
  " Wrap Enabled {{{
  function! StatusWrapON()
    if &wrap
      return "wrap"
    else
      return ""
    endif
  endfunction
  function! StatusWrapOFF()
    if &wrap
      return ""
    else
      return "wrap"
    endif
  endfunction
  " }}}
" }}}

set textwidth=78
set tabstop=8
set shiftwidth=8
set expandtab
set softtabstop=8
set autoindent
set smarttab
set smartindent
set showmatch
set incsearch
set ignorecase 
set listchars=tab:»·,trail:·,eol:<
set nolist
nmap <silent> <F5> :set list!<CR>

set nobackup
set nowb
set noswapfile

set wildmenu
set wildmode=longest,full,full

set ofu=syntaxcomplete#Complete

map <F3> :NERDTreeToggle<CR>
let NERDTreeChDirMode=2
let NERDTreeShowBookmarks=1

set linebreak
set showbreak=···
nmap <silent> <F12> :let &wrap = !&wrap<CR>

set formatoptions+=ctroqwan
highlight ColorColumn guibg=LightGray
highlight ColorColumn ctermbg=LightGray
set colorcolumn=+1

au BufNewFile,BufRead *.R set filetype=R
au BufNewFile,BufRead *.ssc set filetype=R

au FileType haskell setlocal tabstop=2 softtabstop=2 shiftwidth=2
au FileType html setlocal noexpandtab tabstop=2 softtabstop=2 shiftwidth=2
au FileType scheme setlocal tabstop=2 softtabstop=2 shiftwidth=2

au InsertEnter * setlocal cursorcolumn
au InsertLeave * setlocal nocursorcolumn

let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1 
let g:miniBufExplMapCTabSwitchBufs = 1 
let g:miniBufExplModSelTarget = 1 

let b:lhs_markup = 'tex'

nnoremap <esc> :noh<return><esc>
