set textwidth=78
set tabstop=8
set softtabstop=8
set shiftwidth=8
set expandtab
set nocompatible
set ruler
set number
set smarttab
set smartindent
set formatoptions+=r
syntax on
filetype plugin indent on
syntax enable
colorscheme default

au FileType haskell setlocal
	\ tabstop=4 softtabstop=4 shiftwidth=4
