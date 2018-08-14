#!/bin/bash

set -eu

cd $HOME
mkdir -p .vim/vimbackup
ln -s ~/.dotfiles/bash/bashrc ~/.bashrc
ln -s ~/.dotfiles/bash/bash_profile ~/.bash_profile
ln -s ~/.dotfiles/bash/bash_logout ~/.bash_logout
ln -s ~/.dotfiles/vim/vimrc ~/.vimrc
