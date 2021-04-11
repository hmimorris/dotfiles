#!/bin/bash

if [ ! -r $HOME/.bash_profile ]; then ln -sv .dotfiles/runcom/.bash_profile $HOME/; fi

if [ ! -r $HOME/.bashrc ]; then ln -sv .dotfiles/runcom/.bashrc $HOME/; fi

if [ ! -r $HOME/.alias ]; then ln -sv .dotfiles/system/.alias $HOME/; fi

if [ ! -r $HOME/.prompt ]; then ln -sv .dotfiles/system/.prompt $HOME/; fi

if [ ! -r $HOME/.emacs ]; then ln -sv .dotfiles/.emacs $HOME/; fi

if [ ! -r $HOME/.config/i3/config ]; then ln -sv ../.dotfiles/.config/i3 $HOME/.config/; fi

if [ ! -r $HOME/.config/rofi/config ]; then ln -sv ../.dotfiles/.config/rofi $HOME/.config/; fi

#ln -sv “/runcom/.inputrc” ~
#ln -sv “/git/.gitconfig” ~
