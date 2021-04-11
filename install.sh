#!/bin/bash

if [ ! -r $HOME/.bash_profile ]; then ln -sv .dotfiles/runcom/.bash_profile $HOME/; fi

if [ ! -r $HOME/.alias ]; then ln -sv .dotfiles/system/.alias $HOME/; fi

if [ ! -r $HOME/.prompt ]; then ln -sv .dotfiles/system/.prompt $HOME/; fi

#ln -sv “/runcom/.inputrc” ~
#ln -sv “/git/.gitconfig” ~
