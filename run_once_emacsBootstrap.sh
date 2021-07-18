#!/bin/sh

# This script bootstraps my emacs configuration, including tangling my config, setting up doom emacs, and setting up chemacs to switch between them.

emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "~/.local/share/chezmoi/dot_config/emacs-config/init.org")'

[ ! -d ~/.local/share/doom-emacs ] && git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.local/share/doom-emacs && ~/.local/share/doom-emacs/bin/doom install || echo "doom-emacs already initialized"

[ ! -f ~/.emacs.d/chemacs.el ] && git clone https://github.com/plexus/chemacs2.git ~/.emacs.d || echo "chemacs already initialized" 

