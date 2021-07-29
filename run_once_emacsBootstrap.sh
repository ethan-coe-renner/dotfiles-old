#!/bin/sh

# This script bootstraps my emacs configuration, sets up doom emacs, and sets up chemacs to switch between them.

[ ! -d ~/.local/share/doom-emacs ] && git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.local/share/doom-emacs && ~/.local/share/doom-emacs/bin/doom install || echo "doom-emacs already initialized"

[ ! -f ~/.emacs.d/chemacs.el ] && git clone https://github.com/plexus/chemacs2.git ~/.emacs.d || echo "chemacs already initialized" 

