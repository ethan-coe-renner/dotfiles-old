#!/bin/sh

# script creates a symbolic link for my nixos configuration. Enables me to edit the configuration without root privileges.
sudo ln -sf /home/ethan/.local/share/chezmoi/dot_config/nix/configuration.nix /etc/nixos/configuration.nix
