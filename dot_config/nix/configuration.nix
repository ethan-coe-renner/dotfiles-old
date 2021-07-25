{ config, lib, pkgs, ... }:

let kmonad = import /home/ethan/.local/share/chezmoi/dot_config/nix/kmonad.nix;
in {
  imports = [
    /etc/nixos/hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot = {
      enable = true;
      configurationLimit = 10;
    };
    efi.canTouchEfiVariables = true;
  };

  # networking.hostName = "nixos"; # Define your hostname.

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  networking.networkmanager.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Source Code Pro 18";
    keyMap = "dvorak";
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
  };
  nix.optimise.automatic = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraConfig =
      "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1";
  };

  programs = {
    slock.enable = true;
    light.enable = true;
    ssh.startAgent = true;
    zsh = {
      enable = true;
    };
  };

  users.mutableUsers = false;
  users.groups = { uinput = { }; };
  users.users.ethan = {
    isNormalUser = true;
    extraGroups = [ "wheel" "input" "uinput" "video" ];
    hashedPassword =
      "$6$3IQipgfNp$Ydld7uQiSIWlsqhMTgCNcMDehnQTSzuARPyYrjiAIMRhUXG.rJtu/eeV6biro6pNmjUYiD5c9/wWA.zz4DVoa/";
    shell = pkgs.zsh;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    # Internet
    qbittorrent
    oneshot

    ## Browser
    firefox qutebrowser ungoogled-chromium

    # Media
    freetube minitube youtube-dl mpv feh ytmdesktop

    # Terminal
    alacritty tmux vim neovim taskwarrior newsboat

    ## Shell
    tealdeer zoxide starship exa tree dust fd skim bat age rage rclone pandoc bottom

    # Backup
    chezmoi restic

    # Photography
    darktable hugin digikam rawtherapee

    # GUI
    ## Utilities
    bemenu gparted pcmanfm keepassxc xclip xorg.xbacklight grobi kmonad virtualbox

    ## Appearance
    lxappearance papirus-icon-theme dracula-theme font-manager

    # Audio
    ncmpcpp mpc_cli pamixer

    # Productivity
    libreoffice hunspell hunspellDicts.en_US gnuplot

    # Games
    endless-sky

    # School
    discord

    # Programming
    python3 gcc rustup nixfmt nodePackages.prettier git gnumake cmake go-task clang coreutils libtool grex ripgrep ripgrep-all

    # Temporary
    hello
  ];
  fonts.fonts = with pkgs; [ noto-fonts source-code-pro ];
  powerManagement.powertop.enable = true;

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  services = {
    xserver = {
      enable = true;
      displayManager.startx.enable = true;
      windowManager.spectrwm.enable = true;

      libinput = {
        enable = true;
        touchpad.tapping = false;
        mouse.accelSpeed = "-0.3";
      };
      layout = "us";
      xkbOptions = "compose:ralt";
    };

    blueman.enable = true;
    emacs = {
      enable = true;
      defaultEditor = true;
      package = pkgs.emacsGcc;
    };
    chrony.enable = true;
    fwupd.enable = true;
    tlp.enable = true;
    throttled.enable = lib.mkDefault true;
    udev.extraRules = ''
      # KMonad user access to /dev/uinput
      KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
    '';

    picom = {
      enable = true;
      settings = {
        fading = true;
        fade-in-step = 5.0e-2;
        fade-out-step = 5.0e-2;
        inactive-dim = 0.1;
      };
    };
  };


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}
