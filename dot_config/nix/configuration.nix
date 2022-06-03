{ config, lib, pkgs, ... }:

let kmonad = import /home/ethan/.local/share/chezmoi/dot_config/nix/kmonad.nix;
in {
  imports = [ /etc/nixos/hardware-configuration.nix ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot = {
      enable = true;
      configurationLimit = 10;
    };
    efi.canTouchEfiVariables = true;
  };

  networking.hostName = "rocinante"; # Define your hostname.

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  networking.networkmanager.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  # commented out because it causes wifi to break
  # networking.useDHCP = false;
  # networking.interfaces.enp0s31f6.useDHCP = true;
  # networking.interfaces.wlp3s0.useDHCP = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = { keyMap = "dvorak"; };

  nix.gc = {
    automatic = true;
    dates = "weekly";
  };
  nix.optimise.automatic = true;

  nix.settings.trusted-users = [ "root" "ethan" ];
  hardware.bluetooth.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraConfig =
      "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1";
  };

  programs = {
    nm-applet.enable = true;
    slock.enable = true;
    light.enable = true;
    zsh.enable = true;
    ssh.startAgent = true;
    steam.enable = true;
  };

  users = {
    mutableUsers = false;
    groups = { uinput = { }; };
    users.ethan = {
      isNormalUser = true;
      extraGroups = [ "wheel" "input" "uinput" "video" ];
      hashedPassword =
        "$6$3IQipgfNp$Ydld7uQiSIWlsqhMTgCNcMDehnQTSzuARPyYrjiAIMRhUXG.rJtu/eeV6biro6pNmjUYiD5c9/wWA.zz4DVoa/";
      shell = pkgs.zsh;
    };
  };
  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "ethan" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  nixpkgs.config = {
    allowUnfree = true;
    pulseaudio = true;
  };
  environment.systemPackages = with pkgs;
    let
      my-python-packages = python-packages:
        with python-packages;
        [
          # pygame
          matplotlib
          pybluez
        ];
      python-with-my-packages = python3.withPackages my-python-packages;

    in [
      python-with-my-packages
      # Internet
      qbittorrent
      oneshot
      # tor-browser-bundle-bin
      bind
      wget

      ## Browser
      qutebrowser
      firefox
      nyxt
      chromium
      google-chrome

      # Media
      freetube
      yt-dlp
      mpv
      mpc_cli
      ncmpcpp
      vlc
      feh
      sxiv
      simplescreenrecorder

      # Terminal
      alacritty
      tmux
      neovim

      vscodium
      atom


      ## other
      qemu_full
      coreboot-toolchain.riscv
      
      ## Shell
      tealdeer
      python39Packages.howdoi
      yapf
      zoxide
      starship
      exa
      tree
      unzip
      file
      fd
      skim
      bat
      age
      rage
      rclone
      pandoc
      wkhtmltopdf
      python39Packages.pdftotext
      python39Packages.bootstrapped-pip
      bottom
      zip

      # Backup
      chezmoi
      restic

      # Photography
      darktable
      hugin
      digikam
      rawtherapee

      # Image manipulation
      gimp
      inkscape
      imagemagick

      # GUI
      ## Utilities
      xmobar
      polybar
      libnotify
      dunst
      stalonetray
      networkmanagerapplet
      dmenu
      rofi
      flameshot
      scrot
      arandr
      gparted
      pcmanfm
      gtk3
      keepassxc
      xclip
      xorg.xbacklight
      grobi
      kmonad
      stack
      pciutils
      picom
      acpi
      weather
      eww

      ## pentest
      # hashcat
      crunch
      # intel-ocl # required for hashcat

      ## Appearance
      lxappearance
      papirus-icon-theme
      gruvbox-dark-gtk
      gruvbox-dark-icons-gtk

      # Audio
      pamixer
      pavucontrol
      elisa
      kid3
      spotify

      # Productivity
      calibre
      libreoffice
      xournalpp
      zathura
      qt4
      libsForQt5.kde-gtk-config # inhibits zathura error "colorreload module"
      hunspell
      hunspellDicts.en_US
      gnuplot
      texlive.combined.scheme-full
      bitwarden

      # Games
      endless-sky
      desmume
      superTuxKart
      gnugo
      kigo
      discord
      betterdiscordctl

      # Programming
      python3
      jdk
      gcc
      astyle
      valgrind
      rustup
      cargo-edit
      wasm-pack
      nixfmt
      asmfmt
      nodePackages.prettier
      git
      gnumake
      cmake
      go-task
      clang
      ccls
      coreutils
      libtool
      grex
      ripgrep
      ripgrep-all
      pkg-config
      glibc
      nodePackages.npm
      nodejs
      ghc
      haskellPackages.hindent
      swiProlog

      # Temporary
      rpi-imager
      hello


      # fun
      cowsay
      lolcat
      sl
      fortune
      xcowsay
      toilet
      oneko
      espeak
      aalib
      asciiquarium

    ];
  fonts.fonts = with pkgs; [ noto-fonts source-code-pro font-awesome siji ];
  powerManagement.powertop.enable = true;

  services = {
    xserver = {
      enable = true;

      # layout = "dvorak";
      # desktopManager.plasma5.enable = true;
      displayManager.startx.enable = true;

      # XMonad
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
        ];
      };

      layout = "us";

      libinput = {
        enable = true;
        touchpad = {
          accelSpeed = "0.3";
          disableWhileTyping = true;
          tapping = false;
        };
        mouse.accelSpeed = "-0.3";
      };
      xkbOptions = "compose:ralt";
    };
    flatpak.enable = true;
    blueman.enable = true;
    openssh.enable = true;
    emacs = {
      enable = true;
      defaultEditor = true;
    };
    timesyncd.enable = true;
    fwupd.enable = true;
    tlp.enable = true;
    throttled.enable = lib.mkDefault true;
    udev.extraRules = ''
      # KMonad user access to /dev/uinput
      KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
    '';
  };

  xdg.portal = {
    enable = true; # needed for flatpak
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}
