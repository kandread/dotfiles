{ pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    # Writing
    pandoc
    texlive.combined.scheme-full
    texlab
    hunspell
    hunspellDicts.en_US-large

    # Editors
    vim

    # Web
    w3m

    # Graphics
    graphviz

    # Viewers
    zathura

    # Science
    gnuplot
    gdal
    python3Packages.numpy

    # Multimedia
    imagemagick
    ffmpeg

    # Utilities
    ghostscript
    gnupg
    ripgrep
    fd
    coreutils
    fzf
    fasd
    eza
    btop
    unzip
    zip
    pdftk
    bat
    rsync

    # Productivity
    khal
    vdirsyncer
    #zotero

    # Development
    plantuml
    tree-sitter
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    zotero
    brightnessctl
    betterlockscreen
    feh
    bluetuith
    pamixer
    pulsemixer
    inkscape
    slack
    gp-saml-gui
    zoom-us
    libreoffice-fresh
  ];

}
