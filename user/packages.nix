{ pkgs, ... }:

{

  home.packages = with pkgs; [
    # Writing
    #libreoffice-fresh
    pandoc
    texlive.combined.scheme-full
    texlab
    hunspell
    hunspellDicts.en_US-large

    # Editors
    vim

    # Web
    w3m

    # Communication
    #slack
    #gp-saml-gui
    #zoom-us

    # Graphics
    #inkscape
    graphviz

    # Viewers
    zathura

    # Science
    gnuplot
    gdal
    python3Packages.numpy

    # Multimedia
    imagemagick
    #pulsemixer
    #pamixer
    mpv
    ffmpeg

    # Security
    keepassxc

    # Utilities
    ghostscript
    #bluetuith
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
    #betterlockscreen
    #brightnessctl

    # Productivity
    khal
    vdirsyncer
    #zotero

    # Development
    plantuml
    tree-sitter

  ];

}
