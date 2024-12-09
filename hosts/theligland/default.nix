{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware.nix
      ../../system/nixos/common.nix
    ];

  # Bootloader.
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Set host name
  networking.hostName = "theligland"; # Define your hostname.

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Squeezelite
  services.squeezelite = {
    enable = true;
    extraArguments = "-o front:CARD=DACR26,DEV=0 -s 192.168.1.15";
  };

  # Jellyfin media server
  services.jellyfin = {
    enable = true;
    user = "kandread";
    group = "video";
    openFirewall = true;
  };

  environment.systemPackages = with pkgs; [
    ntfs3g
    exfatprogs
    jellyfin-web
    jellyfin-ffmpeg
  ];

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Enable OpenGL
  hardware.graphics = {
    enable = true;
  };

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    # Modesetting is required.
    modesetting.enable = true;
    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of
    # supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # Only available from driver 515.43.04+
    # Currently alpha-quality/buggy, so false is currently the recommended setting.
    open = false;
    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;
    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.beta;
  };

  # Install printer drivers
  services.printing.drivers = with pkgs; [
    mfcl3770cdwcupswrapper
  ];

}
