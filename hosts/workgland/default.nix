{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware.nix
      ../../nixos/common.nix
    ];

  # Bootloader.
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Set host name
  networking.hostName = "workgland"; # Define your hostname.

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

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
    # package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  # Install some packages only on this host
  services.printing.drivers = with pkgs; [
    cups-toshiba-estudio
  ];

}
