{ pkgs, inputs, ... }:

{

  launchd.agents = {
    mbsync = {
      enable = true;
      config = {
        StartInterval = 300;
        Program = "${pkgs.isync}/bin/mbsync";
        ProgramArguments = ["--all" "--verbose"];
      };
    };
    mu-index = {
      enable = true;
      config = {
        StartInterval = 300;
        Program = "${pkgs.mu}/bin/mu"
          ProgramArguments = ["index"];
      };
    };
  };

}
