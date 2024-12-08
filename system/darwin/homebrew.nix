{ pkgs, inputs, ... }:

{
 
  homebrew = {
    enable = true;
    
    casks = [
      "slack"
      "davmail"
      "adoptopenjdk"
    ];

    onActivation = {
      cleanup = "zap";
    };
  };

}

