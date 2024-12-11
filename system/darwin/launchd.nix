{ pkgs, inputs, ... }:

{

  launchd.user.agents = {
    isync = {
      command = "${pkgs.isync}/bin/mbsync -a";
      serviceConfig = {
        ProcessType = "Background";
        LowPriorityIO = true;
        StartInterval = 5 * 60;
        RunAtLoad = true;
        KeepAlive = false;
        StandardOutPath = "/dev/null";
        StandardErrorPath = "/dev/null";
        EnvironmentVariables = {
          "SSL_CERT_FILE" = "/etc/ssl/certs/ca-certificates.crt";
        };
      };
    };
    mu-index = {
      command = "${pkgs.mu}/bin/mu index";
      serviceConfig = {
        ProcessType = "Background";
        LowPriorityIO = true;
        StartInterval = 5 * 60 + 30;
        RunAtLoad = true;
        KeepAlive = false;
      };
    };
  };

}
