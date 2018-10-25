{ enableSsl ? true }: # Make deploy_prod.nix that sets enableSsl to true.
let
  domain = "spook.app";
  webServerPort = "8080";
  dbPort = 5432;
  youtubeKey = builtins.getEnv "YOUTUBE_KEY";
in {
  network.description = "Spook App";

  database = { pkgs, ... }: {
    # TODO: Use ssl - set up cert pairs and send to machines. Use systemd keys?
    services = {
      oidentd.enable = true;

      postgresql = {
        enable = true;
        package = pkgs.postgresql;
        enableTCPIP = true;
        authentication = ''
          host  all   all      127.0.0.1/32   trust
          host  all   all      ::1/128        trust
          host  spook spookapp webserver      md5
        '';
        initialScript = pkgs.writeText "start.sql" ''
          CREATE DATABASE spook;
          CREATE USER spookapp WITH PASSWORD 'password';
          GRANT ALL ON DATABASE spook TO spookapp;
        '';
      };
    };

    networking.firewall.allowedTCPPorts = [ 22 dbPort ];

    users.extraUsers = {
      spookapp = {};
      postgres = {};
    };
  };

  webserver = {pkgs, ...}:
    let release = import ./release.nix { inherit pkgs; };
    in {
      environment.systemPackages = [ release.backend release.frontend release.frontend-static-files ];

      networking.firewall.allowedTCPPorts = [ 22 80 443 ];

      # environment.systemPackages = [ pkgs.postgresql ];

      services = {
        nginx = {
          enable = true;
          recommendedGzipSettings = true;
          recommendedOptimisation = true;
          recommendedProxySettings = true;
          recommendedTlsSettings = true;

          virtualHosts = {
            # TODO Set up test/dev vs production version
            "localhost" = {
              locations."/".root = "${release.frontend}/bin/frontend-exe.jsexe";
              locations."/app".proxyPass = "http://localhost:${webServerPort}";
            };
            "${domain}" = {
              forceSSL = enableSsl;
              enableACME = enableSsl;
              locations."/app/".proxyPass = "http://localhost:${webServerPort}";
              locations."/static/".root = "${release.frontend-static-files}";
              locations."/".root = "${release.frontend}/bin/frontend-exe.jsexe";
            };
            "www.${domain}" = {
              forceSSL = enableSsl;
              enableACME = enableSsl;
              locations."/".extraConfig = "return 301 $scheme://spook.app$request_uri;";
            };
          };
        };

        # Enables journald export for stackdriver on gce.
        # TODO - enable after upgrading nix channel by updating reflex-platform.
        # journaldriver = {
        #   enable = true;
        # };
      };

      systemd.services.spook = {
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          Type = "simple";
          User = "spookapp";
          Restart = "always";
          ExecStart = ''${release.backend}/bin/backend-exe \
            --sitePort=${webServerPort} \
            --dbHost=database \
            --dbPort=${toString dbPort} \
            --secureCookie \
            --youtubeKey=${youtubeKey}
          '';
        };
      };

      users.extraUsers = {
        spookapp = {};
      };
    };

  # TODO: Monitoring instance with prometheus, grafana.
}
