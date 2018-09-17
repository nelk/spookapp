{use-warp ? true}: (import ./reflex-platform {}).project ({ pkgs, ... }: {
  name = "spookapp";

  useWarp = use-warp;

  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  overrides = self: super: {
    inspection-testing = self.callHackage "inspection-testing" "0.2.0.1" {};
    generic-lens = self.callHackage "generic-lens" "0.5.1.0" {};
    # servant = self.callHackage "servant" "0.12.1" {};
  };


  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };

  shellToolOverrides = ghc: super: {
    stylish-haskell = ghc.callHackage "stylish-haskell" "0.9.0.2" {};

    # Want to use Ghcid v0.7 as it fixes many bugs and works with multi-package environments.
    # ghcid = pkgs.haskell.lib.justStaticExecutables super.ghcid;

    # WOW - that was hard to figure out!
    # ghcid = ghc.callHackage "ghcid" "0.7" {}; #pkgs.haskell.lib.justStaticExecutables super.ghcid;
    # v0.7
    ghcid = pkgs.haskell.lib.dontCheck (ghc.callCabal2nix "ghcid" (pkgs.fetchFromGitHub {
      owner = "ndmitchell";
      repo = "ghcid";
      rev = "e10e383fd083e064ff6d2f5b01347de275c10e3f";
      sha256 = "1yzi6a3274h4a22iq1s1sshch0lznka5apz4957kgn49lf5z825j";
    }) {
      # Needed to build ghcid.
      # extra = self.callHackage "extra" "1.6.6" {};
      # v1.6.6
      extra = pkgs.haskell.lib.dontCheck (ghc.callCabal2nix "extra" (pkgs.fetchFromGitHub {
        owner = "ndmitchell";
        repo = "extra";
        rev = "a965db5f93feb7fe86d60c8b0805d5410fc08f66";
        sha256 = "04qznkm2ala8prjx312wdizvli04ir552fszxj8a68m2w7i8yvzf";
      }) {});
    });
  };
})
