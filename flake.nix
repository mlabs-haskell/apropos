{
  description = "apropos";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, flake-compat, flake-compat-ci }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs { inherit system; overlays = [ haskell-nix.overlay ]; inherit (haskell-nix) config; };

      projectFor = system:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;

          fakeSrc = pkgs.runCommand "real-source" {} ''
            cp -rT ${self} $out
            chmod u+w $out/cabal.project
          '';
        in
        (nixpkgsFor system).haskell-nix.cabalProject' {
          src = fakeSrc.outPath;
          compiler-nix-name = "ghc8107";
          cabalProjectFileName = "cabal.project";
          modules = [{
            packages = {
            };
          }];
          shell = {
            withHoogle = true;

            tools.haskell-language-server = { };

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [ pkgs.cabal-install pkgs.hlint pkgs.haskellPackages.fourmolu ];

            additional = ps: [
            ];
          };
          sha256map = {
            "https://github.com/Geometer1729/digraph"."d4dfec22f6a6eb646dcfa9591eaca0a9be88d260" = "sha256-ytQkJ18tYs13rt66s4jdbaGa5mLNEIerF8u24PvyPLA=";
          };
        };
    in
    {
      ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
      };

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake {});

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system: self.flake.${system}.checks);
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          nativeBuildInputs = builtins.attrValues self.checks.${system};
        } "touch $out"
      );
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
