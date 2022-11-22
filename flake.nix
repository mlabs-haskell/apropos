{
  description = "apropos";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, flake-compat, flake-compat-ci }:
    let
      supportedSystems =
        [ "x86_64-linux" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      compiler-nix-name = "ghc924";

      fourmoluFor = system: (nixpkgsFor system).haskell-nix.tool "ghc924" "fourmolu" { };

      projectFor = system:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;

          fakeSrc = pkgs.runCommand "real-source" { } ''
            cp -rT ${self} $out
            chmod u+w $out/cabal.project
          '';
        in
        (nixpkgsFor system).haskell-nix.cabalProject' {
          inherit compiler-nix-name;
          src = fakeSrc.outPath;
          cabalProjectFileName = "cabal.project";
          modules = [{ packages = { }; }];
          shell = {
            withHoogle = true;

            tools.haskell-language-server = { };

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs =
              [
                pkgs.cabal-install
                pkgs.hlint
                (fourmoluFor system)
                pkgs.nixpkgs-fmt
                pkgs.haskellPackages.cabal-fmt
                pkgs.haskellPackages.apply-refact
                pkgs.fd
              ];
            additional = ps: [
            ];
          };
          sha256map = {
          };
        };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [ self.devShell.${system}.nativeBuildInputs ];
          } ''
          cd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'
          make format_check cabalfmt_check nixpkgsfmt_check lint
          mkdir $out
        '';
    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system:
        self.flake.${system}.checks // {
          formatCheck = formatCheckFor system;
        });
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system};
          } "touch $out");
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
