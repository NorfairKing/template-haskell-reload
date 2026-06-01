{
  description = "template-haskell-reload";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-26.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    nixpkgs-25_11.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-25_11
    , pre-commit-hooks
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
        ];
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.haskellPackages.template-haskell-reload;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs:
            let pkgs' = pkgsFor nixpkgs;
            in pkgs'.haskellPackages.template-haskell-reload;
          allNixpkgs = {
            inherit
              nixpkgs-25_11;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ "default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "template-haskell-reload-shell";
        packages = p: with p; [
          template-haskell-reload
        ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          niv
          zlib
          cabal-install
        ] ++ (with pre-commit-hooks;
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
