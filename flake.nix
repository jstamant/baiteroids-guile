{
  description = "Guile + Hoot arcade game dev shell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      devShells.x86_64-linux.default = pkgs.mkShell {
        packages = with pkgs; [
          guile
          guile-hoot
          gnumake
          zip
        ];

        shellHook = ''
          echo "Guile + Hoot dev environment loaded"
        '';
      };
    };
}
