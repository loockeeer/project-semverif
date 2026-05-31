{
  description = "Flake pour Programmation 2";
  inputs = {
      nixpkgs.url = "github:nixos/nixpkgs?ref=25.11";
  };
  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
    in
    {
      formatter.${system} = nixpkgs.legacyPackages.${system}.nixfmt-tree;
      devShells.${system}.default =
        let
          pkgs = import nixpkgs { system = system; };
          ocamlCtx = pkgs.ocaml-ng.ocamlPackages_5_1;
        in
        pkgs.mkShell {
          packages = [
		  	pkgs.mpfr
			ocamlCtx.camlidl
            ocamlCtx.lsp
            ocamlCtx.dune_3
            ocamlCtx.ocaml
            ocamlCtx.utop
            ocamlCtx.fix
            ocamlCtx.ocaml-lsp
            ocamlCtx.menhir
            ocamlCtx.menhirLib
            ocamlCtx.zarith
            ocamlCtx.ocamlformat
			ocamlCtx.apron
          ];
        };
    };
}
