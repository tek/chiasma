{
  description = "Tmux Client";

  inputs = {
    hix.url = github:tek/hix;
    incipit.url = github:tek/incipit;
    old.url = github:NixOS/nixpkgs/1db42b7fe3878f3f5f7a4f2dc210772fd080e205;
  };

  outputs = { hix, incipit, old, ... }:
  let
    overrides = { hackage, source, unbreak, pkgs, system, buildInputs, ... }:
    let
      oldPkgs = import old { inherit system; };
    in {
      chiasma = buildInputs [oldPkgs.tmux pkgs.xterm];
      exon = hackage "0.4.0.0" "098ym81pz8rv88kgf4fmwwh52gz3151j3zvmpmg0a535irajqmq1";
      flatparse = unbreak;
      polysemy = hackage "1.7.1.0" "0qwli1kx3hk68hqsgw65mk81bx0djw1wlk17v8ggym7mf3lailyc";
      polysemy-plugin = hackage "0.4.3.0" "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
    };
  in hix.lib.flake ({ config, lib, ... }: {
    base = ./.;
    packages = {
      chiasma = ./packages/chiasma;
      chiasma-test = ./packages/test;
    };
    main = "chiasma-test";
    inherit overrides;
    depsFull = [incipit];
    compat.enable = false;
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghci = {
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["ImpredicativeTypes"];
      preludePackage = "incipit";
      preludeModule = "Incipit";
    };
    ghcid.shellConfig =
      let oldPkgs = import old { inherit (config) system; };
      in { buildInputs = [oldPkgs.tmux config.devGhc.pkgs.xterm]; };
  });
}
