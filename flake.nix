{
  description = "Tmux Client";

  inputs = {
    hix.url = git+https://git.tryp.io/tek/hix;
    prelate.url = git+https://git.tryp.io/tek/prelate;
    polysemy-conc.url = git+https://git.tryp.io/tek/polysemy-conc;
  };

  outputs = { hix, prelate, polysemy-conc, ... }:
  let
    overrides = { hackage, source, unbreak, pkgs, system, buildInputs, notest, ... }: {
      chiasma-test = buildInputs [pkgs.tmux pkgs.xterm];
      exon = hackage "0.5.0.0" "1i5r4ixmxwna3b36hz254jgkm3wbp6iggs71igfhzc9fym1z10hv";
      polysemy = hackage "1.7.1.0" "0qwli1kx3hk68hqsgw65mk81bx0djw1wlk17v8ggym7mf3lailyc";
      polysemy-conc = source.package polysemy-conc "conc";
      polysemy-plugin = hackage "0.4.3.0" "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
      polysemy-process = source.package polysemy-conc "process";
      type-errors = notest;
    };
  in hix.lib.flake ({ config, lib, ... }: {
    base = ./.;
    packages = {
      chiasma = ./packages/chiasma;
      chiasma-test = ./packages/test;
    };
    main = "chiasma-test";
    inherit overrides;
    depsFull = [prelate];
    compat.enable = false;
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghci = {
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["ImpredicativeTypes"];
      preludePackage = "prelate";
      preludeModule = "Prelate";
    };
    ghcid.shellConfig = { buildInputs = [config.pkgs.tmux config.devGhc.pkgs.xterm]; };
  });
}
