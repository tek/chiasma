{
  description = "Tmux Client";

  inputs = {
    hix.url = git+https://git.tryp.io/tek/hix;
    prelate.url = git+https://git.tryp.io/tek/prelate;
  };

  outputs = { hix, prelate, ... }:
  let
    overrides = { hackage, source, unbreak, pkgs, system, buildInputs, notest, ... }: {
      chiasma-test = buildInputs [pkgs.tmux pkgs.xterm];
      type-errors = notest;
    };
  in hix.lib.pro ({ config, lib, ... }: {
    packages = {
      chiasma = ./packages/chiasma;
      chiasma-test = ./packages/test;
    };
    main = "chiasma-test";
    devGhc.compiler = "ghc902";
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
