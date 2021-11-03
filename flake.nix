{
  description = "Tmux Client";

  inputs = {
    hix.url = github:tek/hix;
    incipit.url = github:tek/incipit;
    polysemy-conc.url = github:tek/polysemy-conc;
    polysemy-time.url = github:tek/polysemy-time;
    old.url = github:NixOS/nixpkgs/1db42b7fe3878f3f5f7a4f2dc210772fd080e205;
  };

  outputs = { hix, incipit, polysemy-conc, polysemy-time, old, ... }:
  let
    overrides = { hackage, source, unbreak, pkgs, system, transform_, ... }:
    let
      oldPkgs = import old { inherit system; };
    in {
      chiasma = transform_ (drv: drv.overrideAttrs (prev: {
        buildInputs = prev.buildInputs ++ [oldPkgs.tmux pkgs.rxvt-unicode];
      }));
      cornea = hackage "0.4.0.1" "00f8fsizqw52001nsd6x9x6amsbh6qqiyy26knfjwm91dcyaanmn";
      exon = hackage "0.3.0.0" "0jgpj8818nhwmb3271ixid38mx11illlslyi69s4m0ws138v6i18";
      flatparse = unbreak;
      polysemy = hackage "1.7.1.0" "0qwli1kx3hk68hqsgw65mk81bx0djw1wlk17v8ggym7mf3lailyc";
      polysemy-chronos = source.package polysemy-time "chronos";
      polysemy-plugin = hackage "0.4.3.0" "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
      polysemy-conc = source.package polysemy-conc "conc";
      polysemy-process = source.package polysemy-conc "process";
      polysemy-time = source.package polysemy-time "time";
    };
  in hix.lib.flake ({ config, ... }: {
    base = ./.;
    packages = {
      chiasma = ./packages/chiasma;
      chiasma-test = ./packages/test;
    };
    main = "chiasma-test";
    inherit overrides;
    deps = [incipit];
    devGhc.compiler = "ghc8107";
    compat.enable = false;
    hackage.versionFile = "ops/hpack/packages/chiasma.yaml";
    ghci = {
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["ImpredicativeTypes"];
      preludePackage = "incipit";
    };
    ghcid.shellConfig =
      let oldPkgs = import old { inherit (config) system; };
      in { buildInputs = [oldPkgs.tmux]; };
  });
}
