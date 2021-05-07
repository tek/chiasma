{
  description = "tmux api";

  inputs.hix.url = github:tek/hix;

  outputs = { hix, ... }:
  let
    overrides = { hackage, only, ... }:
    {
      posix-pty = only "8.6.5" (hackage "0.2.1.1" "0rwb7fj7134lc04dv9vn4j6nq0vadl2qhzlz63pd0an9vqmxw685");
      cornea = hackage "0.4.0.0" "1w9rkf6f861kknkskywb8fczlk7az8m56i3hvmg6a5inpvqf6p7i";
    };
  in hix.flake {
    base = ./.;
    packages.chiasma = ./packages/chiasma;
    inherit overrides;
    compatOverrides = overrides;
    versionFile = "ops/hpack/packages/chiasma.yaml";
  };
}
