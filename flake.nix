{
  description = "A tmux client for Polysemy";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    hls.url = "github:haskell/haskell-language-server?ref=1.9.0.0";
    prelate.url = "git+https://git.tryp.io/tek/prelate";
  };

  outputs = { hix, hls, prelate, ... }: hix.lib.pro ({config, ...}: {
    hackage.versionFile = "ops/version.nix";
    depsFull = [prelate];
    main = "chiasma-test";
    compiler = "ghc925";

    overrides = { hackage, source, unbreak, pkgs, system, buildInputs, notest, ... }: {
      chiasma-test = buildInputs [pkgs.tmux pkgs.xterm];
      type-errors = notest;
    };

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Terminal";
        github = "tek/chiasma";
        extra-source-files = ["readme.md" "changelog.md"];
      };
      ghc-options = ["-fplugin=Polysemy.Plugin"];
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = "^>= 0.5.1";
        };
        module = "Prelate";
      };
      dependencies = ["polysemy" "polysemy-plugin"];
    };

    packages.chiasma = {
      src = ./packages/chiasma;

      cabal.meta.synopsis = "A tmux client for Polysemy";

      library = {
        enable = true;
        dependencies = [
          "attoparsec >= 0.13"
          "bytestring"
          "composition >= 1.0"
          "containers"
          "exon"
          "extra"
          "first-class-families"
          "lens >= 4"
          "path"
          "parsec"
          "parsers >= 0.12"
          "polysemy-conc >= 0.9"
          "polysemy-log >= 0.7"
          "polysemy-process >= 0.9"
          "polysemy-time >= 0.5"
          "prettyprinter >= 1.6"
          "prettyprinter-ansi-terminal >= 1.1"
          "random >= 1.1"
          "text"
          "transformers"
          "typed-process >= 0.2"
          "uuid >= 1.3"
        ];
      };

    };

    packages.chiasma-test = {
      src = ./packages/test;

      cabal.meta.synopsis = "Testing tools for chiasma";

      library = {
        enable = true;
        dependencies = [
          "bytestring"
          "chiasma"
          "chronos"
          "exon"
          "hedgehog"
          "path"
          "path-io"
          "polysemy-chronos"
          "polysemy-conc"
          "polysemy-log"
          "polysemy-process"
          "polysemy-time"
          "polysemy-test >= 0.6"
          "text"
          "typed-process >= 0.2"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "chiasma"
          "hedgehog"
          "lens >= 4"
          "polysemy-test >= 0.6"
          "tasty"
          "tasty-hedgehog"
        ];
      };

      tests.integration = {
        dependencies = [
          "chiasma"
          "chiasma-test"
          "hedgehog"
          "lens >= 4"
          "path-io"
          "polysemy-chronos"
          "polysemy-test >= 0.6"
          "tasty"
          "tasty-hedgehog"
        ];
      };

    };

    envs.dev.buildInputs = [config.pkgs.tmux config.pkgs.xterm];
    envs.hls.hls.package = hls.packages.${config.system}.haskell-language-server-925;
  });
}
