{
  description = "A tmux client for Polysemy";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = {hix, ...}: hix.lib.pro ({config, ...}: {
    main = "chiasma-test";
    gen-overrides.enable = true;

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
      buildInputs = pkgs: [pkgs.tmux pkgs.xterm];

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

    cabal = {
      language = "GHC2021";
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
          version = ">= 0.6 && < 0.9";
        };
        module = "Prelate";
      };
      dependencies = ["polysemy" "polysemy-plugin"];
    };

    managed = {
      enable = true;
      latest.enable = false;
    };

    hackage.repos."hackage.haskell.org" = {
      user = "tek";
    };

    overrides = {jailbreak, force, unbreak, notest, source, ...}: {
      exon = jailbreak;
      fuzzyfind = force;
      incipit = jailbreak;
      incipit-base = jailbreak;
      incipit-core = jailbreak;
      polysemy-chronos = jailbreak;
      polysemy-conc = jailbreak;
      polysemy-http = jailbreak;
      polysemy-log = jailbreak;
      polysemy-process = jailbreak;
      polysemy-resume = jailbreak;
      polysemy-test = jailbreak;
      polysemy-time = jailbreak;
      prelate = jailbreak;
      streamly = force;
      unicode-data = notest;
      zeugma = jailbreak;
    };

    envs = {

      ghc98.overrides = {jailbreak, ...}: {
        chronos = jailbreak;
      };

    };

    envs.dev.buildInputs = [config.pkgs.tmux config.pkgs.xterm];

    internal.hixCli.dev = true;

  });
}
