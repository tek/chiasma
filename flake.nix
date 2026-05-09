{
  description = "A tmux client for Polysemy";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = {hix, ...}: hix.lib.pro ({config, ...}: {
    compiler = "ghc912";
    ghcVersions = ["ghc98" "ghc910" "ghc912"];
    main = "chiasma-test";
    gen-overrides.enable = true;

    nixpkgs.default.source = {
      rev = "c6d65881c5624c9cae5ea6cedef24699b0c0a4c0";
      hash = "sha256-WNGcmeOZ8Tr9dq6ztCspYbzWFswr2mPebM9LpsfGxPk=";
    };

    packages.chiasma = {
      src = ./packages/chiasma;

      cabal.meta.synopsis = "A tmux client for Polysemy";

      library = {
        enable = true;
        dependencies = [
          "attoparsec"
          "bytestring"
          "composition"
          "containers"
          "exon"
          "first-class-families"
          "lens"
          "path"
          "parsec"
          "parsers"
          "polysemy-conc"
          "polysemy-log"
          "polysemy-process"
          "polysemy-time"
          "prettyprinter"
          "prettyprinter-ansi-terminal"
          "random"
          "text"
          "transformers"
          "typed-process"
          "uuid"
        ];
      };

    };

    packages.chiasma-test = {
      src = ./packages/test;

      cabal.meta.synopsis = "Testing tools for chiasma";
      buildInputs = pkgs: [pkgs.tmux pkgs.xterm pkgs.bashInteractive];

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
          "polysemy-test"
          "text"
          "typed-process"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "chiasma"
          "hedgehog"
          "lens"
          "polysemy-process"
          "polysemy-test"
          "tasty"
          "tasty-hedgehog"
        ];
      };

      tests.integration = {
        dependencies = [
          "chiasma"
          "chiasma-test"
          "hedgehog"
          "path-io"
          "polysemy-chronos"
          "polysemy-test"
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
        package = "prelate";
        module = "Prelate";
      };
      dependencies = ["polysemy" "polysemy-plugin"];
    };

    managed = {
      enable = true;
      latest.enable = true;
      lower.enable = true;
      latest = {
        compiler = "ghc912";
        envs = {
          verbatim = {
            overrides = {nodoc, ...}: {
              __all = nodoc;
            };
            libraryProfiling = false;
          };
        };
      };
      lower = {
        compiler = "ghc94";
        envs = {
          solverOverrides = {hackage, ...}: {
            bytesmith = hackage "0.3.11.0" "1z083sx6gbrsnlwfhiwcpym1kwyxmjhwrngsi3axa7bmg5c5za5c";
          };
          verbatim = {
            overrides = {hackage, nodoc, ...}: {
              __all = nodoc;
              bytesmith = hackage "0.3.11.0" "1z083sx6gbrsnlwfhiwcpym1kwyxmjhwrngsi3axa7bmg5c5za5c";
            };
            libraryProfiling = false;
          };
        };
      };
    };

    hackage.repos."hackage.haskell.org".user = "tek";

    package-sets = {

      ghc912.overrides = {hackage, jailbreak, ...}: {
        incipit = jailbreak;
        polysemy-chronos = jailbreak;
        polysemy-conc = jailbreak;
        polysemy-log = jailbreak;
        polysemy-process = jailbreak;
        polysemy-resume = jailbreak;
        polysemy-time = jailbreak;
      };

      ghc98.overrides = {jailbreak, hackage, ...}: {
        chronos = jailbreak;
      };

    };

    envs.dev.package-set.extends = "ghc912";

    envs.min.package-set.extends = "ghc912";

    overrides = {jailbreak, hackage, force, unbreak, notest, source, super, overrideAttrs, ...}: {
      prelate = hackage "0.9.0.2" "0pbfaxl06gxkzqbj6f96jf3xv0g59cwawvw4xsw2h8bnszq43zyz";
      polysemy-test = hackage "0.11.0.1" "0faajcwslgkjigakimz5sxvcd92p8vdzafway8js8622jmprjqjb";
    };

    envs.dev.buildInputs = [config.pkgs.tmux config.pkgs.xterm];

    internal.hixCli.dev = true;

  });
}
