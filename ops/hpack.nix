{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = import ./version.nix;
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2022 Torsten Schmits";
    category = "Terminal";
    build-type = "Simple";
    github = "tek/chiasma";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
    "-fplugin=Polysemy.Plugin"
  ];

  dependencies = [
      { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
      { name = "prelate"; version = ">= 0.2"; mixin = ["(Prelate as Prelude)" "hiding (Prelate)"]; }
      "polysemy"
      "polysemy-plugin"
    ];

  project = name: doc: merge (meta // { library = paths name; } // options) {
    inherit name;
    description = "See https://hackage.haskell.org/package/${name}/docs/${doc}.html";
    library = {
      source-dirs = "lib";
      inherit dependencies;
    };
    default-extensions = config.ghci.extensions;
  };

  exe = name: dir: merge (paths name // {
    main = "Main.hs";
    source-dirs = dir;
    inherit dependencies;
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  chiasma = merge (project "chiasma" "Chiasma") {
    synopsis = "A tmux client for Polysemy";
    library.dependencies = [
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

  chiasma-test = merge (project "chiasma-test" "Chiasma-Test") {
    synopsis = "A tmux client for Polysemy";
    library.dependencies = [
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
    tests.chiasma-test-unit = exe "chiasma-test" "test" {
      dependencies = [
        "chiasma"
        "hedgehog"
        "lens >= 4"
        "polysemy-test >= 0.6"
        "tasty"
        "tasty-hedgehog"
      ];
    };
    tests.chiasma-test-integration = exe "chiasma-test" "integration" {
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

}
