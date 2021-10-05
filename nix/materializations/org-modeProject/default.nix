{
  pkgs = hackage:
    {
      packages = {
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "lucid".revision = (((hackage."lucid")."2.9.12.1").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "blaze-builder".revision = (((hackage."blaze-builder")."0.4.2.1").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.11").revisions).default;
        "ansi-terminal".flags.example = false;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        "call-stack".revision = (((hackage."call-stack")."0.4.0").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "async".revision = (((hackage."async")."2.2.3").revisions).default;
        "async".flags.bench = false;
        "process".revision = (((hackage."process")."1.6.13.2").revisions).default;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.16.1.0").revisions).default;
        "optparse-applicative".flags.process = true;
        "scientific".revision = (((hackage."scientific")."0.3.7.0").revisions).default;
        "scientific".flags.integer-simple = false;
        "scientific".flags.bytestring-builder = false;
        "tasty-hunit".revision = (((hackage."tasty-hunit")."0.10.0.3").revisions).default;
        "megaparsec".revision = (((hackage."megaparsec")."9.2.0").revisions).default;
        "megaparsec".flags.dev = false;
        "base".revision = (((hackage."base")."4.14.3.0").revisions).default;
        "rts".revision = (((hackage."rts")."1.0.1").revisions).default;
        "text".revision = (((hackage."text")."1.2.4.1").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.14.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "parser-combinators".revision = (((hackage."parser-combinators")."1.2.1").revisions).default;
        "parser-combinators".flags.dev = false;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "wcwidth".revision = (((hackage."wcwidth")."0.0.2").revisions).default;
        "wcwidth".flags.split-base = true;
        "wcwidth".flags.cli = false;
        "bytestring".revision = (((hackage."bytestring")."0.10.12.0").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3.1").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "containers".revision = (((hackage."containers")."0.6.5.1").revisions).default;
        "tagged".revision = (((hackage."tagged")."0.8.6.1").revisions).default;
        "tagged".flags.deepseq = true;
        "tagged".flags.transformers = true;
        "primitive".revision = (((hackage."primitive")."0.7.2.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.7").revisions).default;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.three = false;
        "transformers-compat".flags.mtl = true;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.7").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "unbounded-delays".revision = (((hackage."unbounded-delays")."0.1.1.1").revisions).default;
        "tasty".revision = (((hackage."tasty")."1.2.3").revisions).default;
        "tasty".flags.clock = true;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.6").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.3.0").revisions).default;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.random-initial-seed = false;
        "clock".revision = (((hackage."clock")."0.8.2").revisions).default;
        "clock".flags.llvm = false;
        "mmorph".revision = (((hackage."mmorph")."1.2.0").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.1").revisions).default;
        };
      compiler = {
        version = "8.10.7";
        nix-name = "ghc8107";
        packages = {
          "binary" = "0.8.8.0";
          "ghc-prim" = "0.6.1";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.3.0";
          "template-haskell" = "2.16.0.0";
          "pretty" = "1.1.3.6";
          "process" = "1.6.13.2";
          "base" = "4.14.3.0";
          "rts" = "1.0.1";
          "text" = "1.2.4.1";
          "mtl" = "2.2.2";
          "time" = "1.9.3";
          "unix" = "2.7.2.2";
          "bytestring" = "0.10.12.0";
          "containers" = "0.6.5.1";
          "directory" = "1.3.6.0";
          "ghc-boot-th" = "8.10.7";
          "filepath" = "1.4.2.1";
          "deepseq" = "1.4.4.0";
          "transformers" = "0.5.6.2";
          "stm" = "2.5.0.1";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        org-mode = ./.plan.nix/org-mode.nix;
        org-mode-lucid = ./.plan.nix/org-mode-lucid.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "org-mode" = { flags = {}; };
          "org-mode-lucid" = { flags = {}; };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "tagged".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "lucid".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "org-mode".components.tests."org-mode-test".planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "mmorph".components.library.planned = lib.mkOverride 900 true;
          "clock".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          "colour".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "optparse-applicative".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "scientific".components.library.planned = lib.mkOverride 900 true;
          "async".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "call-stack".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "ansi-terminal".components.library.planned = lib.mkOverride 900 true;
          "org-mode".components.library.planned = lib.mkOverride 900 true;
          "blaze-builder".components.library.planned = lib.mkOverride 900 true;
          "integer-logarithms".components.library.planned = lib.mkOverride 900 true;
          "ansi-wl-pprint".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "case-insensitive".components.library.planned = lib.mkOverride 900 true;
          "wcwidth".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "unordered-containers".components.library.planned = lib.mkOverride 900 true;
          "parser-combinators".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "tasty-hunit".components.library.planned = lib.mkOverride 900 true;
          "megaparsec".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "unbounded-delays".components.library.planned = lib.mkOverride 900 true;
          "tasty".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "org-mode-lucid".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "primitive".components.library.planned = lib.mkOverride 900 true;
          "transformers-compat".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }