{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { buildexecutable = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "retrie"; version = "1.0.0.0"; };
      license = "MIT";
      copyright = "Copyright (c) Facebook, Inc. and its affiliates.";
      maintainer = "Andrew Farmer <anfarmer@fb.com>";
      author = "Andrew Farmer <anfarmer@fb.com>";
      homepage = "https://github.com/facebookincubator/retrie";
      url = "";
      synopsis = "A powerful, easy-to-use codemodding tool for Haskell.";
      description = "Retrie is a tool for codemodding Haskell. Key goals include:\n\n* Speed: Efficiently rewrite in large (>1 million line) codebases.\n* Safety: Avoids large classes of codemod-related errors.\n* Ease-of-use: Haskell syntax instead of regular expressions. No hand-rolled AST traversals.\n\nThis package provides a command-line tool (@retrie@) and a library\n(\"Retrie\") for making equational edits to Haskell code.\n\nPlease see the [README](#readme) for examples and usage.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "CHANGELOG.md"
        "CODE_OF_CONDUCT.md"
        "CONTRIBUTING.md"
        "README.md"
        "tests/inputs/*.custom"
        "tests/inputs/*.test"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghc-exactprint" or (errorHandler.buildDepError "ghc-exactprint"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."random-shuffle" or (errorHandler.buildDepError "random-shuffle"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        modules = [
          "Retrie"
          "Retrie/AlphaEnv"
          "Retrie/CPP"
          "Retrie/Context"
          "Retrie/Debug"
          "Retrie/ExactPrint"
          "Retrie/ExactPrint/Annotated"
          "Retrie/Expr"
          "Retrie/Fixity"
          "Retrie/FreeVars"
          "Retrie/GHC"
          "Retrie/GroundTerms"
          "Retrie/Monad"
          "Retrie/Options"
          "Retrie/PatternMap/Bag"
          "Retrie/PatternMap/Class"
          "Retrie/PatternMap/Instances"
          "Retrie/Pretty"
          "Retrie/Quantifiers"
          "Retrie/Query"
          "Retrie/Replace"
          "Retrie/Rewrites"
          "Retrie/Rewrites/Function"
          "Retrie/Rewrites/Patterns"
          "Retrie/Rewrites/Rules"
          "Retrie/Rewrites/Types"
          "Retrie/Run"
          "Retrie/Subst"
          "Retrie/Substitution"
          "Retrie/SYB"
          "Retrie/Types"
          "Retrie/Universe"
          "Retrie/Util"
          ];
        };
      exes = {
        "retrie" = {
          depends = [
            (hsPkgs."retrie" or (errorHandler.buildDepError "retrie"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
            ];
          buildable = if flags.buildexecutable then true else false;
          modules = [ "Fixity" ];
          hsSourceDirs = [ "bin" "hse" ];
          mainPath = [ "Main.hs" ] ++ [ "" ];
          };
        "demo" = {
          depends = [
            (hsPkgs."retrie" or (errorHandler.buildDepError "retrie"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
            ];
          buildable = if flags.buildexecutable then true else false;
          modules = [ "Fixity" ];
          hsSourceDirs = [ "demo" "hse" ];
          mainPath = [ "Main.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."retrie" or (errorHandler.buildDepError "retrie"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
            (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          modules = [
            "AllTests"
            "Annotated"
            "CPP"
            "Demo"
            "Dependent"
            "Exclude"
            "Fixity"
            "Golden"
            "GroundTerms"
            "Ignore"
            "ParseQualified"
            "Targets"
            "Util"
            ];
          hsSourceDirs = [ "tests" "hse" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }