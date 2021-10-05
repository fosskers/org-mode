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
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "org-mode"; version = "2.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2020 - 2021 Colin Woodbury";
      maintainer = "colin@fosskers.ca";
      author = "Colin Woodbury";
      homepage = "https://github.com/fosskers/org-mode";
      url = "";
      synopsis = "Parser for Emacs org-mode files.";
      description = "Parser for Emacs org-mode files.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "CHANGELOG.md"
        "test/simple.org"
        "test/test.org"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        modules = [ "Data/Org" ];
        hsSourceDirs = [ "lib" ];
        };
      tests = {
        "org-mode-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."org-mode" or (errorHandler.buildDepError "org-mode"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Test.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../org-mode; }