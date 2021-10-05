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
      identifier = { name = "org-mode-lucid"; version = "1.6.1"; };
      license = "BSD-3-Clause";
      copyright = "2020 - 2021 Colin Woodbury";
      maintainer = "colin@fosskers.ca";
      author = "Colin Woodbury";
      homepage = "https://github.com/fosskers/org-mode";
      url = "";
      synopsis = "Lucid integration for org-mode.";
      description = "Lucid integration for org-mode.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "CHANGELOG.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."lucid" or (errorHandler.buildDepError "lucid"))
          (hsPkgs."org-mode" or (errorHandler.buildDepError "org-mode"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [ "Data/Org/Lucid" ];
        hsSourceDirs = [ "lib" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../org-mode-lucid; }