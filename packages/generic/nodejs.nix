{ pkgs, userNpmDir ? "npm-packages", ... }:

let
  myNodeJs = pkgs.nodejs_latest;

in pkgs.buildEnv {
  name = "my-nodejs";
  paths = [ myNodeJs ];

  passthru = {
    userNpmDir = userNpmDir;
    userPath = "${userNpmDir}/bin";
  };
}
