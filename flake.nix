{
  nixConfig.bash-prompt = "[uplc2c_parent:] ";
  description = "Parent flake for subfolders";
  inputs =
  {
    uplc2c.url = "path:./compiler";

    #CI integration
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, uplc2c, flake-compat, flake-compat-ci }:
  {
    packages.x86_64-linux = let pkgs = import nixpkgs { system = "x86_64-linux"; };
    in {
     architecture = pkgs.callPackage ./architecture/default.nix {};
     };

     ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
           flake = self;
           systems = [ "x86_64-linux" ];
         };
     defaultPackage.x86_64-linux = uplc2c.defaultPackage.x86_64-linux;
  };
}
