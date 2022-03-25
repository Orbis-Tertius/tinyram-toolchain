{
  nixConfig.bash-prompt = "[uplc2c_parent:] ";
  description = "Parent flake for subfolders";
  inputs =
  {
    uplc2c.url = "path:./compiler";
  };
  outputs = { self, nixpkgs, uplc2c }:
  {
    packages.x86_64-linux = let pkgs = import nixpkgs { system = "x86_64-linux"; };
    in {
      architecture = pkgs.callPackage ./architecture/default.nix {};
     };

     herculesCI.ciSystems = [ "x86_64-linux" ];
     defaultPackage.x86_64-linux = uplc2c.defaultPackage.x86_64-linux;
  };
}
