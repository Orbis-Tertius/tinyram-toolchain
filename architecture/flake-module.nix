{ self, ... }:
{
  perSystem = { config, system, pkgs, self', inputs', ... }: {
    packages.architecture = pkgs.callPackage ./. {};
  };
}
