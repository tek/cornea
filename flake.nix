{
  description = "classy optical monadic state";

  inputs.hix.url = github:tek/hix;

  outputs = { hix, ... }:
  hix.flake {
    base = ./.;
    packages.cornea = ./packages/cornea;
    versionFile = "ops/hpack/packages/cornea.yaml";
  };
}
