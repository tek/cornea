{
  description = "classy optical monadic state";

  inputs.hix.url = github:tek/hix;

  outputs = { hix, ... }:
  hix.lib.flake {
    base = ./.;
    packages.cornea = ./packages/cornea;
    overrides.all = { hackage, ... }: {
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
    };
    devGhc.compiler = "ghc902";
    versionFile = "ops/hpack/packages/cornea.yaml";
  };
}
