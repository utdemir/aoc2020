let
sources = import ./nix/sources.nix;
pkgs = import sources.nixpkgs {};

haskellPackages = pkgs.haskell.packages.ghc8102.override {
  overrides = se: su: {

  };
};

in
pkgs.mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: with ps; [
      vector
      containers
      attoparsec
      bytestring
      text
      unordered-containers
      relude
    ]))
  ];
}
