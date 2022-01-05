let
  packages = import ./.;
  inherit (packages) pkgs cardano-dex-backend;
  inherit (cardano-dex-backend) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with cardano-dex-backend; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ];

    buildInputs = with cardano-dex-backend; [
      pkgs.rdkafka
    ];
  }
